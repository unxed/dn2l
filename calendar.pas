{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source
//
//  Version history:
//
//  CALENDAR.PAS from DN OSP 3.7.0
//  lite edition for DN/2 2.09 by Max Piwamoto at 30-Jan-2006
//  lite edition for DN/2 2.09 by AK155 at 02-Feb-2006
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}

unit Calendar;

{$F+,O+,X+,S-}

interface

uses
  Objects, Defines, Drivers, Objects2, Streams, Views
  ;

type

  PCalendarView = ^TCalendarView;
  TCalendarView = object(TView)
    Year: AWord;
    Month: Byte;
    Days: Byte;
    CurYear: AWord;
    CurMonth: Byte;
    CurDay: Byte;
    FDay: Byte;
    RomanEaster: integer;
    OrthodoxEaster: integer;
    constructor Init(Bounds: TRect);
    constructor Load(var S: TStream);
    destructor Done; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure Draw; virtual;
    procedure Store(var S: TStream);
    procedure NextYear;
    procedure PrevYear;
    procedure NextMonth;
    procedure PrevMonth;
    procedure NextWeek;
    procedure PrevWeek;
    procedure CurDate;
    function GetDateText: String;
  private
    procedure SetDay(ADay: Byte);
    procedure _NextDay;
    procedure _PrevDay;
    procedure FixNext;
    procedure FixPrev;
    procedure YearChanged;
    end;

  PCalendarWindow = ^TCalendarWindow;
  TCalendarWindow = object(TWindow)
    CalendarView: PCalendarView;
    constructor Init;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure Awaken; virtual;
    function GetTitle(MaxSize: integer): TTitleStr; virtual;
    destructor Done; virtual;
    function GetPalette: PPalette; virtual;
    end;

procedure InsertCalendar;

implementation

uses
  Commands, DNApp, Dos, Dialogs, advance1, DNHelp, DnIni, advance, xTime,
  advance7, dnutil
  ;

var
  SundayFirst: Boolean;

const

  MinYear = 1;
  MaxYear = High(AWord);

  CalendarViewWidth = 28;
  CalendarViewHeight = 8;

  GregorianThreshold = 1582; { 5..14 October skipped }

  { TCalendarWindow object }

const

  CCalendarWindow = #32#33#34#35#36#205
  +#206#207#208#209
  +#210#211;

  { Palette layout }
  {  1 = Frame passive }
  {  2 = Frame active }
  {  3 = Frame icon }
  {  4 = ScrollBar page area }
  {  5 = ScrollBar controls }

  {  6 = StaticText/Category 0 }

  {  7 = Category 1 }
  {  8 = Category 2 }
  {  9 = Category 3 }
  { 10 = Category 4 }

  { 11 = Header     }
  { 12 = Info panel }

const
  Calend: PCalendarWindow = nil;

  (*****************************************************************
 *
 * Calendar Tools
 *
 *****************************************************************)

  (*****************************************************************
 *
 * FUNCTION:    GetEasterDate
 *
 * PURPOSE:     This function returns Easter Sunday day and month
 *              for a specified year and method.
 *
 * INPUTS:      Year   - Any year between 326 and 4099.
 *              Method - 1 = the original calculation based on the
 *                           Julian calendar
 *                       2 = the original calculation, with the
 *                           Julian date converted to the
 *                           equivalent Gregorian calendar
 *                       3 = the revised calculation based on the
 *                           Gregorian calendar
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     0          - Error; invalid arguments
 *              Hi(result) - Month
 *              Lo(result) - Day
 *
 * ORIGINAL NOTES:
 *
 *              This algorithm is an arithmetic interpretation
 *              of the 3 step Easter Dating Method developed
 *              by Ron Mallen 1985, as a vast improvement on
 *              the method described in the Common Prayer Book
 *
 *              Published Australian Almanac 1988
 *              Refer to this publication, or the Canberra Library
 *              for a clear understanding of the method used
 *
 *              Because this algorithm is a direct translation of
 *              the official tables, it can be easily proved to be
 *              100% correct
 *
 *              It's free! Please do not modify code or comments!
 *
 * HISTORY:
 *
 *****************************************************************)

function GetEasterDate(Year: word; Method: word): word;
  var
    FirstDig: integer; { intermediate results }
    Remain19: integer;
    temp: integer;
    tA: integer; { table A to E results }
    tB: integer;
    tC: integer;
    tD: integer;
    tE: integer;
    d: integer; { Easter Sunday day    }
  begin
  GetEasterDate := 0; { default invalid result    }
  if  (Year < 326) or (Year > 4099) then
    { Easter dates are valid    }
    Exit; { for years 326..4099       }
  if  (Year < 1583) and (Method <> 1) then
    { Wester or Orthodox Easter }
    Exit; { is valid since 1583       }
  FirstDig := Year div 100; { first two digits of year  }
  Remain19 := Year mod 19; { remainder of year / 19    }
  if  (Method = 1) or (Method = 2) then
    begin
    { calculate PFM date }
    tA := ((225-11*Remain19) mod 30)+21;
    { find the next Sunday }
    tB := (tA-19) mod 7;
    tC := (40-FirstDig) mod 7;
    temp := Year mod 100;
    tD := (temp+temp div 4) mod 7;
    tE := ((20-tB-tC-tD) mod 7)+1;
    d := tA+tE;
    if Method = 2 then
      begin
      { convert Julian to Gregorian date }
      { 10 days were skipped in the Gregorian calendar from 5-14 Oct 1582 }
      temp := 10;
      { only 1 in every 4 century years are leap years in the Gregorian   }
      { calendar (every century is a leap year in the Julian calendar)    }
      if Year > 1600 then
        temp := temp+FirstDig-16-((FirstDig-16) div 4);
      d := d+temp;
      end;
    end
  else if Method = 3 then
    begin
    { calculate PFM date }
    temp := (FirstDig-15) div 2+202-11*Remain19;
    if  (FirstDig > 26) then
      temp := temp-1;
    if  (FirstDig > 38) then
      temp := temp-1;
    if FirstDig in [21, 24, 25, 33, 36, 37] then
      temp := temp-1;
    temp := temp mod 30;
    tA := temp+21;
    if temp = 29 then
      tA := tA-1;
    if  (temp = 28) and (Remain19 > 10) then
      tA := tA-1;
    { find the next Sunday }
    tB := (tA-19) mod 7;
    tC := (40-FirstDig) mod 4;
    if tC = 3 then
      tC := tC+1;
    if tC > 1 then
      tC := tC+1;
    temp := Year mod 100;
    tD := (temp+temp div 4) mod 7;
    tE := ((20-tB-tC-tD) mod 7)+1;
    d := tA+tE;
    end
  else
    begin
    Exit;
    end;
  if d > 61 then
    begin
    { when the original calculation is converted to the Gregorian }
    { calendar, Easter Sunday can occur in May                    }
    GetEasterDate := $0500+(d-61);
    end
  else if d > 31 then
    begin
    GetEasterDate := $0400+(d-31);
    end
  else
    begin
    GetEasterDate := $0300+d;
    end;
  end { GetEasterDate };

(*****************************************************************
 *
 * FUNCTION:    IsLeapYear
 *
 * PURPOSE:     This function checks if given year is a leap year.
 *
 * INPUTS:      Year - A year to check.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     True  - The given year is a leap year.
 *              False - The given year is not a leap year.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function IsLeapYear(Year: word): Boolean;
  begin
  if Year > GregorianThreshold then
    IsLeapYear := ((Year mod 4) = 0)
             and (((Year mod 100) <> 0) or ((Year mod 400) = 0))
  else
    IsLeapYear := (Year mod 4) = 0;
  end;

(*****************************************************************
 *
 * FUNCTION:    DaysInMonth
 *
 * PURPOSE:     This function returns number of days in given
 *              month. The year is important to calculating
 *              February days.
 *
 * INPUTS:      Month - A month number (1..12).
 *              Year  - A year.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     Number of days for selected month.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)
const
  DefaultDaysInMonth: array[1..12] of Word =
    (31, 28, 31, 30, 31, 30,
     31, 31, 30, 31, 30, 31);

function DaysInMonth(Month, Year: word): word;
  begin
  Result := DefaultDaysInMonth[Month];
  if (Month=2) and IsLeapYear(Year) then
    Inc(Result);
  end;

(*****************************************************************
 *
 * FUNCTION:    DayOfYear
 *
 * PURPOSE:     This function return the day number in a year.
 *
 * INPUTS:      Day   - A day in month.
 *              Month - A month in a year (1..12).
 *              Year  - A year.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     The day number in a year:
 *                1   - 1 January
 *                ...
 *                365 - 31 December (ordinary year)
 *                366 - 31 December (leap year)
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function DayOfYear(Day, Month, Year: word): word;
  begin
  if  (Year = GregorianThreshold)
         and (((Month = 10) and (Day > 14)) or (Month > 10))
  then
    Dec(Day, 10);
  while Month > 1 do
    begin
    Dec(Month);
    Inc(Day, DaysInMonth(Month, Year));
    end;
  DayOfYear := Day;
  end;

(*****************************************************************
 *
 * FUNCTION:    JulianDay
 *
 * PURPOSE:     This function calculates day number since 1.1.1.
 *
 * INPUTS:      Day   - A day in month.
 *              Month - A month in a year (1..12).
 *              Year  - A year.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     1   = 1.1.1
 *              365 = 31.12.1
 *              366 = 1.1.2
 *              ...
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function JulianDay(Day, Month, Year: word): LongInt;
  begin
  Dec(Year);
  Result := LongInt(365)*Year+Year div 4;
  if Year >= GregorianThreshold then
    Dec(Result, Year div 100-Year div 400-2);
  Inc(Result, DayOfYear(Day, Month, Year+1));
  end;

(*****************************************************************
 *
 * FUNCTION:    DayOfWeek
 *
 * PURPOSE:     This function returns weekday for given date.
 *
 * INPUTS:      Day   - A day in month.
 *              Month - A month in a year (1..12).
 *              Year  - A year.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     0 - Sunday
 *              1 - Monday
 *              2 - Tuesday
 *              3 - Wednesday
 *              4 - Thursday
 *              5 - Friday
 *              6 - Saturday
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function DayOfWeek(Day, Month, Year: word): word;
  begin
  DayOfWeek := (JulianDay(Day, Month, Year)+5) mod 7;
  end;

(*****************************************************************
 *
 * FUNCTION:    WeekNumber
 *
 * PURPOSE:     This function returns week number in a year for
 *              a given day.
 *
 * INPUTS:      Day         - A day in month.
 *              Month       - A month in a year (1..12).
 *              Year        - A year.
 *              SundayFirst - Is a Sunday first day of week?
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     Week number from 1 to 53.
 *
 * NOTES:       1) What Is the Week Number?
 *                 -----------------------------
 *                 International standard IS-8601 (mentioned in
 *                 section 5.5) assigns a number to each week of
 *                 the year. A week that lies partly in one year
 *                 and partly in another is assigned a number in
 *                 the year in which most of its days lie. This
 *                 means that
 *
 *                   Week 1 of any year is the week that contains
 *                   4 January,
 *
 *                 or equivalently
 *
 *                   Week 1 of any year is the week that contains
 *                   the first Thursday in January.
 *
 *                 Most years have 52 weeks, but years that start
 *                 on a Thursday and leap years that start on a
 *                 Wednesday have 53 weeks.
 *
 *              2) According to note 1) Sunday is a last day in
 *                 a week. But there is SundayFirst argument that
 *                 calculates weeks with Sunday as a first day of
 *                 week. This is not compatible with ISO standard
 *                 but is compatible with "week-rows" in DN's
 *                 calendar :).
 *
 * HISTORY:
 *
 *****************************************************************)

function WeekNumber(Day, Month, Year: word; SundayFirst: Boolean): word;
  var
    w: word;
    d: word;
  begin
  d := DayOfYear(Day, Month, Year); { current day in a year }
  w := DayOfWeek(Day, Month, Year);
  if  (w = 0) and not SundayFirst then
    w := 7;
  { look for thursday and its year in the week }
  if w < 4 then
    begin { forward  }
    Inc(d, 4-w);
    w := DayOfYear(31, 12, Year);
    if d > w then
      begin
      Inc(Year);
      Dec(d, w);
      end;
    end
  else if w > 4 then
    begin { backward }
    Dec(w, 4);
    if d <= w then
      begin
      Dec(Year);
      Inc(d, DayOfYear(31, 12, Year));
      end;
    Dec(d, w);
    end;
  { look for the first thurstay in the year }
  w := (11-DayOfWeek(1, 1, Year)) mod 7;
  { calculate week number }
  WeekNumber := (d-w) div 7+1;
  end { WeekNumber };

(*****************************************************************
 *
 * TCalendarWindow
 *
 *****************************************************************)

constructor TCalendarWindow.Init;
  var
    R: TRect;
  begin
  if CalSundayFirst < 2
  then
    SundayFirst := (CalSundayFirst = 1)
  else
    SundayFirst := not ((UpStrg(ActiveLanguage) = 'RUSSIAN') or
          (UpStrg(ActiveLanguage) = 'UKRAIN'));
  R.Assign(1, 1, CalendarViewWidth+3, CalendarViewHeight+3);
  inherited Init(R, GetString(dlcTitle), 0);
  Options := Options or ofVersion20;
  GrowMode := 0;
  Flags := Flags and not (wfZoom+wfGrow) or (wfMove+wfClose);
  { Not resizeable }
  MoveTo(25, 7);
  GetExtent(R);
  R.Grow(-1, -1);
  CalendarView := New(PCalendarView, Init(R));
  Insert(CalendarView);
  Calend := @Self;
  HelpCtx := hcCalendar;
  end { TCalendarWindow.Init };

function TCalendarWindow.GetTitle(MaxSize: integer): TTitleStr;
  begin
  GetTitle := GetString(dlcTitle);
  end;

procedure TCalendarWindow.HandleEvent(var Event: TEvent);
  var
    S: String;
  begin
  case Event.What of

    evCommand:
      repeat
        case Event.Command of
          cmGetName:
            PString(Event.InfoPtr)^:= GetString(dlcTitle);
          else {case}
            break; { skip ClearEvent without using goto statement }
        end {case};
        ClearEvent(Event);
      until True;

    evKeyDown:
      begin
      case Event.KeyCode of
        kbESC:
          Close;
        kbEnter:
          begin
          InterfaceStr := CalendarView^.GetDateText;
          Event.What := evCommand;
          Event.Command := cmInsertText;
          Application^.PutEvent(Event);
          ClearEvent(Event);
          Close;
          Exit;
          end;
        kbCtrlIns:
          begin
          PutInClip(CalendarView^.GetDateText);
          ClearEvent(Event);
          end;
      end {case};
      end;

  end {case};
  inherited HandleEvent(Event);
  end { TCalendarWindow.HandleEvent };

procedure TCalendarWindow.Awaken;
  begin
  inherited Awaken;
  Calend := @Self;
  end;

destructor TCalendarWindow.Done;
  begin
  Calend := nil;
  inherited Done;
  end;

function TCalendarWindow.GetPalette: PPalette;
  const
    P: String[Length(CCalendarWindow)] = CCalendarWindow;
  begin
  GetPalette := @P;
  end;

(*****************************************************************
 *
 * TCalendarView
 *
 *****************************************************************)

constructor TCalendarView.Init(Bounds: TRect);
  begin
  inherited Init(Bounds);
  Options := Options or ofSelectable;
  EventMask := EventMask or evMouseAuto or evBroadcast;
  UpdTicks := 20;
  RegisterToBackground(@Self);
  YearChanged;
  CurDate;
  DrawView;
  end;

constructor TCalendarView.Load(var S: TStream);
  var
    H, y, m, d: word;
  begin
  inherited Load(S);
  GetDate(y, m, d, H);
  CurYear := y;
  CurMonth := Lo(m);
  CurDay := Lo(d);
  UpdTicks := 20;
  RegisterToBackground(@Self);
  S.Read(Year, SizeOf(Year));
  S.Read(Month, SizeOf(Month));
  S.Read(FDay, SizeOf(FDay));
  Days := DaysInMonth(Month, Year);
  YearChanged;
  DrawView;
  end;

destructor TCalendarView.Done;
  begin
  inherited Done;
  end;

procedure TCalendarView.Draw;
  const
    uPalette = #3#6#7#8#9#10#11#12;
  var
    Colors: array[1..8] of Byte;
    B: array[0..CalendarViewWidth] of AWord;
    S1: String[CalendarViewWidth];
    S2: String[3];
    w: integer;
    i, j, d: integer;
    c: Byte;
    DayOf: Byte;
    Width: integer;
  begin
  {0123456789012345678901234567}
  { July          2000 <  >  = }
  { Mo  Tu  We  Th  Fr  Sa  Su }

  Width := Size.X;

  S1 := uPalette;
  for i := Low(Colors) to High(Colors) do
    Colors[i] := GetColor(Ord(S1[i]));

  { draw header }
  MoveChar(B[0], ' ', Colors[7], Width);
  S1 := GetString(TStrIdx(Ord(dlcJanuary)+Month-1));
  MoveStr(B[1], S1, Colors[7]);
  Str(Year: 4, S1);
  MoveStr(B[15], S1, Colors[7]);
  MoveStr(B[19], ' <  >  = ', Colors[8]);
  WriteLine(0, 0, Width, 1, B);

  { draw weekdays }
  c := Colors[7];
  MoveChar(B[0], ' ', 2, Width);
  S1 := DaysOfWeek;
  if  (Length(S1) <> 14) and (Length(S1) <> 21) then
    begin
    S1 := GetString(stDaysWeek);
    end;
  if Length(S1) = 14 then
    begin
    for i := 6 DownTo 0 do
      Insert(' ', S1, i*2+1);
    end;
  if SundayFirst then
    w := 0
  else
    w := 1;
  for i := 0 to 6 do
    begin
    j := (i+w) mod 7;
    MoveStr(B[i*4], Copy(S1, j*3+1, 3)+' ', c);
    end;
  WriteLine(0, 1, Width, 1, B);

  { draw days }
  DayOf := DayOfWeek(1, Month, Year);
  w := 2-DayOf;
  if SundayFirst then
    Dec(w, 1);
  if w > 1 then
    Dec(w, 7);
  for i := 1 to 6 do
    begin
    MoveChar(B[0], ' ', Colors[2], Width);
    for j := 0 to 6 do
      begin
      c := Colors[2];
      if  (w < 1) or (w > Days) then
        begin
        S1 := '    ';
        end
      else
        begin
        Str(w: 2, S1);
        if (w = CurDay) and (Month=CurMonth) and (Year=CurYear) then
          begin
          if w = FDay then
            c := Colors[6]
          else
            c := Colors[5];
          end
        else if w = FDay then
          c := Colors[4];
        S1 := ' '+S1+' ';
        DayOf := (DayOf+1) mod 7;
        end;
      MoveStr(B[j*4], S1, c);
      Inc(w);
      if  (Year = GregorianThreshold) and (Month = 10)
           and (w in [5..14])
      then
        w := 15;
      end;
    { week/day indicator }
    if  (i = 6) and ((CalOptionFlags and $0008) <> 0) then
      begin
      w := WeekNumber(FDay, Month, Year, SundayFirst and
               ((CalOptionFlags and $0010) <> 0));
      d := DayOfYear(FDay, Month, Year);
      Str(w: 2, S1);
      Str(d: 3, S2);
      S1 := '['+S1+'/'+S2+']';
      MoveStr(B[20], S1, Colors[2]);
      end;
    WriteLine(0, i+1, Width, 1, B);
    end;

  end { TCalendarView.Draw };

procedure TCalendarView.HandleEvent(var Event: TEvent);
  var
    Point: TPoint;
    SelectDay: integer;
  begin
  inherited HandleEvent(Event);
  if  (State and sfSelected <> 0) then
    begin
    if Event.What and (evMouseDown+evMouseAuto) <> 0 then
      begin
      MakeLocal(Event.Where, Point);
      if Point.Y = 0 then
        repeat
          case Point.X of

            18, 19, 20:
              begin
              if  (Event.Buttons and mbLeftButton) <> 0 then
                PrevMonth
              else if (Event.Buttons and mbRightButton) <> 0 then
                PrevYear
              else
                break;
              end;

            21, 22, 23:
              begin
              if  (Event.Buttons and mbLeftButton) <> 0 then
                NextMonth
              else if (Event.Buttons and mbRightButton) <> 0 then
                NextYear
              else
                break;
              end;

            24, 25, 26:
              begin
              CurDate;
              end;

            else {case}
              break;
          end {case};
          DrawView;
        until True
      else if (Point.Y >= 2) and (Point.Y <= 7) then
        begin
        SelectDay := 2-DayOfWeek(1, Month, Year);
        if SundayFirst then
          Dec(SelectDay, 1);
        if SelectDay > 1 then
          Dec(SelectDay, 7);
        Inc(SelectDay, (Point.Y-2)*7+(Point.X div 4));
        if  (Year = GregorianThreshold) and (Month = 10)
             and (SelectDay > 4)
        then
          Inc(SelectDay, 10);
        if  (SelectDay >= 1) and (SelectDay <= DaysInMonth(Month, Year))
        then
          begin
          SetDay(SelectDay);
          DrawView;
          end;
        end;
      end
    else if Event.What = evKeyDown then
      begin
      repeat
        case Event.KeyCode of
          kbRight, kbGrayPlus:
            _NextDay;
          kbLeft, kbGrayMinus:
            _PrevDay;
          kbUp:
            PrevWeek;
          kbDown:
            NextWeek;
          kbCtrlLeft, kbPgUp:
            PrevMonth;
          kbCtrlRight, kbPgDn:
            NextMonth;
          kbAltLeft, kbCtrlPgUp:
            PrevYear;
          kbAltRight, kbCtrlPgDn:
            NextYear;
          kbSpace, kbHome:
            CurDate;
          else {case}
            break;
        end {case};
        ClearEvent(Event);
        DrawView;
      until True;
      end;
    end;
  end { TCalendarView.HandleEvent };

procedure TCalendarView.Store(var S: TStream);
  begin
  inherited Store(S);
  S.Write(Year, SizeOf(Year));
  S.Write(Month, SizeOf(Month));
  S.Write(FDay, SizeOf(FDay));
  end;

procedure TCalendarView.NextYear;
  begin
  if Year < MaxYear then
    begin
    Inc(Year);
    if Month = 2 then
      begin
      Days := DaysInMonth(Month, Year);
      if FDay > Days then
        FDay := Days;
      end;
    YearChanged;
    end
  else
    begin
    Month := 12;
    FDay := 31;
    end;
  FixNext;
  end;

procedure TCalendarView.PrevYear;
  begin
  if Year > MinYear then
    begin
    Dec(Year);
    if Month = 2 then
      begin
      Days := DaysInMonth(Month, Year);
      if FDay > Days then
        FDay := Days;
      end;
    YearChanged;
    end
  else
    begin
    Month := 1;
    FDay := 1;
    end;
  FixPrev;
  end;

procedure TCalendarView.NextMonth;
  begin
  if Month < 12 then
    begin
    Inc(Month);
    end
  else if Year < MaxYear then
    begin
    Month := 1;
    Inc(Year);
    YearChanged;
    end
  else
    begin
    FDay := 31;
    end;
  Days := DaysInMonth(Month, Year);
  if FDay > Days then
    FDay := Days;
  FixNext;
  end;

procedure TCalendarView.PrevMonth;
  begin
  if Month > 1 then
    begin
    Dec(Month);
    end
  else if Year > MinYear then
    begin
    Month := 12;
    Dec(Year);
    YearChanged;
    end
  else
    begin
    FDay := 1;
    end;
  Days := DaysInMonth(Month, Year);
  if FDay > Days then
    FDay := Days;
  FixPrev;
  end;

procedure TCalendarView.NextWeek;
  var
    i: integer;
  begin
  for i := 1 to 7 do
    _NextDay;
  end;

procedure TCalendarView.PrevWeek;
  var
    i: integer;
  begin
  for i := 1 to 7 do
    _PrevDay;
  end;

procedure TCalendarView.CurDate;
  var
    h: word;
    y: word;
    m: word;
    d: word;
    b1: Boolean;
  begin
  GetDate(y, m, d, h);
  CurYear := y;
  CurMonth := m;
  CurDay := d;
  b1 := CurYear <> Year;
  Year := CurYear;
  Month := CurMonth;
  Days := DaysInMonth(Month, Year);
  FDay := CurDay;
  if b1 then
    YearChanged;
  end;

function TCalendarView.GetDateText: String;
  begin
  MakeDateFull(FDay, Month, Year, 0, 0, Result, True);
  SetLength(Result, 10);
  end;

procedure TCalendarView.SetDay(ADay: Byte);
  begin
  if FDay = ADay then
    Exit;
  FDay := ADay;
  end;

procedure TCalendarView._NextDay;
  begin
  if FDay < Days then
    begin
    Inc(FDay);
    end
  else if Month < 12 then
    begin
    FDay := 1;
    Inc(Month);
    Days := DaysInMonth(Month, Year);
    end
  else if Year < MaxYear then
    begin
    FDay := 1;
    Month := 1;
    Inc(Year);
    Days := DaysInMonth(Month, Year);
    YearChanged;
    end;
  FixNext;
  end;

procedure TCalendarView._PrevDay;
  begin
  if FDay > 1 then
    begin
    Dec(FDay);
    end
  else if Month > 1 then
    begin
    Dec(Month);
    Days := DaysInMonth(Month, Year);
    FDay := Days;
    end
  else if Year > MinYear then
    begin
    Dec(Year);
    Month := 12;
    Days := DaysInMonth(Month, Year);
    FDay := Days;
    YearChanged;
    end;
  FixPrev;
  end;

procedure TCalendarView.FixNext;
  begin
  if  (Year = GregorianThreshold) and (Month = 10) and (FDay in [5..14])
  then
    FDay := 15;
  end;

procedure TCalendarView.FixPrev;
  begin
  if  (Year = GregorianThreshold) and (Month = 10) and (FDay in [5..14])
  then
    FDay := 4;
  end;

procedure TCalendarView.YearChanged;
  var
    Easter: word;
  begin
  if Year <= GregorianThreshold then
    begin
    Easter := GetEasterDate(Year, 1);
    RomanEaster := DayOfYear(Lo(Easter), Hi(Easter), Year);
    OrthodoxEaster := RomanEaster;
    end
  else
    begin
    Easter := GetEasterDate(Year, 3);
    RomanEaster := DayOfYear(Lo(Easter), Hi(Easter), Year);
    Easter := GetEasterDate(Year, 2);
    OrthodoxEaster := DayOfYear(Lo(Easter), Hi(Easter), Year);
    end;
  end;

procedure InsertCalendar;
  begin
  if Calend = nil then
    Application^.InsertWindow(New(PCalendarWindow, Init))
  else
    Calend^.Select;
  end;

end.
