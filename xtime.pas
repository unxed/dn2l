{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.08
//  Based on Dos Navigator (C) 1991-99 RIT Research Labs
//
//  This programs is free for commercial and non-commercial use as long as
//  the following conditions are aheared to.
//
//  Copyright remains RIT Research Labs, and as such any Copyright notices
//  in the code are not to be removed. If this package is used in a
//  product, RIT Research Labs should be given attribution as the RIT Research
//  Labs of the parts of the library used. This can be in the form of a textual
//  message at program startup or in documentation (online or textual)
//  provided with the package.
//
//  Redistribution and use in source and binary forms, with or without
//  modification, are permitted provided that the following conditions are
//  met:
//
//  1. Redistributions of source code must retain the copyright
//     notice, this list of conditions and the following disclaimer.
//  2. Redistributions in binary form must reproduce the above copyright
//     notice, this list of conditions and the following disclaimer in the
//     documentation and/or other materials provided with the distribution.
//  3. All advertising materials mentioning features or use of this software
//     must display the following acknowledgement:
//     "Based on Dos Navigator by RIT Research Labs."
//
//  THIS SOFTWARE IS PROVIDED BY RIT RESEARCH LABS "AS IS" AND ANY EXPRESS
//  OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
//  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
//  DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE FOR
//  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
//  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
//  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
//  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
//  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
//  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
//  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
//  The licence and distribution terms for any publically available
//  version or derivative of this code cannot be changed. i.e. this code
//  cannot simply be copied and put under another distribution licence
//  (including the GNU Public Licence).
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}

(***********************************************************************)
(*                                                                     *)
(*  This file contains code from                                       *)
(*  ASYNC Professional Library Copyright (c) TurboPower Software 1991  *)
(*                                                                     *)
(***********************************************************************)

unit xTime;

interface

type

  {For calculating timeouts}
  TEventTimer = record
    StartMSecs: LongInt;
    ExpireMSecs: LongInt;
    end;

  {For internal date/time manipulations}
  Date = Word;
  Time = LongInt;

  DateTimeRec =
  record
    D: Date;
    T: Time;
    end;

function xYMTimeStampToPack(YMTime: LongInt): LongInt;
function xPackToYMTimeStamp(RawTime: LongInt): LongInt;
procedure xIncDateTime(var DT1, DT2: DateTimeRec; Days: Integer;
     Secs: LongInt);

procedure NewTimer(var ET: TEventTimer; MSecs: LongInt);
function TimerExpired(ET: TEventTimer): Boolean;

function ElapsedTime(ET: TEventTimer): LongInt;

procedure GetUNIXDate(Julian: LongInt;
     var Year, Month, Day, Hour, Min, Sec: Word);

implementation
uses
  Dos, Events
  ;
type
  DayType = (Sunday, Monday, Tuesday, Wednesday, Thursday, Friday,
     Saturday);

const
  {Set to 1/1/1970 00:00 GMT}
  StartDate: DateTimeRec = (D: 25567; T: 0);
  GmtHourOffset: Integer = 0; {Default no GMT adjustments}
  Threshold2000: Integer = 1900;

  MinYear = 1900;
  MaxYear = 2078;
  BadDate = $FFFF;
  BadTime = $FFFFFFFF;
  First2Months = 58; {1900 was not a leap year}
  SecondsInMinute = 60;
  SecondsInHour = 60*SecondsInMinute;
  HoursInDay = 24;
  SecondsInDay = SecondsInHour*HoursInDay;
  MSecsInDay = 1000*SecondsInDay;

function IsLeapYear(Year: Integer): Boolean;
  {-Return True if Year is a leap year}
  begin
  IsLeapYear := (Year mod 4 = 0) and (Year mod 4000 <> 0) and
      ( (Year mod 100 <> 0) or (Year mod 400 = 0));
  end;

function DaysInMonth(Month, Year: Integer): Integer;
  {-Return the number of days in the specified month of a given year}
  begin
  if Word(Year) < 100 then
    begin
    Inc(Year, 1900);
    if Year < Threshold2000 then
      Inc(Year, 100);
    end;

  case Month of
    1, 3, 5, 7, 8, 10, 12:
      DaysInMonth := 31;
    4, 6, 9, 11:
      DaysInMonth := 30;
    2:
      DaysInMonth := 28+Ord(IsLeapYear(Year));
    else {case}
      DaysInMonth := 0;
  end {case};
  end;

function ValidDate(Day, Month, Year: Integer): Boolean;
  {-Verify that day, month, year is a valid date}
  begin
  if Word(Year) < 100 then
    begin
    Inc(Year, 1900);
    if Year < Threshold2000 then
      Inc(Year, 100);
    end;

  if  (Day < 1) or (Year < MinYear) or (Year > MaxYear) then
    ValidDate := False
  else
    case Month of
      1..12:
        ValidDate := Day <= DaysInMonth(Month, Year);
      else {case}
        ValidDate := False;
    end
  end;

function DMYtoDate(Day, Month, Year: Integer): Date;
  {-Convert from day, month, year to a julian date}
  begin
  if Word(Year) < 100 then
    begin
    Inc(Year, 1900);
    if Year < Threshold2000 then
      Inc(Year, 100);
    end;

  if not ValidDate(Day, Month, Year) then
    DMYtoDate := BadDate
  else if (Year = MinYear) and (Month < 3) then
    if Month = 1 then
      DMYtoDate := pred(Day)
    else
      DMYtoDate := Day+30
  else
    begin
    if Month > 2 then
      Dec(Month, 3)
    else
      begin
      Inc(Month, 9);
      Dec(Year);
      end;
    Dec(Year, MinYear);
    DMYtoDate :=
        ( (LongInt(Year)*1461) div 4)+
        ( ( (153*Month)+2) div 5)+Day+First2Months;
    end;
  end { DMYtoDate };

procedure DateToDMY(Julian: Date; var Day, Month, Year: Integer);
  {-Convert from a julian date to month, day, year}
  var
    I: LongInt;
  begin
  if Julian = BadDate then
    begin
    Day := 0;
    Month := 0;
    Year := 0;
    end
  else if Julian <= First2Months then
    begin
    Year := MinYear;
    if Julian <= 30 then
      begin
      Month := 1;
      Day := Succ(Julian);
      end
    else
      begin
      Month := 2;
      Day := Julian-30;
      end;
    end
  else
    begin
    I := (4*LongInt(Julian-First2Months))-1;
    Year := I div 1461;
    I := (5*((I mod 1461) div 4))+2;
    Month := I div 153;
    Day := ((I mod 153)+5) div 5;
    if Month < 10 then
      Inc(Month, 3)
    else
      begin
      Dec(Month, 9);
      Inc(Year);
      end;
    Inc(Year, MinYear);
    end;
  end { DateToDMY };

procedure DateTimeDiff(DT1, DT2: DateTimeRec; var Days: Word;
     var Secs: LongInt);
  {-Return the difference in days and seconds between two points in time}
  var
    DTTemp: DateTimeRec;
  begin
  {swap if DT1 later than DT2}
  if  (DT1.D > DT2.D) or ((DT1.D = DT2.D) and (DT1.T > DT2.T)) then
    begin
    DTTemp := DT1;
    DT1 := DT2;
    DT2 := DTTemp;
    end;

  {the difference in days is easy}
  Days := DT2.D-DT1.D;

  {difference in seconds}
  if DT2.T < DT1.T then
    begin
    {subtract one day, add 24 hours}
    Dec(Days);
    Inc(DT2.T, SecondsInDay);
    end;
  Secs := DT2.T-DT1.T;
  end { DateTimeDiff };

procedure TimeToHMS(T: Time; var Hours, Minutes, Seconds: Byte);
  {-Convert a Time variable to Hours, Minutes, Seconds}
  begin
  if T = BadTime then
    begin
    Hours := 0;
    Minutes := 0;
    Seconds := 0;
    end
  else
    begin
    Hours := T div SecondsInHour;
    Dec(T, LongInt(Hours)*SecondsInHour);
    Minutes := T div SecondsInMinute;
    Dec(T, LongInt(Minutes)*SecondsInMinute);
    Seconds := T;
    end;
  end;

function HMStoTime(Hours, Minutes, Seconds: Byte): Time;
  {-Convert Hours, Minutes, Seconds to a Time variable}
  var
    T: Time;
  begin
  Hours := Hours mod HoursInDay;
  T := (LongInt(Hours)*SecondsInHour)+(LongInt(Minutes)*SecondsInMinute)
    +Seconds;
  HMStoTime := T mod SecondsInDay;
  end;

procedure xIncDateTime(var DT1, DT2: DateTimeRec; Days: Integer;
     Secs: LongInt);
  {-Increment (or decrement) DT1 by the specified number of days and seconds
      and put the result in DT2}
  begin
  DT2 := DT1;

  {date first}
  Inc(Integer(DT2.D), Days);

  if Secs < 0 then
    begin
    {change the sign}
    Secs := -Secs;

    {adjust the date}
    Dec(DT2.D, Secs div SecondsInDay);
    Secs := Secs mod SecondsInDay;

    if Secs > DT2.T then
      begin
      {subtract a day from DT2.D and add a day's worth of seconds to DT2.T}
      Dec(DT2.D);
      Inc(DT2.T, SecondsInDay);
      end;

    {now subtract the seconds}
    Dec(DT2.T, Secs);
    end
  else
    begin
    {increment the seconds}
    Inc(DT2.T, Secs);

    {adjust date if necessary}
    Inc(DT2.D, DT2.T div SecondsInDay);

    {force time to 0..SecondsInDay-1 range}
    DT2.T := DT2.T mod SecondsInDay;
    end;
  end { xIncDateTime };

function xYMTimeStampToPack(YMTime: LongInt): LongInt;
  {-Return a file time stamp in packed format from a Ymodem time stamp}
  var
    DT: DateTime;
    DTR: DateTimeRec;
    Ptime: LongInt;
    H, M, S: Byte;
  begin
  {Add the time stamp to StartDate}
  xIncDateTime(StartDate, DTR, 0, YMTime);

  {Add the GMT hour offset}
  xIncDateTime(DTR, DTR, 0, SecondsInHour*GmtHourOffset);

  {Convert to DT format}
  with DT do
    begin
    DateToDMY(DTR.D, Integer(Day), Integer(Month), Integer(Year));
    TimeToHMS(DTR.T, H, M, S);
    Hour := H;
    Min := M;
    Sec := S;
    end;

  {Convert to packed format}
  PackTime(DT, Ptime);
  xYMTimeStampToPack := Ptime;
  end { xYMTimeStampToPack };

procedure NewTimer(var ET: TEventTimer; MSecs: LongInt);
  {-Returns a set EventTimer that will expire in MSecs}
  begin
  {Max acceptable value is 24 hours}
  if MSecs > MSecsInDay then
    MSecs := MSecsInDay;

  with ET do
    begin
    StartMSecs := GetCurMSec;
    ExpireMSecs := StartMSecs+MSecs;
    end;
  end;

function TimerExpired(ET: TEventTimer): Boolean;
  {-Returns True if ET has expired}
  var
    CurMSecs: LongInt;
  begin
  with ET do
    begin
    CurMSecs := GetCurMSec;
    dec(CurMSecs, ExpireMSecs);
    Result := CurMSecs > 0;
    end;
  end;

function ElapsedTime(ET: TEventTimer): LongInt;
  {-Returns elapsed time, in MSecs, for this timer}
  begin
  with ET do
    begin
    Result := GetCurMSec;
    dec(Result, ExpireMSecs);
    end;
  end;

function xPackToYMTimeStamp(RawTime: LongInt): LongInt;
  {-Return date/time stamp as octal seconds since 1/1/1970 00:00 GMT}
  var
    DT: DateTime;
    DTR: DateTimeRec;
    DiffDays: Word;
    DiffSecs: LongInt;
  begin
  {Convert to julian date}
  UnpackTime(RawTime, DT);
  with DT do
    begin
    DTR.D := DMYtoDate(Day, Month, Year);
    DTR.T := HMStoTime(Hour, Min, Sec);
    end;

  {Subtract GMT hour offset}
  xIncDateTime(DTR, DTR, 0, -(SecondsInHour*GmtHourOffset));

  {Diff between date/time stamp and 1/1/1970 (in seconds)}
  DateTimeDiff(DTR, StartDate, DiffDays, DiffSecs);
  xPackToYMTimeStamp := DiffSecs+(DiffDays*SecondsInDay);
  end { xPackToYMTimeStamp };

procedure GetUNIXDate(Julian: LongInt;
     var Year, Month, Day, Hour, Min, Sec: Word);
  var
    L: LongInt;
    DT: DateTime;
  begin
  L := xYMTimeStampToPack(Julian);
  UnpackTime(L, DT);
  Year := DT.Year;
  Month := DT.Month;
  Day := DT.Day;
  Hour := DT.Hour;
  Min := DT.Min;
  Sec := DT.Sec;
  end;

end.
