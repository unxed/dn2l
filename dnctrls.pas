{$MODE DELPHI}
unit dnctrls;

// by unxed
// taken from DN TV as it is the code by DN crew

// some controls not defined in FV

interface

uses Objects, Startup, Commands, views, dialogs;

type
  PLongInputline = ^TLongInputLine;
  {`2 Аналог TInputLine, но GetData и SetData работают с AnsiString.
    Внимание! Load и Store, как и раньше, работают с 1-байтной длиной!.
      TLongInputLine не должен иметь истории, так как это, во-первых,
    приведёт к ошибке в SetData при извлечени из истории, и, во-вторых,
    не имеет смысла, так как история работает с короткими строками, то
    tcnm полноценной истории для TLongInputLine быть не может, пока
    история не будет переведена на AnsiString. }
  TLongInputLine = object(TInputLine)
    procedure GetData(var Rec); virtual;
    procedure SetData(var Rec); virtual;
    end;
    {`}

  PMyScrollBar = ^TMyScrollBar;
  TMyScrollBar = object(TScrollBar)
    procedure Draw; virtual;
  private
    procedure DrawPos(Pos: LongInt);
    end;

function TPoint_EqualsXY(Point: TPoint; AX, AY: LongInt): boolean;

procedure TStream_ReadStrV(var Stream: TStream; var S: String);
function TStream_Eof(var S: TStream): Boolean;

implementation

procedure TLongInputLine.GetData(var Rec);
  begin
  (*
  // fixme: commented by unxed
  if (Data = '') or (Validator = nil) or
      (Validator^.Transfer(Data, @Rec, vtGetData) = 0)
  then
    AnsiString(Rec) := Copy(Data, 1, MaxLen);
    *)
  end;

procedure TLongInputLine.SetData(var Rec);
  begin
  (*
  // fixme: commented by unxed
  if  (Validator = nil) or
      (Validator^.Transfer(Data, @Rec, vtSetData) = 0)
  then
    Data := AnsiString(Rec);
  Message(Owner, evBroadcast, cmUpdateHexViews, @Self);
  SelectAll(True);
  *)
  end;

procedure TMyScrollBar.Draw;
  var
    chrs: TScrollChars;
  begin
  // fixme: commented by unxed
  {
  if Startup.FMSetup.Show and fmsShowScrollBar <> 0 then
    DrawPos(GetPos)
  else
    begin
    chrs := Chars;
    Chars := #186#186#186#186#186;
    DrawPos(GetPos);
    Chars := chrs;
    end;
    }
  end;

procedure TMyScrollBar.DrawPos(Pos: LongInt);
  var
    S: LongInt;
    B: TDrawBuffer;
    col1, col2, col3: Byte;
  begin
  // fixme: commented by unxed
{
  S := GetSize-1;
  if Startup.FMSetup.Show and fmsShowScrollBar <> 0 then
    begin
    col1 := GetColor(1);
    col2 := GetColor(2);
    col3 := GetColor(3);
    end
  else
    begin
    col1 := GetColor(1);
    col2 := col1;
    col3 := col2;
    end;
  MoveChar(B[0], Chars[0], col2, 1);
  if Max = Min then
    MoveChar(B[1], Chars[4], col1, S-1)
  else
    begin
    MoveChar(B[1], Chars[2], col1, S-1);
    MoveChar(B[Pos], Chars[3], col3, 1);
    end;
  MoveChar(B[S], Chars[1], col2, 1);
  WriteBuf(0, 0, Size.X, Size.Y, B);
}  end { TMyScrollBar.DrawPos };


function TPoint_EqualsXY(Point: TPoint; AX, AY: LongInt): boolean;
begin
    Result := (Point.X = AX) and (Point.Y = AY);
end;

procedure TStream_ReadStrV(var Stream: TStream; var S: String);
  var
    L: LongInt;
  begin
  L := 0;
  Stream.Read(L, 1);
  if L > 0 then
    begin
    SetLength(S, L);
    Stream.Read(S[1], L);
    end
  else
    S := '';
  end;

function TStream_Eof(var S: TStream): Boolean;
  begin
    TStream_Eof := (S.GetPos >= S.GetSize);
  end;

end.
