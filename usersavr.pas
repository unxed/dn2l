{$I STDEFINE.INC}

unit UserSavr;

interface

uses
  Views, Objects2
  ;

type

  PUserSaver = ^TUserSaver;
  TUserSaver = object(TView)
    Screen: Pointer;
    SSize, SWidth: AInt;
    CShape, CPos: AWord;
    CheckIO: Boolean;
    isValid: Boolean;
    constructor Init(ACheck: Boolean);
    destructor Done; virtual;
    constructor Load(var S: TStream);
    procedure Store(var S: TStream);
    function Valid(Command: Word): Boolean; virtual;
    end;

procedure InsertUserSaver(ACheck: Boolean);

implementation

uses
  Memory, Drivers, DNUtil, Messages, Commands, DNApp
  ;

{ ------------------------------------------------------------------------- }

constructor TUserSaver.Init;
  var
    R: TRect;
  begin
  R.Assign(0, 0, 0, 0);
  inherited Init(R);
  CheckIO := ACheck;
  SetState(sfVisible, False);
  isValid := True;
  Screen := MemAlloc(UserScreenSize);
  if Screen = nil then
    Fail;
  Move(UserScreen^, Screen^, UserScreenSize);
  CLSAct := False;
  SSize := UserScreenSize;
  SWidth := UserScreenWidth;
  CShape := OldCursorShape;
  CPos := OldCursorPos;
  end;

function TUserSaver.Valid;
  begin
  Valid := isValid
  end;

constructor TUserSaver.Load;
  var
    I: Byte;
  begin
  inherited Load(S);
  DataSaver := @Self;
  S.Read(SSize, 4*SizeOf(AInt)+SizeOf(Boolean));
  if UserScreen <> nil then
    FreeMem(UserScreen, UserScreenSize);
  UserScreenSize := SSize;
  UserScreenWidth := SWidth;
  UserScreen := MemAlloc(SSize);
  OldCursorShape := CShape;
  OldCursorPos := CPos;
  S.Read(UserScreen^, SSize);
  Screen := nil;
  isValid := False;
  I := 0;
  if I <> 0 then
    Msg(dlErrorsOccurred, nil, mfWarning+mfOKButton);
  end;

destructor TUserSaver.Done;
  begin
  if Screen <> nil then
    FreeMem(Screen, SSize);
  inherited Done;
  end;

procedure TUserSaver.Store;
  begin
  inherited Store(S);
  S.Write(SSize, 4*SizeOf(AInt)+SizeOf(Boolean));
  S.Write(Screen^, SSize);
  end;

procedure InsertUserSaver;
  begin
  Desktop^.Insert(New(PUserSaver, Init(ACheck)));
  FreeMem(UserScreen, UserScreenSize);
  UserScreenSize := ScreenWidth*ScreenHeight*2;
  UserScreenWidth := ScreenWidth;
  OldCursorPos := 0;
  OldCursorShape := $FFFF;
  HideMouse;
  GetMem(UserScreen, UserScreenSize);
  System.Move(ScreenBuffer^, UserScreen^, UserScreenSize);
  ShowMouse;
  end;

end.
