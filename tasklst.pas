unit TaskLst;
(******

Task List unit

Based on:
Task List plugin
Copyright (C) 2002 Aleksej Kozlov (Cat)
2:5030/1326.13
******)
{JO: 20-10-2002 переделал из плагина в модуль для компиляции в ядре DN/2 , }
{    добавлены настройки ShowExePaths и KillAllowed в dn.ini ,             }
{    в OS/2 версии добавлен показ типа сессии, показ заголовка окна        }
{    (требует PM) и возможность переключения на окно процесса (требует PM) }

{&Delphi+}
{&Use32+}

interface

uses
  {$IFDEF OS2}Proc_Os2, {$ENDIF}
  {$IFDEF WIN32}Proc_W32, {$ENDIF}
  Defines, Objects, Views, Drivers, Dialogs
  ;

const
  cmButton1 = 65535;

type
  PProcessList = ^TProcessList;
  TProcessList = object(TListBox)
    function GetText(Item: Integer; MaxLen: Integer): String; virtual;
    end;

  PProcessDialog = ^TProcessDialog;
  TProcessDialog = object(TDialog)
    ListBox: PProcessList;
    constructor Init(Collection: PProcessCollection);
    procedure HandleEvent(var Event: TEvent); virtual;
    end;

procedure InsertTaskList;

implementation

uses
  DNApp, Commands, DnIni, DNHelp
  ;

function TProcessList.GetText(Item: Integer; MaxLen: Integer): String;
  begin
  GetText := PProcessItem(List^.At(Item))^.GetString;
  end;

constructor TProcessDialog.Init(Collection: PProcessCollection);
  var
    R: TRect;
    HScrollBar: PScrollBar;
    VScrollBar: PScrollBar;
    X1, Y1: Byte;
    I: LongInt;
  begin
  X1 := ScreenWidth-4;
  Y1 := ScreenHeight-5;
  if Collection^.Count < (Y1-4) then
    Y1 := Collection^.Count+4;
  if Y1 < 10 then
    Y1 := 10;
  {Cat:warn в этом месте возникнут проблемы, если поставить слишком маленькие размеры экрана }
  R.Assign(0, 0, X1, Y1);
  TDialog.Init(R, GetString(dlTasklist));
  Options := Options or ofCentered;
  R.Assign(X1-13, 3, X1-2, 5);
  Insert(New(PButton, Init(R, GetString(dlSwitchButton), cmOK,
     bfDefault)));
  R.Assign(X1-13, 5, X1-2, 7);
  if KillAllowed then
    begin
    Insert(New(PButton, Init(R, GetString(dlKillButton), cmButton1,
           bfNormal)));
    R.Assign(X1-13, 7, X1-2, 9);
    end;
  Insert(New(PButton, Init(R, GetString(dlCancelButton), cmCancel,
         bfNormal)));
  R.Assign(X1-14, 2, X1-13, Y1-2);
  VScrollBar := New(PScrollBar, Init(R));
  Insert(VScrollBar);
  R.Assign(2, Y1-2, X1-14, Y1-1);
  HScrollBar := New(PScrollBar, Init(R));
  Insert(HScrollBar);
  with HScrollBar^ do
    begin
    Options := Options or ofPreProcess;
    SetRange(1, 128);
    SetStep(X1-15, 1);
    end;
  R.Assign(2, 2, X1-14, Y1-2);
  ListBox := New(PProcessList, Init(R, 1, VScrollBar));
  ListBox^.HScrollBar := HScrollBar;
  ListBox^.NewLisT(PCollection(Collection));
  Insert(ListBox);
  {$IFDEF OS2} {JO: в Win32 пока не доделано}
  I := 0;
  while (I < Collection^.Count) and
      (PProcessItem(Collection^.At(I))^.Pid < GetCurPid)
  do
    Inc(I);
  if I >= Collection^.Count then
    I := 0;
  VScrollBar^.SetValue(I);
  {$ENDIF}
  HelpCtx := hcTaskList;
  end { TProcessDialog.Init };

procedure TProcessDialog.HandleEvent(var Event: TEvent);
  begin
  if  (Event.What = evCommand) and (Event.Command = cmButton1)
    or (Event.What = evKeyDown) and (Event.KeyCode = kbDel)
  then
    EndModal(cmButton1);
  inherited HandleEvent(Event);
  end;

procedure InsertTaskList;
  var
    Collection: PProcessCollection;
    Dialog: PProcessDialog;
  label Ret;
  begin
  Collection := GetProcessList;
  Dialog := New(PProcessDialog, Init(Collection));
Ret:
  case Desktop^.ExecView(Dialog) of
    cmOK:
      if Dialog^.ListBox^.List^.Count > 0 then
        if ProcessSwitch
                (PProcessItem(Collection^.At(Dialog^.ListBox^.Focused))^
            .Pid) <> 0
        then
          begin
          Beep(100, 30);
          goto Ret;
          end;
    cmButton1:
      if  (Dialog^.ListBox^.List^.Count > 0) and KillAllowed then
        if ProcessKill
                (PProcessItem(Collection^.At(Dialog^.ListBox^.Focused))^
            .Pid) <> 0
        then
          goto Ret;
  end {case};
  Dispose(Dialog, Done);
  Dispose(Collection, Done);
  end { InsertTaskList };

begin
end.
