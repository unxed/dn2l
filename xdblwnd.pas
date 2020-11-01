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

unit XDblWnd;

interface

uses
  DblWnd, Drivers, Views, Objects2
  ;

type
  PXDoubleWindow = ^TXDoubleWindow;
  TXDoubleWindow = object(TDoubleWindow)
    procedure SetState(AState: Word; Enable: Boolean); virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    end;

implementation
uses
  Commands, Advance, Advance1, Advance2, DNApp, DNUtil
  ;

function TXDoubleWindow.GetPalette;
  const
    S: String[Length(CDoubleWindow)] = CDoubleWindow;
  begin
  GetPalette := @S;
  end;

procedure TXDoubleWindow.SetState(AState: Word; Enable: Boolean);
  begin
  inherited SetState(AState, Enable);
  if  (AState and sfDragging <> 0) or
      (AState and (sfSelected+sfActive) <> 0)
  then
    Separator^.Draw;
  if AState = sfSelected then
    begin
    SetState(sfActive, Enable);
    {$IFDEF TrashCan}
    TrashCan^.Hide;
    {$ENDIF}
    if Enable then
      begin
      Current^.SetState(sfSelected, True);
        // чтобы установились ActivePanel и PassivePanel
      EnableCommands(DblWndCommands)
      end
    else
      DisableCommands(DblWndCommands);
    end
    {$IFDEF TrashCan}
  else if TrashCan^.ImVisible then
    begin
    TrashCan^.Show;
    TrashCan^.MakeFirst;
    end;
  {$ENDIF}
  end { TXDoubleWindow.SetState };

procedure TXDoubleWindow.HandleEvent;
  var
    CE: Boolean;
    Visible, Selected: array[TPanelNum] of Boolean;
    EV: TEvent;
    i: TPanelNum;
  begin
  Visible[pLeft] := False;
  Visible[pRight] := False;
  Selected := Visible;
  for i := pLeft to pRight do
    begin
    with Panel[i] do
      begin
      if AnyPanel <> nil then
        begin
        Visible[i] := AnyPanel^.GetState(sfVisible);
        Selected[i] := AnyPanel^.GetState(sfSelected);
        end;
      end;
    end;

  CE := True;
  case Event.What of
    evKeyDown:
      case Event.KeyCode of
        {kbCtrlP,}
        kbCtrlLeft, kbCtrlRight,
        kbCtrlShiftLeft, kbCtrlShiftRight,
        kbAlt1, kbAlt2, kbAlt3, kbAlt4, kbAlt5,
        kbAlt6, kbAlt7, kbAlt8, kbAlt9, kbAlt0,
        kbCtrl1, kbCtrl2, kbCtrl3, kbCtrl4, kbCtrl5, {DataCompBoy}
        kbCtrl6, kbCtrl7, kbCtrl8, kbCtrl9, kbCtrl0, {DataCompBoy}
        kbAltLeft, kbAltRight,
        kbAltShiftLeft, kbAltShiftRight,
        kbCtrlAltZ, {Knave 11.09.99} {DataCompBoy}
        kbAltCtrlSqBracketL, kbAltCtrlSqBracketR,
        kbCtrlSqBracketL, kbCtrlSqBracketR:
          begin
          HandleCommand(Event);
          CE := False;
          end;
      end {case};
    evBroadcast:
      case Event.Command of
        cmLookForPanels, cmGetUserParams, cmGetUserParamsWL,
        cmChangeDrv,
        cmIsRightPanel:
          begin
          HandleCommand(Event);
          CE := False
          end;
      end {case};
    evCommand:
      case Event.Command of
        cmChangeDirectory:
          begin {AK155 Это сообщение реально можно получить ТОЛЬКО
            от дерева, поэтому сравнение с dtTree, мягко говоря,
            неожиданное, работает правильно. Но, конечно, надо
            вместо этого трюкачества организовать в дереве прямой
            вызов ChDir панели }
          Panel[Selected[NonFilePanelType <> dtTree]].
            FilePanel^.HandleEvent(Event);
          Exit;
          end;
        cmChangeTree:
          if NonFilePanelType = dtTree then
            Panel[NonFilePanel].AnyPanel^.HandleEvent(Event);
        cmRereadInfo:
          begin
          for i := pLeft to pRight do
            begin
            with Panel[i] do
              if AnyPanel <> nil then
                AnyPanel^.HandleEvent(Event);
            end;
          Exit;
          end;
        cmCloseLinked,
        cmMakeForced,
        cmRereadForced,
        cmTotalReread,
        cmUpdateHighlight,
        cmReboundPanel,
        cmRereadDir:
          begin
          EV := Event;
          Panel[pLeft].FilePanel^.HandleEvent(Event);
          Event := EV;
          Panel[pRight].FilePanel^.HandleEvent(Event);
          ClearEvent(Event);
          end;
        cmPushName,
        cmZoom,
        cmMaxi,
        cmGetName,
        cmPostHideRight,
        cmPostHideLeft,
        cmChangeInactive,
        cmPanelCompare,
        cmDiskInfo,
        {$IFDEF NETINFO}cmNetInfo, {-$VIV} {$ENDIF}
        cmLoadViewFile,
        cmPushFullName,
        cmPushFirstName,
        cmPushInternalName,
        cmFindTree,
        cmRereadTree,
        cmHideLeft,
        cmHideRight,
        cmChangeLeft,
        cmChangeRight,
        cmDirTree,
        cmQuickView,
        cmDizView,
        cmSwapPanels,
        cmSwitchOther:
          begin
          HandleCommand(Event);
          CE := False
          end;
      end {case};
  end {case};

  if  (Event.What <> evNothing) and CE then
    inherited HandleEvent(Event);
  end { TXDoubleWindow.HandleEvent };
end.
