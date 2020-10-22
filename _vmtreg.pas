unit _VMTReg;
(******

DN/2 Plugin Interface - object model
Copyright (C) 2002 Aleksej Kozlov (Cat)
2:5030/1326.13

******)

{&Delphi+}
{&Use32+}

interface

{ Пользоваться этим модулем следует с осторожностью, т.к. регистрируя всё  }
{ подряд, получаем в EXE-файле большую кучу мусора; лучше просто надёргать }
{ отсюда действительно нужных строчек                                      }

procedure VMTRegisterAll;

procedure VMTRegisterObjects;
procedure VMTRegisterRegExp;
procedure VMTRegisterStreams;
procedure VMTRegisterCollect;
procedure VMTRegisterViews;
procedure VMTRegisterMenus;
procedure VMTRegisterDialogs;
procedure VMTRegisterApps;
procedure VMTRegisterEditors;
procedure VMTRegisterGauge;
procedure VMTRegisterFViewer;
procedure VMTRegisterDrives;

implementation

uses
  _DNFuncs, _Model1,
  _Objects, _RegExp, _Streams, _Collect,
  _Views, _Menus, _Dialogs, _Apps, _Editors, _Gauge, _FViewer, _Drives
  ;

procedure VMTRegisterObjects;
  begin
  TransportVMT(_TEmptyObject^.VMT, TypeOf(TEmptyObject),
       TypeOf(TEmptyObject), TEmptyObject_VMTSize);
  TransportVMT(_TObject^.VMT, TypeOf(TObject), TypeOf(TObject),
     TObject_VMTSize);
  end;

procedure VMTRegisterRegExp;
  begin
  TransportVMT(_TRegExp^.VMT, TypeOf(TRegExp), TypeOf(TRegExp),
     TRegExp_VMTSize);
  end;

procedure VMTRegisterStreams;
  begin
  TransportVMT(_TStream^.VMT, TypeOf(TStream), TypeOf(TStream),
     TStream_VMTSize);
  TransportVMT(_TDosStream^.VMT, TypeOf(TDOSStream), TypeOf(TDOSStream),
     TDosStream_VMTSize);
  TransportVMT(_TBufStream^.VMT, TypeOf(TBufStream), TypeOf(TBufStream),
     TBufStream_VMTSize);
  TransportVMT(_TMemoryStream^.VMT, TypeOf(TMemoryStream),
       TypeOf(TMemoryStream), TMemoryStream_VMTSize);
  end;

procedure VMTRegisterCollect;
  begin
  TransportVMT(_TCollection^.VMT, TypeOf(TCollection),
     TypeOf(TCollection), TCollection_VMTSize);
  TransportVMT(_TSortedCollection^.VMT, TypeOf(TSortedCollection),
       TypeOf(TSortedCollection), TSortedCollection_VMTSize);
  TransportVMT(_TLineCollection^.VMT, TypeOf(TLineCollection),
       TypeOf(TLineCollection), TLineCollection_VMTSize);
  TransportVMT(_TStringCollection^.VMT, TypeOf(TStringCollection),
       TypeOf(TStringCollection), TStringCollection_VMTSize);
  TransportVMT(_TStrCollection^.VMT, TypeOf(TStrCollection),
       TypeOf(TStrCollection), TStrCollection_VMTSize);
  TransportVMT(_TFilesCollection^.VMT, TypeOf(TFilesCollection),
       TypeOf(TFilesCollection), TFilesCollection_VMTSize);
  end;

procedure VMTRegisterViews;
  begin
  TransportVMT(_TView^.VMT, TypeOf(TView), TypeOf(TView), TView_VMTSize);
  TransportVMT(_TFrame^.VMT, TypeOf(TFrame), TypeOf(TFrame),
     TFrame_VMTSize);
  TransportVMT(_TScrollBar^.VMT, TypeOf(TScrollBar), TypeOf(TScrollBar),
     TScrollBar_VMTSize);
  TransportVMT(_TGroup^.VMT, TypeOf(TGroup), TypeOf(TGroup),
     TGroup_VMTSize);
  TransportVMT(_TWindow^.VMT, TypeOf(TWindow), TypeOf(TWindow),
     TWindow_VMTSize);
  end;

procedure VMTRegisterMenus;
  begin
  TransportVMT(_TMenuView^.VMT, TypeOf(TMenuView), TypeOf(TMenuView),
     TMenuView_VMTSize);
  TransportVMT(_TMenuBar^.VMT, TypeOf(TMenuBar), TypeOf(TMenuBar),
     TMenuBar_VMTSize);
  TransportVMT(_TMenuBox^.VMT, TypeOf(TMenuBox), TypeOf(TMenuBox),
     TMenuBox_VMTSize);
  TransportVMT(_TMenuPopup^.VMT, TypeOf(TMenuPopup), TypeOf(TMenuPopup),
     TMenuPopup_VMTSize);
  TransportVMT(_TStatusLine^.VMT, TypeOf(TStatusLine),
     TypeOf(TStatusLine), TStatusLine_VMTSize);
  end;

procedure VMTRegisterDialogs;
  begin
  TransportVMT(_TDialog^.VMT, TypeOf(TDialog), TypeOf(TDialog),
     TDialog_VMTSize);
  TransportVMT(_TInputLine^.VMT, TypeOf(TInputLine), TypeOf(TInputLine),
     TInputLine_VMTSize);
  TransportVMT(_TButton^.VMT, TypeOf(TButton), TypeOf(TButton),
     TButton_VMTSize);
  TransportVMT(_TCluster^.VMT, TypeOf(TCluster), TypeOf(TCluster),
     TCluster_VMTSize);
  TransportVMT(_TRadioButtons^.VMT, TypeOf(TRadioButtons),
       TypeOf(TRadioButtons), TRadioButtons_VMTSize);
  TransportVMT(_TCheckBoxes^.VMT, TypeOf(TCheckBoxes),
     TypeOf(TCheckBoxes), TCheckBoxes_VMTSize);
  TransportVMT(_TMultiCheckBoxes^.VMT, TypeOf(TMultiCheckBoxes),
       TypeOf(TMultiCheckBoxes), TMultiCheckBoxes_VMTSize);
  TransportVMT(_TScroller^.VMT, TypeOf(TScroller), TypeOf(TScroller),
     TScroller_VMTSize);
  TransportVMT(_TListViewer^.VMT, TypeOf(TListViewer),
     TypeOf(TListViewer), TListViewer_VMTSize);
  TransportVMT(_TListBox^.VMT, TypeOf(TListBox), TypeOf(TListBox),
     TListBox_VMTSize);
  TransportVMT(_TStaticText^.VMT, TypeOf(TStaticText),
     TypeOf(TStaticText), TStaticText_VMTSize);
  TransportVMT(_TParamText^.VMT, TypeOf(TParamText), TypeOf(TParamText),
     TParamText_VMTSize);
  TransportVMT(_TLabel^.VMT, TypeOf(TLabel), TypeOf(TLabel),
     TLabel_VMTSize);
  TransportVMT(_THistoryViewer^.VMT, TypeOf(THistoryViewer),
       TypeOf(THistoryViewer), THistoryViewer_VMTSize);
  TransportVMT(_THistoryWindow^.VMT, TypeOf(THistoryWindow),
       TypeOf(THistoryWindow), THistoryWindow_VMTSize);
  TransportVMT(_THistory^.VMT, TypeOf(THistory), TypeOf(THistory),
     THistory_VMTSize);
  end { VMTRegisterDialogs };

procedure VMTRegisterApps;
  begin
  TransportVMT(_TBackground^.VMT, TypeOf(TBackground),
     TypeOf(TBackground), TBackground_VMTSize);
  TransportVMT(_TDesktop^.VMT, TypeOf(TDesktop), TypeOf(TDesktop),
     TDesktop_VMTSize);
  TransportVMT(_TProgram^.VMT, TypeOf(TProgram), TypeOf(TProgram),
     TProgram_VMTSize);
  TransportVMT(_TApplication^.VMT, TypeOf(TApplication),
       TypeOf(TApplication), TApplication_VMTSize);
  TransportVMT(_TDNApplication^.VMT, TypeOf(TDNApplication),
       TypeOf(TDNApplication), TDNApplication_VMTSize);
  end;

procedure VMTRegisterEditors;
  begin
  TransportVMT(_TUniWindow^.VMT, TypeOf(TUniWindow), TypeOf(TUniWindow),
     TUniWindow_VMTSize);
  TransportVMT(_TXFileEditor^.VMT, TypeOf(TXFileEditor),
       TypeOf(TXFileEditor), TXFileEditor_VMTSize);
  TransportVMT(_TEditWindow^.VMT, TypeOf(TEditWindow),
     TypeOf(TEditWindow), TEditWindow_VMTSize);
  end;

procedure VMTRegisterGauge;
  begin
  TransportVMT(_TPercentGauge^.VMT, TypeOf(TPercentGauge),
       TypeOf(TPercentGauge), TPercentGauge_VMTSize);
  TransportVMT(_TBarGauge^.VMT, TypeOf(TBarGauge), TypeOf(TBarGauge),
     TBarGauge_VMTSize);
  TransportVMT(_TWhileView^.VMT, TypeOf(TWhileView), TypeOf(TWhileView),
     TWhileView_VMTSize);
  end;

procedure VMTRegisterFViewer;
  begin
  TransportVMT(_TViewScroll^.VMT, TypeOf(TViewScroll),
     TypeOf(TViewScroll), TViewScroll_VMTSize);
  TransportVMT(_TFileViewer^.VMT, TypeOf(TFileViewer),
     TypeOf(TFileViewer), TFileViewer_VMTSize);
  end;

procedure VMTRegisterDrives;
  begin
  TransportVMT(_TDrive^.VMT, TypeOf(TDrive), TypeOf(TDrive),
     TDrive_VMTSize);
  TransportVMT(_TFindDrive^.VMT, TypeOf(TFindDrive), TypeOf(TFindDrive),
     TFindDrive_VMTSize);
  TransportVMT(_TTempDrive^.VMT, TypeOf(TTempDrive), TypeOf(TTempDrive),
     TTempDrive_VMTSize);
  TransportVMT(_TArcDrive^.VMT, TypeOf(TArcDrive), TypeOf(TArcDrive),
     TArcDrive_VMTSize);
  TransportVMT(_TArvidDrive^.VMT, TypeOf(TArvidDrive),
     TypeOf(TArvidDrive), TArvidDrive_VMTSize);
  end;

procedure VMTRegisterAll;
  begin
  VMTRegisterObjects;
  VMTRegisterRegExp;
  VMTRegisterStreams;
  VMTRegisterCollect;
  VMTRegisterViews;
  VMTRegisterMenus;
  VMTRegisterDialogs;
  VMTRegisterApps;
  VMTRegisterEditors;
  VMTRegisterGauge;
  VMTRegisterFViewer;
  VMTRegisterDrives;
  end;

end.
