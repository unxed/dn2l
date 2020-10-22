library PlugMan;
(******

Plugin Manager 2.0
Written by Cat 2:5030/1326.13
(EventCatcher plugin for DN/2)

******)

{.$DEFINE DEBUG}
{&Delphi+}
{&Use32+}

{Cat
   05-10-2001 - начато добавление плагинов к ДН-у
   19-10-2001 - плагины типа EventCatcher
   21-10-2001 - плагины используют индексы в файлах ресурсов (Lng и Dlg)
   27-10-2001 - плагины используют индекс в файле помощи (Hlp)
   04-11-2001 - при установке плагина изменяем меню
   26-12-2001 - для разных языков используем различные строки меню
   17-01-2002 - теперь можем менять не только главное меню,
                но и меню редактора и электронной таблицы
   17-01-2002 - решил, что не следует изменять языковой файл и файл
                ресурсов (за исключением добавления/удаления строк меню),
                поэтому индексы в Lng, Dlg, Hlp файлах более не используются,
                а ресурсы плагинов теперь будут содержаться в файлах *.REZ,
                к которым Plugin Manager не будет иметь никакого отношения
                Версия плагинов EventCatcher - 2.0
   23-01-2002 - плагины типа ArchiveViewer
   10-05-2002 - вся структура плагинов переделана заново, без использования
                DN2CAT.DLL
}

uses
  {$IFDEF OS2}Os2Base, {$ENDIF}
  {$IFDEF WIN32}Windows, {$ENDIF}
  Dos, VpSysLow, Commands, ObjType,
  _Defines, _DNFuncs, _Model1,
  _Objects, _Streams, _Collect, _Views, _Menus, _Dialogs, _Apps
  ;

const
  type_EventCatcher = 1;
  type_ArchiveViewer = 2;

  {
const
  dlPlugins=Integer(dlPlugins0);
  dlPluginsEnd=32000;
  dlgPlugins=Integer(dlgPlugins0);
  dlgPluginsEnd=32000;
  hcPlugins=25000;
  hcPluginsEnd=45000;
}

var
  KilledEventCatchersCount: Integer;

type
  PStringArray = ^TStringArray;
  PWordArray = ^TWordArray;
  TStringArray = array[0..0] of ShortString;
  TWordArray = array[0..0] of Word;

  PCommandUsed = ^TCommandUsed;
  {PLngIndexUsed=^TLngIndexUsed;}
  {PDlgIndexUsed=^TDlgIndexUsed;}
  {PHlpIndexUsed=^THlpIndexUsed;}
  PObjTypeUsed = ^TObjTypeUsed;
  TCommandUsed = array[cmPlugins..cmPluginsEnd] of Boolean;
  {TLngIndexUsed=array[dlPlugins..dlPluginsEnd] of Boolean;}
  {TDlgIndexUsed=array[dlgPlugins..dlgPluginsEnd] of Boolean;}
  {THlpIndexUsed=array[hcPlugins..hcPluginsEnd] of Boolean;}
  TObjTypeUsed = array[otPlugins..otPluginsEnd] of Boolean;

  PPluginListItem = ^TPluginListItem;
  TPluginListItem = object(TObject)
    PluginType: LongInt;
    FirstCatchedCommand: Word;
    LastCatchedCommand: Word;
    {FirstLngIndex: Word;}
    {LastLngIndex: Word;}
    {FirstDlgIndex: Word;}
    {LastDlgIndex: Word;}
    {FirstHlpIndex: Word;}
    {LastHlpIndex: Word;}
    FirstObjType: Word;
    LastObjType: Word;
    PluginPath: String[8];
    Description: String[255];
    Installed: Word;
    RegFilePresent: Boolean;
    MenuStr1: PStringArray;
    MenuStr2: PStringArray;
    MenuKey: PWordArray;
    MenuHelpCtx: PWordArray;
    MenuType: PWordArray;
    MenuWantBeAfter: PWordArray;
    constructor Init(PT: LongInt; FCC, LCC,
        {FLI, LLI, FDI, LDI, FHI, LHI,}FOT, LOT: Word; RFP: Boolean;
         PP: String);
    destructor Done; virtual;
    end;

  PPluginList = ^TPluginList;
  TPluginList = object(TListBox)
    function GetText(Item: Integer; MaxLen: Integer): String; virtual;
    procedure FocusItem(Item: Integer); virtual;
    end;

  PPluginManager = ^TPluginManager;
  TPluginManager = object(TDialog)
    CommandUsed: PCommandUsed;
    {LngIndexUsed: PLngIndexUsed;}
    {DlgIndexUsed: PDlgIndexUsed;}
    {HlpIndexUsed: PHlpIndexUsed;}
    ObjTypeUsed: PObjTypeUsed;

    ListBox: PListBox;
    InstallButton: PButton;
    function MakeCollection: PCollection;
    constructor Init;
    constructor Load(var S: TStream);
    procedure Store(var S: TStream);
    procedure HandleEvent(var Event: TEvent); virtual;
    destructor Done; virtual;
    end;

  TMenuChanger = procedure (Item: PPluginListItem; MenuView: PMenuView;
     CurMenuType: Byte);

const
  PluginManager: PPluginManager = nil;

function OverwriteConfigFile: Boolean;
  var
    Stream: PStream;
    I, W: Integer;
  begin
  with DNFunctions^, SomeObjects2^, SomeObjects3^ do
    begin
    Stream := New(PBufStream, Init(SourceDir+'PLUGINS.CFG',
           Open_Access_WriteOnly or Open_Share_DenyReadWrite, 1024));

    W := EventCatchersCount-KilledEventCatchersCount;
    Stream^.Write(W, SizeOf(W));
    for I := 1 to EventCatchersCount do
      with EventCatchers^[I], Stream^ do
        if FirstCatchedCommand <> $FFFF then
          begin
          Write(FirstCatchedCommand, SizeOf(FirstCatchedCommand));
          Write(LastCatchedCommand, SizeOf(LastCatchedCommand));
          {Write(FirstLngIndex, SizeOf(FirstLngIndex));}
          {Write(LastLngIndex, SizeOf(LastLngIndex));}
          {Write(FirstDlgIndex, SizeOf(FirstDlgIndex));}
          {Write(LastDlgIndex, SizeOf(LastDlgIndex));}
          {Write(FirstHlpIndex, SizeOf(FirstHlpIndex));}
          {Write(LastHlpIndex, SizeOf(LastHlpIndex));}
          Write(FirstObjType, SizeOf(FirstObjType));
          Write(LastObjType, SizeOf(LastObjType));
          Write(PluginPath, 1+Length(PluginPath));
          end;

    W := 0;
    for I := ArcFirst to arcLast do
      if  (ArchiveViewers[I] <> nil)
           and (ArchiveViewers[I] <> ArchiveViewers[I-1])
      then
        Inc(W);
    Stream^.Write(W, SizeOf(W));
    for I := ArcFirst to arcLast do
      if  (ArchiveViewers[I] <> nil)
           and (ArchiveViewers[I] <> ArchiveViewers[I-1])
      then
        with ArchiveViewers[I]^, Stream^ do
          Write(PluginPath, 1+Length(PluginPath));
    end;

  Stream^.Truncate;
  Result := (Stream^.Status = stOK);
  Dispose(Stream, Done);
  end { OverwriteConfigFile: };

{ дубликат этой функции находится в MenuEdit.pas, изменения согласовывать }
function ChangeMenuResource(MenuChanger: TMenuChanger; Item: Pointer;
     MenuView: PMenuView; dlgMenu: Integer; CurMenuType: Byte): Boolean;
  var
    P: Pointer;
    I: Integer;
    NextResourceOffset: LongInt;
    MenuResourceOffset: LongInt;
    RestResourceSize: LongInt;
    MenuResourceSize: LongInt;
    Index: PIndexArray;
  begin
  if  (PPluginListItem(Item)^.PluginType and type_EventCatcher) = 0 then
    begin
    Result := True;
    Exit;
    end;

  { изменяем меню }
  MenuChanger(Item, MenuView, CurMenuType);

  with DNFunctions^, SomeObjects1^ do
    begin
    { копируем индекс ресурсов во временный буфер }
    GetMem(Index, Resource^.Count*SizeOf(LongInt));
    Move(Resource^.Index^, Index^, Resource^.Count*SizeOf(LongInt));

    { находим ресурс, располагающийся следом за меню }
    NextResourceOffset := MaxLongInt;
    MenuResourceOffset := Index^[dlgMenu];

    for I := 0 to Resource^.Count-1 do
      if  (Index^[I] > MenuResourceOffset)
           and (Index^[I] < NextResourceOffset)
      then
        NextResourceOffset := Index^[I];
    RestResourceSize := PStream
      (ResourceStream)^.GetSize-NextResourceOffset;
    MenuResourceSize := NextResourceOffset-MenuResourceOffset;

    { переоткрываем файл ресурсов, чтобы можно было в него писать }
    repeat
      PStream(ResourceStream)^.Close;
      PStream(ResourceStream)^.DoOpen(Open_Access_ReadWrite or
         Open_Share_DenyWrite);
      if PStream(ResourceStream)^.Status = stOK then
        Break;
      MessageBox(GetString(Integer(dlPlugins5)), nil, mfError+mfOKButton);
    until False;

    if NextResourceOffset <> MaxLongInt then
      begin

      { сдвигаем на место меню ресурсы, располагающиеся за ним, а само меню сдвигаем в конец файла }
      for I := 0 to Resource^.Count-1 do
        if Index^[I] > MenuResourceOffset then
          Dec(Index^[I], MenuResourceSize);
      Inc(Index^[dlgMenu], RestResourceSize);

      { удаляем меню из файла ресурсов - осторожно!! }
      if MaxAvail < RestResourceSize then
        begin
        Result := False;
        Exit;
        end;
      GetMem(P, RestResourceSize);
      PStream(ResourceStream)^.Seek(NextResourceOffset);
      PStream(ResourceStream)^.Read(P^, RestResourceSize);
      if PStream(ResourceStream)^.Status <> stOK then
        begin
        FreeMem(P {, RestResourceSize});
        Result := False;
        Exit;
        end;
      PStream(ResourceStream)^.Seek(0);
      PStream(ResourceStream)^.Write(Resource^.Count, 2);
      PStream(ResourceStream)^.Write(Index^,
           Resource^.Count*SizeOf(LongInt));
      PStream(ResourceStream)^.Seek(MenuResourceOffset);
      PStream(ResourceStream)^.Write(P^, RestResourceSize);
      FreeMem(P {, RestResourceSize});
      end
    else
      PStream(ResourceStream)^.Seek(MenuResourceOffset);

    { запихиваем меню обратно в Dlg-файл }
    PStream(ResourceStream)^.Put(MenuView);
    Result := (PStream(ResourceStream)^.Status = stOK);

    { если всё прошло успешно, то возвращаем индекс ресурсов из временного буфера }
    Move(Index^, Resource^.Index^, Resource^.Count*SizeOf(LongInt));
    FreeMem(Index {, Resource^.Count*SizeOf(LongInt)});
    end;
  end { ChangeMenuResource };

procedure InstallMenu(Item: PPluginListItem; MenuView: PMenuView;
     CurMenuType: Byte);
  var
    S: String;
    SubMenuPtr, MenuItemPtr: ^PMenuItem;
    Command: Word;
    Count: Word;

  procedure WalkMenu(var MenuItem: PMenuItem);
    var
      MenuItemPtr: ^PMenuItem;
      Command: Word;
    begin
    with Item^ do
      begin
      MenuItemPtr := @MenuItem;
      while MenuItemPtr^ <> nil do
        begin
        if MenuItemPtr^^.Name <> nil then
          if MenuItemPtr^^.Command = 0 then
            WalkMenu(MenuItemPtr^^.SubMenu^.Items)
          else
            for Command := 0 to LastCatchedCommand-FirstCatchedCommand
            do
              if MenuItemPtr^^.Command = MenuWantBeAfter^[Command] then
                begin
                MenuItemPtr := @MenuItemPtr^^.Next;
                MenuItemPtr^:= DNFunctions^.NewItem(MenuStr1^[Command],
                     MenuStr2^[Command], MenuKey^[Command],
                     FirstCatchedCommand+Command, MenuHelpCtx^[Command],
                     MenuItemPtr^);
                MenuWantBeAfter^[Command] := -MenuWantBeAfter^[Command];
                { для обработанных элементов ставим отрицательное значение }
                end;
        MenuItemPtr := @MenuItemPtr^^.Next;
        end;
      end;
    end { WalkMenu };

  begin { InstallMenu }
  { сначала просматриваем меню и добавляем те строчки, }
  { которые хотят вставиться в какое-то определённое место }
  WalkMenu(MenuView^.Menu^.Items);

  { считаем, сколько ещё осталось добавить }
  Count := 0;
  with Item^ do
    for Command := 0 to LastCatchedCommand-FirstCatchedCommand do
      if MenuWantBeAfter^[Command] < 0 then
        MenuWantBeAfter^[Command] := -MenuWantBeAfter^[Command]
      else if MenuType^[Command] = CurMenuType then
        begin
        MenuWantBeAfter^[Command] := 0;
        Inc(Count);
        end;

  { если всё, что надо, уже добавлено, то тихо выходим }
  if Count = 0 then
    Exit;

  { в противном случае... }
  with DNFunctions^, SomeObjects1^ do
    begin
    S := GetString(Integer(dlPlugins4));
    SubMenuPtr := @PMenuBar(MenuView)^.Menu^.Items;

    { ищем подменю "плагины", если такого нету - создаём }
    while SubMenuPtr^ <> nil do
      if  (SubMenuPtr^^.Name <> nil) and (SubMenuPtr^^.Name^ = S)
           and (SubMenuPtr^^.Command = 0)
      then
        Break
      else
        SubMenuPtr := @SubMenuPtr^^.Next;
    if SubMenuPtr^ = nil then
      SubMenuPtr^:= NewSubMenu(S, hcNoContext, NewMenu(nil), nil);

    { спускаемся до конца меню и вставляем туда нужные строки }
    MenuItemPtr := @SubMenuPtr^^.SubMenu^.Items;
    while MenuItemPtr^ <> nil do
      MenuItemPtr := @MenuItemPtr^^.Next;
    with Item^ do
      for Command := 0 to LastCatchedCommand-FirstCatchedCommand do
        if  (MenuStr1^[Command] <> '')
             and (MenuType^[Command] = CurMenuType)
        then
          begin
          MenuItemPtr^:= NewItem(MenuStr1^[Command], MenuStr2^[Command],
               MenuKey^[Command], FirstCatchedCommand+Command,
               MenuHelpCtx^[Command], nil);
          MenuItemPtr := @MenuItemPtr^^.Next;
          end;

    { если текущий элемент не определён - устанавливаем первый текущим }
    if SubMenuPtr^^.SubMenu^.Default = nil then
      SubMenuPtr^^.SubMenu^.Default := SubMenuPtr^^.SubMenu^.Items;
    end;
  end { InstallMenu };

procedure UnInstallMenu(Item: PPluginListItem; MenuView: PMenuView;
     CurMenuType: Byte);

  { рекурсивное очищение подменю от ненужных команд }
  procedure ClearSubMenu(Menu: PMenu);
    var
      MenuItemPtr: ^PMenuItem;
      MenuItem: PMenuItem;
    begin
    MenuItemPtr := @Menu^.Items;
    with DNFunctions^ do
      while MenuItemPtr^ <> nil do
        begin
        { если это подменю - очищаем его рекурсивно }
        { если в результате очистки удалили все пункты - уничтожаем подменю }
        if  (MenuItemPtr^^.Name <> nil) and (MenuItemPtr^^.Command = 0)
        then
          begin
          ClearSubMenu(MenuItemPtr^^.SubMenu);
          if MenuItemPtr^^.SubMenu^.Items = nil then
            begin
            MenuItem := MenuItemPtr^^.Next;
            if MenuItemPtr^^.Name <> nil then
              DisposeStr(MenuItemPtr^^.Name);
            DisposeMenu(MenuItemPtr^^.SubMenu);
            Dispose(MenuItemPtr^);
            if Menu^.Default = MenuItemPtr^ then
              Menu^.Default := MenuItem;
            MenuItemPtr^:= MenuItem;
            end
          else
            MenuItemPtr := @MenuItemPtr^^.Next;
          end
          { если это обычный пункт меню и соответствует диапазону команд - удаляем }
        else if (MenuItemPtr^^.Command >= Item^.FirstCatchedCommand)
             and (MenuItemPtr^^.Command <= Item^.LastCatchedCommand)
        then
          begin
          MenuItem := MenuItemPtr^^.Next;
          if MenuItemPtr^^.Name <> nil then
            DisposeStr(MenuItemPtr^^.Name);
          if MenuItemPtr^^.Param <> nil then
            DisposeStr(MenuItemPtr^^.Param);
          Dispose(MenuItemPtr^);
          if Menu^.Default = MenuItemPtr^ then
            Menu^.Default := MenuItem;
          MenuItemPtr^:= MenuItem;
          end
        else
          MenuItemPtr := @MenuItemPtr^^.Next;
        end;
    if Menu^.Default = nil then
      Menu^.Default := Menu^.Items;
    end { ClearSubMenu };

  begin { UnInstallMenu }
  ClearSubMenu(MenuView^.Menu);
  end { UnInstallMenu };

function Install(Item: PPluginListItem): Boolean;
  var
    FullPath: String;
    P: PArchiveViewerInfo;
    I, J, K: Integer;
    B: Boolean;
  begin
  with DNFunctions^, SomeObjects2^, SomeObjects3^ do
    begin
    if  (Item^.PluginType and type_EventCatcher) <> 0 then
      begin
      { если нету регистрационного файла - не будем устанавливать }
      { если плагинов слишком много - то же самое }
      if not Item^.RegFilePresent or (EventCatchersCount >= 60000) then
        begin
        Result := False;
        Exit;
        end;

      { добавляем плагин в список установленных }
      Inc(EventCatchersCount);
      ReallocMem(EventCatchers,
           EventCatchersCount*SizeOf(TEventCatcherInfo));
      with EventCatchers^[EventCatchersCount] do
        begin
        FirstCatchedCommand := Item^.FirstCatchedCommand;
        LastCatchedCommand := Item^.LastCatchedCommand;
        {FirstLngIndex:=Item^.FirstLngIndex;}
        {LastLngIndex:=Item^.LastLngIndex;}
        {FirstDlgIndex:=Item^.FirstDlgIndex;}
        {LastDlgIndex:=Item^.LastDlgIndex;}
        {FirstHlpIndex:=Item^.FirstHlpIndex;}
        {LastHlpIndex:=Item^.LastHlpIndex;}
        FirstObjType := Item^.FirstObjType;
        LastObjType := Item^.LastObjType;
        PluginPath := Item^.PluginPath;
        LibHandle := 0;
        @Entry := nil;

        Item^.Installed := EventCatchersCount;
        end;

      { переписываем конфигурационный файл }
      Result := OverwriteConfigFile;
      end;

    if  (Item^.PluginType and type_ArchiveViewer) <> 0 then
      begin
      { грузим плагин }
      New(P);
      with P^ do
        begin
        PluginPath := Item^.PluginPath;
        FullPath := SourceDir+PluginPath+'.DLL'#0;
        {$IFDEF OS2}
        if  (DosLoadModule(nil, 0, @FullPath[1], LibHandle) <> 0)
          or (DosQueryProcAddr(LibHandle, 0, 'FormatsCount',
               @FormatsCount) <> 0)
          or (DosQueryProcAddr(LibHandle, 0, 'ArchiveSign', @ArchiveSign)
             <> 0)
          or (DosQueryProcAddr(LibHandle, 0, 'CreateArchiveObject',
               @CreateArchiveObject) <> 0)
          or (DosQueryProcAddr(LibHandle, 0,
               'DetectCreateArchiveObject', @DetectCreateArchiveObject) <>
             0)
        then
          begin
          MessageBox(GetString(Integer(dlCantLoad))+PluginPath, nil,
             mfError+mfOKButton);
          Dispose(P);
          Result := False;
          Exit;
          end;
        {$ENDIF}
        {$IFDEF WIN32}
        LibHandle := LoadLibrary(@FullPath[1]);
        if LibHandle < HINSTANCE_ERROR then
          begin
          MessageBox(GetString(Integer(dlCantLoad))+PluginPath, nil,
             mfError+mfOKButton);
          Dispose(P);
          Result := False;
          Exit;
          end;
        @FormatsCount := GetProcAddress(LibHandle, 'FormatsCount');
        @ArchiveSign := GetProcAddress(LibHandle, 'ArchiveSign');
        @CreateArchiveObject := GetProcAddress(LibHandle,
             'CreateArchiveObject');
        @DetectCreateArchiveObject := GetProcAddress(LibHandle,
             'DetectCreateArchiveObject');
        if not Assigned(FormatsCount) or not Assigned(ArchiveSign)
           or not Assigned(CreateArchiveObject)
             or not Assigned(DetectCreateArchiveObject)
        then
          begin
          MessageBox(GetString(Integer(dlCantLoad))+PluginPath, nil,
             mfError+mfOKButton);
          Dispose(P);
          Result := False;
          Exit;
          end;
        {$ENDIF}
        end;

      { спрашиваем, сколько форматов данный плагин поддерживает }
      { и ищем свободное место, куда бы его приткнуть }
      K := P^.FormatsCount;
      for I := ArcFirst to arcLast-K do
        begin
        B := False;
        for J := I to I+K-1 do
          begin
          B := B or (ArchiveViewers[J] <> nil);
          if B then
            Break;
          end;
        if not B then
          Break;
        end;
      if B then
        begin
        {$IFDEF OS2}
        DosFreeModule(P^.LibHandle);
        {$ENDIF}
        {$IFDEF WIN32}
        FreeLibrary(P^.LibHandle);
        {$ENDIF}
        Dispose(P);
        Result := False;
        Exit;
        end;
      P^.FirstTag := I;
      for J := I to I+K-1 do
        ArchiveViewers[J] := P;

      { переписываем конфигурационный файл }
      Result := OverwriteConfigFile;
      if Result then
        Item^.Installed := I
      else
        begin
        for J := I to I+K-1 do
          ArchiveViewers[J] := nil;
        {$IFDEF OS2}
        DosFreeModule(P^.LibHandle);
        {$ENDIF}
        {$IFDEF WIN32}
        FreeLibrary(P^.LibHandle);
        {$ENDIF}
        for J := I to I+K-1 do
          ArchiveViewers[J] := nil;
        Dispose(P);
        end;
      end;
    end;
  end { Install };

function UnInstall(Item: PPluginListItem): Boolean;
  var
    I, J: Integer;
  begin
  with DNFunctions^, SomeObjects2^, SomeObjects3^ do
    begin
    if  ( (Item^.PluginType and type_EventCatcher) <> 0)
         and (Item^.Installed >= 1)
         and (Item^.Installed <= EventCatchersCount)
    then
      begin
      { убираем возможность запустить плагин }
      with EventCatchers^[Item^.Installed] do
        if FirstCatchedCommand <> $FFFF then
          begin
          FirstCatchedCommand := $FFFF;
          Inc(KilledEventCatchersCount);
          Item^.Installed := 0;
          end;

      { переписываем конфигурационный файл }
      Result := OverwriteConfigFile;
      end;

    if  (Item^.PluginType and type_ArchiveViewer) <> 0 then
      begin
      if  (Item^.Installed < ArcFirst) or (Item^.Installed > arcLast)
      then
        begin
        Result := False;
        Exit;
        end;

      { выгружаем плагин }
      { вычищаем все упоминания о нём }
      J := ArchiveViewers[Item^.Installed]^.FirstTag;
      for I := J+1 to J+ArchiveViewers[Item^.Installed]^.FormatsCount-1
      do
        ArchiveViewers[I] := nil;
      { надо бы выгрузить библиотеку, но она ещё может использоваться }
      { поэтому понадеемся, что при выходе из программы система всё подчистит }
      (*
          {$IFDEF OS2}
          DosFreeModule(ArchiveViewers[J]^.LibHandle);
          {$ENDIF}
          {$IFDEF WIN32}
          FreeLibrary(ArchiveViewers[J]^.LibHandle);
          {$ENDIF}
          *)
      Dispose(ArchiveViewers[J]);
      ArchiveViewers[J] := nil;

      { переписываем конфигурационный файл }
      Result := OverwriteConfigFile;
      if Result then
        Item^.Installed := 0;
      end;
    end;
  end { UnInstall };

constructor TPluginListItem.Init(PT: LongInt; FCC, LCC,
    {FLI, LLI, FDI, LDI, FHI, LHI,}FOT, LOT: Word; RFP: Boolean;
     PP: String);
  var
    CommandCount: Word;
  begin
  _TObject^.Init(nil, @Self);
  PluginType := PT;
  FirstCatchedCommand := FCC;
  LastCatchedCommand := LCC;
  {FirstLngIndex:=FLI;}
  {LastLngIndex:=LLI;}
  {FirstDlgIndex:=FDI;}
  {LastDlgIndex:=LDI;}
  {FirstHlpIndex:=FHI;}
  {LastHlpIndex:=LHI;}
  FirstObjType := FOT;
  LastObjType := LOT;
  RegFilePresent := RFP;
  PluginPath := PP;
  DNFunctions^.UpStr(PluginPath);
  Description := '';
  Installed := 0;
  CommandCount := LastCatchedCommand-FirstCatchedCommand+1;
  GetMem(MenuStr1, CommandCount*SizeOf(String));
  GetMem(MenuStr2, CommandCount*SizeOf(String));
  GetMem(MenuKey, CommandCount*SizeOf(Word));
  GetMem(MenuHelpCtx, CommandCount*SizeOf(Word));
  GetMem(MenuType, CommandCount*SizeOf(Word));
  GetMem(MenuWantBeAfter, CommandCount*SizeOf(Word));
  FillChar(MenuStr1^, CommandCount*SizeOf(String), #0);
  FillChar(MenuStr2^, CommandCount*SizeOf(String), #0);
  FillChar(MenuKey^, CommandCount*SizeOf(Word), #0);
  FillChar(MenuHelpCtx^, CommandCount*SizeOf(Word), #0);
  FillChar(MenuType^, CommandCount*SizeOf(Word), #0);
  FillChar(MenuWantBeAfter^, CommandCount*SizeOf(Word), #0);
  end { TPluginListItem.Init };

destructor TPluginListItem.Done;
  var
    CommandCount: Word;
  begin
  CommandCount := LastCatchedCommand-FirstCatchedCommand+1;
  FreeMem(MenuStr1 {, CommandCount*SizeOf(String)});
  FreeMem(MenuStr2 {, CommandCount*SizeOf(String)});
  FreeMem(MenuKey {, CommandCount*SizeOf(Word)});
  FreeMem(MenuHelpCtx {, CommandCount*SizeOf(Word)});
  FreeMem(MenuType {, CommandCount*SizeOf(Word)});
  FreeMem(MenuWantBeAfter {, CommandCount*SizeOf(Word)});
  _TObject^.VMT^.Done(0, @Self);
  end;

function TPluginList.GetText(Item: Integer; MaxLen: Integer): String;
  begin
  with PPluginListItem(List^.At(Item))^ do
    begin
    if Description <> '' then
      Result := Description
    else
      Result := PluginPath;
    if Installed <> 0 then
      Result := '* '+Result
    else
      Result := '  '+Result;
    end;
  if Length(Result) > MaxLen then
    SetLength(Result, MaxLen);
  end;

procedure TPluginList.FocusItem(Item: LongInt);
  begin
  _TListBox^.VMT^.FocusItem(Item, @Self);
  if Owner <> nil then
    with PPluginManager(Owner)^.InstallButton^, DNFunctions^ do
      begin
      DisposeStr(Title);
      if PPluginListItem(List^.At(Item))^.Installed = 0 then
        Title := NewStr(GetString(Integer(dlPlugins8)))
      else
        Title := NewStr(GetString(Integer(dlPlugins9)));
      DrawView;
      end;
  end;

function TPluginManager.MakeCollection: PCollection;
  label
    1, 2, 3, L1, L2, L3, L4, L5;
  var
    I, J: Integer;
    WW: array[1..16] of Word;
    FirstCatchedCommand: Word;
    LastCatchedCommand: Word;
    {FirstLngIndex: Word;}
    {LastLngIndex: Word;}
    {FirstDlgIndex: Word;}
    {LastDlgIndex: Word;}
    {FirstHlpIndex: Word;}
    {LastHlpIndex: Word;}
    FirstObjType: Word;
    LastObjType: Word;
    Name: String[8];
    Description: String[255];
    Item: PPluginListItem;
    SR: lSearchRec;
    Stream: PStream;
    MyLngId, PluginLngId: String[255];
  begin { TPluginManager.MakeCollection: }
  Result := New(PCollection, Init(16, 16));

  with DNFunctions^, SomeObjects2^, SomeObjects3^ do
    begin
    MyLngId := LngId;
    UpStr(MyLngId);

    { просматриваем установленные плагины EventCatcher }
    for I := 1 to EventCatchersCount do
      with EventCatchers^[I] do
        if  (FirstCatchedCommand <> $FFFF) then
          begin
          Item := New(PPluginListItem, Init(type_EventCatcher,
                FirstCatchedCommand, LastCatchedCommand,
                {FirstLngIndex, LastLngIndex,}
                {FirstDlgIndex, LastDlgIndex,}
                {FirstHlpIndex, LastHlpIndex,}
                FirstObjType, LastObjType,
                False,
                PluginPath));
          Item^.Installed := I;
          Result^.Insert(Item);
          for J := FirstCatchedCommand to LastCatchedCommand do
            CommandUsed^[J] := True;
          {
              if FirstLngIndex<>$FFFF then
                for J:=FirstLngIndex to LastLngIndex do
                  LngIndexUsed^[J]:=True;
              if FirstDlgIndex<>$FFFF then
                for J:=FirstDlgIndex to LastDlgIndex do
                  DlgIndexUsed^[J]:=True;
              if FirstHlpIndex<>$FFFF then
                for J:=FirstHlpIndex to LastHlpIndex do
                  HlpIndexUsed^[J]:=True;
              }
          if FirstObjType <> $FFFF then
            for J := FirstObjType to LastObjType do
              ObjTypeUsed^[J] := True;
          end;

    { просматриваем установленные плагины ArchiveViewer }
    for I := ArcFirst to arcLast do
      if  (ArchiveViewers[I] <> nil)
           and (ArchiveViewers[I] <> ArchiveViewers[I-1])
      then
        begin
        Item := New(PPluginListItem, Init(type_ArchiveViewer,
              0, 0,
              $FFFF, $FFFF,
              False,
              ArchiveViewers[I]^.PluginPath));
        Item^.Installed := I;
        Result^.Insert(Item);
        end;

    { просматриваем регистрационные файлы }
    lFindFirst(SourceDir+'*.REG', AnyFile and not(Directory+VolumeID), SR);
    while DosError = 0 do
      begin
      Stream := New(PBufStream, Init(SourceDir+SR.FullName, stOpenRead,
             512));
      Stream^.Read(WW, SizeOf(WW));
      Stream^.Read(Name, SizeOf(Name));
      UpStr(Name);
      Stream^.ReadStrV(Description);
      if  (Stream^.Status <> stOK) or (WW[7] <> $777A {Magic})
           or (WW[8] <> $DEF2 {Magic})
      then
        MessageBox(GetString(Integer(dlCantLoad))+SR.FullName, nil,
           mfError+mfOKButton)
      else
        begin
        { проверяем, не попался ли уже этот плагин среди установленных }
        for I := 0 to Result^.Count-1 do
          if PPluginListItem(Result^.At(I))^.PluginPath = Name then
            begin
            Item := PPluginListItem(Result^.At(I));
            Item^.Description := Description;
            Item^.RegFilePresent := True;
            goto 1;
            end;

        { ищем незанятую область команд нужной длины }
        for I := cmPlugins to cmPluginsEnd-WW[1] do
          begin
          for J := I to I+WW[1] do
            if CommandUsed^[J] then
              Break;
          if not CommandUsed^[J] then
            begin
            FirstCatchedCommand := I;
            LastCatchedCommand := I+WW[1];
            for J := I to I+WW[1] do
              CommandUsed^[J] := True;
            goto L1;
            end;
          end;
        MessageBox(GetString(Integer(dlPlugins1)), nil,
           mfError+mfOKButton);
        Dispose(Stream, Done);
        goto 2;
L1:

        { ищем незанятую область индексов Lng-файла нужной длины }
        {
              if WW[2]=0 then
                begin
                  FirstLngIndex:=$FFFF;
                  LastLngIndex:=$FFFF;
                end
              else
                begin
                  for I:=dlPlugins to dlPluginsEnd-WW[2] do
                    begin
                      for J:=I to I+WW[2]-1 do
                        if LngIndexUsed^[J] then
                          Break;
                      if not LngIndexUsed^[J] then
                        begin
                          FirstLngIndex:=I;
                          LastLngIndex:=I+WW[2]-1;
                          for J:=I to I+WW[2]-1 do
                            LngIndexUsed^[J]:=True;
                          goto L2;
                        end;
                    end;
                  MessageBox(GetString(Integer(dlPlugins1)), nil, mfError+mfOKButton);
                  Dispose(Stream, Done);
                  goto 2;
                  L2:
                end;
              }

        { ищем незанятую область индексов Dlg-файла нужной длины }
        {
              if WW[3]=0 then
                begin
                  FirstDlgIndex:=$FFFF;
                  LastDlgIndex:=$FFFF;
                end
              else
                begin
                  for I:=dlgPlugins to dlgPluginsEnd-WW[3] do
                    begin
                      for J:=I to I+WW[3]-1 do
                        if DlgIndexUsed^[J] then
                          Break;
                      if not DlgIndexUsed^[J] then
                        begin
                          FirstDlgIndex:=I;
                          LastDlgIndex:=I+WW[3]-1;
                          for J:=I to I+WW[3]-1 do
                            DlgIndexUsed^[J]:=True;
                          goto L3;
                        end;
                    end;
                  MessageBox(GetString(Integer(dlPlugins1)), nil, mfError+mfOKButton);
                  Dispose(Stream, Done);
                  goto 2;
                  L3:
                end;
              }

        { ищем незанятую область индексов Hlp-файла нужной длины }
        {
              if WW[4]=0 then
                begin
                  FirstHlpIndex:=$FFFF;
                  LastHlpIndex:=$FFFF;
                end
              else
                begin
                  for I:=hcPlugins to hcPluginsEnd-WW[4] do
                    begin
                      for J:=I to I+WW[4]-1 do
                        if HlpIndexUsed^[J] then
                          Break;
                      if not HlpIndexUsed^[J] then
                        begin
                          FirstHlpIndex:=I;
                          LastHlpIndex:=I+WW[4]-1;
                          for J:=I to I+WW[4]-1 do
                            HlpIndexUsed^[J]:=True;
                          goto L4;
                        end;
                    end;
                  MessageBox(GetString(Integer(dlPlugins1)), nil, mfError+mfOKButton);
                  Dispose(Stream, Done);
                  goto 2;
                  L4:
                end;
              }

        { ищем незанятую область регистрационных кодов объектов нужной длины }
        if WW[9] = 0 then
          begin
          FirstObjType := $FFFF;
          LastObjType := $FFFF;
          end
        else
          begin
          for I := otPlugins to otPluginsEnd-WW[9]-1 do
            begin
            for J := I to I+WW[9] do
              if ObjTypeUsed^[J] then
                Break;
            if not ObjTypeUsed^[J] then
              begin
              FirstObjType := I;
              LastObjType := I+WW[9];
              for J := I to I+WW[9] do
                ObjTypeUsed^[J] := True;
              goto L5;
              end;
            end;
          MessageBox(GetString(Integer(dlPlugins1)), nil,
             mfError+mfOKButton);
          Dispose(Stream, Done);
          goto 2;
L5:
          end;

        Item := New(PPluginListItem, Init(WW[6],
              FirstCatchedCommand, LastCatchedCommand,
              {FirstLngIndex, LastLngIndex,}
              {FirstDlgIndex, LastDlgIndex,}
              {FirstHlpIndex, LastHlpIndex,}
              FirstObjType, LastObjType,
              True,
              Name));
        Result^.Insert(Item);

1:

        { дочитываем из регистрационного файла информацию о меню }
        { повторяем до тех пор, пока не найдём нужный язык }
        { если языки закончились, а нужный не найден - используем последний }
        with Stream^, Item^ do
          for J := 0 to WW[5] do
            begin
            ReadStrV(PluginLngId);
            UpStr(PluginLngId);

            for I := 0 to WW[1] do
              begin
              ReadStrV(MenuStr1^[I]);
              ReadStrV(MenuStr2^[I]);
              Read(MenuKey^[I], SizeOf(Word));
              Read(MenuHelpCtx^[I], SizeOf(Word));
              Read(MenuType^[I], SizeOf(Word));
              Read(MenuWantBeAfter^[I], SizeOf(Word));
              end;

            if Stream^.Status <> stOK then
              begin
              MessageBox(GetString(Integer(dlCantLoad))+SR.FullName,
                 nil, mfError+mfOKButton);
              Dispose(Item, Done);
              goto 3;
              end;

            if PluginLngId = MyLngId then
              Break;
            end;

        Item^.Description := Description;
        end;
3:
      Dispose(Stream, Done);
      lFindNext(SR);
      end;
2:
    lFindClose(SR);
    end;
  end { TPluginManager.MakeCollection: };

constructor TPluginManager.Init;
  var
    R: TRect;
    ScrollBar: PScrollBar;
  begin
  with DNFunctions^ do
    begin
    PluginManager := @Self;

    R.Assign(0, 0, 58, 20);
    _TDialog^.Init(R, GetString(Integer(dlPlugins0)), nil, @Self);

    New(CommandUsed);
    {New(LngIndexUsed);}
    {New(DlgIndexUsed);}
    {New(HlpIndexUsed);}
    New(ObjTypeUsed);
    FillChar(CommandUsed^, SizeOf(CommandUsed^), #0);
    {FillChar(LngIndexUsed^, SizeOf(LngIndexUsed^), #0);}
    {FillChar(DlgIndexUsed^, SizeOf(DlgIndexUsed^), #0);}
    {FillChar(HlpIndexUsed^, SizeOf(HlpIndexUsed^), #0);}
    FillChar(ObjTypeUsed^, SizeOf(ObjTypeUsed^), #0);

    Number := GetWinNumber;
    Options := Options or ofCentered;
    R.Assign(5, 17, 22, 19);
    InstallButton := New(PButton, Init(R,
           GetString(Integer(dlPlugins9)), cmOK, bfDefault));
    Insert(InstallButton);
    R.Assign(24, 17, 41, 19);
    Insert(New(PButton, Init(R, GetString(Integer(dlCloseButton)),
           cmCancel, bfNormal)));
    R.Assign(55, 1, 56, 16);
    ScrollBar := New(PScrollBar, Init(R));
    Insert(ScrollBar);
    R.Assign(2, 1, 55, 16);
    ListBox := New(PPluginList, Init(R, 1, ScrollBar));
    ListBox^.NewLisT(MakeCollection);
    Insert(ListBox);
    end;
  end { TPluginManager.Init };

constructor TPluginManager.Load(var S: TStream);
  var
    FocusOn: Integer;
  begin
  Init;
  S.Read(Origin, SizeOf(Origin));
  S.Read(Size, SizeOf(Size));
  S.Read(FocusOn, SizeOf(FocusOn));
  if FocusOn >= ListBox^.Range then
    FocusOn := ListBox^.Range-1;
  if FocusOn >= 0 then
    ListBox^.FocusItem(FocusOn);
  end;

procedure TPluginManager.Store(var S: TStream);
  var
    FocusOn: Integer;
  begin
  FocusOn := ListBox^.Focused;
  S.Write(Origin, SizeOf(Origin));
  S.Write(Size, SizeOf(Size));
  S.Write(FocusOn, SizeOf(FocusOn));
  end;

procedure TPluginManager.HandleEvent(var Event: TEvent);
  {$IFDEF DEBUG}
  const
    InstStr: array[Boolean] of String[7] =
      ('Remove', 'Install');

  function ItoS(L: LongInt): String;
    begin
    Str(L, Result);
    end;
  {$ENDIF}

  var
    PluginListItem: PPluginListItem;
    P: PString;
  begin
  _TDialog^.VMT^.HandleEvent(Event, @Self);
  if Event.What = evCommand then
    case Event.Command of
      cmOK:
        begin
        PluginListItem := PPluginListItem
              (ListBox^.List^.At(ListBox^.Focused));
        with PluginListItem^, DNFunctions^, SomeObjects1^ do
          begin
          {$IFDEF DEBUG}
          MessageBox(InstStr[Installed = 0]+': "'+PluginPath+'"'
            +^M'   cmd = '+ItoS(FirstCatchedCommand)+'..'+ItoS(
              LastCatchedCommand)
            +^M'   obj = '+ItoS(FirstObjType)+'..'+ItoS(LastObjType)
            {+^M'   lng = '+ItoS(FirstLngIndex)+'..'+ItoS(LastLngIndex)}
            {+^M'   dlg = '+ItoS(FirstDlgIndex)+'..'+ItoS(LastDlgIndex)}
            {+^M'   hlp = '+ItoS(FirstHlpIndex)+'..'+ItoS(LastHlpIndex)}
            , nil, mfInformation+mfOKButton);
          {$ENDIF}
          P := PString(@PluginPath);
          if Installed <> 0 then
            if  (PluginListItem^.PluginPath <> 'PLUGMAN')
              and UnInstall(PluginListItem)
              and ChangeMenuResource(UnInstallMenu, PluginListItem,
                 MenuBar, Integer(dlgMainMenu), 0)
              and ChangeMenuResource(UnInstallMenu, PluginListItem,
                 PMenuView(LoadResource(Integer(dlgEditorMenu))),
                   Integer(dlgEditorMenu), 1)
              and ChangeMenuResource(UnInstallMenu, PluginListItem,
                 PMenuView(LoadResource(Integer(dlgWkzMenuBar))),
                   Integer(dlgWkzMenuBar), 2)
            then
              ListBox^.DrawView
            else
              MessageBox(GetString(Integer(dlPlugins3)), @P,
                 mfError+mfOKButton)
          else if Install(PluginListItem)
            and ChangeMenuResource(InstallMenu, PluginListItem, MenuBar,
               Integer(dlgMainMenu), 0)
            and ChangeMenuResource(InstallMenu, PluginListItem,
                 PMenuView(LoadResource(Integer(dlgEditorMenu))),
                 Integer(dlgEditorMenu), 1)
            and ChangeMenuResource(InstallMenu, PluginListItem,
                 PMenuView(LoadResource(Integer(dlgWkzMenuBar))),
                 Integer(dlgWkzMenuBar), 2)
          then
            ListBox^.DrawView
          else
            MessageBox(GetString(Integer(dlPlugins2)), @P,
               mfError+mfOKButton);
          with InstallButton^ do
            begin
            DisposeStr(Title);
            if Installed = 0 then
              Title := NewStr(GetString(Integer(dlPlugins8)))
            else
              Title := NewStr(GetString(Integer(dlPlugins9)));
            DrawView;
            end;
          end;
        end;
      cmCancel:
        Close;
      cmGetName:
        PString(Event.InfoPtr)^:= DNFunctions^.GetString
              (Integer(dlPlugins0));
    end {case};
  end { TPluginManager.HandleEvent };

destructor TPluginManager.Done;
  begin
  Dispose(CommandUsed);
  {Dispose(LngIndexUsed);}
  {Dispose(DlgIndexUsed);}
  {Dispose(HlpIndexUsed);}
  Dispose(ObjTypeUsed);
  _TDialog^.VMT^.Done(0, @Self);
  PluginManager := nil;
  end;

{&Cdecl+}
procedure CatchCommand(Command, ObjType: SmallWord;
     const PluginName: ShortString; Functions, Methods: Pointer;
     var FinalizationProc: Pointer);
  const
    Initialized: Boolean = False;
  begin
  FinalizationProc := nil;
  if not Initialized then
    begin
    InitDNFunctions(Functions, Methods);

    with DNFunctions^ do
      if APIVersion < 4 then
        begin
        MessageBox
          ('Newer version of DN/2 is required for start this plugin'^M^M+
          'Для запуска этого плагина требуется более новая версия DN/2',
           nil, mfError+mfOKButton);
        Exit;
        end;

    Initialized := True;

    TransportVMT(_TObject^.VMT, TypeOf(TObject),
       TypeOf(TPluginListItem), TObject_VMTSize);
    TransportVMT(_TListBox^.VMT, TypeOf(TListBox), TypeOf(TPluginList),
       TListBox_VMTSize);
    TransportVMT(_TDialog^.VMT, TypeOf(TDialog), TypeOf(TPluginManager),
       TDialog_VMTSize);

    TransportVMT(_TBufStream^.VMT, TypeOf(TBufStream),
       TypeOf(TBufStream), TBufStream_VMTSize);
    TransportVMT(_TCollection^.VMT, TypeOf(TCollection),
         TypeOf(TCollection), TCollection_VMTSize);
    TransportVMT(_TScrollBar^.VMT, TypeOf(TScrollBar),
       TypeOf(TScrollBar), TScrollBar_VMTSize);
    TransportVMT(_TButton^.VMT, TypeOf(TButton), TypeOf(TButton),
       TButton_VMTSize);

    KilledEventCatchersCount := 0;
    end;

  if Command = 0 then
    if PluginManager = nil then
      PDesktop(DNFunctions^.SomeObjects1^.Desktop)^.Insert
          (New(PPluginManager, Init))
    else
      PluginManager^.Select;
  end { CatchCommand };

exports
CatchCommand Name'CatchCommand';
{&Cdecl-}

begin
end.
