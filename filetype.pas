unit FileType;

interface

function GetFileType(const S: String; Attr: Byte): Integer;
procedure PrepareExtCollection;

const
  Executables: String = 'exe;bat;com';
  {JO: дополнительные расширения для поиска в архивах}
  AddArchives: String =
   'exe;com';
  Archives: String =
    {archive volumes for ace,ain,arj,rar,zxzip}
    'a||;r||;c||;$z|;'+
    {supported archives in order of appearance}
    'ace;ain;arc;pak;arj;bs2;bsa;cab;chz;ha;hap;hpk;hyp;z;'+
    'lha;ice;lzh;lim;ark;qrk;rar;sqz;tar;taz;tgz;gz;uc2;ufa;'+
    'zip;zoo;$z;zxz;7z;bz2;tbz2'+
    {arvid}
    'avt;tdr;'
    +'pk|';

  {sources}
  CustomMask1: String =
    'pas;asm;inc;c;cpp;cc;cxx;hdr;def;pp;h;hpp;hxx;rc;idb;bas;js;jav;java';

  {raw text}
  CustomMask2: String =
    '.;asc;dok;txt;ans;me;1st;diz;dif;inf;info;patch;pat;nfo;uue;uu|;u||'
    ;

  {databases & speadsheets}
  CustomMask3: String = 'dbf;db;wkz;wks;123;wk1;xls;mdb;mde';

  {kill'em!}
  CustomMask4: String = 'tmp;$$$;bak;qab;swp;log;lut;c2t;err;old';

  {graphics}
  CustomMask5: String =
     'pcx;bmp;gif;rle;ico;jpg;jpe;jpeg;png;tif;tga;wpg;psd;'+
    'pcd;cur;ani;dib';

  {config}
  CustomMask6: String = 'ini;cfg;ctl;nif;lst;m3u;pls;reg;mak';

  {audio & video}
  CustomMask7: String =
    {audio streams}
    'wav;voc;ym;aud;snd;wma;'+
    {compressed audio streams}
    'mp1;mp2;mp3;mpg;vqf;vtx;asf;as4;'+
    {sampled music}
    'mod;stm;s3m;xm;it;mdl;mtm;mid;cmf;sid;'+
    {video}
    'avi;mpeg;mpe;smk;bik;gl;mvy;mve;mov;fli;flc;tgv';

  {fonts}
  CustomMask8: String = 'fon;fot;fnt;ttf;ofm;pfm;afm;sft;eft;pft;xft;chr';

  {Office documents}
  CustomMask9: String = 'doc;wp;rtf;sdw;lwp;sam;html;htm';

  {system}
  CustomMask10: String = 'dll;drv;ifs;flt;add;sys;vxd;386;rtl;ovr;ovl';

implementation

uses
  Commands, Dos, Advance1
  , Objects
  ;

  {-DataCompBoy-} {piwamoto} {JO}
  { note: BIN, DAT, RAW are NOT included in any group, }
  { becouse there is TOO MUCH filetypes with these extensions }


type
  PExtItem =^TExtItem;
  TExtItem = record
    Mask: string[10];
    FType: Integer;
    end;

  PExtCollection = ^TExtCollection;
  TExtCollection = object(TSortedCollection)
    function Compare2(Key1, Key2: Pointer): Integer; //virtual; // "2" added by unxed
    procedure FreeItem(Item: Pointer); virtual;
    end;

var
  ExtCollection: PExtCollection;

function TExtCollection.Compare2(Key1, Key2: Pointer): Integer;
  var
    Ext1: PExtItem absolute Key1;
    Ext2: PExtItem absolute Key2;
    i: Integer;
    C1, C2: Char;
  begin
  i := 0;
  while True do
    begin
    Inc(i);
    if i > Length(Ext1^.Mask) then
      begin
      if i > Length(Ext2^.Mask) then
        Result := 0
      else
        Result := -1;
      Exit;
      end;
    if i > Length(Ext2^.Mask) then
      begin
      if i > Length(Ext1^.Mask) then
        Result := 0
      else
        Result := +1;
      Exit;
      end;
    C1 := Ext1^.Mask[i];
    C2 := Ext2^.Mask[i];
    if (C1 = C2) or (C1 = '?') or (C2 = '?') then
      Continue;
    if (C1 = '|') then
      begin
      case C2 of
        #0..Char(Byte('0')-1):
          Result := 1;
        '0'..'9':
          Continue;
        Char(Byte('9')+1)..#255:
          Result := -1;
      end {case};
      Exit;
      end;
    if (C2 = '|') then
      begin
      case C1 of
        #0..Char(Byte('0')-1):
          Result := -1;
        '0'..'9':
          Continue;
        Char(Byte('9')+1)..#255:
          Result := 1;
      end {case};
      Exit;
      end;
    if (C1 = '*') or (C2 = '*') then
      begin
      Result := 0;
      Exit;
      end;
    if C1 < C2 then
      Result := -1
    else
      Result := 1;
    Exit;
    end;
  end;

procedure TExtCollection.FreeItem(Item: Pointer);
  begin
  Dispose(Item);
  end;

procedure PutExtFilter(Filter: string; T: Integer);
  var
    i, l, j: Integer;
    P: PExtItem;
    M, MZ: String;
  begin
  UpStr(Filter);
  l := Length(Filter);
  while l > 0 do
    begin
    i := l;
    while (i <> 0) and (Filter[i] <> ';') do
      Dec(i);
    M := Copy(Filter, i+1, l-i);
    MZ := M;
    for l := 1 to Length(MZ) do
      if MZ[i] = '|' then
        MZ[i] := '0';
    l := i-1;

// fixme: commented by unxed
//    if ExtCollection^.Search(@MZ, i) then
      { дублирование: такого быть не должно }
//    else
      begin
      New(P);
      P^.Mask := M;
      P^.FType := T;
      ExtCollection^.AtInsert(i, P);
      end;
    end;
  end;

procedure PrepareExtCollection;
  begin
  if ExtCollection <> nil then
    Dispose(ExtCollection, Done);
  New(ExtCollection, Init(50,10));
  PutExtFilter(Executables, ttExec);
  PutExtFilter(Archives, ttArc);
  PutExtFilter(CustomMask1, ttCust1);
  PutExtFilter(CustomMask2, ttCust2);
  PutExtFilter(CustomMask3, ttCust3);
  PutExtFilter(CustomMask4, ttCust4);
  PutExtFilter(CustomMask5, ttCust5);
  PutExtFilter(CustomMask6, ttCust6);
  PutExtFilter(CustomMask7, ttCust7);
  PutExtFilter(CustomMask8, ttCust8);
  PutExtFilter(CustomMask9, ttCust9);
  PutExtFilter(CustomMask10, ttCust10);
  end;

function GetFileType(const S: String; Attr: Byte): Integer;
  var
    Ext: String;
    N: Integer;
  begin
  if S = '..' then
    GetFileType := ttUpDir
  else if Attr and Directory <> 0 then
    GetFileType := ttDirectory
  else
    begin
    Ext := UpStrg(Copy(S, PosLastDot(S)+1, 255));
    if Ext = '' then
      Ext := '.';{no extension}
    Result := 0;
    // fixme: commented by unxed
    {
    if ExtCollection^.Search(@Ext, N) then
      begin
        Result := PExtItem(ExtCollection^.At(N))^.FType
      end
    else
      Result := 0
    }
    end;
  end { GetFileType };

end.
