{$IFDEF WIN32}
 {.$DEFINE SEVENZIP}{piwamoto:unpack ARJ,CAB,ZIP with 7-Zip (www.7-zip.org)}
{$ENDIF}

{$DEFINE DEBUG}

 {$IFDEF VIRTUALPASCAL}{$DEFINE NONBP}{$DEFINE LINEPOSIT}{$ENDIF}
 {$IFDEF FPC}{$DEFINE NONBP}{$ENDIF}

 {$IFNDEF VIRTUALPASCAL} {$IFNDEF DPMI} {$DEFINE REALMODE} {$ENDIF} {$ENDIF}

 {$IFDEF VIRTUALPASCAL}
  {$DEFINE USESYSUTILS}   {Cat: если хочется подключать модуль SysUtils или использовать механизм исключений; в плагинной версии ни на что не влияет}
  {.$DEFINE PLUGIN}        {Cat: плагинская версия}
  {.$DEFINE COLORER}       {Cat: плагин Colorer - может использоваться и не в плагинской версии тоже}
  {$DEFINE USELONGSTRING}  {Cat: использование длинных строк кое-где (в частности, в редакторе)}
  {.$DEFINE USEANSISTRING} {Cat: использование длинных строк везде ($H+); уже компилируется,     }
                           {     но ещё не работает; доделаю, если будет нужно, что вряд ли  ;-) }
  { !!  при изменении этого параметра ресурсы получаются несовместимыми  !! }
 {$ENDIF}

{$IFDEF DEBUG}
 {$IFDEF VIRTUALPASCAL}
  {$IFDEF OS2} {.$DEFINE DEBUGMEM} {$ENDIF} {Cat: проверяем на выходе и пишем в лог, сколько памяти скушалось и в каком месте}
  {.$DEFINE LOGOBJLOADSTORE} {Cat: пишем в лог все вызовы методов Load и Store}
  {$DEFINE SHOWRC} {Cat: добавлять ли к сообщениям об ошибках числовые коды ошибок (RC)}
 {$ENDIF}
{$ENDIF}

{.$DEFINE RegExp}
{$DEFINE Calendar}
{$DEFINE Game}
{$DEFINE TrashCan}
{$DEFINE CHCOL}
{$DEFINE SS}

{$IFDEF REALMODE}
 {$DEFINE LITE}
{$ENDIF}

{.$DEFINE UUENCODE}
{.$DEFINE UUDECODE}
{$DEFINE DBView}
{$IFNDEF NONBP}{TEMPORARY COMPILE LITE VERSION FOR NON BP}
 {.$DEFINE Modem}
 {.$DEFINE Link}
 {$DEFINE DiskEdit}
 {$DEFINE DiskFormat}
 {$DEFINE Reanimator}
 {$DEFINE DBView}
 {$DEFINE PrintManager}
 {$DEFINE SpreadSheet}
 {$DEFINE CdPlayer}
 {$DEFINE MEMINFO}
 {$DEFINE SYSINFO}
 {$DEFINE NETINFO}
 {.$DEFINE PHONES}
 {$DEFINE ARVID}

 {$DEFINE DisAsm}
 {.$DEFINE UPLOWSTRSINDLG}

 {$IFDEF DPMI}
  {$DEFINE BIGCOLLECTION}
  {.$DEFINE QSort}{$DEFINE POGLSORT}
  {$DEFINE NO_OVERLAY}
  {$DEFINE NOEXTRA}
  {$DEFINE PKTVIEW}
  {.$DEFINE OldCollection}
 {$ELSE}
  {.$DEFINE BIGCOLLECTION}
  {.$DEFINE QSort}{$DEFINE POGLSORT}
  {$UNDEF NO_OVERLAY}
  {$UNDEF NOEXTRA}
  {$DEFINE OFFS}
  {$DEFINE OldCollection} {старые ассемблерные методы *Collection}
 {$ENDIF}

 {$DEFINE BIT_16}

 {.$DEFINE StreamDebug}
 {.$DEFINE SORTDEBUG}
 {.$DEFINE CHDIRDEBUG}
 {$DEFINE UserSaver}

{$ELSE VIRTUALPASCAL}
 {$DEFINE NO_OVERLAY}
 {$DEFINE NOEXTRA}
 {$DEFINE BIT_32}
 {$DEFINE FILE_32}
 {$IFDEF WIN32}
  {$DEFINE WIN95_HIGHPRIORITY} {Cat: для Win9x повышаем приоритет ДН-а, иначе дико тормозит из-за кривого ConAgent-а}
  {$DEFINE USELFN}
  {.$DEFINE RecodeWhenDraw} {кириллические имена файлов хранятся в ANSI,}
                            {а перекодируются только при использовании}
 {$ENDIF}
 {$IFDEF DPMI32}
  {.$DEFINE NOASM} {Cat:todo DPMI32}
  {$DEFINE USELFN}
  {$DEFINE DualName}
 {$ENDIF}
 {$IFDEF USELFN}
  {$DEFINE STRINGLFN} {включает более быстрый, но жрущий много памяти алгоритм работы с виндовыми LFN}
 {$ENDIF}
 {$DEFINE VPBETA}     {включает создание DN.ERR при критических ошибках}
                      {влияет на вариант подключения dn2cat.dll в виндовой плагинной версии}
 {.$DEFINE OldCollection} {старые ассемблерные методы *Collection}
 {$DEFINE NoCallspcb} {внимание! конфиги версий, скомпилённых с и без NoCallspcb несовместимы!}
 {$DEFINE EAOP}       {операции с Extended Attributes в OS/2}
                      {операции с Security Attributes в WinNT}
 {$IFNDEF DPMI32}
  {$DEFINE USEWPS}    {открытие фолдеров WPS}
 {$ENDIF}
 {$Use32+}
 {$DEFINE QSORT}
 {.$DEFINE POGLSORT}
 {$DEFINE SpreadSheet}
 {$DEFINE PrintManager}
 {$DEFINE Printer}
 {$DEFINE PrintFromEditor}
 {.$DEFINE PKTVIEW} {нормально компилится и работает, но надо ли}
 {$DEFINE DisAsm}  {нормально компилится и работает, но надо ли}
 {$DEFINE PHONES}
 {$DEFINE ARVID}
 {.$DEFINE NETINFO}
 {$DEFINE FileNotify}
 {$IFNDEF DPMI32}
  {$IFNDEF WIN32} {JO: временно, в Win32 ещё не совсем готов}
   {.$DEFINE Tasklist}
  {$ENDIF}
 {$ENDIF}
 {.$DEFINE CHDIRTIMER}
 {.$DEFINE SORTTIMER}
 {.$DEFINE COPYTIMER}
{$ENDIF VIRTUALPASCAL}

{$IFDEF BPIDE}
 {$UNDEF CHCOL}
 {$UNDEF SYSINFO}
 {$UNDEF MEMINFO}
 {$UNDEF UUENCODE}
 {$UNDEF UUDECODE}
 {$UNDEF Modem}
 {$UNDEF Link}
 {$UNDEF DisAsm}
{$ENDIF}

{$IFDEF LITE}
 {.$UNDEF TrashCan}
 {$UNDEF Game}
 {.$UNDEF UUENCODE}
 {.$UNDEF UUDECODE}
 {.$UNDEF Calendar}
 {.$UNDEF CdPlayer}
 {$UNDEF NETINFO}
 {.$UNDEF PHONES}
 {.$UNDEF SS}
 {.$UNDEF PrintManager}
 {.$UNDEF CHCOL}
 {$UNDEF SpreadSheet}
 {.$UNDEF DBView}
 {.$UNDEF Link}
 {$UNDEF DisAsm}
 {.$DEFINE MINARCH}
 {$UNDEF ARVID}
 {$UNDEF DiskEdit}
 {$UNDEF Reanimator}
{$ENDIF}

{.$DEFINE BANK}

{$IFDEF BANK}
 {$UNDEF Modem}
 {$UNDEF DiskEdit}
 {$UNDEF DBView}
 {$UNDEF DisAsm}
{$ENDIF}

{$DEFINE FULLOVR}
{$DEFINE FULLOVR2}
{$DEFINE VIEWSOVR}
{$DEFINE ADVANCEOVR}
{$DEFINE CollectOVR}
{$DEFINE MicroedOvr}
{$DEFINE VIDEOOVR}

{ Only TPW uses "WObjects"; BP 7 calls the same unit "Objects" }
{$IFDEF windows}
  {$IFNDEF ver70}
    {$DEFINE wobjects}
  {$ENDIF}
{$ENDIF}

{$IFDEF Modem}
  {$IFDEF DN}
    {$DEFINE CL_MASTER}
  {$ENDIF}
  {$IFDEF NLCLIENT}
    {$DEFINE CL_MASTER}
  {$ENDIF}
{$ENDIF}

{.$DEFINE PACKFILE}

{$IFDEF PLUGIN} {$DEFINE PLUGINRES} {$ENDIF}
{$IFNDEF DNPRG} {$UNDEF PLUGIN} {$ENDIF}
{$IFDEF PLUGIN} {$IFDEF OS2} {$DEFINE PLUGIN_OS2} {$DEFINE TRUE_DYNAMIC} {$ENDIF} {$IFDEF WIN32} {$DEFINE PLUGIN_WIN32} {$IFNDEF VPBETA} {$DEFINE TRUE_DYNAMIC} {$ENDIF} {$ENDIF} {$ENDIF} {Cat}

{.$IFDEF PLUGIN} {.$UNDEF GAME} {.$ENDIF} {Cat}
{$IFDEF PLUGIN} {.$DEFINE MINARCH} {$ENDIF} {Cat}
{$IFDEF PLUGIN_WIN32} {$DEFINE MIRRORVARS} {$ENDIF} {Cat}
{$IFNDEF BIT_32} {$UNDEF USEANSISTRING} {$UNDEF USELONGSTRING} {$ENDIF} {Cat}
{$IFDEF LOGOBJLOADSTORE} {$UNDEF NoCallspcb} {$ENDIF} {Cat}
{$IFDEF VIRTUALPASCAL} {$IFDEF USEANSISTRING} {$H+} {$DEFINE USELONGSTRING} {$ELSE} {$H-} {$ENDIF} {$ENDIF} {Cat}

{$IFDEF Win32}{$DEFINE DualName } {$ENDIF} {AK155}
{$IFDEF Win32} {$DEFINE NetBrowser} {$ENDIF} {JO}
