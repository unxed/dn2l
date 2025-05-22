unit DosLow;

interface

var {Адреса 1024-байтной рабочей области, используемой для связи
  с real mode прерываниями. Эта область предполагается всегда
  свободной, так что хранить данные в этой области нельзя. }

  DosSeg: SmallWord;
    {Real mode segment; far16 адрес область есть DosSeg:0}
  DosSegFlat: Pointer;
    {Flat адрес области }

implementation

uses
  dpmi32df, dpmi32;

begin
getdosmem(DosSeg, 1024);
DosSegFlat := Ptr(dosseg_linear(DosSeg));
end.

