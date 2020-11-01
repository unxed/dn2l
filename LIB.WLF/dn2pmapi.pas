(*█▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀█
//█                                                       █
//█          Dos Navigator/2 runtime library              █
//█      OS/2 Presentation Manager API interface          █
//█      ─────────────────────────────────────────────────█
//█      by Jaroslaw Osadtchiy (JO), 2:5030/1082.53       █
//█      modified by Aleksej Kozlov (Cat), 2:5030/1326.13 █
//█      modified by Alexey Korop (AK155), 2:461/155      █
//█                                                       █
//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
*)

{$X+,T-,Cdecl+,AlignRec-,OrgName+,V-,Use32-,Delphi+}

unit Dn2PmApi;

interface
uses
  vpkbdw32;

function DN_IsBGWindow: Boolean; inline;
  begin
  Result := vpkbdw32.WindowNotFocused;
  end;

implementation

end.
