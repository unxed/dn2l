(*ÛßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßÛ
//Û                                                       Û
//Û          Dos Navigator/2 runtime library              Û
//Û      OS/2 Presentation Manager API interface          Û
//Û      ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÛ
//Û      by Jaroslaw Osadtchiy (JO), 2:5030/1082.53       Û
//Û      modified by Aleksej Kozlov (Cat), 2:5030/1326.13 Û
//Û      modified by Alexey Korop (AK155), 2:461/155      Û
//Û                                                       Û
//ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*)

{$X+,T-,Cdecl+,AlignRec-,OrgName+,V-,Use32-,Delphi+}

unit Dn2PmApi;

interface

function DN_IsBGWindow: Boolean; inline;
  begin
  Result := False;
  end;

implementation

end.
