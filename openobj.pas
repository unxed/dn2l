program OpenObj;
uses
  Dos, OS2PMApi, Strings
  ;
var
  Handle: LongInt;
  PS: PChar;
  PSArr: array[0..255] of Char;
begin
if ParamCount <> 1 then
  begin
  Writeln('WPS file object opening utility for DN/2');
  Writeln('(c) J.Osadtchiy, 2001');
  Writeln;
  Writeln('Usage: OPENOBJ <FileToOpen>');
  Halt(1);
  end;
PS := PSArr;
PS := StrPCopy(PS, ParamStr(1));
Handle := WinQueryObject(PS);
{Writeln('Handle = ', Handle);}
if WinOpenObject(Handle, OPEN_DEFAULT, True) then
  Writeln('object successfully opened')
else
  Writeln('failed to open object');
end.
