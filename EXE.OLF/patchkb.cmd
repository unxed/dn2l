/* Patch kbdbase.sys to enable Ctrl-number keys
(c) by Jaroslaw Osadtchiy (2:5030/1082.53) */

parse arg FName

if FName = "" then
do
  say 'Patch kbdbase.sys to enable Ctrl-number keys '
  say 'Usage: PatchKB FileName'
  exit 2
end

Call CharOut,'Patching 'FName' ... '
Say ''
if Stream(FName,'c','query exists')='' then
  Say 'File not found'
else
  do
    Call Stream FName,'c','open'
    p=Pos( X2C('FF834C0C3F'), CharIn(FName,1,Chars(FName)) )
    if p>0 then do
      Call CharOut FName, X2C('0090909090'), p
      Say 'Patch applied successfully'
    end
      else Say 'Can not apply patch'
      Call Stream FName,'c','close'
end
exit 0
