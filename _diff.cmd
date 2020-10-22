/* */
FileName = 'ak' || Substr(DATE('S'), 4, 5) || '.dif'
"diff.exe -urN -X G:\DN2DIFF\IGNORE.LST -x dn.ini I:\dn2s .  > "FileName
