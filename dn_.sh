#/bin/bash
patch -N sysutils.pas sysutils.patch
patch -N vpsyslnx.pas vpsyslnx.patch
if [ ! -f version.inc ]; then
    ../vp/vpc -M -T -\$S- -\$I- -\$Zd+ -\$Asm+ -\$D+ -\$LocInfo+ version.pas
    ../vp/pe2elf version.exe
    chmod +x version
    ./version version.inc linux 0
fi
../vp/vpc -M -T -\$S- -\$I- -\$Zd+ -\$Asm+ -\$D+ -\$LocInfo+ dn.pas 
read -n1 -r -p "Press any key (try Ctrl-C to quit DN)..."
../vp/pe2elf dn.exe 
chmod +x dn
#was needed for some previous builds to run
#rm DN.INI
./dn > dn.123
cat dn.123
read -n1 -r -p "Press any key..."
