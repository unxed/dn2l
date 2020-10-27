#!/bin/bash
if ! command -v 7z &> /dev/null
then
    echo "7z not found. trying to install via apt"
    echo "install p7zip-full manually if you are using non deb-based distro"
    sudo apt install p7zip-full
fi
# cleanup
rm -rf dn2l_tryouts
# init
mkdir dn2l_tryouts
cd dn2l_tryouts
# get sources
git clone https://github.com/unxed/dn2l.git
# switch to experimental branch
cd dn2l
#git checkout second_try
cd ..
# get virtual pascal
mkdir vp_full
cd vp_full
wget https://web.archive.org/web/20140919203444/http://old-dos.ru/dl.php?id=2044
#wget http://old-dos.ru/dl.php?id=2044
mv dl.php\?id=2044 vp.zip
7z x vp.zip
rm -rf vp.zip
# rtl
mkdir artlsrc
cp artlsrc.rar artlsrc/
cd artlsrc
7z x artlsrc.rar
rm -rf artlsrc.rar
rename 'y/A-Z/a-z/' *
cp dos.pas math.pas strings.pas sysutils.pas use32.pas vpsyslnx.pas vpsyslow.pas vputils.pas windos.pas ../../dn2l
# system.pas system.vps
cd sys
rename 'y/A-Z/a-z/' *
cp system.pas system.vps ../../../dn2l
cd ../..
# linux.pas lnxres.pas 
mkdir lrtlsrc
cp lrtlsrc.rar lrtlsrc/
cd lrtlsrc
7z x lrtlsrc.rar
rm -rf lrtlsrc.rar
rename 'y/A-Z/a-z/' *
cp linux.pas lnxres.pas ../../dn2l
cd ..
# linux.res sysutils.res
mkdir lresbin
cp lresbin.rar lresbin/
cd lresbin
7z x lresbin.rar
rm -rf lresbin.rar
rename 'y/A-Z/a-z/' *
cp linux.res sysutils.res ../../dn2l
cd ..
# lvpbase
mkdir lvpbase
cp lvpbase.rar lvpbase/
cd lvpbase
7z x lvpbase.rar
rm -rf lvpbase.rar
rename 'y/A-Z/a-z/' *
cd ..
mv lvpbase ../vp
# lv aidevph
mkdir aidevph
cp aidevph.rar aidevph/
cd aidevph
7z x aidevph.rar
rm -rf aidevph.rar
rename 'y/A-Z/a-z/' *
cd ..
cp aidevph/* ../vp/
# build
cd ..
cd dn2l
chmod +x ../vp/vpc
chmod +x ../vp/pe2elf
#rm -rf dn.exe
#rm -rf dn
#../vp/vpc -B -\$S- dn.pas
#../vp/pe2elf dn.exe
#chmod +x dn
#
./dn_.sh
