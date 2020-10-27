# dn2l
dn2l — Dos Navigator Open Source Project linux port tryouts

To build and run (very experimental) dn2l, use this one-liner:
```
wget https://raw.githubusercontent.com/unxed/dn2l/main/linux/init.sh && chmod +x init.sh && ./init.sh
```

### Current state:
![](https://user-images.githubusercontent.com/1151423/97242979-4c113a80-1806-11eb-8b8a-b3419a738b48.png)

### How to build with IDE?

1. Copy vp*.* from dn2l_tryouts/vp directory (it should be created by init.sh) to dn2l_tryouts/dn2l directory (confirm overwriting existing files if asked)
2. cd dn2l_tryouts/dn2l
3. chmod +x vp
4. mkdir out.lnx
5. mkdir out.lnx/units
6. run ./vp
7. Go to "options"-"compiler"-"unit aliases" and remove "messages=windows"
8. Go to "options"-"directories" and replace "{BASEDIR}" with "." in "output directory" and "exe output directory"
9. Open "dn.pas", "compile"-"build"
10. Run pe2elf from dn2l_tryouts/vp on dn2l_tryouts/dn2l/out.lnx/dn.exe to get executable ELF file for linux, don't forget to do chmod +x on it

### Licensing and legal notices

License: modified BSD with a separate exception prohibiting re-licensing under another license, including, but not limited to, GPL. See LICENSE.txt for full text.

The original version of the source code taken from DN open source project (dnosp.com) contained some files from the RTL libraries of Virtual Pascal and Delphi. The main developer of dn2l is a programmer, not a lawyer, and therefore not sure if modified versions of those files can be redistributed or not. To avoid legal problems, all these files were removed from the source code tree, patches with the necessary changes over the original sources from VP RTL are used instead. Those patches are:
1) sysutils.patch, applied upon sysutils.pas header file from VP. According to FSF, headers can not be copyrighted.
2) vpsyslnx.patch, applied upon vpsyslnx.pas from VP RTL src. Contains only minimal required citations from original, so definitly a fair use.
3) vpsyslow.patch, applied upon vpsyslow.pas from VP RTL src. Contains only minimal required citations from original, so definitly a fair use.
4) vpsysd32.patch, vpsysos2.patch, vpsysw32.patch — not used by dn2l at all, provied for educational and historical purposes only, contain only minimal required citations from originals, so can be considered a fair use also.

dn2l developer is not sure how to correctly interpret the licensing terms for the improvements that were introduced by the DN OSP team (including the listed patches). Some of them do not belong to the original DN source code files published by Ritlabs and do not contain any licensing information inside. He assumes the distribution terms for that code can be considered to be the same as for the entire project, since this code was written specifically for this project and was distributed in the same archive with it.
