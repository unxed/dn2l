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

### Possible further improvements (no concrete plans on it all, though)

- Fix remaining bugs
- Switch to Free Pascal Compiler
- Add Unicode support

### Licensing and legal notices

License: modified BSD with an exception prohibiting re-licensing under another license, including, but not limited to, GPL. See LICENSE.txt for full text.

Currently dn2l is built using freeware Virtual Pascal compiler obtained from old-dos.ru (big thanks, old-dos.ru team!). It is redistributed with dn2l in it's original unmodified form as permitted by license agreement. See vp_dist/LICENCE.TXT for more information on Virtual Pascal distribution terms.

The original version of the source code taken from DN open source project (dnosp.com) contained some files from the RTL libraries of Virtual Pascal. The main developer of dn2l is a programmer, not a lawyer, and therefore not sure if modified versions of those files can be redistributed or not (the Virtual Pascal license agreement prohibits "distribution of a modified version of Virtual Pascal", and it's unclear if this applies to RTLs supplied in source code form, or not). Anyway, to avoid possible legal problems, all such files were removed from the source code tree. Patches with the necessary changes over the original sources from VP RTL are used instead, so there are no modified VP parts bundled with dn2l, just as VP license says. Those patches are:
1) sysutils.patch, applied upon sysutils.pas header file from VP. According to FSF, headers can not be copyrighted.
2) vpsyslnx.patch, applied upon vpsyslnx.pas from VP RTL src. Contains only minimal required quotations from original, so definitly a fair use.
3) vpsyslow.patch, vpsysd32.patch, vpsysos2.patch, vpsysw32.patch — not used by dn2l at all, provided for educational and historical purposes only, contain only minimal required quotations from originals, so should be considered a fair use also.

dn2l developer is not sure how to correctly interpret the licensing terms for the improvements that were introduced by the DN OSP team (including the listed patches). Some of them do not belong to the original DN source code files published by Ritlabs and do not contain any licensing information inside. He assumes the distribution terms for that code can be considered to be the same as for the entire project, since this code was written specifically for this project and was distributed in the same archive with it.

