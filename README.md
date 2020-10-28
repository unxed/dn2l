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

### Playing with the code

- Some parts that are failing are commented out for now. You can search code by string "by unxed" to find out such disabled code paths if whishing to fix some of them.
- If you plan to try building dn2l with Free Pascal Compiler, don't forget to use -Sd switch and to add {$asmMode intel} directive to all files with assembly code. Also FPC does not supports "inline" functions having body before "implementation" part of a unit, AFAIK.
- dn2l internal screen rendering code currently supports only one-byte-per-character charsets (it is hardcoded cp866 for now, see vpsyslnx.patch). To switch to UTF-8 we possibly should move from "Char" to "array[0..3] of Char" in TScrCell structure defined in vpsyslnx.pas, change PScrBuffer/TScrBuffer (and other screen buffer arrays, you can search for "TAWordArray(" to find some) definitions also, and rewrite all code that works with such buffers as arrays of Words (don't forget about assembly code inside views.vp and drivers.vp; we probably should consider rewriting all such assembly parts in Pascal as it also simplifies porting to FPC).

### Licensing and legal notices

License: modified BSD with an exception prohibiting re-licensing under another license, including, but not limited to, GPL. See LICENSE.txt for full text.

Currently dn2l is built using freeware Virtual Pascal compiler obtained from old-dos.ru (big thanks, old-dos.ru team!). It is redistributed with dn2l in it's original unmodified form as permitted by license agreement. See vp_dist/LICENCE.TXT for more information on Virtual Pascal distribution terms.

The original version of the source code taken from DN open source project (dnosp.com) contained some files from the RTL libraries of Virtual Pascal. The main developer of dn2l is a programmer, not a lawyer, and therefore not sure if modified versions of those files can be redistributed or not (the Virtual Pascal license agreement prohibits "distribution of a modified version of Virtual Pascal", and it's unclear if this applies to RTLs supplied in source code form, or not). Anyway, to avoid possible legal problems, all such files were removed from the source code tree. Patches with the necessary changes over the original sources from VP RTL are used instead, so there are no modified VP parts bundled with dn2l, just as VP license says. Those patches are:
1) sysutils.patch, applied upon sysutils.pas header file from VP. According to FSF, headers can not be copyrighted.
2) vpsyslnx.patch, applied upon vpsyslnx.pas from VP RTL src. Contains only minimal required quotations from original, so definitly a fair use.
3) vpsyslow.patch, vpsysd32.patch, vpsysos2.patch, vpsysw32.patch — not used by dn2l at all, provided for educational and historical purposes only, contain only minimal required quotations from originals, so should be considered a fair use also.

The main developer of dn2 is not sure about exact licensing terms for the DN code changes that were made by the DN OSP team (including the listed patches). Some of them do not belong to the original DN source code files published by Ritlabs and do not contain any licensing information inside. He assumes the distribution terms for that code can be considered to be the same as for the entire project, since this code was written specifically for this project and was distributed in the same archive with it.

dn2l also contains "English.DLG" and "English.LNG " resouce files. Those files should be generated using rcp.pas from config files in RESOURCE folder, but rcp.pas is currently broken, so ready to use resource files are taken from DN binary distribution. The source scripts for those files are licensed under the same conditions as entire DN.

### Acknowledgments

Based on Dos Navigator by RIT Research Labs, with many thanks for opening the code. Also many thanks to DN OSP team for making old style DOS source code 32-bit and multi-platform friendly. Thanks to old-dos.ru team for hosting Virtual Pascal distribution — dn2l, perhaps, would never have been born without the ability to find and download VP. Thanks to VP developers for making their product available as freeware — without permission to use their compiler, dn2l might never have been built. And also thanks to habr.com community for inspiration!
