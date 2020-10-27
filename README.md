# dn2l
dn2l — Dos Navigator Open Source Project linux port tryouts

To build and run (very experimental) dn2l, use this one-liner:
```
wget https://raw.githubusercontent.com/unxed/dn2l/main/linux/init.sh && chmod +x init.sh && ./init.sh
```

Current state:
![](https://user-images.githubusercontent.com/1151423/97242979-4c113a80-1806-11eb-8b8a-b3419a738b48.png)

How to build with IDE?

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

