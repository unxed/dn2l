{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.08
//  Based on Dos Navigator (C) 1991-99 RIT Research Labs
//
//  This programs is free for commercial and non-commercial use as long as
//  the following conditions are aheared to.
//
//  Copyright remains RIT Research Labs, and as such any Copyright notices
//  in the code are not to be removed. If this package is used in a
//  product, RIT Research Labs should be given attribution as the RIT Research
//  Labs of the parts of the library used. This can be in the form of a textual
//  message at program startup or in documentation (online or textual)
//  provided with the package.
//
//  Redistribution and use in source and binary forms, with or without
//  modification, are permitted provided that the following conditions are
//  met:
//
//  1. Redistributions of source code must retain the copyright
//     notice, this list of conditions and the following disclaimer.
//  2. Redistributions in binary form must reproduce the above copyright
//     notice, this list of conditions and the following disclaimer in the
//     documentation and/or other materials provided with the distribution.
//  3. All advertising materials mentioning features or use of this software
//     must display the following acknowledgement:
//     "Based on Dos Navigator by RIT Research Labs."
//
//  THIS SOFTWARE IS PROVIDED BY RIT RESEARCH LABS "AS IS" AND ANY EXPRESS
//  OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
//  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
//  DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE FOR
//  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
//  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
//  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
//  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
//  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
//  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
//  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
//  The licence and distribution terms for any publically available
//  version or derivative of this code cannot be changed. i.e. this code
//  cannot simply be copied and put under another distribution licence
//  (including the GNU Public Licence).
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}

unit Commands;

interface

function Bit(N: Word): Word;

const
  MaxStringLength = 255;
  MaxReasonableStringLength = 255;
  MaxLongStringLength = 1048575;

const
    kbCtrlShiftLeft = $f0; //stubs
    kbCtrlShiftRight = $f1;
    kbAltShiftLeft = $f2;
    kbAltShiftRight = $f3;
    kbAltCtrlSqBracketL = $f4;
    kbCtrlSqBracketL = $f5;
    kbAltCtrlSqBracketR = $f6;
    kbCtrlSqBracketR = $f7;
    kbCtrl1 = $f8;
    kbCtrl2 = $f9;
    kbCtrl3 = $fa;
    kbCtrl4 = $fb;
    kbCtrl5 = $fc;
    kbCtrl6 = $fd;
    kbCtrl7 = $fe;
    kbCtrl8 = $ff;
    kbCtrl9 = $e0;
    kbCtrl0 = $e1;
    kbCtrlAltZ = $e2;
    kbAltShiftSlash = $e3;
    kbBackUp = $e4;
    kbShiftUp = $e5;
    kbShiftDown = $e6;
    kbShiftLeft = $e7;
    kbShiftRight = $e8;
    kbCtrlShiftIns = $e9;
    kbShiftEnd = $ea;
    kbShiftHome = $eb;


// use FV constans instead; by unxed
(*
const
  {Cat: переделал все коды клавиш по следующему единому принципу:
        kbNNNN = $XXYYZZ
        XX - флажки сдвигов: 3 - Shift, 4 - Ctrl, 8 - Alt
        YY - скан-код клавиши
        ZZ - символьный код клавиши
}

  kbESC = $00011B;
  kbAltEsc = $080100;
  kbShiftEsc = $03011B;
  kbAltShiftEsc = $0B0100;
  kbCtrlEsc = $040100;
  kbCtrlAltEsc = $0C0100;
  kbCtrlShiftEsc = $070100;
  kbCtrlAltShiftEsc = $0F0100;

  kbF1 = $003B00;
  kbShiftF1 = $035400;
  kbCtrlF1 = $045E00;
  kbAltF1 = $086800;
  kbF2 = $003C00;
  kbShiftF2 = $035500;
  kbCtrlF2 = $045F00;
  kbAltF2 = $086900;
  kbF3 = $003D00;
  kbShiftF3 = $035600;
  kbCtrlF3 = $046000;
  kbAltF3 = $086A00;
  kbF4 = $003E00;
  kbShiftF4 = $035700;
  kbCtrlF4 = $046100;
  kbAltF4 = $086B00;
  kbF5 = $003F00;
  kbShiftF5 = $035800;
  kbCtrlF5 = $046200;
  kbAltF5 = $086C00;
  kbF6 = $004000;
  kbShiftF6 = $035900;
  kbCtrlF6 = $046300;
  kbAltF6 = $086D00;
  kbF7 = $004100;
  kbShiftF7 = $035A00;
  kbCtrlF7 = $046400;
  kbAltF7 = $086E00;
  kbF8 = $004200;
  kbShiftF8 = $035B00;
  kbCtrlF8 = $046500;
  kbAltF8 = $086F00;
  kbF9 = $004300;
  kbShiftF9 = $035C00;
  kbCtrlF9 = $046600;
  kbAltF9 = $087000;
  kbF10 = $004400;
  kbShiftF10 = $035D00;
  kbCtrlF10 = $046700;
  kbAltF10 = $087100;
  kbF11 = $008500;
  kbShiftF11 = $038700;
  kbCtrlF11 = $048900;
  kbAltF11 = $088B00;
  kbF12 = $008600;
  kbShiftF12 = $038800;
  kbCtrlF12 = $048A00;
  kbAltF12 = $088C00;

  kbCtrlShiftF1 = $075E00;
  kbAltShiftF1 = $0B6800;
  kbCtrlShiftF2 = $075F00;
  kbAltShiftF2 = $0B6900;
  kbCtrlShiftF3 = $076000;
  kbAltShiftF3 = $0B6A00;
  kbCtrlShiftF4 = $076100;
  kbAltShiftF4 = $0B6B00;
  kbCtrlShiftF5 = $076200;
  kbAltShiftF5 = $0B6C00;
  kbCtrlShiftF6 = $076300;
  kbAltShiftF6 = $0B6D00;
  kbCtrlShiftF7 = $076400;
  kbAltShiftF7 = $0B6E00;
  kbCtrlShiftF8 = $076500;
  kbAltShiftF8 = $0B6F00;
  kbCtrlShiftF9 = $076600;
  kbAltShiftF9 = $0B7000;
  kbCtrlShiftF10 = $076700;
  kbAltShiftF10 = $0B7100;
  kbCtrlShiftF11 = $078900;
  kbAltShiftF11 = $0B8B00;
  kbCtrlShiftF12 = $078A00;
  kbAltShiftF12 = $0B8C00;

  kbCtrlAltF1 = $0C6800;
  kbCtrlAltShiftF1 = $0F6800;
  kbCtrlAltF2 = $0C6900;
  kbCtrlAltShiftF2 = $0F6900;
  kbCtrlAltF3 = $0C6A00;
  kbCtrlAltShiftF3 = $0F6A00;
  kbCtrlAltF4 = $0C6B00;
  kbCtrlAltShiftF4 = $0F6B00;
  kbCtrlAltF5 = $0C6C00;
  kbCtrlAltShiftF5 = $0F6C00;
  kbCtrlAltF6 = $0C6D00;
  kbCtrlAltShiftF6 = $0F6D00;
  kbCtrlAltF7 = $0C6E00;
  kbCtrlAltShiftF7 = $0F6E00;
  kbCtrlAltF8 = $0C6F00;
  kbCtrlAltShiftF8 = $0F6F00;
  kbCtrlAltF9 = $0C7000;
  kbCtrlAltShiftF9 = $0F7000;
  kbCtrlAltF10 = $0C7100;
  kbCtrlAltShiftF10 = $0F7100;
  kbCtrlAltF11 = $0C8B00;
  kbCtrlAltShiftF11 = $0F8B00;
  kbCtrlAltF12 = $0C8C00;
  kbCtrlAltShiftF12 = $0F8C00;

  kbTilde = $002960;
  kbShiftTilde = $03297E;
  kbAltTilde = $082900;
  kbBack = $000E08;
  kbShiftBack = $030E08;
  kbCtrlBack = $040E00;
  kbAltBack = $080E00;
  kbTab = $000F09;
  kbShiftTab = $030F00;
  kbCtrlTab = $049400;
  kbAltTab = $08A500;
  kbEnter = $001C0D;
  kbShiftEnter = $031C0D;
  kbCtrlEnter = $041C00;
  kbAltEnter = $081C00;
  kbSpace = $003920;
  kbShiftSpace = $033920;
  kbCtrlSpace = $043900;
  kbAltSpace = $083920;
  kbAltGrayEnter = $08A600;

  kbAltShiftTilde = $0B2900;
  kbCtrlAltTilde = $0C2900;
  kbCtrlShiftBack = $070E00;
  kbAltShiftBack = $0B0E00;
  kbCtrlShiftTab = $079400;
  kbAltShiftTab = $0BA500;
  kbCtrlShiftEnter = $071C00;
  kbAltShiftEnter = $0B1C00;
  kbCtrlShiftSpace = $073900;
  kbAltShiftSpace = $0B3920;

  kbCtrlAltShiftTilde = $0F2900;
  kbCtrlAltBack = $0C0E00;
  kbCtrlAltShiftBack = $0F0E00;
  kbCtrlAltTab = $0CA500;
  kbCtrlAltShiftTab = $0FA500;
  kbCtrlAltEnter = $0C1C00;
  kbCtrlAltShiftEnter = $0F1C00;
  kbCtrlAltSpace = $0C3900;
  kbCtrlAltShiftSpace = $0F3900;

  kbIns = $005200;
  kbShiftIns = $035200;
  kbCtrlIns = $049200;
  kbAltIns = $08A200;
  kbDel = $005300;
  kbShiftDel = $035300;
  kbCtrlDel = $049300;
  kbAltDel = $08A300;
  kbHome = $004700;
  kbShiftHome = $034700;
  kbCtrlHome = $047700;
  kbAltHome = $089700;
  kbEnd = $004F00;
  kbShiftEnd = $034F00;
  kbCtrlEnd = $047500;
  kbAltEnd = $089F00;
  kbPgUp = $004900;
  kbShiftPgUp = $034900;
  kbCtrlPgUp = $048400;
  kbAltPgUp = $089900;
  kbPgDn = $005100;
  kbShiftPgDn = $035100;
  kbCtrlPgDn = $047600;
  kbAltPgDn = $08A100;

  kbCtrlShiftIns = $079200;
  kbAltShiftIns = $0BA200;
  kbCtrlShiftDel = $079300;
  kbAltShiftDel = $0BA300;
  kbCtrlShiftHome = $077700;
  kbAltShiftHome = $0B9700;
  kbCtrlShiftEnd = $077500;
  kbAltShiftEnd = $0B9F00;
  kbCtrlShiftPgUp = $078400;
  kbAltShiftPgUp = $0B9900;
  kbCtrlShiftPgDn = $077600;
  kbAltShiftPgDn = $0BA100;

  kbCtrlAltIns = $0CA200;
  kbCtrlAltShiftIns = $0FA200;
  kbCtrlAltDel = $0CA300;
  kbCtrlAltShiftDel = $0FA300;
  kbCtrlAltHome = $0C9700;
  kbCtrlAltShiftHome = $0F9700;
  kbCtrlAltEnd = $0C9F00;
  kbCtrlAltShiftEnd = $0F9F00;
  kbCtrlAltPgUp = $0C9900;
  kbCtrlAltShiftPgUp = $0F9900;
  kbCtrlAltPgDn = $0CA100;
  kbCtrlAltShiftPgDn = $0FA100;

  kbLeft = $004B00;
  kbShiftLeft = $034B00;
  kbCtrlLeft = $047300;
  kbAltLeft = $089B00;
  kbRight = $004D00;
  kbShiftRight = $034D00;
  kbCtrlRight = $047400;
  kbAltRight = $089D00;
  kbUp = $004800;
  kbShiftUp = $034800;
  kbCtrlUp = $048D00;
  kbAltUp = $089800;
  kbDown = $005000;
  kbShiftDown = $035000;
  kbCtrlDown = $049100;
  kbAltDown = $08A000;

  kbCtrlShiftLeft = $077300;
  kbAltShiftLeft = $0B9B00;
  kbCtrlShiftRight = $077400;
  kbAltShiftRight = $0B9D00;
  kbCtrlShiftUp = $078D00;
  kbAltShiftUp = $0B9800;
  kbCtrlShiftDown = $079100;
  kbAltShiftDown = $0BA000;

  kbCtrlAltLeft = $0C9B00;
  kbCtrlAltShiftLeft = $0F9B00;
  kbCtrlAltRight = $0C9D00;
  kbCtrlAltShiftRight = $0F9D00;
  kbCtrlAltUp = $0C9800;
  kbCtrlAltShiftUp = $0F9800;
  kbCtrlAltDown = $0CA000;
  kbCtrlAltShiftDown = $0FA000;

  kb2 = $000332;

  kbCtrl1 = $040200;
  kbAlt1 = $087800;
  kbCtrl2 = $040300;
  kbAlt2 = $087900;
  kbCtrl3 = $040400;
  kbAlt3 = $087A00;
  kbCtrl4 = $040500;
  kbAlt4 = $087B00;
  kbCtrl5 = $040600;
  kbAlt5 = $087C00;
  kbCtrl6 = $040700;
  kbAlt6 = $087D00;
  kbCtrl7 = $040800;
  kbAlt7 = $087E00;
  kbCtrl8 = $040900;
  kbAlt8 = $087F00;
  kbCtrl9 = $040A00;
  kbAlt9 = $088000;
  kbCtrl0 = $040B00;
  kbAlt0 = $088100;
  kbCtrlEqual = $040D00;
  kbCtrlQ = $041011;
  kbAltQ = $081000;
  kbCtrlW = $041117;
  kbAltW = $081100;
  kbCtrlE = $041205;
  kbAltE = $081200;
  kbCtrlR = $041312;
  kbAltR = $081300;
  kbCtrlT = $041414;
  kbAltT = $081400;
  kbCtrlY = $041519;
  kbAltY = $081500;
  kbCtrlU = $041615;
  kbAltU = $081600;
  kbCtrlI = $041709;
  kbAltI = $081700;
  kbCtrlO = $04180F;
  kbAltO = $081800;
  kbCtrlP = $041910;
  kbAltP = $081900;
  kbCtrlA = $041E01;
  kbAltA = $081E00;
  kbCtrlS = $041F13;
  kbAltS = $081F00;
  kbCtrlD = $042004;
  kbAltD = $082000;
  kbCtrlF = $042106;
  kbAltF = $082100;
  kbCtrlG = $042207;
  kbAltG = $082200;
  kbCtrlH = $042308;
  kbAltH = $082300;
  kbCtrlJ = $04240A;
  kbAltJ = $082400;
  kbCtrlK = $04250B;
  kbAltK = $082500;
  kbCtrlL = $04260C;
  kbAltL = $082600;
  kbCtrlZ = $042C1A;
  kbAltZ = $082C00;
  kbCtrlX = $042D18;
  kbAltX = $082D00;
  kbCtrlC = $042E03;
  kbAltC = $082E00;
  kbCtrlV = $042F16;
  kbAltV = $082F00;
  kbCtrlB = $043002;
  kbAltB = $083000;
  kbCtrlN = $04310E;
  kbAltN = $083100;
  kbCtrlM = $04320D;
  kbAltM = $083200;

  kbCtrlShift1 = $070200;
  kbAltShift1 = $0B7800;
  kbCtrlAlt1 = $0C7800;
  kbCtrlAltShift1 = $0F7800;
  kbCtrlShift2 = $070300;
  kbAltShift2 = $0B7900;
  kbCtrlAlt2 = $0C7900;
  kbCtrlAltShift2 = $0F7900;
  kbCtrlShift3 = $070400;
  kbAltShift3 = $0B7A00;
  kbCtrlAlt3 = $0C7A00;
  kbCtrlAltShift3 = $0F7A00;
  kbCtrlShift4 = $070500;
  kbAltShift4 = $0B7B00;
  kbCtrlAlt4 = $0C7B00;
  kbCtrlAltShift4 = $0F7B00;
  kbCtrlShift5 = $070600;
  kbAltShift5 = $0B7C00;
  kbCtrlAlt5 = $0C7C00;
  kbCtrlAltShift5 = $0F7C00;
  kbCtrlShift6 = $070700;
  kbAltShift6 = $0B7D00;
  kbCtrlAlt6 = $0C7D00;
  kbCtrlAltShift6 = $0F7D00;
  kbCtrlShift7 = $070800;
  kbAltShift7 = $0B7E00;
  kbCtrlAlt7 = $0C7E00;
  kbCtrlAltShift7 = $0F7E00;
  kbCtrlShift8 = $070900;
  kbAltShift8 = $0B7F00;
  kbCtrlAlt8 = $0C7F00;
  kbCtrlAltShift8 = $0F7F00;
  kbCtrlShift9 = $070A00;
  kbAltShift9 = $0B8000;
  kbCtrlAlt9 = $0C8000;
  kbCtrlAltShift9 = $0F8000;
  kbCtrlShift0 = $070B00;
  kbAltShift0 = $0B8100;
  kbCtrlAlt0 = $0C8100;
  kbCtrlAltShift0 = $0F8100;
  kbCtrlShiftQ = $071000;
  kbAltShiftQ = $0B1000;
  kbCtrlAltQ = $0C1000;
  kbCtrlAltShiftQ = $0F1000;
  kbCtrlShiftW = $071100;
  kbAltShiftW = $0B1100;
  kbCtrlAltW = $0C1100;
  kbCtrlAltShiftW = $0F1100;
  kbCtrlShiftE = $071200;
  kbAltShiftE = $0B1200;
  kbCtrlAltE = $0C1200;
  kbCtrlAltShiftE = $0F1200;
  kbCtrlShiftR = $071300;
  kbAltShiftR = $0B1300;
  kbCtrlAltR = $0C1300;
  kbCtrlAltShiftR = $0F1300;
  kbCtrlShiftT = $071400;
  kbAltShiftT = $0B1400;
  kbCtrlAltT = $0C1400;
  kbCtrlAltShiftT = $0F1400;
  kbCtrlShiftY = $071500;
  kbAltShiftY = $0B1500;
  kbCtrlAltY = $0C1500;
  kbCtrlAltShiftY = $0F1500;
  kbCtrlShiftU = $071600;
  kbAltShiftU = $0B1600;
  kbCtrlAltU = $0C1600;
  kbCtrlAltShiftU = $0F1600;
  kbCtrlShiftI = $071700;
  kbAltShiftI = $0B1700;
  kbCtrlAltI = $0C1700;
  kbCtrlAltShiftI = $0F1700;
  kbCtrlShiftO = $071800;
  kbAltShiftO = $0B1800;
  kbCtrlAltO = $0C1800;
  kbCtrlAltShiftO = $0F1800;
  kbCtrlShiftP = $071900;
  kbAltShiftP = $0B1900;
  kbCtrlAltP = $0C1900;
  kbCtrlAltShiftP = $0F1900;
  kbCtrlShiftA = $071E00;
  kbAltShiftA = $0B1E00;
  kbCtrlAltA = $0C1E00;
  kbCtrlAltShiftA = $0F1E00;
  kbCtrlShiftS = $071F00;
  kbAltShiftS = $0B1F00;
  kbCtrlAltS = $0C1F00;
  kbCtrlAltShiftS = $0F1F00;
  kbCtrlShiftD = $072000;
  kbAltShiftD = $0B2000;
  kbCtrlAltD = $0C2000;
  kbCtrlAltShiftD = $0F2000;
  kbCtrlShiftF = $072100;
  kbAltShiftF = $0B2100;
  kbCtrlAltF = $0C2100;
  kbCtrlAltShiftF = $0F2100;
  kbCtrlShiftG = $072200;
  kbAltShiftG = $0B2200;
  kbCtrlAltG = $0C2200;
  kbCtrlAltShiftG = $0F2200;
  kbCtrlShiftH = $072300;
  kbAltShiftH = $0B2300;
  kbCtrlAltH = $0C2300;
  kbCtrlAltShiftH = $0F2300;
  kbCtrlShiftJ = $072400;
  kbAltShiftJ = $0B2400;
  kbCtrlAltJ = $0C2400;
  kbCtrlAltShiftJ = $0F2400;
  kbCtrlShiftK = $072500;
  kbAltShiftK = $0B2500;
  kbCtrlAltK = $0C2500;
  kbCtrlAltShiftK = $0F2500;
  kbCtrlShiftL = $072600;
  kbAltShiftL = $0B2600;
  kbCtrlAltL = $0C2600;
  kbCtrlAltShiftL = $0F2600;
  kbCtrlShiftZ = $072C00;
  kbAltShiftZ = $0B2C00;
  kbCtrlAltZ = $0C2C00;
  kbCtrlAltShiftZ = $0F2C00;
  kbCtrlShiftX = $072D00;
  kbAltShiftX = $0B2D00;
  kbCtrlAltX = $0C2D00;
  kbCtrlAltShiftX = $0F2D00;
  kbCtrlShiftC = $072E00;
  kbAltShiftC = $0B2E00;
  kbCtrlAltC = $0C2E00;
  kbCtrlAltShiftC = $0F2E00;
  kbCtrlShiftV = $072F00;
  kbAltShiftV = $0B2F00;
  kbCtrlAltV = $0C2F00;
  kbCtrlAltShiftV = $0F2F00;
  kbCtrlShiftB = $073000;
  kbAltShiftB = $0B3000;
  kbCtrlAltB = $0C3000;
  kbCtrlAltShiftB = $0F3000;
  kbCtrlShiftN = $073100;
  kbAltShiftN = $0B3100;
  kbCtrlAltN = $0C3100;
  kbCtrlAltShiftN = $0F3100;
  kbCtrlShiftM = $073200;
  kbAltShiftM = $0B3200;
  kbCtrlAltM = $0C3200;
  kbCtrlAltShiftM = $0F3200;

  kbAltMinus = $088200;
  kbAltShiftMinus = $0B8200;
  kbAltPlus = $088300;
  kbAltShiftPlus = $0B8300;
  kbAltBSlash = $082B00;
  kbAltShiftBSlash = $0B2B00;
  kbAltSqBracketL = $081A00;
  kbAltShiftSqBracketL = $0B1A00;
  kbAltSqBracketR = $081B00;
  kbAltShiftSqBracketR = $0B1B00;
  kbAltSemicolon = $082700;
  kbAltShiftSemicolon = $0B2700;
  kbAltQuote = $082800;
  kbAltShiftQuote = $0B2800;
  kbAltLess = $083300;
  kbAltShiftLess = $0B3300;
  kbAltMore = $083400;
  kbAltShiftMore = $0B3400;
  kbAltSlash = $083500;
  kbAltShiftSlash = $0B3500;

  kbCtrlAltMinus = $0C8200;
  kbCtrlAltShiftMinus = $0F8200;
  kbCtrlAltPlus = $0C8300;
  kbCtrlAltShiftPlus = $0F8300;
  kbCtrlAltBSlash = $0C2B00;
  kbCtrlAltShiftBSlash = $0F2B00;
  kbCtrlAltSqBracketL = $0C1A00;
  kbCtrlAltShiftSqBracketL = $0F1A00;
  kbCtrlAltSqBracketR = $0C1B00;
  kbCtrlAltShiftSqBracketR = $0F1B00;
  kbCtrlAltSemicolon = $0C2700;
  kbCtrlAltShiftSemicolon = $0F2700;
  kbCtrlAltQuote = $0C2800;
  kbCtrlAltShiftQuote = $0F2800;
  kbCtrlAltLess = $0C3300;
  kbCtrlAltShiftLess = $0F3300;
  kbCtrlAltMore = $0C3400;
  kbCtrlAltShiftMore = $0F3400;
  kbCtrlAltSlash = $0C3500;
  kbCtrlAltShiftSlash = $0F3500;

  kbCtrlMinus = $040C00;
  kbCtrlBSlash = $042B00;
  kbCtrlShiftBSlash = $072B00;
  kbCtrlSqBracketL = $041A00;
  kbCtrlShiftSqBracketL = $071A00;
  kbCtrlSqBracketR = $041B00;
  kbCtrlShiftSqBracketR = $071B00;

  kbGrayPlus = $004E2B;
  kbShiftGrayPlus = $034E2B;
  kbGrayMinus = $004A2D;
  kbShiftGrayMinus = $034A2D;
  kbGrayAst = $00372A;
  kbShiftGrayAst = $03372A;
  kbGraySlash = $00E02F;
  kbShiftGraySlash = $03E02F;
  kbNumPad5 = $008F00;
  kbCtrlNumPad5 = $048F00; {AK155}

  kbCtrlGrayPlus = $049000;
  kbAltGrayPlus = $084E00;
  kbCtrlGrayMinus = $048E00;
  kbAltGrayMinus = $084A00;
  kbCtrlGrayAst = $049600;
  kbAltGrayAst = $083700;
  kbCtrlGraySlash = $049500;
  kbAltGraySlash = $08A400;

  kbCtrlShiftGrayPlus = $079000;
  kbAltShiftGrayPlus = $0B4E00;
  kbCtrlShiftGrayMinus = $078E00;
  kbAltShiftGrayMinus = $0B4A00;
  kbCtrlShiftGrayAst = $079600;
  kbAltShiftGrayAst = $0B3700;
  kbCtrlShiftGraySlash = $079500;
  kbAltShiftGraySlash = $0BA400;

  kbCtrlAltGrayPlus = $0C4E00;
  kbCtrlAltShiftGrayPlus = $0F4E00;
  kbCtrlAltGrayMinus = $0C4A00;
  kbCtrlAltShiftGrayMinus = $0F4A00;
  kbCtrlAltGrayAst = $0C3700;
  kbCtrlAltShiftGrayAst = $0F3700;
  kbCtrlAltGraySlash = $0CA400;
  kbCtrlAltShiftGraySlash = $0FA400;

  kbCtrlPrtSc = $047200;
  kbCtrlShiftPrtSc = $077200;

  kbLeftSuxx = $00EC00;
  kbShiftLeftSuxx = $03EC00;
  kbRightSuxx = $00ED00;
  kbShiftRightSuxx = $03ED00;
  kbMenuSuxx = $00EE00;
  kbShiftMenuSuxx = $03EE00;

  kbCtrlLeftSuxx = $04EC00;
  kbAltLeftSuxx = $08EC00;
  kbCtrlMenuSuxx = $04EE00;
  kbCtrlRightSuxx = $04ED00;
  kbAltRightSuxx = $08ED00;
  kbAltMenuSuxx = $08EE00;

  kbCtrlShiftLeftSuxx = $07EC00;
  kbAltShiftLeftSuxx = $0BEC00;
  kbCtrlShiftRightSuxx = $07ED00;
  kbAltShiftRightSuxx = $0BED00;
  kbCtrlShiftMenuSuxx = $07EE00;
  kbAltShiftMenuSuxx = $0BEE00;

  kbCtrlAltLeftSuxx = $0CEC00;
  kbCtrlAltShiftLeftSuxx = $0FEC00;
  kbCtrlAltRightSuxx = $0CED00;
  kbCtrlAltShiftRightSuxx = $0FED00;
  kbAtrlAltMenuSuxx = $0CEE00;
  kbCtrlAltShiftMenuSuxx = $0FEE00;

  { псевдо-коды }

  kbNoKey = $000000;
  kbShortCut = $F0FFFF;
  kbDoubleAlt = $040000;
  kbDoubleCtrl = $020000;
  kbBackUp = $00FD00;
  kbDownUp = $00FE00;
  kbUpUp = $00FF00;

  { дублирующиеся имена }

  kbAltCtrlSqBracketL = kbCtrlAltSqBracketL;
  kbAltCtrlSqBracketR = kbCtrlAltSqBracketR;

  kbCtrlSlash = kbCtrlGraySlash;
  kbCtrlShiftSlash = kbCtrlShiftGraySlash;

  kbGPlus = kbGrayPlus;
  kbShiftGPlus = kbShiftGrayPlus;
  kbGMinus = kbGrayMinus;
  kbShiftGMinus = kbShiftGrayMinus;
  kbGAst = kbGrayAst;
  kbShiftGAst = kbShiftGrayAst;
  kbGSlash = kbGraySlash;
  kbShiftGSlash = kbShiftGraySlash;

  kbCtrlGPlus = kbCtrlGrayPlus;
  kbAltGPlus = kbAltGrayPlus;
  kbCtrlGMinus = kbCtrlGrayMinus;
  kbAltGMinus = kbAltGrayMinus;
  kbCtrlGAst = kbCtrlGrayAst;
  kbAltGAst = kbAltGrayAst;
  kbCtrlGSlash = kbCtrlGraySlash;
  kbAltGSlash = kbCtrlGraySlash;

  kbCtrlShiftGPlus = kbCtrlShiftGrayPlus;
  kbCtrlShiftGMinus = kbCtrlShiftGrayMinus;
  kbCtrlShiftGAst = kbCtrlShiftGrayAst;
  kbCtrlShiftGSlash = kbCtrlShiftGraySlash;
  kbAltShiftGPlus = kbAltShiftGrayPlus;
  kbAltShiftGMinus = kbAltShiftGrayMinus;
  kbAltShiftGAst = kbAltShiftGrayAst;
  kbAltShiftGSlash = kbAltShiftGraySlash;
  kbCtrlAltGPlus = kbCtrlAltGrayPlus;
  kbCtrlAltGMinus = kbCtrlAltGrayMinus;
  kbCtrlAltGAst = kbCtrlAltGrayAst;
  kbCtrlAltGSlash = kbCtrlAltGraySlash;
  {/Cat}
*)
const

  { Event codes }

  evMouseDown = $0001;
  evMouseUp = $0002;
  evMouseMove = $0004;
  evMouseAuto = $0008;
  evKeyDown = $0010;
  evCommand = $0100;
  evBroadcast = $0200;

  { Event masks }

  evNothing = $0000;
  evMouse = $000F;
  evKeyboard = $0010;
  evMessage = $FF00;

const
  kbRightShift = $0001;
    {` Нажат правый Shift `}
  kbLeftShift = $0002;
    {` Нажат левый Shift `}
  kbCtrlShift = $0004;
    {` Нажат Ctrl `}
  kbAltShift = $0008;
    {` Нажат Alt `}
  kbScrollState = $0010;
  kbNumState = $0020;
  kbCapsState = $0040;
  kbInsState = $0080;

  kbAnyShift = $000F;
    {` Нажат любой Shift, Alt или Ctrl `}

  { ViewMode constants }
  vmText = 0;
  vmHex = 1;
  vmDump = 2;
  vmAsm = 3;
  vmInternal = 16;
  vmExternal = 32;
  vmDB = 100;
  vmSpread = 101;
  vmSpreadSL = 111; {AK155: это с линиями между колонками}
  vmPKT = 102;
  vmMSG = 103;

  ebfCBF = 1 shl 0;
  ebfBSU = 1 shl 1;
  ebfABr = 1 shl 2;
  ebfAId = 1 shl 3;
  ebfAwr = 1 shl 4;
  ebfJwr = 1 shl 5;
  ebfVBl = 1 shl 6;
  ebfOfl = 1 shl 7;
  ebfHLn = 1 shl 8;
  ebfHCl = 1 shl 9;
  ebfPBl = 1 shl 10;
  ebfObl = 1 shl 11;
  ebfLck = 1 shl 12;
  ebfTRp = 1 shl 13;
  {TabReplace}
  ebfHlt = 1 shl 0; {Highlight}
  ebfSmt = 1 shl 1; {SmartTab}

  vbfHex = 1 shl 0;
  vbfWrap = 1 shl 1;
  vbfWordWrap = 1 shl 2;
  vbfHlt = 1 shl 3;
  vbfScrollAfterEOF = 1 shl 4;
  vbfAutoscroll = 1 shl 5;
  vbfDisAsm = 1 shl 6;

  ouiClock = $0001;
  ouiHideMenu = $0002;
  ouiHideStatus = $0004;
  ouiEsc = $0008;
  ouiHideCmdline = $0010;
  ouiAutoCmdLine = $0020;
  ouiBlockInsertCursor = $0040;
  ouiStoreEditorPosition = $0080;
  ouiStoreViewerPosition = $0100;
  ouiTrackEditors = $0200;
  ouiTrackViewers = $0400;
  ouiTrackDirs = $0800;


  omsReverse = $01;
  omsCursor = $02;

  osuAutoMenu = $01;
  osuRestoreScrMode = $02;
  osuKillHistory = $04;
  { osuOverlayXMS     = $08;}
  { osuOverlayEMS     = $10;}
  osuResetPalette = $08;

  osuInactivityExit = $01;
  osuAutosave = $02;
  osuBlinking = $04;
  osuPreserveDir = $08;
  osuRestorePal = $10;

  osuInt28 = $01;
  osuSleep = $02;
  osuInt2F = $04;
  osuInt15 = $08;

  ossEditor = $01;
  ossUseSysClip = $02;
//  ossShowHidden = $04;
  ossFastExec = $04;
  ossDisableXMS = $08;
  ossCheckDDA = $10;
  ossRemoveCD_RO = $20;
  ossAdvCopy = $40;
  {$IFDEF DPMI32}
  ossFlushDsk = $100;
  {$ENDIF}

  ossVerify = $01;
  ossCheckFreeSpace = $02; {DataCompBoy}
  ossUnarcToDirectly = $04; {JO}

  cdnHideCmdLine = $0001; { DN Misc Information }

  {JO}
  ditAddQick = $01;
  ditAddTemp = $02;

  ditHDD = $01;
  ditFloppy = $02;
  ditCDMO = $04;
  ditProgr = $08;
  ditNet = $10;
  {/JO}

  dfMDY = 00;
  dfDMY = 01;
  dfYMD = 02;
  {-DataCompBoy-}

  { Editor History }

  hsFindMask = 109;
  hsFindText = 110;
  hsSelectBox = 111;
  hsFileCopyName = 112;
  hsLongCopy = 113;
  hsEditOpen = 114;
  hsEditSave = 115;
  hsEditPasteFrom = 116;
  hsFileMask = 117;
  hsFindWhat = 118;
  hsViewFind = 110;

  hsSaveSheetAs = 120;
  hsLoadSheet = 121;
  hsGotoCell = 122;
  hsFindCellPattern = 123;
  hsReplacePattern = 124;
  hsColors = 125;
  hsExtract = 126;
  hsFBBCopy = 127;
  hsCustoms = 128;
  hsdbSearch = 129;
  hsImportPhones = 130;
  hsMakeDir = 131;
  hsInputParams = 132;
  hsPrintOut = 133;
  hsTetris = 134;
  hsManualPhone = 135;
  hsCalcLine = 136;
  hsArcFiles = 137;
  hsEditDesc = 138;
  hsFindAfter = 139;
  hsFindBefore = 140;
  hsNewVariable = 141;
  hsExecDOSCmd = 142;
  hsMakeList = 143;
  hsEditHistory = 144;
  hsOpenXLT = 145;
  hsEditDBF = 146;
  hsUUEncode = 147;
  hsUUDecode = 148;
  hsUULines = 149;
  hsMakeListHeader = 150;
  hsMakeListFooter = 151;
  hsFmtVolumeL = 152;
  hsAddOptions = 153;
  hsRenameFil = 154;
  hsFBBCopySize = 155;
  hsFBBCopyNum = 156;
  hsEditEALongname = 157;
  hsCreateWPSObject = 158;
  hsDBFGotoRow = 159; {John_SW}
  hsDBFGotoCol = 160; {John_SW}
  hsKbdToggleLayot = 165; {AK155}
  hsWinCodeTable = 166; {AK155}
  hsCodeTables = 167; {AK155}
  hsABCSortTable = 168; {AK155}

  {UU Encode options}

  ckNone = 0;
  ckEntire = 1;
  ckStd = 2;
  ckEach = 3;
  ck64 = 4;

  ckFileTime = 1;
  ckMapTable = 2;
  ckStatistic = 4;

  { QuickSearch key }
  pqsAlt = 0;
  pqsCtrl = 1;
  pqsCaps = 2;


  { Default interior for panels, may differs in instances }

  { footer elements show options }
  fseNotShow = 0;
  { 1..4 - in lines 1..4 respectively }
  fseInDivider = 5;
  fseInDividerBrief = 6;

//  fmiDirLen = $0001;
//  fmiCurrent = $0002;
//  fmiSelected = $0004;
//  fmiTotals = $0008;
  fmiFree = $0010;
  fmiFullNameInBottom = $0100;
  fmiDescriptionInBottom = $0200;

  { Common panel interior, same for all instances }
  fmsSortIndicator = $0001;
  fmsDriveLine = $0002;
  fmsShowScrollBar = $0004;
  fmsHiliteFiles = $0008;
  fmsShowHidden = $0010;

  { Options for panels }
  fmoAutoChangeDir = $0001;
  fmoDragAndDrop = $0002;
  fmoBeep = $0004;
  fmoEnterArchives = $0008;
  fmoSpaceToggle = $0010;
  fmoDelErase = $0020;
  fmoUseArrows = $0040;
  fmoAltDifference = $0080;
  fmoBackGoesBack = $0100;
  fmoPreserveDesc = $0200;
  fmoKillContainer = $0400;
  fmoAlwaysCopyDesc = $0800;
  fmoAutorefreshDriveLine = $1000; // вместо ini  AutoRefreshDriveLine=1
  fmoAutorefreshPanels = $2000; // вместо ini  AutoRefreshPanels=1
  fmoDescrByShortNames = $4000; // вместо ini DescrByShortNames

  { L(R) Ctrl in DriveLine }
  fdlNoDifference = 0;
  fdlPassive = 1;
  fdlLeft = 2;

  { New manager panel types }
  fdoDriveDrive = 0;
  fdoInfoDrive = 1;
  fdoTreeFrive = 2;
  fdoRightOnly = 3;

  { DriveInfo data items }
  fdiTitle = $0001;
  fdiTotals = $0002;
  fdiVolumeSize = $0004;
  fdiVolumeFree = $0008;
  fdiVolumeLabel = $0010;
  fdiLowMemory = $0020;
  fdiAvailLowMemory = $0040;
  fdiMemAvail = $0080;
  fdiEMSFree = $0100;
  fdiXMSFree = $0200;
  fdiDIZ = $0400;
  fdiSerialNo = $0800; { Rainbow }
  fdiFileSys = $1000; { Rainbow }

  { Confirmations }
  cfSingleErase = $0001;
  cfMultiErase = $0002;
  cfEraseSubDir = $0004;
  cfEraseReadonly = $0008;
  cfCreateSubdir = $0010;
  cfMouseConfirm = $0020;
  cfExitConfirm = $0040;

  { Column data items. See also PDSetup.FixColWidht}

type
  TFileColNumber =
    {` Номер колонки в описательных массивах;
    колонка имени в нумерацию не входит.
     }
   (psnShowSize,
    psnShowPacked,
    psnShowRatio,
    psnShowDate,
    psnShowTime,
    psnShowCrDate,
    psnShowCrTime,
    psnShowLADate,
    psnShowLATime,
    psnShowDescript,
    psnShowDir);
    {`}
const
  psShowSize = 1 shl Ord(psnShowSize);
  psShowPacked = 1 shl Ord(psnShowPacked);
  psShowRatio = 1 shl Ord(psnShowRatio);
  psShowDate = 1 shl Ord(psnShowDate);
  psShowTime = 1 shl Ord(psnShowTime);
  psShowCrDate = 1 shl Ord(psnShowCrDate);
  psShowCrTime = 1 shl Ord(psnShowCrTime);
  psShowLADate = 1 shl Ord(psnShowLADate);
  psShowLATime =  1 shl Ord(psnShowLATime);
  psShowDescript = 1 shl Ord(psnShowDescript);
  psShowDir = 1 shl Ord(psnShowDir);
  psLFN_InColumns = psShowDir shl 1; {Win32 only}

  { Terminal emulation }
  emTTY = 0;
  emANSIBBS = 1;
  emANSI = 2;
  emVT52 = 3;
  emVT102 = 4;

  { Terminal Options }
  toENTERLF = $0001;
  toCTRLCLS = $0002;
  toLocalEcho = $0004;
  toAutoZModem = $0008;
  toInsertMode = $0010;
  toCheckDCD = $0020;
  toConnectSnd = $0040;
  toCaptureCtrls = $0080;

  ttDirectory = 1;
  ttExec = 2;
  ttArc = 3;
  ttCust1 = 4;
  ttCust2 = 5;
  ttCust3 = 6;
  ttCust4 = 7;
  ttCust5 = 8;
  ttCust6 = 9; {JO}
  ttCust7 = 10;
  ttCust8 = 11;
  ttCust9 = 12;
  ttCust10 = 13; {JO}
  ttUpDir = 14; {AK155}
  {Phones}
  cmHold = 11304;
  cmUnhold = 11005;

  { Standard command codes }

  cmValid = 0;
  cmQuit = 1;
  cmError = 2;
  cmMenu = 3;
  cmClose = 4;
  cmZoom = 5;
  cmMaxi = 45;
  cmMinimize = 46;
  cmResize = 6;
  cmNext = 7;
  cmPrev = 8;
  cmHelp = 9;
  cmHelp2 = $DCB;
  cmNext2 = 101;
  cmPrev2 = 102;
  cmNext3 = 103;
  cmPrev3 = 104;

  { Application command codes }

  cmCut = 20;
  cmCopy = 21;
  cmPaste = 22;
  cmUndo = 23;
  cmRedo = 30; {-$VOL}
  cmClear = 24;
  cmTile = 25;
  cmCascade = 26;

  { TDialog standard commands }

  cmOK = 10;
  cmCancel = 11;
  cmYes = 12;
  cmNo = 13;
  cmDefault = 14;
  cmSkip = 15;
  cmTest = 16;

  { Dialog broadcast commands }

  cmRecordHistory = 60;

  { Standard messages }

  cmFind = 47;
  cmSelectAll = 48; {+CN}
  cmIncSearch = 49;

  cmReceivedFocus = 50;
  cmReleasedFocus = 51;
  cmCommandSetChanged = 52;

  { TScrollBar messages }

  cmScrollBarChanged = 53;
  cmScrollBarClicked = 54;

  { TWindow select messages }

  cmSelectWindowNum = 55;

  { TListViewer messages }

  cmListItemSelected = 56;

  cmSaveText = 12101;
  cmLoadText = 12102;
  cmSaveTextAs = 12103;
  cmSwitchBlock = 12104;
  cmSwitchIndent = 12105;
  cmSwitchFill = 12106;
  cmSwitchBack = 12107;
  cmSwitchSave = 12108;

  cmWordLeft = 12110;
  cmWordRight = 12111;
  cmDelWordRight = 12112;
  cmDelWordLeft = 12113;
  cmScrollUp = 12114;
  cmScrollDn = 12115;
  cmCenter = 12116;
  cmStartSearch = 12117;
  cmContSearch = 12118;
  cmDelBackChar = 12119;
  cmDelChar = 12120;
  cmSwitchIns = 12121;
  cmPgUp = 12122;
  cmPgDn = 12123;
  cmMoveUp = 12124;
  cmMoveLeft = 12125;
  cmMoveRight = 12126;
  cmMoveDown = 12127;
  cmBlockStart = 12128;
  cmBlockEnd = 12129;
  cmMarkWord = 12130;
  cmMarkLine = 12131;
  cmTab = 12135;
  cmSpecChar = 12138;
  cmReplace = 12139;
  cmEnd = 12140;
  cmEnter = 12141;
  cmInsLine = 12142;
  cmLJustify = 12147;
  cmLCenter = 12148;
  cmLLeft = 12149;
  cmLRight = 12150;
  cmDeltoEOLN = 12151;
  cmSetMargins = 12152;
  cmCtrlHome = 12153;
  cmCtrlEnd = 12154;
  cmSwitchHighLight = 12155;
  cmGotoLineNumber = 12156;
  cmSwitchWrap = 12157;
  cmSwitchHiLine = 12158;
  cmSwitchHiColumn = 12159;

{ Каждая команда перекодировки строки должна иметь код меньше, чем
  cmUpcaseBlock, см. TFileEditor.ChangeBlockCase}
  cmUpString = 12160;
  cmLowString = 12161;
  cmCapString = 12162;
  cmToggleCaseString = 12163;
  cmRusEngConvString = 12164;

  cmInsertText = 12165;
  cmInsertTime = 12166;
  cmInsertDate = 12167;
  cmMoveBlockStart = 12168;
  cmMoveBlockEnd = 12169;
  cmInsertOn = 12170;
  cmInsertOff = 12171;
  cmIndentOn = 12172;
  cmIndentOff = 12173;
  cmSelectMacro = 12174;
  cmPlayMacro = 12175;
  cmSwitchBrackets = 12176;
  cmBracketPair = 12177;

{ Каждая команда перекодировки строки должна иметь код меньше, чем
  cmUpcaseBlock, см. TFileEditor.ChangeBlockCase}
  cmUpcaseBlock = 12178;
  cmLowcaseBlock = 12179;
  cmCapitalizeBlock = 12180;
  cmToggleCaseBlock = 12181;
  cmRusEngConvBlock = 12182;

  cmAdvancePortSetup = 12183;
  cmNavyLinkSetup = 12184;
  cmSwitchKeyMapping = 12185; {-$VIV}
  cmOpenFileAtCursor = 12186; {-$VIV}
  cmEditSPF = 12187; {-$VIV}
  cmEditINI = 12188; {-$VIV}
  cmSwitchTabReplace = 12190; {-$VOL}
  cmEditCrLfMode = 12191;
  cmEditLfMode = 12192;
  cmEditCrMode = 12193;
  cmSwitchDrawMode = 12195;
  cmLoadXlatTable = 12196;

  cmGotoLineNumber2 = 12198;
  cmSwitchSmartTab = 12199;
  cmPlaceMarker1 = 14001;
  cmPlaceMarker2 = 14002;
  cmPlaceMarker3 = 14003;
  cmPlaceMarker4 = 14004;
  cmPlaceMarker5 = 14005;
  cmPlaceMarker6 = 14006;
  cmPlaceMarker7 = 14007;
  cmPlaceMarker8 = 14008;
  cmPlaceMarker9 = 14009;
  cmGoToMarker1 = 14011;
  cmGoToMarker2 = 14012;
  cmGoToMarker3 = 14013;
  cmGoToMarker4 = 14014;
  cmGoToMarker5 = 14015;
  cmGoToMarker6 = 14016;
  cmGoToMarker7 = 14017;
  cmGoToMarker8 = 14018;
  cmGoToMarker9 = 14019;
  cmBackSearch = 14020;

  cmCopyUnselect = 10001;
  cmRereadDir = 10004;
  cmRereadInfo = 10005;
  cmPushName = 10006;
  cmRereadTree = 10007;
  cmTree = 10009;
  cmFileView = 10011;
  cmFileEdit = 10013;
  cmInsertBlock = 10014;
  cmDisableSky = 10018;
  cmChangeDrv = 10019;
  cmInsertDrive = 10021;
  cmMenuOn = 10022;
  cmMenuOff = 10023;
  cmExecGrabber = 10024;
  cmCloseDialer = 10025;
  cmFileTextView = 10026; {AK155}

  cmFindEdit = 10101;
  cmFindView = 10102;
  cmFindPrint = 10103;
  cmFindSortBy = 10104;
  cmFindMakeList = 10105;
  cmFindGotoFile = 10106;

  cmPanelXSelect = 10203;
  cmPanelXUnselect = 10204;
  cmPanelCompare = 10211;
  cmPushFirstName = 10214;
  cmPushFullName = 10215;
  cmPushInternalName = 10216;

  cmInsertName = 10301;
  cmInfoPresent = 10302;
  cmGetDirName = 10303;
    {` Команда панели - дать путь к каталогу панели `}
//  cmLocalReread = 10304;
  cmDlgNotFound = 10305;

  cmPrepareToDelete = 10401;
  cmChangeDirectory = 10402;
  cmChangeTree = 10403;
  cmLoadViewFile = 10404;
  cmGLoadViewFile = 10405;
  cmLViewFile = 10406;
  cmStandAt = 10407;
  cmIntFileView = 10410;
  cmIntFileEdit = 10411;

  cmExecCommandLine = 10500;
  cmExecFile = 10501;
  cmDropped = 10502;
  cmFindCalculator = 10503;
  cmFindTree = 10504;
  cmFindTempDrive = 10505;
  cmTempDrive = 10506;
  cmCopyToTemp = 10508;
  cmClearCommandLine = 10509;

  cmSystemSetup = 10600;
  cmChangeColors = 10601;
  cmExtFileEdit = 10602;
  cmMenuFileEdit = 10603;
  cmExternalViewers = 10604;
  cmExternalEditors = 10605;
  cmScreenRest = 10606;
  cmHexMode = 10607;
  cmSearchFor = 10608;
  cmContinueSearch = 10609;
  cmStartup = 10610;
  cmInterfaceSetup = 10611;
  cmSaversSetup = 10612;
  cmChScreenMode = 10613;

  cmSSortName = 10801;
  cmSSortExt = 10802;
  cmSSortSize = 10803;
  cmSSortTime = 10804;
  cmSSortUnSort = 10800;
  cmShowFields = 10805;
  cmShowMemo = 10806;
  cmEditDBField = 10807;
  cmDeleteDBRec = 10808;
  cmDBFGoto = 10809;
  cmInsertDBRec = 10810;
  cmUpDBRec = 10811;
  cmDownDBRec = 10812;

  cmNewTable = 10901;
  cmChangeWidth = 10904;
  cmSheetLoad = 10905;
  cmChangeFormat = 10906;
  cmInsertLine = 10907;
  cmDeleteLine = 10908;
  cmInsertColumn = 10909;
  cmDeleteColumn = 10910;
  cmRecalc = 10911;
  cmFindCell = 10912;
  cmReplaceCell = 10913;
  cmSearchAgain = 10914;
  cmGotoError = 10916;
  cmImportToFile = 10917;
  cmImportToClip = 10918;
  cmSaveSheetAs = 10919;
  cmToggleDescriptions = 10920;
  cmToggleShowMode = 10921;
  cmToggleLongNames = 10922;
  cmOpenWPSWindow = 10923;
  cmCreateWPSObject = 10924;
  cmExportToCsv = 10925; {KV}
  cmExportToDbf = 10926; {KV}
  cmExportToXls = 10927; {KV}
  cmImportFromCsv = 10928; {KV}
  cmImportFromDbf = 10929; {KV}

  cmExtract = 11000;
  cmSetPassword = 11001;
  cmArcDelete = 11002;
  cmArcView = 11003;
  cmArcTest = 11004;
  cmTermWrite = 11005;
  cmTermLog = 11006;
  cmTermVisible = 11007;
  cmExtractTo = 11008;
  cmFullView = 11009;
  cmArchiveFiles = 11010;
  cmExecString = 11011;
  cmReceiveFile = 11012;
  cmSendFile = 11013;
  cmGetFileName = 11014;
  cmShowOutput = 11015;
  cmUnArchive = 11016;
  cmCheckArchive = 11017;
  cmInsertFile = 11018;
  cmMarkFiles = 11019;
  cmUnMarkFiles = 11020;
  cmMarkFile = 11021;
  cmRChDrive = 11022;
  cmRViewFile = 11023;
  cmReanimateFile = 11024;
  cmNewGame = 11025;
  cmEraseGroup = 11026;
//  cmPutInClipboard = 11027;
//  cmGetFromClipboard = 11028;
  cmReboundPanel = 11029;
  cmCopyCollection = 11030;
  cmSendInitModem = 11031;
  cmSendModemBreak = 11032;
  cmReadArchive = 11033;
  cmRereadForced = 11034;
  cmMakeForced = 11035;
  cmFindForced = 11036;
  cmTouchFile = 11037;
  cmDoSendLocated = 11038;
  cmGetUserParams = 11039;
  cmTreeChanged = 11040;
  cmCloseFormat = 11041;
  cmCloseLinked = 11042;
  cmGetUserParamsWL = 11043;
  {Cat}
//  cmPutInClipboardLong = 11077;
//  cmGetFromClipboardLong = 11078;
  {/Cat}

  cmQuickChange1 = 11100;
  cmQuickChange2 = 11101;
  cmQuickChange3 = 11102;
  cmQuickChange4 = 11103;
  cmQuickChange5 = 11104;
  cmQuickChange6 = 11105;
  cmQuickChange7 = 11106;
  cmQuickChange8 = 11107;
  cmQuickChange9 = 11108;

  cmUpdateHexViews = 11111;

  {  Additional Commands   }
  cmAddVariable = 11205;
  cmRenVariable = 11206;
  cmDelVariable = 11207;

  cmSetup = 19001;
  cmStop = 19002;
  cmShowHi = 19004;
  cmSendTerminalString = 19005;
  cmPostHideLeft = 19006;
  cmPostHideRight = 19007;
  cmDuplicateLine = 19008;
  { Special Commands }
  cmChangeInactive = 19010;
  cmMainMenu = 19011;
  cmAddFilter = 19012;
//  cmGetDirInfo = 19014;
  cmSinglePrint = 19015;
  { cmSingleAttr         = 19016;} {JO}
  cmSingleTag = 19017;
  cmSingleUntag = 19018;
//AK155  cmGetCurrentPosFiles = 19019;
  cmSyncClipOut = 19020;
  cmSyncClipIn = 19021;
  cmWindowsPaste = 19022;
  cmWindowsCopy = 19023;
  cmReleaseFile = 19024;
  cmFastRename = 19025;
  cmGetNetInfo = 19026; {-$VIV}
  cmViewFilter = 19027;

  cmMemInfo = 100;


  { Terminal/ScrollBack }
  cmScrollBack = 19114;
  cmSBBKill = 19115;
  cmSBBStore = 19116;
  cmSBBGrab = 19117;

  cdPlay = 19900;
  cdStop = 19901;
  cdNext = 19902;
  cdPrev = 19903;
  cdEject = 19904;
  cdPause = 19905;
  cdFFwd = 19906;
  cdFRew = 19907;
  cdSetup = 19908;
  cdDisplay = 19909;
  cdZoom = 19910;
  cdMode = 19911;
  cdOrder = 19912;
  cdTitle = 19913;
  cdMix = 19914;
  cdSong = 19915;

  cmTextMode = 1002;
  cmDirMode = 1003;
  cmFATMode = 1004;
  cmFixFile = 1005;
  cmFixDir = 1006;
  cmFixDrive = 1007;
  cmFixCluster = 1008;
  cmFixSector = 1009;
  cmFixBlockWrite = 240;

  cmOpenFile = 3001;
  cmOpenWindow = 3002;
  cmChangeMode = 3003;
  cmFindFile = 3004;
  cmCreatePanel = 3006;
  cmCreateTree = 3007;
  cmAbout = 3008;
  cmRefresh = 3009;
  cmXViewFile = 3011;
  cmShowUserScreen = 3012;
  cmFormatDisk = 3013;
  cmXEditFile = 3015;
  cmMemoryInfo = 3016;
  cmLoadDesk = 5434;
  cmSaveDesk = 5435;
  cmRetrieveSwp = 5436; {JO}
  {cmDiskCopy          = 3017;}
  cmSystemDisk = 3018;
  cmReanimator = 3019;
  cmDiskEdit = 3020;
  cmSetVolumeLabel = 3021;
  cmCalculator = 3022;
  cmHideShowTools = 3023;
  cmChangeUserMode1 = 3024;
  cmChangeUserMode2 = 3025;
  cmUserMenu = 3026;
  cmLocalMenuFileEdit = 3027;
  cmGetName = 3028;
    {` Команда окна и панели - дать текст для списка окон `}
  cmPhoneBook = 3030;
  cmStoreColors = 3031;
  cmLoadColors = 3032;
  cmHangUp = 3033;
  cmSetupModem = 3034;
  cmTerminal = 3035;
  cmUndial = 3036;
  cmTotalReread = 3038;
  cmHideWindows = 3040;
  cmHistoryList = 3041;
  cmGame = 3043;
  cmKillUsed = 3044;
//  cmFMDefaults = 3045;
  cmEditorDefaults = 3046;
  cmSetupArchive = 3047;
  cmUpdateConfig = 3048;
  cmPrintFile = 3049;
  cmPrintFileEd = 30491; {JO}
  {temporarily}
  cmHighlightGroups = 3050;
  cmSetupMouse = 3054;
  cmSetupPrinter = 3055;
  cmFilePrint = 3056;
  cmNavyLink = 3057;
//  cmColumnDefaultsDisk = 3058; {DataCompBoy}
  cmEditQuickRun = 3060;
  cmRestart = 3061; {DataCompBoy}
  cmTagData = 3062;
  cmUntagData = 3063;
  cmToggleTagData = 3064;
//  cmColumnDefaultsArvd = 3065; {DataCompBoy}
  cmASCIITable = 3066;
  cmSystemInfo = 3067;
  cmUpdateHighlight = 3068; {JO}
  cmForceTagData = 3069;
  cmForceUntagData = 3070;

  cmLoConfigArchiver = 3101;

  cmConfigACE = 3101;
  cmConfigARJ = 3104;
  cmConfigHA = 3109;
  cmConfigLHA = 3114;
  cmConfigRAR = 3117;
  cmConfigZIP = 3123;
  cmConfigAIN = 3102;
  cmConfigARC = 3103;
  cmConfigBS2 = 3105;
  cmConfigBSA = 3106;
  cmConfigCAB = 3107;
  cmConfigCHZ = 3108;
  cmConfigHAP = 3110;
  cmConfigHPK = 3111;
  cmConfigHYP = 3112;
  cmConfigIS3 = 3113;
  cmConfigLIM = 3115;
  cmConfigQUARK = 3116;
  cmConfigSQZ = 3118;
  cmConfigTAR = 3119;
  cmConfigTGZ = 3120;
  cmConfigUC2 = 3121;
  cmConfigUFA = 3122;
  cmConfigZOO = 3124;
  cmConfigZXZ = 3125;
  cmConfig7Z  = 3126;
  cmConfigBZ2 = 3127;
  cmHiConfigArchiver = 3127;
  cmUpdateArcFile = 3200;

  cmGlobalUserMenu = 3220;
  cmEditMenu = 3221;
  cmClearData = 3222;
  cmSearchAdvance = 3223;
  cmCountrySetup = 3224;
  cmSetupConfirmation = 3225;
  cmTerminalDefaults = 3226;
  cmGetTeam = 3227;
  cmOpenSmartpad = 3236;
  cmTetrisPreview = 3237;
  cmTetrisIncLevel = 3238;
  cmPlayCD = 3239;
  cmEnvEdit = 3240;
  cmEditHGL = 3241;
  { cmSingleCopy        = 3242;} {JO}
  { cmSingleRename      = 3243;} {JO}
  cmExecuteDOScmd = 3244;
  cmFMSetup = 3246;
  cmDriveInfoSetup = 3247;
  cmHideCmdLine = 3248;
//  cmRecountDirs = 3249;
  cmChLngId = 3250;
  cmIsRightPanel = 3251;

  cmLookForPanels = 3550; { Are there any panels? }
  cmFirstTimePanel = 3551; { Open a panel if none exist }
  cmReverseSearch = 3552;
  cmEditHistory = 3553;
  cmViewHistory = 3554;

  cmOpenClipBoard = 3555; {-$VOL}

  cmShowTimeInfo = 3556; {DataCompBoy}

  cmTextView = 22189;
  cmHexView = 22190;
  cmDBFView = 22191;
  cmWKZView = 22192;

  cmSetValue = 10000;
  cmSetError = 10001;
  cmCalcValue = 10002;
  cmCopyClip = 10003;

  cmGetPtr1 = 23000;
  cmGetPtr2 = 23001;

  { R - Commands      }

  cmRFormat = 24000;
  cmNewStrColl = 24001;
  cmGetCmpNfo = 24418;
  cmRSearchAdvance = 24123;

  cmCalendar = 27000;
  cmTasklist = 27001;

  { Open/Save commands }

  cmOpen = 27;
  cmSave = 28;
  cmSaveAll = 29;

  { Editor Commands }

  cmBlockRead = 120;
  cmBlockWrite = 121;
  cmBlockPrint = 122;

  cmFJustify = 123;
  cmFCenter = 124;
  cmFLeft = 125;
  cmFRight = 126;

  cmSortBlock = 127;
  cmCalcBlock = 128;
  cmCopyBlock = 129;
  cmMoveBlock = 130;
  cmHideBlock = 131;

  cmIndentBlock = 135;
  cmUnIndentBlock = 136;

  cmRevSortBlock = 139;

  { Panel Commands }

  PanelCommands = [149..199,230,231];

  cmForceRescan = 149;
  cmViewFile = 150;
  cmEditFile = 151;
  cmIntViewFile = 153;
  cmIntEditFile = 154;
  cmCopyFiles = 155;
  cmTempCopyFiles = 156;
  cmPanelLongCopy = 157;
  cmMoveFiles = 158;
  cmPanelMkDir = 159;
  cmPanelErase = 160;
  cmMakeList = 161;
  cmCompareDir = 162;
  cmSetFAttr = 163;
  cmPanelSortSetup = 164;
  cmSortBy = 165;
  cmPanelSelect = 166;
  cmPanelUnselect = 167;
  cmPanelInvertSel = 168;
  cmPanelReread = 169;
  cmChangeDrive = 170;
  cmChangeDir = 171;
  cmPanelShowSetup = 172;
  cmPanelPrint = 173;
  cmPanelArcFiles = 174;
  cmExtractArchive = 175;
  cmPanelMakeList = 176;
  cmListOfDirs = 177;
  cmCountLen = 178;
  cmUUEncodeFile = 179;
  cmUUDecodeFile = 180;
  cmDirHistory = 181;
  cmUnpDiskImg = 182;
  cmSingleDel = 183;
  cmAdvFilter = 184;
  cmSingleAttr = 185; {JO}
  cmDirBranch = 186;
  cmChangeNameCase = 187;
  cmSetEALongname = 188;
  cmSelectPreset = 230; {JO}
  cmDirBranchFull = 231; {JO}

  { ??? Commands }

  cmViewText = 189;
  cmViewHex = 190;
  cmViewDBF = 191;
  cmViewWKZ = 192;
  cmPrintBlock = 193;
  cmDialPhone = 194;
  cmWindowManager = 195;
  cmClearDesktop = 196;
  cmUnWrap = 197;
  cmSingleCopy = 198; {JO}
  cmSingleRename = 199; {JO}
  cmGotoCell = 508;

  NumSortModes = 8;

  { Sort commands }
   {AK155: коды команд в этой секции должны идти подряд
    и их порядок должен соответствовать порядку констант
    dlSortName и далее, а также psmLongName и далее.
    И этот порядок должен соответствовать порядку чекбоксов
    в диалоге настроек сортировки. См. также CM_SortBy }
  cmSortName = 13000;
  cmSortExt = 13001;
  cmSortSize = 13002;
  cmSortDate = 13003;
  cmSortCrDate = 13004;
  cmSortLADate = 13005;
  cmSortDIZ = 13006;
  cmSortUnsorted = 13007;

  cmSortOwnerToggle = 13257; {JO}
  cmSortGrpToggle = 13258;
  cmSortInvToggle = 13259; {JO}
  cmSortDirsByNameToggle = 13260; {AK155}

  { Panel sort mode }
  psmLongName = 0;
  psmLongExt = 1;
  psmSize = 2;
  psmTime = 3;
  psmCrTime = 4;
  psmLATime = 5;
  psmDIZ = 6;
  psmUnsorted = 7;

{//JO: ниже идут специальные значения, которые TFilesCollection.SortMode
 //    принимает перед использованием метода TFilesCollection.FileCompare}
  fcmCompSize = 1; {сравнить размер файлов}
  fcmCompTime = 2; {сравнить дату и время файлов}
  fcmCompAttr = 4; {сравнить атрибуты файлов}
  fcmCompContent = 8; {сравнить содержимое файлов}
  fcmCaseSensitive = 16; {регистрочувствительное сравнение имён файлов}
  fcmPreciseCompare = 32; {сравнение всего сразу для групповых операций}

  psfOwnerFirst = 1; {JO: сортировать сначала пути к файлам}
  psfSortByType = 2; {JO: сортировать сначала по группе}
  psfInverted = 4; {JO: сортировать в обратном порядке}
  psfDirsByName = 8; {JO: сортировать каталоги всегда по имени}

  { Groups of files placed to the top during panel sorting }
  upsNone = 0;
  upsDirs = 1;
  upsArchives = 2;
  upsExecutables = 3;
  upsHidSysFiles = 4;

  { DblWnd Commands }

  DblWndCommands = [cmMaxi, 200..219];

  cmDirTree = 200;
  cmQuickView = 201;
  cmSwapPanels = 202;
  cmHideLeft = 203;
  cmHideRight = 204;
  cmChangeLeft = 205;
  cmChangeRight = 206;
  cmDiskInfo = 207;
  cmSwitchOther = 208;
  cmNetInfo = 209; {-$VIV}
  cmDizView = 210;

  { Phonebook Commands }

  PhonesCommands = [221..228];
  cmPhoneBookMode = 221;
  cmImportPhones = 222;
  cmSearchPhone = 223;
  cmHideInactive = 224;
  cmCopyPhone = 225;
  cmInsertPhone = 226;
  cmDeletePhone = 227;
  cmEditPhone = 228;

  cmPalVGA = 229;

  cmSelectMenuCommands = 30000;
  cmMoreCommand = 30998;
  cmNoneCommand = 30999;
  cmSwitch = 31000;

  cmPlugins = 32000; {Cat}
  {..........занято..........} {Cat}
  cmPluginsEnd = 65000; {Cat}

  { Help Contexts }

  hcPluginManager = 25000; {Cat}

  hcNew = $FF01;
  hcOpen = $FF02;
  hcSave = $FF03;
  hcSaveAs = $FF04;
  hcSaveAll = $FF05;
  hcChangeDir = $FF06;
  hcDosShell = $FF07;
  hcExit = $FF08;

  hcUndo = $FF10;
  hcCut = $FF11;
  hcCopy = $FF12;
  hcPaste = $FF13;
  hcClear = $FF14;

  hcTile = $FF20;
  hcCascade = $FF21;
  hcCloseAll = $FF22;
  hcResize = $FF23;
  hcZoom = $FF24;
  hcNext = $FF25;
  hcPrev = $FF26;
  hcClose = $FF27;

  hcZIPWithoutCentralDir = 22000; {AK155}

  ofSelectable = $0001;
  ofTopSelect = $0002;
  ofFirstClick = $0004;
  ofFramed = $0008;
  ofPreProcess = $0010;
  ofPostProcess = $0020;
  ofBuffered = $0040;
  ofTileable = $0080;
  ofCenterX = $0100;
  ofCenterY = $0200;
  ofCentered = $0300;
  ofValidate = $0400;
  ofSecurity = $0800;
  ofVersion = $3000;
  ofVersion10 = $0000;
  ofVersion20 = $1000;

  {Wrap constants} {DataCompBoy}
  wmNone = 0;
  wmWidth = 1;
  wmWords = 2;

type
  TCRLF = (cfNone, cfCRLF, cfCR, cfLF);

type
  TStrIdx = (
    {Cat: эти индексы поставлены в начало списка, чтобы в будущем не возникало
      несоответствий индексов ресурсов, когда файлы DN.EXE и PLUGMAN.DLL
      взяты из разных версий ДН-а}
    dlPlugins0,
    dlPlugins1,
    dlPlugins2,
    dlPlugins3,
    dlPlugins4,
    dlPlugins5,
    dlPlugins6,
    dlPlugins7,
    dlPlugins8,
    dlPlugins9,
    {/Cat}

    dlAltTable,
    erInvalidFormula,
    erInvalidCell,
    erInvalidValue,
    erInvalidFunction,
    erRecurseTooDeep,
    erDeepDependence,
    erInvalidFileFormat,
    erInvalidIF,
    erGotoInvalidNumber,
    erCantReadDesktop,
    erDesktopError,
    erInvalidDesktop,
    erCantCreateFile,
    erNotEnoughMemory,
    erCantOpenConfig,
    erCantOpenHelp,
    erInvalidDrive,
    erInvalidFileName,
    erNoDiskSpace,
    erNoDiskSpacePre,
    erTextNotFound,
    erNoQuickDirs,

    eruuNoStuff,
    eruuUnknownMode,
    eruuBlankLine,
    eruuNonUULine,
    eruuLineNo,
    eruuIncorrectTerm,
    dlUUDecoding,

    dlPromptForQDir,
    dlOpenFile,
    dlOpenFileName,
    dlSaveFileAs,
    dlSaveFileAsName,
    dlFileExist,
    dlCanNotWrite,
    dlValuteDialog,
    dlValuteDlgName,
    dlValuteDlgSymbol,
    dlValuteDlgSForward,
    dlValuteDlgSComma,
    dlOptions,
    dlCellDialog,
    dlCellAsIs,
    dlCellDec,
    dlCellBool,
    dlCellCurrency,
    dlCellDisplay,
    dlCellJLeft,
    dlCellJRight,
    dlCellJCenter,
    dlCellJustify,
    dlCellCurrencyName,
    dlCellDecimals,
    dlCellWidth,
    dlCellExponent,
    dlCellNothing,
    dlCellProtect,
    dlCellComma,
    dlCellPercent,
    dlGotoCellNum,
    dlGotoCellName,
    dlFindCellTitle,
    dlFindCellPattern,
    dlFindCellSensitive,
    dlFindCellWhole,
    dlFindCellSearchAs,
    dlFindCellString,
    dlFindCellValue,
    dlReplaceTitle,
    dlReplacePattern,
    dlReplacePrompt,
    dlYesButton,
    dlOKButton,
    dlNoButton,
    dlNextButton,
    dlAppendButton,
    dl2YesButton,
    dlAllButton,
    dlCancelButton,
    dlHelpButton,
    dlTreeButton,
    dlDriveButton,
    dlViewButton,
    dlHexASCIIButton,
    dlMkDirButton,
    dlRereadButton,
    dlOverwriteButton,
    dlSkipButton,
    dlYesForAll,
    dlRead,
    dlWrite,
    dlChooseDir,
    dlChooseFile,
    dlColors,
    dlColorsGroup,
    dlColorsItem,
    dlColorsText,
    dlColorsBack,
    dlColorsColor,
    dlColorsNormal,
    dlColorsHighlight,
    dlColorsUnderline,
    dlColorsInverse,

    dlDBViewNumeric,
    dlDBViewCharacter,
    dlDBViewMemo,
    dlDBViewLogical,
    dlDBViewDate,
    dlDBViewFloat,
    dlDBViewPicture,
    dlDBViewEmpty,
    dlDBViewNoMemo,
    dlDBViewViewMemo,
    dlDBViewInfo,
    dlDBViewInfoString,
    dlDBViewSearchNot,
    dlDBViewSearch,
    dlDBViewSearchingIn,
    dlDBViewTextSearch,
    dlDBEditField,
    dlDBValue,
    dlDBCantEdit,
    dlDBEmptyStruc,

    dlDICurDir,
    dlDINoFiles,
    dlDIFiles,
    dlDIDirectory, {JO}
    dlDIBytes,
    dlDIFile,
    dlDIByte,
    dlDIWith,
    dlDITotalDisk,
    dlDIFreeDisk,
    dlDIVolumeID,
    dlDISerialNo, { Rainbow }
    dlDIFileSys, { Rainbow }
    dlDIUsed,
    dlDIClusterSize,
    dlDIMemoryTotal,
    dlDIMemoryForUser,
    dlDIMemoryForDN,
    dlDIEMS1,
    dlDIEMS2,
    dlDIXMSFree,
    dlDINone,

    dlStdDlgManyFiles,
    dlDirectory,
    dlFile,
    dlDrive,
    dlFiles,
    dlDirectoriesLabel,
    dlClear,
    dlOpen,
    dlStop,
    dlReplace,
    dlWarning,
    dlCloseButton,
    dlOCloseButton,
    dlCopyButton,
    dlRenameButton,
    dlDTDiskFull1,
    dlDTDiskFull2,
    dlEraseRO,
    dlEraseCantDelDir,
    dlEraseDirNotEmpty,
    dlEraseConfirm1,
    dlEraseConfirms1,
    dlEraseConfirm2,
    dlErase,
    dlErasingFile,
    dlErasingDir,
    dlErasingNoFile,
    dlEraseConfirmDir,
    dlFBBNotReadSource,
    dlFBBOver1,
    dlFBBOver2,
    dlFBBInsertDisk,
    dlFBBDiskFull1,
    dlFBBNoCreate,
    dlFBBNoWrite,
    dlFBBDiskFull2,
    dlFBBFragment,
    dlFBBDEFragment,
    dlFBBFragmentTo,
    dlFBBNoOpen,
    dlSplit_to,
    dlFlushingBuffers,
    dlComparing,
    dlFCOver,
    dlFCItself,
    dlFCNotDiskSpace,
    dlFCCopy1,
    dlFCMove1,
    dlFCCopy2,
    dlFCMove2,
    dlFCNoRename1,
    dlFCNoRename2,
    dlDIDir,
    dlFCNoCreateDir,
    dlFCCopy,
    dlFCMove,
    dlFCVerifyFailed,
    dlNoFilesSelected,
    dlBytesIn,
    dlSelectedFiles,
    dlTotal,
    dlFileFind,
    dlTempDrive,
    dlNoFilesFound,
    dlFilesFound,
    dlChangeDir,
    dlSetAttr,

    dlSortName, {AK155: порядок в этой секции менять нельзя! См. CM_SortBy }
    dlSortExt,
    dlSortSize,
    dlSortDate,
    dlSortCrDate,
    dlSortLADate,
    dlSortDIZ,
    dlSortUnsorted,

    dlSortOwner, {JO}
    dlSortGroup, {JO}
    dlSortInverted, {JO}
    dlSortDirsByName,{AK155}
    dlSortTag, {AK155}
    dlViewQuery,
    dlViewSearchFor,
    dlViewCase,

    dlArcMsg1,
    dlArcMsg4,
    dlArcMsg5,
    dlArcMsg6,
    dlArcMsg7,
    dlArcMsg8,
    dlArcReadArc,
    dlArcExtract,
    dlArcDest,
    dlArcSetPsw,
    dlArcMove,
    dlArcAdd,
    dlArcInto,
    dlArcOpt,
    dlArcDelete,
    dlArcName,
    dlArcUSize,
    dlArcPSize,
    dlArcRatio,
    dlArcDate,

    dlQueryExit,
    dlStoreColorPal,
    dlLoadColorPal,
    dlFileName,
    dlDisconnect,
    dlPercentComplete,
    dlTopLFN,
    dlTopSplit,
    dlTopName,
    dlTopSize,
    dlTopPacked,
    dlTopRatio,
    dlTopTime,
    dlTopDate,
    dlTopCrTime,
    dlTopCrDate,
    dlTopLATime,
    dlTopLADate,
    dlTopPath,
    dlTopOriginal,
    dlASCIIChart,
    dlWorkSheet,
    dlNotSaved,
    dlExportTitle,
    dlExportLabel,
    dlRunButton,
    dlDropButton,
    dlKillButton,
    dlDeleteButton,
    dlWindowsLabel,
    dlWindowManager,
    dlPhoneBook,
    dlPhonesLabel,
    dlPhonesLabelGroup,
    dlPhonesLabelPhones,
    dlPhonesUpDir,
    dlDialButton,
    dlEditButton,
    dlPhoneAppend,
    dlPhoneDelete,
    dlPhoneDeleteQuery,
    dlFileManager,
    dlDBNumeric,
    dlDBCharacter,
    dlDBMemo,
    dlDBLogical,
    dlDBDate,
    dlDBFloat,
    dlDBPicture,
    dlDBViewName,
    dlJanuary,
    dlFebruary,
    dlMarch,
    dlApril,
    dlMay,
    dlJune,
    dlJuly,
    dlAugust,
    dlSeptember,
    dlOctober,
    dlNovember,
    dlDecember,
    dlCluster,
    dlSector,
    dl1st,
    dl2nd,
    dl3rd,
    dlSometh,
    dlCopyOfFAT,
    dlReservedSector,
    dlDiskFixer,
    dlFixerFill,
    dlCopyTo,
    dlNumberClusters,
    dlStartingCluster,
    dlValidClusters,
    dlClusterRange,
    dlNumberSectors,
    dlStartingSector,
    dlValidSectors,
    dlSectorRange,
    dlSBootSector,
    dlBootSector,
    dlS1stFAT,
    dlS2ndFAT,
    dlRootDirectory,
    dlUnusedDirEntry,
    dlSearching,
    dlPasteFromTitle,
    dlPasteFromLabel,
    dlQueryModified,
    dlReadingFile,
    dlWritingFile,
    dlQueryReplace,
    dlEditTitle,
    dlGameTitle,
    dlNewButton,
    dlSetupButton,
    dlTop10Button,
    dlPauseButton,
    dlTetName,
    dlTetrisBest,
    dlTetrisNext,
    dlGameScore,
    dlGameLevel,
    dlGameLines,
    dlGameScore2,
    dlGameInfo,
    dlTetris,
    dlPentix,
    dlTop10,
    dlGameOver,
    dlTotalWrite,
    dlTotalWrited,
    dlBytes,
    dlViewFile,
    dlExportingDIZ,
    dlDeletingDIZ,
    dlDeletingSource,
    erIntoItself,
    dlCreatingDirectory,
    dlPhoneDirDelete,
    dlPleaseStandBy,
    dlArcPacking,
    dlArcUnpacking,
    dlLoadingViewer,
    dlLoadingEditor,
    dlErrorsOccurred,
    dlSelectDirectory,
    dlScreenSaverSetup,
    dlSS_S_electedSavers,
    dlSS_A_dd,
    dlSS_R_emove,
    dlSSA_v_ailableSavers,
    dlSS_T_ime,
    dlSSUse_M_ouse,
    dlCalculator,
    dlUnknownWindowType,
    dlColorsTitle,
    dl_C_olor,
    dl_B_ackground,
    dl_F_oreground,
    dl_I_tem,
    dl_G_roup,
    dlFBBBinary,
    dlFBBVerify,
    dlQueryCreateDir,
    dlQueryAbort,
    dlFCNotOverDir,
    dlNotRenameDir,
    dlFCRename,
    dlFCRenameNew,
    dlNoPossibleName,
    dlEditDesc,
    dl_D_escription,
    dlFC_Writing,
    dlFC_Written,
    dlFC_Total,
    dlFC_To,
    dlFC_Reading,
    dlFC_WasRead,
    dlFCCheckingDirs,
    dlFC_Older,
    dlFC_Exists,
    dlQueryCancelSearch,
    dlReadingList,
    {dlListDriveTop,}
    dlFillQuery,
    dlSystemInfo,
    dlSI_Ports,
    dlSI_OSVer,
    dlSI_Lpt,
    dlSI_COM,
    dlSI_Memory,
    dlSI_Main,
    dlSI_MainBoard,
    dlSI_TotalMem,
    dlSI_Expanded,
    dlSI_Extended,
    dlSI_MemAvail,
    dlSI_MaxAvail,
    dlSI_DiskDrivers,
    dlSI_1stHard,
    dlSI_2ndHard,
    dlSI_FloppyDrives,
    dlSI_Heads,
    dlSI_Tracks,
    dlSI_SectTrack,
    dlSI_NotPresent,
    dlDetailButton,
    dlTSRTitle,
    dlMemoryInfo,
    dlHookVectors,
    dlUnknown,
    dlMI_CmdLine,
    dlMI_PgmPath,
    dlMI_DeviceHandle1,
    dlMI_DeviceHandle2,
    dlMI_DeviceName,
    dlMI_DeviceFlags,
    dlMI_ResArea,
    dlMI_Files,
    dlMI_Buffers,
    dlMI_FCBs,
    dlMI_Drives,
    dlMI_Stacks,
    dlMI_IFS,
    dlMI_XBuffers,
    dlMsgWarning,
    dlMsgError,
    dlMsgInformation,
    dlMsgConfirm,
    dlMsgQuery,
    dlMsgAbout,

    dlED_VertNeed,
    dlED_Printed,
    dlED_Print,
    dlED_PrintQuery,
    dlED_PrintLine,
    dlED_OpenFile,
    dlED_OverQuery,
    dlED_ModifyRO,

    dlModemNoSetup,
    dlNoModemPort,
    dlManualDial,
    dl_P_honeNumber,
    dlMD_Manual,
    dlPB_SearchPhone,
    dlPB_S_earchString,
    dlPB_NoFind,
    dlPB_ImportPhones,
    dlPB_Working,
    dlPB_CnvReport,
    dlAutoDialer,
    dlAD_Name,
    dlAD_Queue,
    dlAD_Status,
    dlAD_Redial,
    dlAD_ForceDial,
    dlAD_Delete,
    dlAD_HoldUnhold,
    dlAD_Unhold,
    dlAD_Hold,
    dlAD_Dialing,
    dlAD_Holded1,
    dlAD_Holded2,
    dlAD_NoDial,
    dlAD_ModemSays,
    dlPrintingPaused,
    dlPrinting,
    dlPrintOut,
    dlPrintNoInit,
    dlPrintCancelQuery,
    dlPManagerTitle,
    dlPDeleteQeury1,
    dlPDeleteQeury2,
    dlCantPrint,
    dlCantPrintFile,
    dlNotPrinter,
    dlTermProtocol,
    dlTermProtocolFile,
    dlTermProtocolRece,
    dlTermProtocolSent,
    dlTermProtocolErrors,
    dlTermProtocolEstimate,
    dlTermReceXmodem,
    dlTermReceiving,
    dlTermWaitAnswer,
    dlTermSendFile,
    dlTermSending,
    dlTermWriteFile,
    dlTermSetLog,
    dlTermCommandsHistory,
    dlTermCopy_H_istory,
    dlTermCopy_C_lipboard,
    dlTerminalTitle,
    dlScanningDirs,
    dlTreeFilesWith,
    dlTreeTitle,
    dlTree1FileWith,
    dlMNUNotFound,
    dlMenuParams,
    dlMenuParamLabel,
    dlGrabWelcome,
    dlValueNotInRange,
    dlEnvVarLabel,
    dlEnvDelConfirm,
    dlEnvAddTitle,
    dlEnvVariable,
    dlEnvRenVar,

    dlTeamHint,

    dlTeamAll,
    dlTeam2_000,
    dlTeam2_001,
    dlTeam2_002,
    dlTeam2_003,
    dlTeam2_004,
    dlTeam2_005,
    dlTeam2_006,
    dlTeam2_007,
    dlTeam2_008,
    dlTeam2_009,
    dlTeam2_010,
    dlTeam2_011,
    dlTeam2_012,
    dlTeam000,
    dlTeam001,
    dlTeam002,
    dlTeam003,
    dlTeam004,
    dlTeam005,
    dlTeam006,
    dlTeam007,
    dlTeam008,
    dlTeam009,
    dlTeam010,
    dlTeam011,
    dlTeam012,
    dlTeam013,
    dlTeam014,
    dlTeam015,
    dlTeam016,
    dlTeam017,
    dlTeam018,
    dlTeam019,
    dlTeam020,
    dlTeam021,
    dlTeam022,
    dlTeam023,
    dlTeam024,
    dlTeam025,
    dlTeam026,
    dlTeam027,
    dlTeam028,
    dlTeam029,
    dlTeam030,
    dlTeam031,
    dlTeam032,
    dlTeam033,
    dlTeam034,
    dlTeam035,
    dlTeam036,
    dlTeam037,
    dlTeam038,
    dlTeam039,
    dlTeam040,
    dlTeam041,
    dlTeam042,
    dlTeam043,
    dlTeam044,
    dlTeam045,
    dlTeam046,
    dlTeam047,
    dlTeam048,
    dlTeam049,
    dlTeam050,
    dlTeam051,
    dlTeam052,
    dlTeam053,
    dlTeam054,
    dlTeam055,
    dlTeam056,
    dlTeam057,
    dlTeam058,
    dlTeam059,
    dlTeam060,
    dlTeam061,
    dlTeam062,
    dlTeam063,
    dlTeam064,
    dlTeam065,
    dlTeam066,
    dlTeam067,
    dlTeam068,
    dlTeam069,
    dlTeam070,
    dlTeam071,
    dlTeam072,
    dlTeam073,
    dlTeam074,
    dlTeam075,
    dlTeam076,
    dlTeam077,
    dlTeam078,
    dlTeam079,
    dlTeam080,
    dlTeam081,
    dlTeam082,
    dlTeam083,
    dlTeam084,
    dlTeam085,
    dlTeam086,
    dlTeam087,
    dlTeam088,
    dlTeam089,
    dlTeam090,
    dlTeam091,
    dlTeam092,
    dlTeam093,
    dlTeam094,
    dlTeam095,
    dlTeam096,
    dlTeam097,
    dlTeam098,
    dlTeam099,
    dlTeam100,
    dlTeam101,
    dlTeam102,
    dlTeam103,
    dlTeam104,
    dlTeam105,
    dlTeam106,
    dlTeam107,
    dlTeam108,
    dlTeam109,
    dlTeam110,
    dlTeam111,
    dlTeam112,
    dlTeam113,
    dlTeam114,
    dlTeam115,
    dlTeam116,
    dlTeam117,
    dlTeam118,
    dlTeam119,
    dlTeam120,
    dlTeam121,
    dlTeam122,
    dlTeam123,
    dlTeam124,
    dlTeam125,
    dlTeam126,
    dlTeam127,
    dlTeam128,
    dlTeam129,
    dlTeam130,
    dlTeam131,
    dlTeam132,
    dlTeam133,
    dlTeam134,
    dlTeam135,
    dlTeam136,
    dlTeam137,
    dlTeam138,
    dlTeam139,
    dlTeam140,
    dlTeam141,
    dlTeam142,
    dlTeam143,
    dlTeam144,
    dlTeam145,
    dlTeam146,
    dlTeam147,
    dlTeam148,
    dlTeam149,
    dlTeam150,

    dlHelp,
    dlOutputTitle,

    dlDITemporary,
    dlDIFileFind,
    dlDIFFMask,
    dlBranch,
    dlDICurArchive,
    dlDIArcTotalFiles,
    dlDIPackedSize,
    dlDIUnpackedSize,
    dlDIVersionToExtract, {JO}
    dlUpDir,
    dlSubDir,
    dlSymLink, {AK155}
    dlWKZ_Empty,
    dlPM_Print,
    dlArvid_Title,
    dlArvid_Type,
    dlArvid_TimeUsed,
    dlArvid_TimeLeft,
    dlArvid_TapeDir,
    dlUUDecode, {+UUDecode}
    dlUUDecodingTo,
    dlUUDecodeFiles,
    dlUUDecodeErrors, {-UUDecode}
    dlUUEncode, {+UUEncode}
    dlUUEncoding,
    dlUUEncodeSFN,
    dlUUEncodeOS,
    dlUUEncodeCreated,
    dlUUEncodeTime,
    dlUUEncodeSize,
    dlUUEncodeSections,
    dlUUEncodeLines, {-UUEncode}
    dlFileIsSmall,
    dlMaxFiles,

    dlleSection,
    dlleSections,
    dlleListed,
    dlleCalculated,
    dlleFileSizeMismatch,
    dlleFileCRCMismatch,
    dlleErrorOpenTMP,
    dlleCantCreate,
    dlleFailedToDecode,
    dlleFileNameExp,
    dlleFileNamesMismatch,
    dlleFileTimeExp,
    dlleInvFileTimeNum,
    dlleCRC_Err,
    dlleSizeMism,
    dlleChkSumFmt,
    dlleSectionHdr,
    dlleMaxSectNumMism,
    dlleDuplicateSection,
    dlleOfFile,
    dlleIsAbsent,
    dlleAreAbsent,
    dlleUnexpEND,
    dlleNoTerm,
    dlleFailedFinal,

    dlOverflow,
    dlElapsedTime,
    dlEditHistory,
    dlReplacesMade,
    dlScrollBack,

    dlColorsBlack,
    dlColorsDark,
    dlColorsGray,
    dlColorsWhite,
    dlColorsColors,
    dlColorsDefault,
    dlColorsBlinking,
    dlColorsB_W_,
    dlColorsMono,
    dlColorsVGA,
    dlColors_VGA,
    dlColors_P_alette,
    dlColorsColor_,
    dlColors_R_ed,
    dlColors_G_reen,
    dlColors_B_lue,
    dlColors_G_ray,
    dlColors_D_efault,

    dlColors_C00,
    dlColors_C01,
    dlColors_C02,
    dlColors_C03,
    dlColors_C04,
    dlColors_C05,
    dlColors_C06,
    dlColors_C07,
    dlColors_C08,
    dlColors_C09,
    dlColors_C10,
    dlColors_C11,
    dlColors_C12,
    dlColors_C13,
    dlColors_C14,
    dlColors_C15,
    dlAutoexecWarning,
    dlSelectXLT,
    dlSetDirHistory,
    dlSetEditHistory,
    dlSetViewHistory,

    dlLinkTitle, {+LINK}
    dlLinkStarted,
    dlLinkClose,
    dlLinkErrors,
    dlLinkWaitingCmd,
    dlLinkSendingFile,
    dlLinkReceivingFile,
    dlLinkWaitFile,
    dlLinkQueryExit,
    dlLinkNoRemote,
    dlLinkTransmitFile,
    dlLinkNoRemoteLink,
    dlLinkRemoteError,
    dlLinkRemoteWait,
    dlLinkEraseDirCfm,
    dlLinkErasingFile,
    dlLinkErase,
    dlLinkRemoteDirInfo,
    dlLinkSend,
    dlLinkRcv, {-LINK}

    dlBytesOf,
    dlExecDOScmdHdr,
    dlExecDOScmdLine,
    dlPnlDescription,

    dlCE_CriticalError,
    dlCE_WriteProtected,
    dlCE_DiskNotReady,
    dlCE_DataIntegrity,
    dlCE_SeekError,
    dlCE_UnknownMedia,
    dlCE_SectorNotFound,
    dlCE_OutOfPaper,
    dlCE_WriteFault,
    dlCE_ReadFault,
    dlCE_GeneralFailure,
    dlCE_BadImageOfFAT,
    dlCE_DeviceError,
    dlCE_InsertDisk,
    dlCE_SharingViolation,
    dlCE_WrongDisk, {JO}
    dlCE_DiskFull,
    dlCE_DeviceInUse, {/JO}
    dlCE_Filename_Exced_Range, {AK155}
    dlCE_Dirty_Flag, {/AK155}
    dlCE_Filename_Illegal, {JO}
    dlCE_ErrWndTitle,

    dlCE_ButAbort,
    dlCE_ButRetry,
    dlCE_ButIgnore,
    dlCE_ButStop,

    dlViewSaveXlat,

    dlFileMask, {-$VIV}
    dlNetInfo,
    dlServer,
    dlConnections,
    dlLookedFiles,
    dlVeryDeepSearch,
    dlLookedDirs,

    dlSE_Dir2Long, {-VOL}

    stDaysWeek, {-SSK}

    dlPresent,
    dlAbsent,

    dlImage,
    dlImageError,
    dlUnpackImages,
    dlTargetDir,

    {-DataCompBoy-}
    dlSPF01,
    dlSPF02,
    dlSPF03,
    dlSPF04,
    dlSPF05,
    dlSPF06,
    dlSPF07,
    dlSPF08,
    dlSPF09,
    dlSPF10,
    dlSPF11,
    dlSPF12,
    dlSPF13,
    dlSPF14,
    dlSPF15,
    dlSPF16,
    dlSPF17,
    dlSPF18,
    dlSPF19,
    dlSPF20,
    dlSPF21,
    dlSPF22,
    dlSPF23,
    dlSPF24,
    dlSPF25,
    dlSPFht,
    dlCopied,
    dlTestButton,
    {-DataCompBoy-}
    dlArvidNeedDisk,
    dlArvidCmdFileCreated,
    dlArvidCmdFileAppended,
    dlArvidCanChangeOnlyAVT,
    dlArvidVolumeIsNotTape,
    dlArvidNoReal,

    dlAnonymous, (*X-Man*)
    dlMenuItemOn, (*X-Man*)
    dlMenuItemOff, (*X-Man*)
    {-DataCompBoy-}
    dlMachineTypeFF, {+SYSINFO}
    dlMachineTypeFE,
    dlMachineTypeFD,
    dlMachineTypeFC_1,
    dlMachineTypeFC_2,
    dlMachineTypeFC_4,
    dlMachineTypeFC_5,
    dlMachineTypeFC_6,
    dlMachineTypeFC_8,
    dlMachineTypeFC_0b,
    dlMachineTypeFC_20,
    dlMachineTypeFC_42,
    dlMachineTypeFC_45,
    dlMachineTypeFC_48,
    dlMachineTypeFC_4F,
    dlMachineTypeFC_50,
    dlMachineTypeFC_51,
    dlMachineTypeFC_52,
    dlMachineTypeFC_94,
    dlMachineTypeFC_else,
    dlMachineTypeFB86,
    dlMachineType80,
    dlMachineTypeFA_1,
    dlMachineTypeFA_else,
    dlMachineTypeF9,
    dlMachineTypeF8_4_9_B,
    dlMachineTypeF8_else,
    dlMachineTypeB6,
    dlMachineType9A,
    dlMachineType2D,
    dlMachineTypeE1,
    dlMachineType30,
    dlMachineTypeElse,
    dlMHz,
    dlUnknownDiskDriverType,
    dlEmulated,
    dlInternal,
    dlNone,
    dlUnknownCPUFPU, {-SYSINFO}
    {-DataCompBoy-}
    dlcTitle, {JO} {+CALENDAR}
    dlcLine,
    dlcJanuary,
    dlcFebruary,
    dlcMarch,
    dlcApril,
    dlcMay,
    dlcJune,
    dlcJuly,
    dlcAugust,
    dlcSeptember,
    dlcOctober,
    dlcNovember,
    dlcDecember, {JO} {-CALENDAR}

    dlOpenNewWindow,
    dlMore, {JO}
    dlFindText,
    dlViewFilter,

    dlTrashCaption, {X-Man}
    dlFilDir,
    dlDirectories,
    dlFindPanel,
    dlListPanel, {-DataCompBoy-}
    dlEmpty,
    dlFileSearch,
    dlKillBuffer,
    dlSaveScrollback,
    dlScrollbackSaved,
    dlWriteASCII,
    dlWriteAvatar,
    dlWriteAnsi,
    dlCopyToTerminal,
    dlCopyToHistory,
    dlCopyToClipboard,
    dlSaveAsASCII,
    dlSaveAsANSI,
    dlSaveAsAvatar,
    dlPktView,
    dlPktHeader,
    dlPktFile,
    dlPktMsg,
    dlViewMsg,
    dlSaveMsg,
    dlMsgFile,
    dlErrorWriting,
    dlNetMailView,
    dlLastMsg,
    dlFirstMsg,
    dlDelMsg,
    dlNotPkt,
    dlReadingPkt,
    dlLine,
    dlNameLong,
    dlNameShort,
    dlFullInfo,
    dlDescr,
    dlPath,
    dlAdvancedFormat, {JO}
    dlBootSector1,
    dlEntries,
    dlDirEntries,
    dlSize,
    dlClusterSize,
    dlTracks,
    dlHeads,
    dlSectors1,
    dlSectors2,
    dlStartTr1,
    dlStartTr2,
    dlStartTr3,
    dlInterleave1,
    dlInterleave2,
    dlSectorSh1,
    dlSectorSh2,
    dlSectorSh3,
    dlHeadSectSh1,
    dlHeadSectSh2,
    dlHeadSectSh3,
    dlTrackSectSh1,
    dlTrackSectSh2,
    dlTrackSectSh3,
    dlTrack,
    dlSectorSize1,
    dlSectorSize2,
    dlDrivesType,
    dlDisk,
    dlDOS,
    dlAutodetect, {/JO}

    dl_Failed_to_enumerate_EA, {JO - Extended Attributes}
    dl_Failed_to_retrieve_EA,
    dl_Failed_to_store_EA,
    dl_Critical_EA_Copy_Fail,
    dlEditEALongname,
    dl_EALongname, {/JO}
    dl_Failed_to_set_volume_label, {JO}

    sdtError, {X-Man >>>}
    sdtFixed,
    sdtRemovable,
    sdtRemote,
    sdtProgram,
    sdtCDROM,
    sdtRAMDrive, {X-Man <<<}
    sdtOptical, {JO}
    sdtSubst, {AK155}

    dlDoes_Not_Exist, {JO}
    dlCmdLineTooLong, {JO}
    dlCantLoad, {JO}
    dlIniDataReadError, {PZ}
    dlIniDataTooLong, {PZ}
    dlInvalidDBFHeader, {JOHN_SW}
    dlInvalidRecNumber, {JOHN_SW}
    dlInvalidRecNumber1, {JOHN_SW}
    dlInvalidRecSize, {JOHN_SW}
    dlUseCalcRecNumber, {JOHN_SW}
    dlCopyTimeSpeed,    {John_SW}
    dlNotValidForCurSession, {JO}
    dlCreateObject, {JO}
    dlObjectTitle,
    dl_CodePage_FS_Error, {JO}
    dlCre, {JO}
    dlLac, {JO}

    {PZ begin - regular expressions }
    dlResErrorTitle,
    dlResUnknown,
    dlResCanceled,
    dlResTooComplex,
    dlResOutOfSpace,
    dlResExpectedParenthesis,
    dlResOperandCouldBeEmpty,
    dlResNested,
    dlResInvalidEscape,
    dlResInvalidSetRange,
    dlResExpectedSquareBracket,
    dlResFollowsNothing,
    dlResTrailingBackSlash,
    {PZ end - regular expressions }

    dlHistDelCurDir, {John_SW}
    dlWkzQuerySeparatorTitle, {KV}
    dlWkzQuerySeparatorLabel, {KV}
    dlWkzWarningClear,
    dlWkzWarningClearCell, {AK155}
    dlWkzWarningCellTypeChange, {AK155}
    dlArcEncrypted, {JO}
    {AK155}
    dlMissingLeftBracket,
    dlMissingRightBracket,
    dlWrongText,
    dlNoOperand,
    dlMissingOperation,
    dlWrongComma,
    {/AK155}
    dlOperationNotValidForDdrive, {JO}
    {JO}
    dlTasklist,
    dlSwitchButton,
    {/JO}
    dlSpacesInPassword, { Flash >>> }
    dlPressF1, {JO}
    dlLayoutErr,
    dlWinErr,
    dlSortError,
    dlCodetablesErr,
    dlCoutrySetupErr,
    dlOtherPanel,
    dlUndoPanelSetup,
    dlAllPanelTypes,
    dlCurPanelType,
    dlAppearanceOnly,
    dlFullPanelSetup,
    {JO}
    dlInBranch,
    dlInFound,
    dlInBranchOfArchive,
    dlInFoundInArchive,
    dlInList,
    dlNetworkError,
    dlNoNetworkPath
    {/JO}
    );

  TDlgIdx = (
    dlgSelect,
    dlgUnselect,
    dlgSetupArc,
    dlgCommandsHistory,
    dlgExtract,
    dlgSetPassword,
    dlgLoginPassword,
    dlgCompareDirs,
    dlgFilesAttr,
    dlgFileAttr,
    dlgFMSetup,
    dlgPanelShowSetup,
    dlgPanelSortSetup,
    dlgSavePanelSetup,
    dlgDriveInfoSetup,
    dlgDiskError,
    dlgMakeList,
    dlgCopyDialog,
    dlgRenFl,
    dlgMkDir,
    dlgFileFind,
    dlgArcFileFind,
    dlgFoundFileFind,
    dlgFoundArcFileFind,
    dlgEditorReplace,
    dlgEditorFind,
    dlgEditorFormat,
    dlgGotoLine,
    dlgHighlightGroups,
    dlgColors,
    dlgWkzMenuBar,
    dlgSetCellFormat,
    dlgGotoCellNumber,
    dlgFindCell,
    dlgReplaceCell,
    dlgDbFind,
    dlgDBFGoto, {John_SW}
    dlgFixerMenu,
    dlgGameSetup,
    dlgPrinterSetup,
    dlgEditDirectory,
    dlgAppendDirectory,
    dlgEditNumber,
    dlgAppendNumber,
    dlgMainMenu,
    dlgStatusLine,
    dlgEditorDefaults,
    dlgSystemSetup,
    dlgSetupModem,
    dlgSetupTerminal,
    dlgSaversSetup,
    dlgInterfaceSetup,
    dlgMouseSetup,
    dlgStartupSetup,
    dlgArchiveFiles,
    dlgReenterPassword,
    dlgAdvanceSearch,
    dlgCountrySetup,
    dlgConfirmations,
    dlgEditorCommands,
    dlgEditorMenu,
    dlgGotoAddress,
    dlgPhoneBook,
    dlgManualDial,
    dlgCalculator,
    dlgCalcFormat,
    dlgWindowManager,
    dlgVolumeLabel,
    dlgEditEnvironment,
    dlgTetrisWinner,
    dlgSplitFile,
    dlgCombineFile,
    dlgTetrisTop10,
    dlgPentixTop10,
    dlgAdvancedFilter,
    dlgUUDecode, {UUDECODE}
    dlgUUEncode, {UUECNODE}
    dlgOverwriteQuery,
    dlgAppendQuery, {AK155}
    dlgQueryAbort, {AK155}
    dlgViewerFind,
    dlgArvidFileFind,
    dlgArvidFindResults,
    dlgDirectoryHistory,
    dlgEditHistory,
    dlgViewHistory,
    dlgAdvancedCOMSetup,
    dlgCpuFlagInfo,
    dlgFTNInfo,
    dlgSrchFailed,
    {DataCompBoy}
    dlgSetupColumnsDisk,
    dlgSetupColumnsFind,
    dlgSetupColumnsTemp,
    dlgSetupColumnsArch,
    dlgSetupColumnsArvd,
    dlgNextSection,
    dlgNameCase,
    dlgChScreenMode, {JO}
    dlgSetExportCsvFormat, {KV}
    dlgSkipBadFile
    );

implementation

function Bit(N: Word): Word;
  begin
  Result := Word(1) shl N;
  end;

end.

