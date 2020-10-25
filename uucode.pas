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
{AK155 = Alexey Korop, 2:461/155@fidonet}
{Cat = Aleksej Kozlov, 2:5030/1326.13@fidonet}

{AK155
   Переписал все ассемблерные и inline подпрограммы под
   32-разрядный VP. При этом никаких условных компиляций не делал.
   Мутную и неэффективную процедуру MoveSection переписал короче и проще.

   По состоянию на 08/03/2001 компилируется только декодирование.

   04/06/2001 - подключил кодирование с новым модулем uue2inc.pas (см.).
   По-прежнему все ассемблерные вставки чисто 32-разрядные.
}
{Cat
   23/08/2001 - переделал очень много чего, основное - заменил в операциях
   с дисковыми буферами, да и во многих других местах Word-ы на Longint-ы,
   что позволяет избавиться от ограничения в 900 строк в секции и наверняка
   ещё кучи глюков, которые выискивать не хочется - проще исправить  ;-)
}
unit UUCode;

interface

uses
  Defines
  {$IFDEF UUDECODE}
  , Collect
  {$ENDIF}
  {$IFDEF UUENCODE}
//  , Uue2Inc
// temporary disabled by unxed
  {$ENDIF}
  ;
{$IFDEF UUDECODE}
procedure UUDecode(AFileCollection: PCollection);
{$ENDIF}

{$IFDEF UUENCODE}
procedure UUEncode(const FName: String);
{$ENDIF}

implementation

procedure UUDecode(AFileCollection: PCollection);
begin
end;

procedure UUEncode(const FName: String);
begin
end;

end.