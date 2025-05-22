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

uses
  Dos, VPUtils, Defines
  ;

//===============================

function D2(N: Integer): string;
  begin
  Str(100+N, Result); Delete(Result, 1, 1);
  end;

function GetCurTime: String;
  var
    H, Mn, SS, S100: Word;
  begin
  GetTime(H, Mn, SS, S100);
  Result := D2(H) + ':' + D2(Mn) + ':' + D2(SS);
  end;

//===============================

const
  VersionName: String = '2.14 beta';
  VersionDate: String = '';
  VersionWord: AWord = 15198;

var
  F: Text;
  Day, Month, Year, dow: Word;

const
  DOWs: array[0..6] of String[3] =
    ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
  Months: array[1..12] of String[3] =
    ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep',
    'Oct', 'Nov', 'Dec');
  Days: array[1..31] of String[2] =
    ('01', '02', '03', '04', '05', '06', '07', '08', '09', '10',
    '11', '12', '13', '14', '15', '16', '17', '18', '19', '20',
    '21', '22', '23', '24', '25', '26', '27', '28', '29', '30', '31');

begin
if  (ParamCount = 0) or (ParamCount > 3) then
  Halt(1);
if ParamStr(2) = '' then
  VersionName := VersionName+'/'+
    {$IFDEF OS2}
    +'OS2'
    {$ELSE}
    {$IFDEF WIN32}
    +'Win32'
    {$ELSE}
    {$IFDEF DPMI32}
    +'DPMI32'
    {$ELSE}
    +'32bit'
    {$ENDIF}
    {$ENDIF}
    {$ENDIF}
else
  VersionName := VersionName+'/'+ParamStr(2);
{Cat: компиляция плагинской версии:
        - либо если задан параметр PLUGIN (компиляция посредством _DNC.CMD)
        - либо если в STDEFINE.INC установлена директива PLUGIN (компиляция вручную)}
{$IFNDEF PLUGIN}
if ParamStr(ParamCount) = 'PLUGIN' then
  {$ENDIF}
  VersionName := VersionName+'/Plugin';

Writeln(' '#254' Creating ', ParamStr(1)+' '+VersionName);

GetDate(Year, Month, Day, dow);
Str(Year, VersionDate);
VersionDate := DOWs[dow]+', '+Days[Day]+' '+Months[Month]+' '+
  VersionDate+' at '+GetCurTime;
Assign(F, ParamStr(1));
Rewrite(F);
Writeln(F,
  '{/////////////////////////////////////////////////////////////////////////'
  );
Writeln(F, '//');
Writeln(F, '//  Dos Navigator Open Source '+VersionName);
Writeln(F,
  '//  Based on Dos Navigator (C) 1991-99 RIT Research Labs');
Writeln(F, '//');
Writeln(F,
  '//  This programs is free for commercial and non-commercial use as long as'
  );
Writeln(F, '//  the following conditions are aheared to.');
Writeln(F, '//');
Writeln(F,
  '//  Copyright remains RIT Research Labs, and as such any Copyright notices'
  );
Writeln(F,
  '//  in the code are not to be removed. If this package is used in a'
  );
Writeln(F,
  '//  product, RIT Research Labs should be given attribution as the RIT Research'
  );
Writeln(F,
  '//  Labs of the parts of the library used. This can be in the form of a textual'
  );
Writeln(F,
  '//  message at program startup or in documentation (online or textual)'
  );
Writeln(F, '//  provided with the package.');
Writeln(F, '//');
Writeln(F,
  '//  Redistribution and use in source and binary forms, with or without'
  );
Writeln(F,
  '//  modification, are permitted provided that the following conditions are'
  );
Writeln(F, '//  met:');
Writeln(F, '//');
Writeln(F,
  '//  1. Redistributions of source code must retain the copyright');
Writeln(F,
  '//     notice, this list of conditions and the following disclaimer.'
  );
Writeln(F,
  '//  2. Redistributions in binary form must reproduce the above copyright'
  );
Writeln(F,
  '//     notice, this list of conditions and the following disclaimer in the'
  );
Writeln(F,
  '//     documentation and/or other materials provided with the distribution.'
  );
Writeln(F,
  '//  3. All advertising materials mentioning features or use of this software'
  );
Writeln(F, '//     must display the following acknowledgement:');
Writeln(F,
  '//     "Based on Dos Navigator by RIT Research Labs.');
Writeln(F, '//');
Writeln(F,
  '//  THIS SOFTWARE IS PROVIDED BY RIT RESEARCH LABS "AS IS" AND ANY EXPRESS'
  );
Writeln(F,
  '//  OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED'
  );
Writeln(F,
  '//  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE'
  );
Writeln(F,
  '//  DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE FOR'
  );
Writeln(F,
  '//  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL'
  );
Writeln(F,
  '//  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE'
  );
Writeln(F,
  '//  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS')
;
Writeln(F,
  '//  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER'
  );
Writeln(F,
  '//  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR'
  );
Writeln(F,
  '//  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF'
  );
Writeln(F, '//  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.');
Writeln(F, '//');
Writeln(F,
  '//  The licence and distribution terms for any publically available'
  );
Writeln(F,
  '//  version or derivative of this code cannot be changed. i.e. this code'
  );
Writeln(F,
  '//  cannot simply be copied and put under another distribution licence'
  );
Writeln(F, '//  (including the GNU Public Licence).');
Writeln(F, '//');
Writeln(F,
  '//////////////////////////////////////////////////////////////////////////}'
  );
Writeln(F, '  Const');
Writeln(F, '     VersionName =''', VersionName, ''';');
Writeln(F, '     VersionDate =''', VersionDate, ''';');
Writeln(F, '     VersionWord:AWord = $', Int2Hex(VersionWord, 4), ';');

Close(F);
end.
