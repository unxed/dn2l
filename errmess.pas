{$I STDEFINE.INC} {Cat}

unit ErrMess;

interface
{AK155}
procedure MessFileNotOpen(const Path: String; RC: LongInt);
procedure MessFileNotRename(const s1, s2: String; ErrCode: LongInt);
procedure MessFileNotRead(const Path: String; RC: LongInt);

implementation

uses
  DNApp, Messages, Commands, Advance1
  ;

procedure MessFileNotOpen(const Path: String; RC: LongInt);
  begin
  MessageBox(GetString(dlFBBNoOpen)+Path+^M^C'(RC=%d)',
    @RC, mfError+mfOKButton);
  end;

procedure MessFileNotRename(const s1, s2: String; ErrCode: LongInt);
  begin
  MessageBox(GetString(dlFCNoRename1)+GetString(dlDIFile)
    +^M^C'(RC=%d)'+^M^C+Cut(s1, 60)
    +GetString(dlFCNoRename2)+Cut(s2, 60),
    @ErrCode, mfError+mfOKButton)
  end;

procedure MessFileNotRead(const Path: String; RC: LongInt);
  begin
  MessageBox(GetString(dlFBBNotReadSource)+Path+^M^C'(RC=%d)',
    @RC, mfError+mfOKButton);
  end;

end.


