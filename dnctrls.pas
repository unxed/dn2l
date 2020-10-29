unit dnctrls;

// by unxed
// taken from DN TV as it is the code by DN crew

// some controls not defined in FV

interface

uses dialogs;

type
  PLongInputline = ^TLongInputLine;
  {`2 Аналог TInputLine, но GetData и SetData работают с AnsiString.
    Внимание! Load и Store, как и раньше, работают с 1-байтной длиной!.
      TLongInputLine не должен иметь истории, так как это, во-первых,
    приведёт к ошибке в SetData при извлечени из истории, и, во-вторых,
    не имеет смысла, так как история работает с короткими строками, то
    tcnm полноценной истории для TLongInputLine быть не может, пока
    история не будет переведена на AnsiString. }
  TLongInputLine = object(TInputLine)
    procedure GetData(var Rec); virtual;
    procedure SetData(var Rec); virtual;
    end;
    {`}

implementation

procedure TLongInputLine.GetData(var Rec);
  begin
  (*
  // fixme: commented by unxed
  if (Data = '') or (Validator = nil) or
      (Validator^.Transfer(Data, @Rec, vtGetData) = 0)
  then
    AnsiString(Rec) := Copy(Data, 1, MaxLen);
    *)
  end;

procedure TLongInputLine.SetData(var Rec);
  begin
  (*
  // fixme: commented by unxed
  if  (Validator = nil) or
      (Validator^.Transfer(Data, @Rec, vtSetData) = 0)
  then
    Data := AnsiString(Rec);
  Message(Owner, evBroadcast, cmUpdateHexViews, @Self);
  SelectAll(True);
  *)
  end;


end.
