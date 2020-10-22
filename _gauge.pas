unit _Gauge;
(******

DN/2 Plugin Interface - object model
Copyright (C) 2002 Aleksej Kozlov (Cat)
2:5030/1326.13

******)

{&Delphi+}
{&Use32+}

interface

uses
  _Defines, _Collect, _Views, _Dialogs
  ;

type
  PPercentGauge = ^TPercentGauge;
  TPercentGauge = object(TView)
    MaxValue: LongInt;
    CurValue: LongInt;
    constructor Init(var Bounds: TRect; AMaxValue: LongInt);
    {procedure Draw; virtual;}
    procedure UpdateView(Progress: LongInt); virtual;
    procedure AddProgress(Progress: LongInt);
    {procedure HandleEvent(var Event: TEvent); virtual;}
    function SolveForX(Y, Z: LongInt): Integer;
    function SolveForY(X, Z: LongInt): Integer;
    end;

  PBarGauge = ^TBarGauge;
  TBarGauge = object(TPercentGauge)
    {procedure Draw; virtual;}
    end;

  PWhileView = ^TWhileView;
  TWhileView = object(TGroup)
    Lines: PCollection;
    But: PButton;
    QuitNormal: Boolean;
    Top, Bottom: String;
    constructor Init(Bounds: TRect);
    procedure Write(N: Integer; S: String);
    {function GetPalette: PPalette; virtual;}
    {function Valid(C: Word): Boolean; virtual;}
    {procedure SetState(AState: Word; Enable: Boolean); virtual;}
    {procedure Draw; virtual;}
    {procedure HandleEvent(var Event: TEvent); virtual;}
    {destructor Done; virtual;}
    procedure ClearInterior;
  private
    Side: (sdLeft, sdRight);
    end;

implementation

uses
  _DNFuncs
  ;

constructor TPercentGauge.Init(var Bounds: TRect; AMaxValue: LongInt);
  begin
  _TPercentGauge^.Init(Bounds, AMaxValue, nil, @Self);
  end;

procedure TPercentGauge.UpdateView(Progress: LongInt);
  assembler; {&Frame-}
asm
end;

procedure TPercentGauge.AddProgress(Progress: LongInt);
  begin
  _TPercentGauge^.AddProgress(Progress, @Self);
  end;

function TPercentGauge.SolveForX(Y, Z: LongInt): Integer;
  begin
  Result := _TPercentGauge^.SolveForX(Y, Z, @Self);
  end;

function TPercentGauge.SolveForY(X, Z: LongInt): Integer;
  begin
  Result := _TPercentGauge^.SolveForY(X, Z, @Self);
  end;

constructor TWhileView.Init(Bounds: TRect);
  begin
  _TWhileView^.Init(Bounds, nil, @Self);
  end;

procedure TWhileView.Write(N: Integer; S: String);
  begin
  _TWhileView^.Write(N, S, @Self);
  end;

procedure TWhileView.ClearInterior;
  begin
  _TWhileView^.ClearInterior(@Self);
  end;

end.
