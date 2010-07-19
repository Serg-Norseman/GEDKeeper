unit GKProgress;

{$I GEDKeeper.inc}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, ComCtrls, StdCtrls;

type
  TfmProgress = class(TForm)
    ProgressBar1: TProgressBar;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label4: TLabel;
    Label9: TLabel;
  private
  public
  end;

procedure ProgressInit(aMax: Integer; aTitle: string);
procedure ProgressDone();
procedure ProgressStep();

implementation

{$R *.dfm}

var
  fmProgress: TfmProgress;
  StartTime: TDateTime;

procedure ProgressInit(aMax: Integer; aTitle: string);
begin
  ProgressDone();

  fmProgress := TfmProgress.Create(Application);
  fmProgress.ProgressBar1.Min := 0;
  fmProgress.ProgressBar1.Max := aMax;
  fmProgress.Label1.Caption := aTitle;
  fmProgress.Show;

  Application.ProcessMessages;

  StartTime := Now();
end;

procedure ProgressDone();
begin
  if (fmProgress <> nil) then begin
    fmProgress.Destroy;
    fmProgress := nil;
  end;
end;

procedure ProgressStep();
var
  PassTime, RestTime: TDateTime;
  count, pos: Integer;
begin
  if (fmProgress <> nil) then begin
    PassTime := Now() - StartTime;
    fmProgress.Label7.Caption := TimeToStr(PassTime);

    count := fmProgress.ProgressBar1.Max;
    pos := fmProgress.ProgressBar1.Position;

    if (pos = 0) then pos := 1;

    RestTime := (PassTime / pos) * (count - pos);
    fmProgress.Label8.Caption := TimeToStr(RestTime);

    fmProgress.Label9.Caption := TimeToStr(PassTime + RestTime);

    fmProgress.ProgressBar1.StepBy(1);
    fmProgress.Update();
  end;
end;

end.
