unit GKProgress; {prepare:fin}

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
    StartTime: TDateTime;
    procedure Step();
  public
  end;

procedure ProgressInit(aMax: Integer; aTitle: string);
procedure ProgressDone();
procedure ProgressStep();

implementation

{$R *.dfm}

var
  fmProgress: TfmProgress;

procedure ProgressInit(aMax: Integer; aTitle: string);
begin
  ProgressDone();

  fmProgress := TfmProgress.Create(Application);
  fmProgress.ProgressBar1.Min := 0;
  fmProgress.ProgressBar1.Max := aMax;
  fmProgress.Label1.Caption := aTitle;
  fmProgress.Show;
  fmProgress.StartTime := Now();

  Application.ProcessMessages;
end;

procedure ProgressDone();
begin
  if (fmProgress <> nil) then begin
    fmProgress.Destroy;
    fmProgress := nil;
  end;
end;

procedure ProgressStep();
begin
  if (fmProgress <> nil)
  then fmProgress.Step();
end;

{ TfmProgress }

procedure TfmProgress.Step();
var
  PassTime, RestTime: TDateTime;
  count, pos: Integer;
begin
  PassTime := Now() - StartTime;
  Label7.Caption := TimeToStr(PassTime);

  count := ProgressBar1.Max;
  pos := ProgressBar1.Position;

  if (pos = 0) then pos := 1;

  RestTime := (PassTime / pos) * (count - pos);
  Label8.Caption := TimeToStr(RestTime);
  Label9.Caption := TimeToStr(PassTime + RestTime);

  ProgressBar1.StepBy(1);
  Update();
end;

end.
