unit GKProgress;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls;

type
  TfmProgress = class(TForm)
    ProgressBar1: TProgressBar;
    Label1: TLabel;
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

procedure ProgressInit(aMax: Integer; aTitle: string);
begin
  ProgressDone();

  fmProgress := TfmProgress.Create(Application);
  fmProgress.ProgressBar1.Min := 0;
  fmProgress.ProgressBar1.Max := aMax;
  fmProgress.Label1.Caption := aTitle;
  fmProgress.Show;

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
  then fmProgress.ProgressBar1.StepBy(1);
end;

end.
