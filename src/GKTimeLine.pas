unit GKTimeLine; {prepare:fin; trans:fin}

{$I GEDKeeper.inc}

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, ComCtrls,
  GedCom551, GKBase, GKLists, GKLangs;

type
  TfmTimeLine = class(TForm, ILocalization)
    tbTimeLine: TTrackBar;
    StatusBar1: TStatusBar;
    procedure FormShow(Sender: TObject);
    procedure tbTimeLineChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FBase: TfmBase;

    procedure StatusUpdate();
    function GetIListMan(): TIndividualListMan;
  public
    procedure CheckTimeWin(aBase: TfmBase);

    procedure SetLang();
  end;

var
  fmTimeLine: TfmTimeLine;

implementation

{$R *.dfm}

uses
  GKMain;

{ TfmTimeLine }

procedure TfmTimeLine.FormCreate(Sender: TObject);
begin
  SetLang();
end;

procedure TfmTimeLine.SetLang();
begin
  Caption := LSList[LSID_MITimeLine];
end;

procedure TfmTimeLine.CheckTimeWin(aBase: TfmBase);
begin
  FBase := aBase;
  if (FBase = nil) then Exit;

  try
    tbTimeLine.OnChange := nil;
    tbTimeLine.Max := GetIListMan().YearMax + 1;
    tbTimeLine.Min := GetIListMan().YearMin - 1;
    tbTimeLine.Position := FBase.TimeLine_GetYear();
    tbTimeLine.OnChange := tbTimeLineChange;

    StatusUpdate();

    FBase.TimeLine_Init();
  finally
  end;
end;

procedure TfmTimeLine.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i: Integer;
begin
  for i := 0 to fmGEDKeeper.MDIChildCount - 1 do
    if (fmGEDKeeper.MDIChildren[i] is TfmBase)
    then TfmBase(fmGEDKeeper.MDIChildren[i]).TimeLine_Done();

  fmGEDKeeper.miTimeLine.Checked := False;
  fmTimeLine := nil;
  Action := caFree;
end;

procedure TfmTimeLine.FormShow(Sender: TObject);
begin
  CheckTimeWin(fmGEDKeeper.GetCurrentFile());
end;

function TfmTimeLine.GetIListMan(): TIndividualListMan;
begin
  Result := TIndividualListMan(FBase.ListPersons.ListMan);
end;

procedure TfmTimeLine.StatusUpdate();
begin
  if Assigned(FBase) then begin
    StatusBar1.Panels[0].Text := LSList[LSID_TimeScale] + ': ' + IntToStr(GetIListMan().YearMin) + ' - ' + IntToStr(GetIListMan().YearMax);
    StatusBar1.Panels[1].Text := LSList[LSID_CurrentYear] + ': ' + IntToStr(FBase.TimeLine_GetYear());
  end;
end;

procedure TfmTimeLine.tbTimeLineChange(Sender: TObject);
begin
  if Assigned(FBase) then FBase.TimeLine_SetYear(tbTimeLine.Position);
  StatusUpdate();
end;
    
end.
