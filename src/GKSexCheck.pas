unit GKSexCheck; {prepare:fin; trans:fin}

{$I GEDKeeper.inc}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, ComCtrls, StdCtrls,
  GedCom551, Buttons, GKTreeTools, GKEngine, GKCommon, GKLangs;

type
  TfmSexCheck = class(TForm, ILocalization)
    edName: TEdit;
    GroupBox1: TGroupBox;
    sbNone: TSpeedButton;
    sbMale: TSpeedButton;
    sbFemale: TSpeedButton;
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    procedure FormCreate(Sender: TObject);
  private
  public
    procedure SetLang();
  end;

function DefineSex(iName, iPatr: string; aNamesTable: TNamesTable): TGEDCOMSex;
procedure CheckPersonSex(iRec: TGEDCOMIndividualRecord; aNamesTable: TNamesTable);

implementation

uses GKMain;

{$R *.dfm}

function DefineSex(iName, iPatr: string; aNamesTable: TNamesTable): TGEDCOMSex;
var
  dlg: TfmSexCheck;
  sx: TGEDCOMSex;
begin
  sx := aNamesTable.GetSexByName(iName);
  Result := sx;
  if (sx <> svNone) then Exit;

  dlg := TfmSexCheck.Create(Application);
  try
    if (dlg <> nil) then begin
      dlg.edName.Text := iName + ' ' + iPatr;

      sx := GetSex(iName, iPatr, False);

      case sx of
        svNone, svUndetermined: dlg.sbNone.Down := True;
        svMale: dlg.sbMale.Down := True;
        svFemale: dlg.sbFemale.Down := True;
      end;

      if (ShowModalEx(dlg) = mrOk) then begin
        if (dlg.sbNone.Down) then sx := svNone
        else
        if (dlg.sbMale.Down) then sx := svMale
        else
        if (dlg.sbFemale.Down) then sx := svFemale;

        Result := sx;

        if (sx <> svNone)
        then aNamesTable.SetNameSex(iName, sx);
      end;
    end;
  finally
    dlg.Destroy;
  end;
end;

procedure CheckPersonSex(iRec: TGEDCOMIndividualRecord; aNamesTable: TNamesTable);
var
  f_name, f_patr, f_fam: string;
begin
  if (iRec.Sex in [svNone, svUndetermined]) then begin
    GetNameParts(iRec, f_fam, f_name, f_patr);
    iRec.Sex := DefineSex(f_name, f_patr, aNamesTable);
  end;
end;

{ TfmSexCheck }

procedure TfmSexCheck.FormCreate(Sender: TObject);
begin
  SetLang();
end;

procedure TfmSexCheck.SetLang();
begin
  btnAccept.Caption := LSList[LSID_DlgAccept];
  btnCancel.Caption := LSList[LSID_DlgCancel];

  Caption := LSList[LSID_WinCheckSex];

  GroupBox1.Caption := LSList[LSID_Sex];
  sbMale.Caption := LSList[LSID_SexM];
  sbFemale.Caption := LSList[LSID_SexF];
end;

end.
