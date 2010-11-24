unit GKSexCheck;

{$I GEDKeeper.inc}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, ComCtrls, StdCtrls,
  GedCom551, Buttons, GKTreeTools, GKCommon;

type
  TfmSexCheck = class(TForm)
    edName: TEdit;
    GroupBox1: TGroupBox;
    sbNone: TSpeedButton;
    sbMale: TSpeedButton;
    sbFemale: TSpeedButton;
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
  private
  public
  end;

procedure scStep(iRec: TGEDCOMIndividualRecord; aNamesTable: TNamesTable);

implementation

uses GKMain;

{$R *.dfm}

var
  fmSexCheck: TfmSexCheck;

procedure scStep(iRec: TGEDCOMIndividualRecord; aNamesTable: TNamesTable);
var
  sx: TGEDCOMSex;
  f_name, f_pat, f_fam: string;
begin
  GetNameParts(iRec, f_fam, f_name, f_pat);

  sx := aNamesTable.GetSexByName(f_name);
  if (sx <> svNone) then begin
    iRec.Sex := sx;
    Exit;
  end;

  fmSexCheck := TfmSexCheck.Create(Application);
  try
    if (fmSexCheck <> nil) then begin
      fmSexCheck.edName.Text := GetNameStr(iRec);

      sx := GetSex(f_name, f_pat, False);
      if (sx = svNone) then sx := iRec.Sex;

      case sx of
        svNone: fmSexCheck.sbNone.Down := True;
        svMale: fmSexCheck.sbMale.Down := True;
        svFemale: fmSexCheck.sbFemale.Down := True;
        //svUndetermined: fmSexCheck.sbUndetermined.Down := True;
      end;

      if (ShowModalEx(fmSexCheck) = mrOk) then begin
        if (fmSexCheck.sbNone.Down) then sx := svNone
        else
        if (fmSexCheck.sbMale.Down) then sx := svMale
        else
        if (fmSexCheck.sbFemale.Down) then sx := svFemale{
        else
        if (fmSexCheck.sbUndetermined.Down) then sx := svUndetermined};

        iRec.Sex := sx;
        aNamesTable.SetNameSex(f_name, sx);
      end;
    end;
  finally
    fmSexCheck.Destroy;
    fmSexCheck := nil;
  end;
end;

end.
