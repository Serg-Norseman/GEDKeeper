unit GKPersonNew;

{$I GEDKeeper.inc}

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, Buttons,
  GedCom551, GKCommon;

type
  TfmPersonNew = class(TForm)
    Label1: TLabel;
    EditFamily: TEdit;
    Label2: TLabel;
    EditName: TEdit;
    Label3: TLabel;
    EditPatronymic: TComboBox;
    Label4: TLabel;
    EditSex: TComboBox;
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure EditFamilyKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FTarget: TGEDCOMIndividualRecord;
    FTargetMode: TTargetMode;
    procedure SetTarget(const Value: TGEDCOMIndividualRecord);
  public
    property Target: TGEDCOMIndividualRecord read FTarget write SetTarget;
    property TargetMode: TTargetMode read FTargetMode write FTargetMode;
  end;

implementation

uses GKMain, uVista;

{$R *.dfm}

procedure TfmPersonNew.EditFamilyKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  ss, st: string;
begin
  if (Key = VK_DOWN) and (ssCtrl in Shift) then begin
    ss := TEdit(Sender).Text;
    st := AnsiLowerCase(ss);
    st[1] := ss[1];
    TEdit(Sender).Text := st;
  end;
end;

procedure TfmPersonNew.FormCreate(Sender: TObject);
var
  sx: TGEDCOMSex;
begin
  if IsWindowsVista() then SetVistaFonts(Self);

  for sx := Low(TGEDCOMSex) to High(TGEDCOMSex) do EditSex.Items.Add(Sex[sx]);
end;

procedure TfmPersonNew.SetTarget(const Value: TGEDCOMIndividualRecord);
var
  iFamily, iName, iPatronymic: string;
  names: TNamesTable;
  sx: TGEDCOMSex;
begin
  FTarget := Value;

  names := fmGEDKeeper.NamesTable;

  if (FTarget <> nil) then begin
    GetNameParts(FTarget, iFamily, iName, iPatronymic);

    EditFamily.Text := iFamily;

    case FTargetMode of
      tmNone: ;

      tmAncestor: begin
        EditPatronymic.Items.Add(names.GetPatronymicByName(iName, svMale));
        EditPatronymic.Items.Add(names.GetPatronymicByName(iName, svFemale));
      end;

      tmDescendant: begin
        sx := TGEDCOMSex(EditSex.ItemIndex);

        case sx of
          svMale: begin
            EditName.Text := names.GetNameByPatronymic(iPatronymic, svMale);
          end;

          svFemale: begin
            EditFamily.Text := '(' + EditFamily.Text + ')';
          end;
        end;
      end;
    end;
  end;
end;

end.
