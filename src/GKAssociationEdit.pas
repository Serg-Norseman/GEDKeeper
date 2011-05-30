unit GKAssociationEdit; {prepare:fin; trans:fin}

{$I GEDKeeper.inc}

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, Buttons,
  GedCom551, GKBase, GKLangs;

type
  TfmAssociationEdit = class(TForm, ILocalization)
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    Label1: TLabel;
    EditRelation: TComboBox;
    Label2: TLabel;
    EditPerson: TEdit;
    btnPersonAdd: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure btnPersonAddClick(Sender: TObject);
    procedure btnAcceptClick(Sender: TObject);
  private
    FAssociation: TGEDCOMAssociation;
    FTempInd: TGEDCOMIndividualRecord;
    procedure SetAssociation(const Value: TGEDCOMAssociation);
    function GetBase: TfmBase;
  public
    property Association: TGEDCOMAssociation read FAssociation write SetAssociation;
    property Base: TfmBase read GetBase;

    procedure SetLang();
  end;

implementation

uses GKEngine, GKRecordSelect, GKMain;

{$R *.dfm}

{ TfmAssociation }

procedure TfmAssociationEdit.FormCreate(Sender: TObject);
begin
  EditRelation.Items.Assign(fmGEDKeeper.Options.Relations);

  SetLang();
end;

procedure TfmAssociationEdit.SetAssociation(const Value: TGEDCOMAssociation);
begin
  FAssociation := Value;

  EditRelation.Text := FAssociation.Relation;
  EditPerson.Text := GetNameStr(FAssociation.Individual);
end;

procedure TfmAssociationEdit.SetLang();
begin
  btnAccept.Caption := LSList[LSID_DlgAccept];
  btnCancel.Caption := LSList[LSID_DlgCancel];

  Caption := LSList[LSID_Association];
  Label1.Caption := LSList[LSID_Relation];
  Label2.Caption := LSList[LSID_Person];
end;

procedure TfmAssociationEdit.btnPersonAddClick(Sender: TObject);
begin
  FTempInd := Base.SelectPerson(nil, tmNone, svNone);
  EditPerson.Text := GetNameStr(FTempInd);
end;

procedure TfmAssociationEdit.btnAcceptClick(Sender: TObject);
var
  rel: string;
begin
  rel := Trim(EditRelation.Text);
  if (rel <> '') then begin
    if (fmGEDKeeper.Options.Relations.IndexOf(rel) < 0)
    then fmGEDKeeper.Options.Relations.Add(rel);
  end;

  FAssociation.Relation := EditRelation.Text;
  FAssociation.Individual := FTempInd;
end;

function TfmAssociationEdit.GetBase: TfmBase;
begin
  Result := TfmBase(Owner);
end;

end.
