unit GKAssociationEdit;

{$I GEDKeeper.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, GedCom551, GKBase;

type
  TfmAssociationEdit = class(TForm)
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    Label1: TLabel;
    EditRelation: TEdit;
    Label2: TLabel;
    EditPerson: TEdit;
    btnPersonAdd: TSpeedButton;
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
  end;

implementation

uses GKCommon, GKRecordSelect;

{$R *.dfm}

{ TfmAssociation }

procedure TfmAssociationEdit.SetAssociation(const Value: TGEDCOMAssociation);
begin
  FAssociation := Value;

  EditRelation.Text := FAssociation.Relation;
  EditPerson.Text := GetNameStr(FAssociation.Individual);
end;

procedure TfmAssociationEdit.btnPersonAddClick(Sender: TObject);
begin
  FTempInd := Base.SelectPerson(nil, tmNone, svNone);
  EditPerson.Text := GetNameStr(FTempInd);
end;

procedure TfmAssociationEdit.btnAcceptClick(Sender: TObject);
begin
  FAssociation.Relation := EditRelation.Text;
  FAssociation.Individual := FTempInd;
end;

function TfmAssociationEdit.GetBase: TfmBase;
begin
  Result := TfmBase(Owner);
end;

end.
