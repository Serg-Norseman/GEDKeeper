unit GKAssociationEdit;

{$I GEDKeeper.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, GedCom551;

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
  public
    property Association: TGEDCOMAssociation read FAssociation write SetAssociation;
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
  FTempInd := SelectPerson(nil, tmNone, svNone);
  EditPerson.Text := GetNameStr(FTempInd);
end;

procedure TfmAssociationEdit.btnAcceptClick(Sender: TObject);
begin
  FAssociation.Relation := EditRelation.Text;
  FAssociation.Individual := FTempInd;
end;

end.
