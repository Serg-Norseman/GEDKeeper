unit GKSourceCitEdit;

{$I GEDKeeper.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, GedCom551, GKBase;

type
  TfmSourceCitEdit = class(TForm)
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    Label1: TLabel;
    EditPage: TEdit;
    Label2: TLabel;
    EditSource: TEdit;
    btnSourceAdd: TSpeedButton;
    procedure btnSourceAddClick(Sender: TObject);
    procedure btnAcceptClick(Sender: TObject);
  private
    FSourceCitation: TGEDCOMSourceCitation;
    FTempSrc: TGEDCOMSourceRecord;
    procedure SetSourceCitation(const Value: TGEDCOMSourceCitation);
    function GetBase: TfmBase;
  public
    property Base: TfmBase read GetBase;
    property SourceCitation: TGEDCOMSourceCitation read FSourceCitation write SetSourceCitation;
  end;

implementation

uses GKCommon, GKRecordSelect;

{$R *.dfm}

{ TfmSourceCitEdit }

procedure TfmSourceCitEdit.SetSourceCitation(const Value: TGEDCOMSourceCitation);
begin
  FSourceCitation := Value;

  FTempSrc := TGEDCOMSourceRecord(FSourceCitation.Value);
  if (FTempSrc <> nil)
  then EditSource.Text := FTempSrc.FiledByEntry;

  EditPage.Text := FSourceCitation.Page;
end;

procedure TfmSourceCitEdit.btnSourceAddClick(Sender: TObject);
begin
  FTempSrc := TGEDCOMSourceRecord(Base.SelectRecord(smSource));

  if (FTempSrc <> nil)
  then EditSource.Text := FTempSrc.FiledByEntry;
end;

procedure TfmSourceCitEdit.btnAcceptClick(Sender: TObject);
begin
  FSourceCitation.Value := FTempSrc;
  FSourceCitation.Page := EditPage.Text;
end;

function TfmSourceCitEdit.GetBase: TfmBase;
begin
  Result := TfmBase(Owner);
end;

end.
