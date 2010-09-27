unit GKCommunicationEdit;

{$I GEDKeeper.inc}

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, Buttons, ComCtrls,
  ExtCtrls, GedCom551, GKBase, GKCommon, Mask, GKSheetList, bsCtrls;

type
  TfmCommunicationEdit = class(TForm)
    GroupBox1: TGroupBox;
    PagesGroupData: TPageControl;
    SheetNotes: TTabSheet;
    SheetMultimedia: TTabSheet;
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    Label1: TLabel;
    EditName: TEdit;
    Label4: TLabel;
    EditDate: TMaskEdit;
    Label2: TLabel;
    EditCorrType: TComboBox;
    EditDir: TComboBox;
    Label5: TLabel;
    EditCorresponder: TEdit;
    btnPersonAdd: TSpeedButton;
    procedure btnAcceptClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnPersonAddClick(Sender: TObject);
  private
    FCommunication: TGEDCOMCommunicationRecord;
    FTempInd: TGEDCOMIndividualRecord;

    FNotesList: TSheetList;
    FMediaList: TSheetList;

    function GetBase: TfmBase;
    procedure ListModify(Sender: TObject; ItemData: TObject; Action: TRecAction);
    procedure ListsRefresh();
    procedure SetCommunication(const Value: TGEDCOMCommunicationRecord);
  public
    property Base: TfmBase read GetBase;
    property Communication: TGEDCOMCommunicationRecord read FCommunication write SetCommunication;
  end;

implementation

uses
  bsComUtils, GKMain, GKRecordSelect, GKPersonEdit;

{$R *.dfm}

{ TfmCommunicationEdit }

procedure TfmCommunicationEdit.FormCreate(Sender: TObject);
var
  ct: TCommunicationType;
begin
  for ct := Low(TCommunicationType) to High(TCommunicationType) do
    EditCorrType.Items.Add(CommunicationNames[ct]);

  FNotesList := TSheetList.Create(SheetNotes, lmBox);
  FNotesList.OnModify := ListModify;
  Base.SetupRecNotesList(FNotesList);

  FMediaList := TSheetList.Create(SheetMultimedia);
  FMediaList.OnModify := ListModify;
  Base.SetupRecMediaList(FMediaList);

  FTempInd := nil;
end;

procedure TfmCommunicationEdit.ListsRefresh();
begin
  Base.RecListNotesRefresh(FCommunication, FNotesList.List, nil);
  Base.RecListMediaRefresh(FCommunication, TBSListView(FMediaList.List), nil);
end;

procedure TfmCommunicationEdit.SetCommunication(const Value: TGEDCOMCommunicationRecord);
var
  dir: TCommunicationDir;
begin
  FCommunication := Value;

  try
    if (FCommunication = nil) then begin
      EditName.Text := '';
      EditCorrType.ItemIndex := -1;
      EditDate.Text := '';
      EditDir.ItemIndex := 0;
      EditCorresponder.Text := '';
    end else begin
      EditName.Text := FCommunication.Name;
      EditCorrType.ItemIndex := Ord(FCommunication.CommunicationType);
      EditDate.Text := GEDCOMDateToStr(FCommunication.Date);

      FCommunication.GetCorresponder(dir, FTempInd);

      if (FTempInd <> nil) then begin
        EditDir.ItemIndex := Ord(dir);
        EditCorresponder.Text := GetNameStr(FTempInd);
      end else begin
        EditDir.ItemIndex := 0;
        EditCorresponder.Text := '';
      end;
    end;

    ListsRefresh();
  except
    on E: Exception do LogWrite('CommunicationEdit.SetCommunication(): ' + E.Message);
  end;
end;

procedure TfmCommunicationEdit.btnAcceptClick(Sender: TObject);
begin
  FCommunication.Name := EditName.Text;
  FCommunication.CommunicationType := TCommunicationType(EditCorrType.ItemIndex);
  FCommunication.Date.ParseString(StrToGEDCOMDate(EditDate.Text));
  FCommunication.SetCorresponder(TCommunicationDir(EditDir.ItemIndex), FTempInd);

  Base.ChangeRecord(FCommunication);
end;

function TfmCommunicationEdit.GetBase: TfmBase;
begin
  Result := TfmBase(Owner);
end;

procedure TfmCommunicationEdit.ListModify(Sender: TObject; ItemData: TObject; Action: TRecAction);
begin
  if (Sender = FNotesList) then begin
    if Base.ModifyRecNote(FCommunication, TGEDCOMNotes(ItemData), Action)
    then ListsRefresh();
  end
  else
  if (Sender = FMediaList) then begin
    if Base.ModifyRecMultimedia(FCommunication, TGEDCOMMultimediaLink(ItemData), Action)
    then ListsRefresh();
  end
end;

procedure TfmCommunicationEdit.btnPersonAddClick(Sender: TObject);
begin
  FTempInd := Base.SelectPerson(nil, tmNone, svNone);
  EditCorresponder.Text := GetNameStr(FTempInd);
end;

end.
