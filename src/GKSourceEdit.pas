unit GKSourceEdit;

{$I GEDKeeper.inc}

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, Buttons, ComCtrls,
  GedCom551, GKBase, GKCommon;

type
  TfmSourceEdit = class(TForm)
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    PagesData: TPageControl;
    SheetNotes: TTabSheet;
    SheetMultimedia: TTabSheet;
    SheetRepositories: TTabSheet;
    SheetText: TTabSheet;
    EditText: TMemo;
    SheetCommon: TTabSheet;
    Label1: TLabel;
    EditShortTitle: TEdit;
    Label3: TLabel;
    EditAuthor: TMemo;
    Label2: TLabel;
    EditTitle: TMemo;
    Label4: TLabel;
    EditPublication: TMemo;
    procedure btnAcceptClick(Sender: TObject);
    procedure EditShortTitleChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FSourceRecord: TGEDCOMSourceRecord;

    FNotesList: TSheetList;
    FMediaList: TSheetList;
    FRepositoriesList: TSheetList;

    procedure ControlsRefresh();
    function GetBase: TfmBase;
    procedure ListModify(Sender: TObject; Index: Integer; Action: TRecAction);
    procedure SetSourceRecord(const Value: TGEDCOMSourceRecord);
  public
    property Base: TfmBase read GetBase;
    property SourceRecord: TGEDCOMSourceRecord read FSourceRecord write SetSourceRecord;
  end;

implementation

uses GKMain, GKRecordSelect;

{$R *.dfm}

procedure TfmSourceEdit.FormCreate(Sender: TObject);
begin
  FNotesList := TSheetList.Create(SheetNotes);
  FNotesList.OnModify := ListModify;
  Base.SetupRecNotesList(FNotesList.List);

  FMediaList := TSheetList.Create(SheetMultimedia);
  FMediaList.OnModify := ListModify;
  Base.SetupRecMediaList(FMediaList.List);

  FRepositoriesList := TSheetList.Create(SheetRepositories);
  FRepositoriesList.OnModify := ListModify;
  Base.SetupRecRepositoriesList(FRepositoriesList.List);
end;

procedure TfmSourceEdit.ControlsRefresh();
var
  k: Integer;
  rep: TGEDCOMRepositoryRecord;
begin
  Base.RecListNotesRefresh(FSourceRecord, FNotesList.List, nil);
  Base.RecListMediaRefresh(FSourceRecord, FMediaList.List, nil);

  FRepositoriesList.List.Items.BeginUpdate();
  FRepositoriesList.List.Clear();
  for k := 0 to FSourceRecord.RepositoryCitationsCount - 1 do begin
    rep := TGEDCOMRepositoryRecord(FSourceRecord.RepositoryCitations[k].Value);
    FRepositoriesList.List.AddItem(rep.RepositoryName, TObject(k));
  end;
  FRepositoriesList.List.Items.EndUpdate();
end;

procedure TfmSourceEdit.SetSourceRecord(const Value: TGEDCOMSourceRecord);
begin
  FSourceRecord := Value;

  EditShortTitle.Text := FSourceRecord.FiledByEntry;
  EditAuthor.Text := Trim(FSourceRecord.Originator.Text);
  EditTitle.Text := Trim(FSourceRecord.Title.Text);
  EditPublication.Text := Trim(FSourceRecord.Publication.Text);
  EditText.Text := Trim(FSourceRecord.Text.Text);

  ControlsRefresh();
end;

procedure TfmSourceEdit.btnAcceptClick(Sender: TObject);
begin
  FSourceRecord.FiledByEntry := EditShortTitle.Text;

  FSourceRecord.Originator.Clear;
  FSourceRecord.Originator := EditAuthor.Lines;

  FSourceRecord.Title.Clear;
  FSourceRecord.Title := EditTitle.Lines;

  FSourceRecord.Publication.Clear;
  FSourceRecord.Publication := EditPublication.Lines;

  FSourceRecord.Text.Clear;
  FSourceRecord.Text := EditText.Lines;

  FSourceRecord.ChangeDate.ChangeDateTime := Now();

  Base.Modified := True;
end;

procedure TfmSourceEdit.ListModify(Sender: TObject; Index: Integer; Action: TRecAction);
var
  rep: TGEDCOMRepositoryRecord;
  cit: TGEDCOMRepositoryCitation;
begin
  if (Sender = FNotesList) then begin
    if Base.ModifyRecNote(FSourceRecord, Index, Action)
    then ControlsRefresh();
  end
  else
  if (Sender = FMediaList) then begin
    if Base.ModifyRecMultimedia(FSourceRecord, Index, Action)
    then ControlsRefresh();
  end
  else
  if (Sender = FRepositoriesList) then begin
    case Action of
      raAdd: begin
        rep := TGEDCOMRepositoryRecord(Base.SelectRecord(smRepository));
        if (rep <> nil) then begin
          cit := TGEDCOMRepositoryCitation.Create(Base.Tree, FSourceRecord);
          cit.Value := rep;
          FSourceRecord.AddRepositoryCitation(cit);

          ControlsRefresh();
        end;
      end;

      raEdit: ;

      raDelete: begin
        if (Index < 0) then Exit;
        if (MessageDlg('Удалить ссылку на архив?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
        then Exit;

        cit := FSourceRecord.RepositoryCitations[Index];
        FSourceRecord.RemoveRepositoryCitation(cit);

        ControlsRefresh();
      end;
    end;
  end;
end;

function TfmSourceEdit.GetBase: TfmBase;
begin
  Result := TfmBase(Owner);
end;

procedure TfmSourceEdit.EditShortTitleChange(Sender: TObject);
begin
  Caption := 'Источник "'+EditShortTitle.Text+'"';
end;

end.
