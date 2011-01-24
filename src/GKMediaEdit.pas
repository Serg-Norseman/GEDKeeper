unit GKMediaEdit;

{$I GEDKeeper.inc}

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, Buttons, ComCtrls,
  GedCom551, GKBase, GKEngine, GKLists, bsCtrls;

type
  TfmMediaEdit = class(TForm)
    PagesData: TPageControl;
    SheetNotes: TTabSheet;
    SheetSources: TTabSheet;
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    OpenDialog1: TOpenDialog;
    btnView: TBitBtn;
    SheetCommon: TTabSheet;
    Label1: TLabel;
    edName: TEdit;
    Label2: TLabel;
    cbMediaType: TComboBox;
    Label4: TLabel;
    cbStoreType: TComboBox;
    Label3: TLabel;
    edFile: TEdit;
    btnFileSelect: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnAcceptClick(Sender: TObject);
    procedure btnFileSelectClick(Sender: TObject);
    procedure btnViewClick(Sender: TObject);
    procedure edNameChange(Sender: TObject);
  private
    FIsNew: Boolean;
    FMediaRec: TGEDCOMMultimediaRecord;

    FNotesList: TSheetList;
    FSourcesList: TSheetList;

    procedure AcceptChanges();
    procedure ControlsRefresh();
    function  GetBase(): TfmBase;
    procedure ListModify(Sender: TObject; ItemData: TObject; Action: TRecAction);
    procedure SetMediaRec(const Value: TGEDCOMMultimediaRecord);
  public
    property Base: TfmBase read GetBase;
    property MediaRec: TGEDCOMMultimediaRecord read FMediaRec write SetMediaRec;
  end;

implementation

uses
  bsComUtils, GKRecordSelect, GKMain;

{$R *.dfm}

{ TfmFamilyEdit }

procedure TfmMediaEdit.FormCreate(Sender: TObject);
var
  mt: TGEDCOMMediaType;
  gst: TGKStoreType;
begin
  for mt := Low(TGEDCOMMediaType) to High(TGEDCOMMediaType) do
    cbMediaType.Items.Add(MediaTypes[mt].Name);

  for gst := Low(TGKStoreType) to High(TGKStoreType) do
    cbStoreType.Items.Add(GKStoreType[gst].Name);
  cbStoreType.ItemIndex := 0;

  FNotesList := TSheetList.Create(SheetNotes, lmBox);
  FNotesList.OnModify := ListModify;
  Base.SetupRecNotesList(FNotesList);

  FSourcesList := TSheetList.Create(SheetSources);
  FSourcesList.OnModify := ListModify;
  Base.SetupRecSourcesList(FSourcesList);
end;

procedure TfmMediaEdit.ControlsRefresh();
var
  file_ref: TGEDCOMFileReferenceWithTitle;
  gst: TGKStoreType;
  dummy: string;
begin
  file_ref := FMediaRec.FileReferences[0];

  FIsNew := (file_ref.StringValue = '');

  edName.Text := file_ref.Title;
  cbMediaType.ItemIndex := Ord(file_ref.MediaType);
  edFile.Text := file_ref.StringValue;

  gst := Base.GetStoreType(file_ref.StringValue, dummy);
  cbStoreType.ItemIndex := Ord(gst);

  edFile.Enabled := (FIsNew);
  btnFileSelect.Enabled := (FIsNew);
  cbStoreType.Enabled := (FIsNew);

  Base.RecListNotesRefresh(FMediaRec, FNotesList.List, nil);
  Base.RecListSourcesRefresh(FMediaRec, TBSListView(FSourcesList.List), nil);
end;

procedure TfmMediaEdit.SetMediaRec(const Value: TGEDCOMMultimediaRecord);
begin
  FMediaRec := Value;

  try
    ControlsRefresh();
  except
    on E: Exception do LogWrite('MediaEdit.SetMediaRec(): ' + E.Message);
  end;
end;

procedure TfmMediaEdit.AcceptChanges();
var
  file_ref: TGEDCOMFileReferenceWithTitle;
  gst: TGKStoreType;
  source_fn, ref_fn: string;
begin
  file_ref := FMediaRec.FileReferences[0];
  file_ref.Title := edName.Text;

  if (FIsNew) then begin
    gst := TGKStoreType(cbStoreType.ItemIndex);

    if (gst in [gstArchive, gstStorage]) then begin
      if not(Base.IsAdvanced) then begin
        MessageDlg('Для выбранного режима хранения не включено расширение', mtError, [mbOk], 0);
        if (Base.FileProperties(fpmAdvanced) = mrCancel) or not(Base.IsAdvanced)
        then begin
          ModalResult := mrNone;
          Exit;
        end;
      end;

      if not(Base.CheckPath()) then begin
        ModalResult := mrNone;
        Exit;
      end;
    end;

    source_fn := edFile.Text;
    Base.MediaSave(source_fn, gst, ref_fn);
    file_ref.LinkFile(ref_fn, TGEDCOMMediaType(cbMediaType.ItemIndex));
  end else begin
    file_ref.MediaType := TGEDCOMMediaType(cbMediaType.ItemIndex);
  end;

  ControlsRefresh();
end;

procedure TfmMediaEdit.btnAcceptClick(Sender: TObject);
begin
  AcceptChanges();

  Base.ChangeRecord(FMediaRec);
end;

procedure TfmMediaEdit.ListModify(Sender: TObject; ItemData: TObject;
  Action: TRecAction);
begin
  if (Sender = FNotesList) then begin
    if Base.ModifyRecNote(Self, FMediaRec, TGEDCOMNotes(ItemData), Action)
    then ControlsRefresh();
  end
  else
  if (Sender = FSourcesList) then begin
    if Base.ModifyRecSource(Self, FMediaRec, TGEDCOMSourceCitation(ItemData), Action)
    then ControlsRefresh();
  end;
end;

procedure TfmMediaEdit.btnFileSelectClick(Sender: TObject);
begin
  if OpenDialog1.Execute
  then edFile.Text := OpenDialog1.FileName;
end;

procedure TfmMediaEdit.btnViewClick(Sender: TObject);
begin
  AcceptChanges();
  Base.ShowMedia(FMediaRec);
end;

function TfmMediaEdit.GetBase(): TfmBase;
begin
  Result := TfmBase(Owner);
end;

procedure TfmMediaEdit.edNameChange(Sender: TObject);
begin
  Caption := 'Мультимедиа объект "'+edName.Text+'"';
end;

end.
