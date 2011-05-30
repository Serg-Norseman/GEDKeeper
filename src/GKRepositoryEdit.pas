unit GKRepositoryEdit; {prepare:fin; trans:fin}

{$I GEDKeeper.inc}

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, Buttons, ComCtrls, ExtCtrls,
  GedCom551, GKBase, GKEngine, GKLists, GKLangs;

type
  TfmRepositoryEdit = class(TForm, ILocalization)
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    edName: TEdit;
    PagesData: TPageControl;
    SheetNotes: TTabSheet;
    btnAddress: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure btnAcceptClick(Sender: TObject);
    procedure btnAddressClick(Sender: TObject);
  private
    FRepository: TGEDCOMRepositoryRecord;

    FNotesList: TSheetList;

    procedure ControlsRefresh();
    function GetBase: TfmBase;
    procedure ListModify(Sender: TObject; ItemData: TObject; Action: TRecAction);
    procedure SetRepository(const Value: TGEDCOMRepositoryRecord);
  public
    property Base: TfmBase read GetBase;
    property Repository: TGEDCOMRepositoryRecord read FRepository write SetRepository;

    procedure SetLang();
  end;

implementation

uses GKAddressEdit;

{$R *.dfm}

procedure TfmRepositoryEdit.FormCreate(Sender: TObject);
begin
  FNotesList := TSheetList.Create(SheetNotes, lmBox);
  FNotesList.OnModify := ListModify;
  Base.SetupRecNotesList(FNotesList);

  SetLang();
end;

procedure TfmRepositoryEdit.SetLang();
begin
  btnAccept.Caption := LSList[LSID_DlgAccept];
  btnCancel.Caption := LSList[LSID_DlgCancel];

  Caption := LSList[LSID_Repository];

  Label1.Caption := LSList[LSID_Title];
  SheetNotes.Caption := LSList[LSID_RPNotes];
  btnAddress.Caption := LSList[LSID_Address] + '...';
end;

procedure TfmRepositoryEdit.ControlsRefresh();
begin
  Base.RecListNotesRefresh(FRepository, FNotesList.List, nil);
end;

procedure TfmRepositoryEdit.SetRepository(const Value: TGEDCOMRepositoryRecord);
begin
  FRepository := Value;

  edName.Text := FRepository.RepositoryName;

  ControlsRefresh();
end;

procedure TfmRepositoryEdit.btnAcceptClick(Sender: TObject);
begin
  FRepository.RepositoryName := edName.Text;
  Base.ChangeRecord(FRepository);
end;

function TfmRepositoryEdit.GetBase: TfmBase;
begin
  Result := TfmBase(Owner);
end;

procedure TfmRepositoryEdit.btnAddressClick(Sender: TObject);
begin
  Base.ModifyAddress(Self, FRepository.Address);
end;

procedure TfmRepositoryEdit.ListModify(Sender: TObject; ItemData: TObject;
  Action: TRecAction);
begin
  if (Sender = FNotesList) then begin
    if Base.ModifyRecNote(Self, FRepository, TGEDCOMNotes(ItemData), Action)
    then ControlsRefresh();
  end;
end;

end.
