unit GKChart; {prepare:partial; trans:fin}

{$I GEDKeeper.inc}

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls,
  Menus, ToolWin, ComCtrls, StdCtrls, GedCom551, GKChartCore, GKBase, GKCommon,
  GKLangs;

type
  TfmChart = class(TForm, ILocalization)
    SaveDialog1: TSaveDialog;
    ToolBar1: TToolBar;
    tbImageSave: TToolButton;
    ToolButton1: TToolButton;
    ListDepthLimit: TComboBox;
    Label1: TLabel;
    ToolButton2: TToolButton;
    tbPrev: TToolButton;
    tbNext: TToolButton;
    ToolButton3: TToolButton;
    TrackBar1: TTrackBar;
    PopupMenu1: TPopupMenu;
    miEdit: TMenuItem;
    N1: TMenuItem;
    miSpouseAdd: TMenuItem;
    miSonAdd: TMenuItem;
    miDaughterAdd: TMenuItem;
    miFamilyAdd: TMenuItem;
    ToolButton5: TToolButton;
    N2: TMenuItem;
    miDelete: TMenuItem;
    N3: TMenuItem;
    miRebuildKinships: TMenuItem;
    tbFilter: TToolButton;
    ToolButton6: TToolButton;
    tbMode: TToolButton;
    PopupMenu2: TPopupMenu;
    miModeBoth: TMenuItem;
    miModeAncestors: TMenuItem;
    miModeDescendants: TMenuItem;
    N7: TMenuItem;
    miTraceRoot: TMenuItem;
    miRebuildTree: TMenuItem;
    procedure ImageTreeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageTreeMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ImageTreeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tbImageSaveClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListDepthLimitChange(Sender: TObject);
    procedure ImageTreeDblClick(Sender: TObject);
    procedure tbPrevClick(Sender: TObject);
    procedure tbNextClick(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure miEditClick(Sender: TObject);
    procedure miSpouseAddClick(Sender: TObject);
    procedure miSonAddClick(Sender: TObject);
    procedure miDaughterAddClick(Sender: TObject);
    procedure miFamilyAddClick(Sender: TObject);
    procedure chkUseFilterClick(Sender: TObject);
    procedure miDeleteClick(Sender: TObject);
    procedure miRebuildKinshipsClick(Sender: TObject);
    procedure tbFilterClick(Sender: TObject);
    procedure miTraceRootClick(Sender: TObject);
    procedure miModeDescendantsClick(Sender: TObject);
    procedure miRebuildTreeClick(Sender: TObject);
  private
    FDown: Boolean;
    FX, FY: Integer;
    FTree: TGEDCOMTree;
    FPerson: TGEDCOMIndividualRecord;
    FChartKind: TChartKind;
    FFileName: string;
    FBase: TfmBase;

    FBackman: TBackManager;

    TreeBox: TAncestryChartBox;

    procedure CreateControls();
    procedure InternalChildAdd(aNeedSex: TGEDCOMSex);
    procedure NavRefresh();
    procedure NavAdd(aRec: TGEDCOMIndividualRecord);
    procedure UpdateChart();
    procedure SetChartKind(const Value: TChartKind);
  public
    property Base: TfmBase read FBase write FBase;
    property ChartKind: TChartKind read FChartKind write SetChartKind;
    property FileName: string read FFileName write FFileName;
    property Person: TGEDCOMIndividualRecord read FPerson write FPerson;
    property Tree: TGEDCOMTree read FTree write FTree;

    class function CheckData(aTree: TGEDCOMTree; iRec: TGEDCOMIndividualRecord;
      aKind: TChartKind): Boolean;
    procedure GenChart(aShow: Boolean = True);

    procedure SetLang();
  end;

implementation

uses
  Types, GKEngine, GKMain, GKPersonEdit, GKTreeFilter;

{$R *.dfm}

procedure TfmChart.FormCreate(Sender: TObject);
begin
  CreateControls();
  FBackman := TBackManager.Create;
  NavRefresh();

  SetLang();
end;

procedure TfmChart.FormDestroy(Sender: TObject);
begin
  FBackman.Free;
end;

procedure TfmChart.SetLang();
begin
  Label1.Caption := LSList[LSID_GenerationsVisible] + ' ';
  ListDepthLimit.Items[0] := LSList[LSID_Unlimited];
  ListDepthLimit.ItemIndex := 0;
  // both menu...

  miModeBoth.Caption := LSList[LSID_TM_Both];
  miModeAncestors.Caption := LSList[LSID_TM_Ancestors];
  miModeDescendants.Caption := LSList[LSID_TM_Descendants];
  miTraceRoot.Caption := LSList[LSID_TM_TraceRoot];

  miEdit.Caption := LSList[LSID_DoEdit];
  miFamilyAdd.Caption := LSList[LSID_FamilyAdd];
  miSpouseAdd.Caption := LSList[LSID_SpouseAdd];
  miSonAdd.Caption := LSList[LSID_SonAdd];
  miDaughterAdd.Caption := LSList[LSID_DaughterAdd];
  miDelete.Caption := LSList[LSID_DoDelete];
  miRebuildTree.Caption := LSList[LSID_RebuildTree];
  miRebuildKinships.Caption := LSList[LSID_RebuildKinships];
end;

procedure TfmChart.CreateControls();
begin
  TreeBox := TAncestryChartBox.Create(Self);
  with TreeBox do begin
    Parent := Self;
    Left := 0;
    Top := 32;
    Width := 717;
    Height := 487;
    Align := alClient;

    OnDblClick := ImageTreeDblClick;
    OnMouseDown := ImageTreeMouseDown;
    OnMouseMove := ImageTreeMouseMove;
    OnMouseUp := ImageTreeMouseUp;
  end;
end;

procedure TfmChart.GenChart(aShow: Boolean = True);
begin
  if (FPerson = nil) then begin
    MessageDlg(LSList[LSID_NotSelectedPerson], mtError, [mbOk], 0);
    Exit;
  end;

  try
    NavAdd(FPerson);

    if (ListDepthLimit.ItemIndex = 0)
    then TreeBox.DepthLimit := -1
    else TreeBox.DepthLimit := ListDepthLimit.ItemIndex;

    TreeBox.Options := fmGEDKeeper.Options.ChartOptions;
    TreeBox.Engine := Base.Engine;
    TreeBox.Tree := FTree;
    TreeBox.ShieldState := Base.ShieldState;
    TreeBox.Scale := TrackBar1.Position * 10;

    TreeBox.GenChart(FPerson, FChartKind);

    case FChartKind of
      ckAncestors: Caption := LSList[LSID_MITreeAncestors];
      ckDescendants: Caption := LSList[LSID_MITreeDescendants];
      ckBoth: Caption := LSList[LSID_MITreeBoth];
    end;

    Caption := Caption + ' "' + FFileName + '"';

    if (aShow) then Show();
  except
    on E: Exception do MessageDlg(E.Message, mtError, [mbOk], 0);
  end;
end;

procedure TfmChart.ImageTreeMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FX := X;
  FY := Y;

  if (Button = mbRight) then begin
    TreeBox.Cursor := crSizeAll;
    FDown := True;
  end;
end;

procedure TfmChart.ImageTreeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if (FDown) then begin
    TreeBox.LeftPos := TreeBox.LeftPos - (X - FX);
    TreeBox.TopPos := TreeBox.TopPos - (Y - FY);

    FX := X;
    FY := Y;
  end;
end;

procedure TfmChart.ImageTreeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
begin
  if (FDown) then begin
    TreeBox.Cursor := crDefault;
    FDown := False;
  end;

  TreeBox.SelectBy(X, Y);

  if (TreeBox.Selected <> nil) and (TreeBox.Selected.Rec <> nil) then begin
    case Button of
      mbLeft: begin
        if miTraceRoot.Checked then begin
          Person := TreeBox.Selected.Rec;
          GenChart();
          TreeBox.SelectByRec(FPerson);
        end;
      end;

      mbRight: begin
        pt := TreeBox.ClientToScreen(Point(X, Y));
        PopupMenu1.Popup(pt.X, pt.Y);
      end;
    end;
  end;
end;

class function TfmChart.CheckData(aTree: TGEDCOMTree; iRec: TGEDCOMIndividualRecord;
  aKind: TChartKind): Boolean;
var
  anc_count, desc_count: Integer;
begin
  Result := True;

  if (aKind in [ckAncestors, ckBoth]) then begin
    InitExtCounts(aTree);
    anc_count := GetAncestorsCount(iRec);
    if (anc_count > 2048) then begin
      ShowMessage(Format(LSList[LSID_AncestorsNumberIsInvalid], [IntToStr(anc_count)]));
      Result := False;
      Exit;
    end;
  end;

  if (aKind in [ckDescendants, ckBoth]) then begin
    InitExtCounts(aTree);
    desc_count := GetDescendantsCount(iRec);
    if (desc_count > 2048) then begin
      ShowMessage(Format(LSList[LSID_DescendantsNumberIsInvalid], [IntToStr(desc_count)]));
      Result := False;
      Exit;
    end;
  end;
end;

procedure TfmChart.chkUseFilterClick(Sender: TObject);
begin
  GenChart();
end;

procedure TfmChart.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfmChart.tbImageSaveClick(Sender: TObject);
begin
  if SaveDialog1.Execute
  then TreeBox.SaveSnapshot(SaveDialog1.FileName);
end;

procedure TfmChart.ListDepthLimitChange(Sender: TObject);
begin
  GenChart();
end;

procedure TfmChart.UpdateChart();
begin
  if (FBase <> nil) then FBase.ListsRefresh();
  GenChart(True);
end;

procedure TfmChart.ImageTreeDblClick(Sender: TObject);
var
  p: TPerson;
  i_rec: TGEDCOMIndividualRecord;
begin
  p := TreeBox.Selected;
  if (p <> nil) and (p.Rec <> nil) then begin
    i_rec := p.Rec;
    if FBase.ModifyPerson(i_rec)
    then UpdateChart();
  end;
end;

procedure TfmChart.miEditClick(Sender: TObject);
var
  p: TPerson;
  i_rec: TGEDCOMIndividualRecord;
begin
  p := TreeBox.Selected;
  if (p <> nil) and (p.Rec <> nil) then begin
    i_rec := p.Rec;
    if FBase.ModifyPerson(i_rec)
    then UpdateChart();
  end;
end;

procedure TfmChart.miFamilyAddClick(Sender: TObject);
var
  p: TPerson;
  fam: TGEDCOMFamilyRecord;
begin
  p := TreeBox.Selected;
  if (p <> nil) and (p.Rec <> nil) then begin
    if not(p.Rec.Sex in [svMale, svFemale]) then begin
      MessageDlg(LSList[LSID_IsNotDefinedSex], mtError, [mbOk], 0);
      Exit;
    end;

    fam := CreateFamilyEx(FTree);
    Base.Engine.AddFamilySpouse(fam, p.Rec);

    UpdateChart();
  end;
end;

procedure TfmChart.miSpouseAddClick(Sender: TObject);
var
  p: TPerson;
  sx: TGEDCOMSex;
  i_rec, i_spouse: TGEDCOMIndividualRecord;
  fam: TGEDCOMFamilyRecord;
begin
  p := TreeBox.Selected;
  if (p <> nil) and (p.Rec <> nil) then begin
    i_rec := p.Rec;
    case i_rec.Sex of
      svMale: sx := svFemale;
      svFemale: sx := svMale;
      else begin
        MessageDlg(LSList[LSID_IsNotDefinedSex], mtError, [mbOk], 0);
        Exit;
      end;
    end;

    i_spouse := Base.SelectPerson(nil, tmNone, sx);
    if (i_spouse <> nil) then begin
      fam := CreateFamilyEx(FTree);
      Base.Engine.AddFamilySpouse(fam, i_rec);
      Base.Engine.AddFamilySpouse(fam, i_spouse);

      UpdateChart();
    end;
  end;
end;

procedure TfmChart.miTraceRootClick(Sender: TObject);
begin
  miTraceRoot.Checked := not miTraceRoot.Checked;
end;

procedure TfmChart.InternalChildAdd(aNeedSex: TGEDCOMSex);
var
  p: TPerson;
  i_rec, i_child: TGEDCOMIndividualRecord;
  fam: TGEDCOMFamilyRecord;
begin
  p := TreeBox.Selected;
  if (p <> nil) and (p.Rec <> nil) then begin
    i_rec := p.Rec;

    if (i_rec.SpouseToFamilyLinksCount = 0) then begin
      MessageDlg(LSList[LSID_IsNotFamilies], mtWarning, [mbOk], 0);
      Exit;
    end;

    if (i_rec.SpouseToFamilyLinksCount > 1) then begin
      MessageDlg('У данной персоны несколько семей. Выбор еще не реализован.', mtWarning, [mbOk], 0);
      Exit;
    end;

    fam := i_rec.SpouseToFamilyLinks[0].Family;
    i_child := Base.SelectPerson(TGEDCOMIndividualRecord(fam.Husband.Value), tmAncestor, aNeedSex);
    if (i_child <> nil) and Base.Engine.AddFamilyChild(fam, i_child)
    then UpdateChart();
  end;
end;

procedure TfmChart.miSonAddClick(Sender: TObject);
begin
  InternalChildAdd(svMale);
end;

procedure TfmChart.miDaughterAddClick(Sender: TObject);
begin
  InternalChildAdd(svFemale);
end;

procedure TfmChart.miDeleteClick(Sender: TObject);
var
  p: TPerson;
begin
  p := TreeBox.Selected;
  if (p <> nil) and (p.Rec <> nil) and (p <> TreeBox.Root) then begin
    FBase.DeleteIndividualRecord(p.Rec, True);
    GenChart();
    NavRefresh();
  end;
end;

procedure TfmChart.tbPrevClick(Sender: TObject);
begin
  FBackman.BeginNav();
  try
    FPerson := TGEDCOMIndividualRecord(FBackman.Back());
    GenChart();
    NavRefresh();
  finally
    FBackman.EndNav();
  end;
end;

procedure TfmChart.tbNextClick(Sender: TObject);
begin
  FBackman.BeginNav();
  try
    FPerson := TGEDCOMIndividualRecord(FBackman.Next());
    GenChart();
    NavRefresh();
  finally
    FBackman.EndNav();
  end;
end;

procedure TfmChart.miModeDescendantsClick(Sender: TObject);
var
  newMode: TChartKind;
begin
  if (miModeBoth <> Sender) then miModeBoth.Checked := False;
  if (miModeAncestors <> Sender) then miModeAncestors.Checked := False;
  if (miModeDescendants <> Sender) then miModeDescendants.Checked := False;
  (Sender as TMenuItem).Checked := True;

  ///

  if (miModeBoth.Checked) then newMode := ckBoth
  else
  if (miModeAncestors.Checked) then newMode := ckAncestors
  else
  if (miModeDescendants.Checked) then newMode := ckDescendants;

  if (FChartKind <> newMode) then begin
    FChartKind := newMode;
    GenChart();
  end;
end;

procedure TfmChart.NavAdd(aRec: TGEDCOMIndividualRecord);
begin
  if (aRec <> nil) and not(FBackman.Busy) then begin
    FBackman.Current := aRec;
    NavRefresh();
  end;
end;

procedure TfmChart.NavRefresh();
begin
  tbPrev.Enabled := FBackman.CanBackward();
  tbNext.Enabled := FBackman.CanForward();
end;

procedure TfmChart.SetChartKind(const Value: TChartKind);
begin
  FChartKind := Value;

  case FChartKind of
    ckAncestors: miModeAncestors.Checked := True;
    ckDescendants: miModeDescendants.Checked := True;
    ckBoth: miModeBoth.Checked := True;
  end;
end;

procedure TfmChart.TrackBar1Change(Sender: TObject);
begin
  GenChart();
end;

procedure TfmChart.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: Close;
    VK_F6: miRebuildTreeClick(nil);
    VK_F7: TreeBox.RebuildKinships();
  end;
end;

procedure TfmChart.miRebuildKinshipsClick(Sender: TObject);
begin
  TreeBox.RebuildKinships();
end;

procedure TfmChart.miRebuildTreeClick(Sender: TObject);
var
  p: TPerson;
begin
  p := TreeBox.Selected;
  if (p <> nil) and (p.Rec <> nil) then begin
    FPerson := p.Rec;
    GenChart();
    NavRefresh();
  end;
end;

procedure TfmChart.tbFilterClick(Sender: TObject);
var
  fmTreeFilter: TfmTreeFilter;
begin
  fmTreeFilter := TfmTreeFilter.Create(Base);
  try
    fmTreeFilter.Filter := TreeBox.Filter;
    //if (ShowModalEx(fmTreeFilter, Self) = mrOk) then GenChart();
    ShowModalEx(fmTreeFilter, Self);
    GenChart();
  finally
    fmTreeFilter.Destroy;
  end;
end;

end.
