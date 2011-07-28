unit GKChart; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.Drawing, System.ComponentModel, System.Windows.Forms,
  GedCom551, GKChartCore, GKBase, GKCommon, GKEngine, GKMain, GKTreeFilter,
  GKUtils, GKLangs;

type
  TfmChart = class(System.Windows.Forms.Form)
  strict private
    SaveDialog1: System.Windows.Forms.SaveFileDialog;
    ToolBar1: System.Windows.Forms.ToolBar;
    tbImageSave: System.Windows.Forms.ToolBarButton;
    ToolButton1: System.Windows.Forms.ToolBarButton;
    ToolButton2: System.Windows.Forms.ToolBarButton;
    tbPrev: System.Windows.Forms.ToolBarButton;
    tbNext: System.Windows.Forms.ToolBarButton;
    ToolButton3: System.Windows.Forms.ToolBarButton;
    MenuPerson: System.Windows.Forms.ContextMenu;
    miEdit: System.Windows.Forms.MenuItem;
    N1: System.Windows.Forms.MenuItem;
    miSpouseAdd: System.Windows.Forms.MenuItem;
    miSonAdd: System.Windows.Forms.MenuItem;
    miDaughterAdd: System.Windows.Forms.MenuItem;
    miFamilyAdd: System.Windows.Forms.MenuItem;
    ToolButton5: System.Windows.Forms.ToolBarButton;
    N2: System.Windows.Forms.MenuItem;
    miDelete: System.Windows.Forms.MenuItem;
    N3: System.Windows.Forms.MenuItem;
    miRebuildKinships: System.Windows.Forms.MenuItem;
    tbFilter: System.Windows.Forms.ToolBarButton;
    ToolButton6: System.Windows.Forms.ToolBarButton;
    tbModes: System.Windows.Forms.ToolBarButton;
    MenuModes: System.Windows.Forms.ContextMenu;
    miModeBoth: System.Windows.Forms.MenuItem;
    miModeAncestors: System.Windows.Forms.MenuItem;
    miModeDescendants: System.Windows.Forms.MenuItem;
    N7: System.Windows.Forms.MenuItem;
    miTraceRoot: System.Windows.Forms.MenuItem;
    miRebuildTree: System.Windows.Forms.MenuItem;
    tbScales: System.Windows.Forms.ToolBarButton;
    MenuScales: System.Windows.Forms.ContextMenu;
    N501: System.Windows.Forms.MenuItem;
    N601: System.Windows.Forms.MenuItem;
    N701: System.Windows.Forms.MenuItem;
    N801: System.Windows.Forms.MenuItem;
    N901: System.Windows.Forms.MenuItem;
    N1001: System.Windows.Forms.MenuItem;
    tbGens: System.Windows.Forms.ToolBarButton;
    MenuGens: System.Windows.Forms.ContextMenu;
    miGensInf: System.Windows.Forms.MenuItem;
    miGens1: System.Windows.Forms.MenuItem;
    miGens2: System.Windows.Forms.MenuItem;
    miGens3: System.Windows.Forms.MenuItem;
    miGens4: System.Windows.Forms.MenuItem;
    miGens5: System.Windows.Forms.MenuItem;
    miGens6: System.Windows.Forms.MenuItem;
    miGens7: System.Windows.Forms.MenuItem;
    miGens8: System.Windows.Forms.MenuItem;
    miGens9: System.Windows.Forms.MenuItem;

    FBackman: TBackManager;
    FBase: TfmBase;
    FChartKind: TCustomChartBox.TChartKind;
    FDown: Boolean;
    FFileName: string;
    FGensLimit: Integer;
    FPerson: TGEDCOMIndividualRecord;
    FScale: Integer;
    FTree: TGEDCOMTree;
    FTreeBox: TAncestryChartBox;
    FX, FY: Integer;

    procedure DoFilter();
    procedure DoImageSave();
    procedure DoNext();
    procedure DoPrev();
    procedure InternalChildAdd(aNeedSex: TGEDCOMObject.TGEDCOMSex);
    procedure NavRefresh();
    procedure NavAdd(aRec: TGEDCOMIndividualRecord);
    procedure UpdateChart();
    procedure SetChartKind(const Value: TCustomChartBox.TChartKind);

    procedure InitializeComponent;

    procedure TfmChart_KeyDown(sender: System.Object; e: System.Windows.Forms.KeyEventArgs);
    procedure ToolBar1_ButtonClick(sender: System.Object; e: System.Windows.Forms.ToolBarButtonClickEventArgs);
    procedure ImageTree_MouseDown(sender: System.Object; e: System.Windows.Forms.MouseEventArgs);
    procedure ImageTree_MouseMove(sender: System.Object; e: System.Windows.Forms.MouseEventArgs);
    procedure ImageTree_MouseUp(sender: System.Object; e: System.Windows.Forms.MouseEventArgs);
    procedure ImageTree_DblClick(sender: System.Object; e: System.EventArgs);
    procedure miGens9Click(sender: System.Object; e: System.EventArgs);
    procedure N1001Click(sender: System.Object; e: System.EventArgs);
    procedure miEditClick(sender: System.Object; e: System.EventArgs);
    procedure miSpouseAddClick(sender: System.Object; e: System.EventArgs);
    procedure miSonAddClick(sender: System.Object; e: System.EventArgs);
    procedure miDaughterAddClick(sender: System.Object; e: System.EventArgs);
    procedure miFamilyAddClick(sender: System.Object; e: System.EventArgs);
    procedure miDeleteClick(sender: System.Object; e: System.EventArgs);
    procedure miRebuildKinshipsClick(sender: System.Object; e: System.EventArgs);
    procedure miTraceRootClick(sender: System.Object; e: System.EventArgs);
    procedure miModeDescendantsClick(sender: System.Object; e: System.EventArgs);
    procedure miRebuildTreeClick(sender: System.Object; e: System.EventArgs);
  strict protected
    procedure Dispose(Disposing: Boolean); override;
  public
    constructor Create(aBase: TfmBase);

    property Base: TfmBase read FBase write FBase;
    property ChartKind: TCustomChartBox.TChartKind read FChartKind write SetChartKind;
    property FileName: string read FFileName write FFileName;
    property Person: TGEDCOMIndividualRecord read FPerson write FPerson;
    property Tree: TGEDCOMTree read FTree write FTree;

    class function CheckData(aTree: TGEDCOMTree; iRec: TGEDCOMIndividualRecord;
      aKind: TCustomChartBox.TChartKind): Boolean;
    procedure GenChart(aShow: Boolean = True);

    procedure SetLang();
  end;

implementation

procedure TfmChart.InitializeComponent;
type
  TArrayOfSystem_Windows_Forms_ToolBarButton = array of System.Windows.Forms.ToolBarButton;
  TArrayOfSystem_Windows_Forms_MenuItem = array of System.Windows.Forms.MenuItem;
begin
  Self.SaveDialog1 := System.Windows.Forms.SaveFileDialog.Create;
  Self.ToolBar1 := System.Windows.Forms.ToolBar.Create;
  Self.tbImageSave := System.Windows.Forms.ToolBarButton.Create;
  Self.ToolButton1 := System.Windows.Forms.ToolBarButton.Create;
  Self.tbGens := System.Windows.Forms.ToolBarButton.Create;
  Self.MenuGens := System.Windows.Forms.ContextMenu.Create;
  Self.miGensInf := System.Windows.Forms.MenuItem.Create;
  Self.miGens1 := System.Windows.Forms.MenuItem.Create;
  Self.miGens2 := System.Windows.Forms.MenuItem.Create;
  Self.miGens3 := System.Windows.Forms.MenuItem.Create;
  Self.miGens4 := System.Windows.Forms.MenuItem.Create;
  Self.miGens5 := System.Windows.Forms.MenuItem.Create;
  Self.miGens6 := System.Windows.Forms.MenuItem.Create;
  Self.miGens7 := System.Windows.Forms.MenuItem.Create;
  Self.miGens8 := System.Windows.Forms.MenuItem.Create;
  Self.miGens9 := System.Windows.Forms.MenuItem.Create;
  Self.ToolButton2 := System.Windows.Forms.ToolBarButton.Create;
  Self.tbPrev := System.Windows.Forms.ToolBarButton.Create;
  Self.tbNext := System.Windows.Forms.ToolBarButton.Create;
  Self.ToolButton3 := System.Windows.Forms.ToolBarButton.Create;
  Self.tbScales := System.Windows.Forms.ToolBarButton.Create;
  Self.MenuScales := System.Windows.Forms.ContextMenu.Create;
  Self.N501 := System.Windows.Forms.MenuItem.Create;
  Self.N601 := System.Windows.Forms.MenuItem.Create;
  Self.N701 := System.Windows.Forms.MenuItem.Create;
  Self.N801 := System.Windows.Forms.MenuItem.Create;
  Self.N901 := System.Windows.Forms.MenuItem.Create;
  Self.N1001 := System.Windows.Forms.MenuItem.Create;
  Self.ToolButton5 := System.Windows.Forms.ToolBarButton.Create;
  Self.tbFilter := System.Windows.Forms.ToolBarButton.Create;
  Self.ToolButton6 := System.Windows.Forms.ToolBarButton.Create;
  Self.tbModes := System.Windows.Forms.ToolBarButton.Create;
  Self.MenuModes := System.Windows.Forms.ContextMenu.Create;
  Self.miModeBoth := System.Windows.Forms.MenuItem.Create;
  Self.miModeAncestors := System.Windows.Forms.MenuItem.Create;
  Self.miModeDescendants := System.Windows.Forms.MenuItem.Create;
  Self.N7 := System.Windows.Forms.MenuItem.Create;
  Self.miTraceRoot := System.Windows.Forms.MenuItem.Create;
  Self.MenuPerson := System.Windows.Forms.ContextMenu.Create;
  Self.miEdit := System.Windows.Forms.MenuItem.Create;
  Self.N1 := System.Windows.Forms.MenuItem.Create;
  Self.miFamilyAdd := System.Windows.Forms.MenuItem.Create;
  Self.miSpouseAdd := System.Windows.Forms.MenuItem.Create;
  Self.miSonAdd := System.Windows.Forms.MenuItem.Create;
  Self.miDaughterAdd := System.Windows.Forms.MenuItem.Create;
  Self.N2 := System.Windows.Forms.MenuItem.Create;
  Self.miDelete := System.Windows.Forms.MenuItem.Create;
  Self.N3 := System.Windows.Forms.MenuItem.Create;
  Self.miRebuildTree := System.Windows.Forms.MenuItem.Create;
  Self.miRebuildKinships := System.Windows.Forms.MenuItem.Create;
  Self.SuspendLayout;
  // 
  // SaveDialog1
  // 
  Self.SaveDialog1.DefaultExt := 'tga';
  Self.SaveDialog1.Filter := 'Файлы BMP (*.bmp)|*.bmp|Файлы JPEG (*.jpg)|*.j' +
  'pg|Файлы EMF (*.emf)|*.emf';
  Self.SaveDialog1.FilterIndex := 2;
  // 
  // ToolBar1
  // 
  Self.ToolBar1.Appearance := System.Windows.Forms.ToolBarAppearance.Flat;
  Self.ToolBar1.Buttons.AddRange(TArrayOfSystem_Windows_Forms_ToolBarButton.Create(Self.tbImageSave, 
          Self.ToolButton1, Self.tbGens, Self.ToolButton2, Self.tbPrev, Self.tbNext, 
          Self.ToolButton3, Self.tbScales, Self.ToolButton5, Self.tbFilter, Self.ToolButton6, 
          Self.tbModes));
  Self.ToolBar1.DropDownArrows := True;
  Self.ToolBar1.Location := System.Drawing.Point.Create(0, 0);
  Self.ToolBar1.Name := 'ToolBar1';
  Self.ToolBar1.ShowToolTips := True;
  Self.ToolBar1.Size := System.Drawing.Size.Create(822, 28);
  Self.ToolBar1.TabIndex := 0;
  Include(Self.ToolBar1.ButtonClick, Self.ToolBar1_ButtonClick);
  // 
  // tbImageSave
  // 
  Self.tbImageSave.ImageIndex := 6;
  Self.tbImageSave.ToolTipText := 'Сохранить изображение';
  // 
  // ToolButton1
  // 
  Self.ToolButton1.ImageIndex := 7;
  Self.ToolButton1.Style := System.Windows.Forms.ToolBarButtonStyle.Separator;
  // 
  // tbGens
  // 
  Self.tbGens.DropDownMenu := Self.MenuGens;
  Self.tbGens.ImageIndex := 22;
  Self.tbGens.Style := System.Windows.Forms.ToolBarButtonStyle.DropDownButton;
  // 
  // MenuGens
  // 
  Self.MenuGens.MenuItems.AddRange(TArrayOfSystem_Windows_Forms_MenuItem.Create(Self.miGensInf, 
          Self.miGens1, Self.miGens2, Self.miGens3, Self.miGens4, Self.miGens5, 
          Self.miGens6, Self.miGens7, Self.miGens8, Self.miGens9));
  // 
  // miGensInf
  // 
  Self.miGensInf.Checked := True;
  Self.miGensInf.Index := 0;
  Self.miGensInf.Text := 'Inf';
  Include(Self.miGensInf.Click, Self.miGens9Click);
  // 
  // miGens1
  // 
  Self.miGens1.Index := 1;
  Self.miGens1.Text := '1';
  Include(Self.miGens1.Click, Self.miGens9Click);
  // 
  // miGens2
  // 
  Self.miGens2.Index := 2;
  Self.miGens2.Text := '2';
  Include(Self.miGens2.Click, Self.miGens9Click);
  // 
  // miGens3
  // 
  Self.miGens3.Index := 3;
  Self.miGens3.Text := '3';
  Include(Self.miGens3.Click, Self.miGens9Click);
  // 
  // miGens4
  // 
  Self.miGens4.Index := 4;
  Self.miGens4.Text := '4';
  Include(Self.miGens4.Click, Self.miGens9Click);
  // 
  // miGens5
  // 
  Self.miGens5.Index := 5;
  Self.miGens5.Text := '5';
  Include(Self.miGens5.Click, Self.miGens9Click);
  // 
  // miGens6
  // 
  Self.miGens6.Index := 6;
  Self.miGens6.Text := '6';
  Include(Self.miGens6.Click, Self.miGens9Click);
  // 
  // miGens7
  // 
  Self.miGens7.Index := 7;
  Self.miGens7.Text := '7';
  Include(Self.miGens7.Click, Self.miGens9Click);
  // 
  // miGens8
  // 
  Self.miGens8.Index := 8;
  Self.miGens8.Text := '8';
  Include(Self.miGens8.Click, Self.miGens9Click);
  // 
  // miGens9
  // 
  Self.miGens9.Index := 9;
  Self.miGens9.Text := '9';
  Include(Self.miGens9.Click, Self.miGens9Click);
  // 
  // ToolButton2
  // 
  Self.ToolButton2.ImageIndex := 8;
  Self.ToolButton2.Style := System.Windows.Forms.ToolBarButtonStyle.Separator;
  // 
  // tbPrev
  // 
  Self.tbPrev.Enabled := False;
  Self.tbPrev.ImageIndex := 22;
  // 
  // tbNext
  // 
  Self.tbNext.Enabled := False;
  Self.tbNext.ImageIndex := 23;
  // 
  // ToolButton3
  // 
  Self.ToolButton3.ImageIndex := 24;
  Self.ToolButton3.Style := System.Windows.Forms.ToolBarButtonStyle.Separator;
  // 
  // tbScales
  // 
  Self.tbScales.DropDownMenu := Self.MenuScales;
  Self.tbScales.ImageIndex := 22;
  Self.tbScales.Style := System.Windows.Forms.ToolBarButtonStyle.DropDownButton;
  // 
  // MenuScales
  // 
  Self.MenuScales.MenuItems.AddRange(TArrayOfSystem_Windows_Forms_MenuItem.Create(Self.N501, 
          Self.N601, Self.N701, Self.N801, Self.N901, Self.N1001));
  // 
  // N501
  // 
  Self.N501.Index := 0;
  Self.N501.Text := '  50%';
  Include(Self.N501.Click, Self.N1001Click);
  // 
  // N601
  // 
  Self.N601.Index := 1;
  Self.N601.Text := '  60%';
  Include(Self.N601.Click, Self.N1001Click);
  // 
  // N701
  // 
  Self.N701.Index := 2;
  Self.N701.Text := '  70%';
  Include(Self.N701.Click, Self.N1001Click);
  // 
  // N801
  // 
  Self.N801.Index := 3;
  Self.N801.Text := '  80%';
  Include(Self.N801.Click, Self.N1001Click);
  // 
  // N901
  // 
  Self.N901.Index := 4;
  Self.N901.Text := '  90%';
  Include(Self.N901.Click, Self.N1001Click);
  // 
  // N1001
  // 
  Self.N1001.Checked := True;
  Self.N1001.Index := 5;
  Self.N1001.Text := '100%';
  Include(Self.N1001.Click, Self.N1001Click);
  // 
  // ToolButton5
  // 
  Self.ToolButton5.ImageIndex := 25;
  Self.ToolButton5.Style := System.Windows.Forms.ToolBarButtonStyle.Separator;
  // 
  // tbFilter
  // 
  Self.tbFilter.ImageIndex := 16;
  Self.tbFilter.ToolTipText := 'Фильтрация древа';
  // 
  // ToolButton6
  // 
  Self.ToolButton6.ImageIndex := 17;
  Self.ToolButton6.Style := System.Windows.Forms.ToolBarButtonStyle.Separator;
  // 
  // tbModes
  // 
  Self.tbModes.DropDownMenu := Self.MenuModes;
  Self.tbModes.ImageIndex := 21;
  Self.tbModes.Style := System.Windows.Forms.ToolBarButtonStyle.DropDownButton;
  Self.tbModes.ToolTipText := 'Режимы';
  // 
  // MenuModes
  // 
  Self.MenuModes.MenuItems.AddRange(TArrayOfSystem_Windows_Forms_MenuItem.Create(Self.miModeBoth, 
          Self.miModeAncestors, Self.miModeDescendants, Self.N7, Self.miTraceRoot));
  // 
  // miModeBoth
  // 
  Self.miModeBoth.Checked := True;
  Self.miModeBoth.Index := 0;
  Self.miModeBoth.Text := 'miModeBoth';
  Include(Self.miModeBoth.Click, Self.miModeDescendantsClick);
  // 
  // miModeAncestors
  // 
  Self.miModeAncestors.Index := 1;
  Self.miModeAncestors.Text := 'miModeAncestors';
  Include(Self.miModeAncestors.Click, Self.miModeDescendantsClick);
  // 
  // miModeDescendants
  // 
  Self.miModeDescendants.Index := 2;
  Self.miModeDescendants.Text := 'miModeDescendants';
  Include(Self.miModeDescendants.Click, Self.miModeDescendantsClick);
  // 
  // N7
  // 
  Self.N7.Index := 3;
  Self.N7.Text := '-';
  // 
  // miTraceRoot
  // 
  Self.miTraceRoot.Index := 4;
  Self.miTraceRoot.Text := 'miTraceRoot';
  Include(Self.miTraceRoot.Click, Self.miTraceRootClick);
  // 
  // MenuPerson
  // 
  Self.MenuPerson.MenuItems.AddRange(TArrayOfSystem_Windows_Forms_MenuItem.Create(Self.miEdit, 
          Self.N1, Self.miFamilyAdd, Self.miSpouseAdd, Self.miSonAdd, Self.miDaughterAdd, 
          Self.N2, Self.miDelete, Self.N3, Self.miRebuildTree, Self.miRebuildKinships));
  // 
  // miEdit
  // 
  Self.miEdit.Index := 0;
  Self.miEdit.Text := 'miEdit';
  Include(Self.miEdit.Click, Self.miEditClick);
  // 
  // N1
  // 
  Self.N1.Index := 1;
  Self.N1.Text := '-';
  // 
  // miFamilyAdd
  // 
  Self.miFamilyAdd.Index := 2;
  Self.miFamilyAdd.Text := 'miFamilyAdd';
  Include(Self.miFamilyAdd.Click, Self.miFamilyAddClick);
  // 
  // miSpouseAdd
  // 
  Self.miSpouseAdd.Index := 3;
  Self.miSpouseAdd.Text := 'miSpouseAdd';
  Include(Self.miSpouseAdd.Click, Self.miSpouseAddClick);
  // 
  // miSonAdd
  // 
  Self.miSonAdd.Index := 4;
  Self.miSonAdd.Text := 'miSonAdd';
  Include(Self.miSonAdd.Click, Self.miSonAddClick);
  // 
  // miDaughterAdd
  // 
  Self.miDaughterAdd.Index := 5;
  Self.miDaughterAdd.Text := 'miDaughterAdd';
  Include(Self.miDaughterAdd.Click, Self.miDaughterAddClick);
  // 
  // N2
  // 
  Self.N2.Index := 6;
  Self.N2.Text := '-';
  // 
  // miDelete
  // 
  Self.miDelete.Index := 7;
  Self.miDelete.Text := 'miDelete';
  Include(Self.miDelete.Click, Self.miDeleteClick);
  // 
  // N3
  // 
  Self.N3.Index := 8;
  Self.N3.Text := '-';
  // 
  // miRebuildTree
  // 
  Self.miRebuildTree.Index := 9;
  Self.miRebuildTree.Text := 'miRebuildTree';
  Include(Self.miRebuildTree.Click, Self.miRebuildTreeClick);
  // 
  // miRebuildKinships
  // 
  Self.miRebuildKinships.Index := 10;
  Self.miRebuildKinships.Text := 'miRebuildKinships';
  Include(Self.miRebuildKinships.Click, Self.miRebuildKinshipsClick);
  // 
  // TfmChart
  // 
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.ClientSize := System.Drawing.Size.Create(822, 453);
  Self.Controls.Add(Self.ToolBar1);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.KeyPreview := True;
  Self.Name := 'TfmChart';
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := 'Диаграмма';
  Include(Self.KeyDown, Self.TfmChart_KeyDown);
  Self.ResumeLayout(False);
end;

constructor TfmChart.Create(aBase: TfmBase);
begin
  inherited Create;
  InitializeComponent;
  MdiParent := fmGEDKeeper;

  FBase := aBase;

  ToolBar1.ImageList := fmGEDKeeper.ImageList_Buttons;

  FTreeBox := TAncestryChartBox.Create;
  FTreeBox.Dock := DockStyle.Fill;
  Include(Self.FTreeBox.MouseDown, Self.ImageTree_MouseDown);
  Include(Self.FTreeBox.MouseUp, Self.ImageTree_MouseUp);
  Include(Self.FTreeBox.MouseMove, Self.ImageTree_MouseMove);
  Include(Self.FTreeBox.DoubleClick, Self.ImageTree_DblClick);
  Controls.Add(FTreeBox);

  Self.Controls.SetChildIndex(FTreeBox, 0);
  Self.Controls.SetChildIndex(ToolBar1, 1);

  FBackman := TBackManager.Create;
  NavRefresh();

  FGensLimit := -1;
  FScale := 10;

  SetLang();
end;

procedure TfmChart.Dispose(Disposing: Boolean);
begin
  if Disposing then begin
    FBackman.Free;
  end;
  inherited Dispose(Disposing);
end;

procedure TfmChart.SetLang();
begin
  //Label1.Caption := LSList[LSID_GenerationsVisible] + ' ';
  miGensInf.Text := LSList[LSID_Unlimited];
  miGensInf.Checked := True;
  // both menu...

  miModeBoth.Text := LSList[LSID_TM_Both];
  miModeAncestors.Text := LSList[LSID_TM_Ancestors];
  miModeDescendants.Text := LSList[LSID_TM_Descendants];
  miTraceRoot.Text := LSList[LSID_TM_TraceRoot];

  miEdit.Text := LSList[LSID_DoEdit];
  miFamilyAdd.Text := LSList[LSID_FamilyAdd];
  miSpouseAdd.Text := LSList[LSID_SpouseAdd];
  miSonAdd.Text := LSList[LSID_SonAdd];
  miDaughterAdd.Text := LSList[LSID_DaughterAdd];
  miDelete.Text := LSList[LSID_DoDelete];
  miRebuildTree.Text := LSList[LSID_RebuildTree];
  miRebuildKinships.Text := LSList[LSID_RebuildKinships];
end;

procedure TfmChart.ToolBar1_ButtonClick(sender: System.Object; e: System.Windows.Forms.ToolBarButtonClickEventArgs);
begin
  if (e.Button = tbImageSave) then DoImageSave()
  else
  if (e.Button = tbPrev) then DoPrev()
  else
  if (e.Button = tbNext) then DoNext()
  else
  if (e.Button = tbFilter) then DoFilter();
end;

procedure TfmChart.GenChart(aShow: Boolean = True);
begin
  if (FPerson = nil) then begin
    TGKUtils.ShowError(LSList[LSID_NotSelectedPerson]);
    Exit;
  end;

  try
    NavAdd(FPerson);

    FTreeBox.DepthLimit := FGensLimit;
    FTreeBox.Options := fmGEDKeeper.Options.ChartOptions;
    FTreeBox.Engine := Base.Engine;
    FTreeBox.Tree := FTree;
    FTreeBox.ShieldState := Base.ShieldState;
    FTreeBox.Scale := FScale * 10;

    FTreeBox.GenChart(FPerson, FChartKind);

    case FChartKind of
      ckAncestors: Text := LSList[LSID_MITreeAncestors];
      ckDescendants: Text := LSList[LSID_MITreeDescendants];
      ckBoth: Text := LSList[LSID_MITreeBoth];
    end;

    Text := Text + ' "' + FFileName + '"';

    if (aShow) then Show();
  except
    on E: Exception do TGKUtils.ShowError(E.Message);
  end;
end;

procedure TfmChart.ImageTree_MouseDown(sender: System.Object; e: System.Windows.Forms.MouseEventArgs);
begin
  FX := e.X;
  FY := e.Y;

  if (e.Button = System.Windows.Forms.MouseButtons.Right) then begin
    FTreeBox.Cursor := System.Windows.Forms.Cursors.SizeAll;
    FDown := True;
  end;
end;

procedure TfmChart.ImageTree_MouseMove(sender: System.Object; e: System.Windows.Forms.MouseEventArgs);
begin
  if (FDown) then begin
    FTreeBox.LeftPos := FTreeBox.LeftPos - (e.X - FX);
    FTreeBox.TopPos := FTreeBox.TopPos - (e.Y - FY);

    FX := e.X;
    FY := e.Y;
  end;
end;

procedure TfmChart.ImageTree_MouseUp(sender: System.Object; e: System.Windows.Forms.MouseEventArgs);
begin
  if (FDown) then begin
    FTreeBox.Cursor := System.Windows.Forms.Cursors.Default;
    FDown := False;
  end;

  FTreeBox.SelectBy(e.X, e.Y);

  if (FTreeBox.Selected <> nil) and (FTreeBox.Selected.Rec <> nil) then begin
    case e.Button of
      System.Windows.Forms.MouseButtons.Left: begin
        if miTraceRoot.Checked then begin
          Person := FTreeBox.Selected.Rec;
          GenChart();
          FTreeBox.SelectByRec(FPerson);
        end;
      end;

      System.Windows.Forms.MouseButtons.Right: begin
        MenuPerson.Show(FTreeBox, Point.Create(e.X, e.Y));
      end;
    end;
  end;
end;

procedure TfmChart.ImageTree_DblClick(sender: System.Object; e: System.EventArgs);
var
  p: TPerson;
  i_rec: TGEDCOMIndividualRecord;
begin
  p := FTreeBox.Selected;
  if (p <> nil) and (p.Rec <> nil) then begin
    i_rec := p.Rec;
    if FBase.ModifyPerson(i_rec)
    then UpdateChart();
  end;
end;

class function TfmChart.CheckData(aTree: TGEDCOMTree; iRec: TGEDCOMIndividualRecord;
  aKind: TCustomChartBox.TChartKind): Boolean;
var
  anc_count, desc_count: Integer;
begin
  Result := True;

  if (aKind in [ckAncestors, ckBoth]) then begin
    TGenEngine.InitExtCounts(aTree);
    anc_count := TGenEngine.GetAncestorsCount(iRec);
    if (anc_count > 2048) then begin
      TGKUtils.ShowMessage(System.String.Format(LSList[LSID_AncestorsNumberIsInvalid], [anc_count.ToString()]));
      Result := False;
      Exit;
    end;
  end;

  if (aKind in [ckDescendants, ckBoth]) then begin
    TGenEngine.InitExtCounts(aTree);
    desc_count := TGenEngine.GetDescendantsCount(iRec);
    if (desc_count > 2048) then begin
      TGKUtils.ShowMessage(System.String.Format(LSList[LSID_DescendantsNumberIsInvalid], [desc_count.ToString()]));
      Result := False;
      Exit;
    end;
  end;
end;

procedure TfmChart.DoImageSave();
begin
  if (SaveDialog1.ShowDialog = System.Windows.Forms.DialogResult.OK)
  then FTreeBox.SaveSnapshot(SaveDialog1.FileName);
end;

procedure TfmChart.UpdateChart();
begin
  if (FBase <> nil) then FBase.ListsRefresh();
  GenChart(True);
end;

procedure TfmChart.miEditClick(sender: System.Object; e: System.EventArgs);
var
  p: TPerson;
  i_rec: TGEDCOMIndividualRecord;
begin
  p := FTreeBox.Selected;
  if (p <> nil) and (p.Rec <> nil) then begin
    i_rec := p.Rec;
    if FBase.ModifyPerson(i_rec)
    then UpdateChart();
  end;
end;

procedure TfmChart.miFamilyAddClick(sender: System.Object; e: System.EventArgs);
var
  p: TPerson;
  fam: TGEDCOMFamilyRecord;
begin
  p := FTreeBox.Selected;
  if (p <> nil) and (p.Rec <> nil) then begin
    if not(p.Rec.Sex in [svMale, svFemale]) then begin
      TGKUtils.ShowError(LSList[LSID_IsNotDefinedSex]);
      Exit;
    end;

    fam := TGenEngine.CreateFamilyEx(FTree);
    Base.Engine.AddFamilySpouse(fam, p.Rec);

    UpdateChart();
  end;
end;

procedure TfmChart.miSpouseAddClick(sender: System.Object; e: System.EventArgs);
var
  p: TPerson;
  sx: TGEDCOMObject.TGEDCOMSex;
  i_rec, i_spouse: TGEDCOMIndividualRecord;
  fam: TGEDCOMFamilyRecord;
begin
  p := FTreeBox.Selected;
  if (p <> nil) and (p.Rec <> nil) then begin
    i_rec := p.Rec;
    case i_rec.Sex of
      svMale: sx := svFemale;
      svFemale: sx := svMale;
      else begin
        TGKUtils.ShowError(LSList[LSID_IsNotDefinedSex]);
        Exit;
      end;
    end;

    i_spouse := Base.SelectPerson(nil, tmNone, sx);
    if (i_spouse <> nil) then begin
      fam := TGenEngine.CreateFamilyEx(FTree);
      Base.Engine.AddFamilySpouse(fam, i_rec);
      Base.Engine.AddFamilySpouse(fam, i_spouse);

      UpdateChart();
    end;
  end;
end;

procedure TfmChart.miTraceRootClick(sender: System.Object; e: System.EventArgs);
begin
  miTraceRoot.Checked := not miTraceRoot.Checked;
end;

procedure TfmChart.InternalChildAdd(aNeedSex: TGEDCOMObject.TGEDCOMSex);
var
  p: TPerson;
  i_rec, i_child: TGEDCOMIndividualRecord;
  fam: TGEDCOMFamilyRecord;
begin
  p := FTreeBox.Selected;
  if (p <> nil) and (p.Rec <> nil) then begin
    i_rec := p.Rec;

    if (i_rec.SpouseToFamilyLinksCount = 0) then begin
      TGKUtils.ShowError(LSList[LSID_IsNotFamilies]);
      Exit;
    end;

    if (i_rec.SpouseToFamilyLinksCount > 1) then begin
      TGKUtils.ShowError('У данной персоны несколько семей. Выбор еще не реализован.');
      Exit;
    end;

    fam := i_rec.SpouseToFamilyLinks[0].Family;
    i_child := Base.SelectPerson(TGEDCOMIndividualRecord(fam.Husband.Value), tmAncestor, aNeedSex);
    if (i_child <> nil) and Base.Engine.AddFamilyChild(fam, i_child)
    then UpdateChart();
  end;
end;

procedure TfmChart.miSonAddClick(sender: System.Object; e: System.EventArgs);
begin
  InternalChildAdd(svMale);
end;

procedure TfmChart.miDaughterAddClick(sender: System.Object; e: System.EventArgs);
begin
  InternalChildAdd(svFemale);
end;

procedure TfmChart.miDeleteClick(sender: System.Object; e: System.EventArgs);
var
  p: TPerson;
begin
  p := FTreeBox.Selected;
  if (p <> nil) and (p.Rec <> nil) and (p <> FTreeBox.Root) then begin
    FBase.DeleteIndividualRecord(p.Rec, True);
    GenChart();
    NavRefresh();
  end;
end;

procedure TfmChart.DoPrev();
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

procedure TfmChart.DoNext();
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

procedure TfmChart.miModeDescendantsClick(sender: System.Object; e: System.EventArgs);
var
  newMode: TCustomChartBox.TChartKind;
begin
  if (miModeBoth <> Sender) then miModeBoth.Checked := False;
  if (miModeAncestors <> Sender) then miModeAncestors.Checked := False;
  if (miModeDescendants <> Sender) then miModeDescendants.Checked := False;
  (Sender as MenuItem).Checked := True;

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

procedure TfmChart.N1001Click(sender: System.Object; e: System.EventArgs);
begin
  N501.Checked := False;
  N601.Checked := False;
  N701.Checked := False;
  N801.Checked := False;
  N901.Checked := False;
  N1001.Checked := False;
  MenuItem(sender).Checked := True;

  if (Sender = N501) then FScale := 5;
  if (Sender = N601) then FScale := 6;
  if (Sender = N701) then FScale := 7;
  if (Sender = N801) then FScale := 8;
  if (Sender = N901) then FScale := 9;
  if (Sender = N1001) then FScale := 10;
  //
  GenChart();
end;

procedure TfmChart.miGens9Click(sender: System.Object; e: System.EventArgs);
begin
  miGensInf.Checked := False;
  miGens1.Checked := False;
  miGens2.Checked := False;
  miGens3.Checked := False;
  miGens4.Checked := False;
  miGens5.Checked := False;
  miGens6.Checked := False;
  miGens7.Checked := False;
  miGens8.Checked := False;
  miGens9.Checked := False;
  MenuItem(sender).Checked := True;

  if (Sender = miGensInf) then FGensLimit := -1;
  if (Sender = miGens1) then FGensLimit := 1;
  if (Sender = miGens2) then FGensLimit := 2;
  if (Sender = miGens3) then FGensLimit := 3;
  if (Sender = miGens4) then FGensLimit := 4;
  if (Sender = miGens5) then FGensLimit := 5;
  if (Sender = miGens6) then FGensLimit := 6;
  if (Sender = miGens7) then FGensLimit := 7;
  if (Sender = miGens8) then FGensLimit := 8;
  if (Sender = miGens9) then FGensLimit := 9;
  //
  GenChart();
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

procedure TfmChart.SetChartKind(const Value: TCustomChartBox.TChartKind);
begin
  FChartKind := Value;

  case FChartKind of
    ckAncestors: miModeAncestors.Checked := True;
    ckDescendants: miModeDescendants.Checked := True;
    ckBoth: miModeBoth.Checked := True;
  end;
end;

procedure TfmChart.TfmChart_KeyDown(sender: System.Object; e: System.Windows.Forms.KeyEventArgs);
begin
  case e.KeyCode of
    Keys.Escape: Close;
    Keys.F6: miRebuildTreeClick(nil, nil);
    Keys.F7: FTreeBox.RebuildKinships();
  end;
end;

procedure TfmChart.miRebuildKinshipsClick(sender: System.Object; e: System.EventArgs);
begin
  FTreeBox.RebuildKinships();
end;

procedure TfmChart.miRebuildTreeClick(sender: System.Object; e: System.EventArgs);
var
  p: TPerson;
begin
  p := FTreeBox.Selected;
  if (p <> nil) and (p.Rec <> nil) then begin
    FPerson := p.Rec;
    GenChart();
    NavRefresh();
  end;
end;

procedure TfmChart.DoFilter();
var
  fmTreeFilter: TfmTreeFilter;
begin
  fmTreeFilter := TfmTreeFilter.Create(Base);
  try
    fmTreeFilter.Filter := FTreeBox.Filter;

    if (fmTreeFilter.ShowDialog() = System.Windows.Forms.DialogResult.OK)
    then GenChart();
  finally
    fmTreeFilter.Free;
  end;
end;

end.
