unit GKTreeTools; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.IO, System.Drawing, System.ComponentModel, System.Windows.Forms,
  System.Resources, System.Text, VCLStub,
  GedCom551, GKImport, GKBase, GKEngine, GKCtrls, GKUtils, GKProgress, GKLangs;

type
  TfmTreeTools = class(System.Windows.Forms.Form)
  private
    type
      TMergeMode = (mmPerson, mmNote, mmFamily, mmSource);

      TCheckDiag = (cdPersonLonglived, cdPersonSexless, cdLiveYearsInvalid,
        cdStrangeSpouse, cdStrangeParent);

      TCheckSolve = (csSkip, csSetIsDead, csDefineSex);

      TCheckObj = class(System.Object)
      private
        FComment: string;
        FDiag: TCheckDiag;
        FRec: TGEDCOMRecord;
        FSolve: TCheckSolve;

        function GetRecName(): string;
      public
        property Comment: string read FComment write FComment;
        property Diag: TCheckDiag read FDiag write FDiag;
        property Rec: TGEDCOMRecord read FRec write FRec;
        property RecName: string read GetRecName;
        property Solve: TCheckSolve read FSolve write FSolve;
      end;

      TPlaceObj = class(System.Object)
      public
        Name: string;
        Facts: TList;

        constructor Create;
        destructor Destroy; override;
      end;

    const
      HelpTopics: array [0..8] of string = (
        '::/gkhTools_TreeCompare.htm',
        '::/gkhTools_TreeMerge.htm',
        '::/gkhTools_TreeSplit.htm',
        '::/gkhTools_DubsMerge.htm',
        '::/gkhTools_TreeImport.htm',
        '::/gkhTools_FamiliesConnectivity.htm',
        '::/gkhTools_TreeCheck.htm',
        '::/gkhTools_PatSearch.htm',
        '::/gkhTools_PlacesManage.htm'
      );
  strict private
    PageControl: System.Windows.Forms.TabControl;
    SheetTreeCompare: System.Windows.Forms.TabPage;
    ListCompare: System.Windows.Forms.TextBox;
    btnClose: System.Windows.Forms.Button;
    Label1: System.Windows.Forms.Label;
    edCompareFile: System.Windows.Forms.TextBox;
    btnFileChoose: System.Windows.Forms.Button;
    OpenDialog1: System.Windows.Forms.OpenFileDialog;
    SheetTreeMerge: System.Windows.Forms.TabPage;
    SheetTreeSplit: System.Windows.Forms.TabPage;
    SaveDialog1: System.Windows.Forms.SaveFileDialog;
    btnSelectAll: System.Windows.Forms.Button;
    ListSelected: System.Windows.Forms.ListBox;
    ListSkipped: System.Windows.Forms.ListBox;
    btnSelectFamily: System.Windows.Forms.Button;
    btnSelectAncestors: System.Windows.Forms.Button;
    btnSelectDescendants: System.Windows.Forms.Button;
    btnDelete: System.Windows.Forms.Button;
    btnSave: System.Windows.Forms.Button;
    SheetRecMerge: System.Windows.Forms.TabPage;
    PageControl1: System.Windows.Forms.TabControl;
    SheetMerge: System.Windows.Forms.TabPage;
    Lab1: System.Windows.Forms.Label;
    Lab2: System.Windows.Forms.Label;
    btnSearch: System.Windows.Forms.Button;
    Edit1: System.Windows.Forms.TextBox;
    Edit2: System.Windows.Forms.TextBox;
    btnRec1Select: System.Windows.Forms.Button;
    btnRec2Select: System.Windows.Forms.Button;
    btnMergeToLeft: System.Windows.Forms.Button;
    btnMergeToRight: System.Windows.Forms.Button;
    btnSkip: System.Windows.Forms.Button;
    ProgressBar1: System.Windows.Forms.ProgressBar;
    SheetOptions: System.Windows.Forms.TabPage;
    rgMode: System.Windows.Forms.GroupBox;
    GroupBox1: System.Windows.Forms.GroupBox;
    Label5: System.Windows.Forms.Label;
    Label6: System.Windows.Forms.Label;
    rbDirectMatching: System.Windows.Forms.RadioButton;
    rbIndistinctMatching: System.Windows.Forms.RadioButton;
    edNameAccuracy: System.Windows.Forms.NumericUpDown;
    edYearInaccuracy: System.Windows.Forms.NumericUpDown;
    chkBirthYear: System.Windows.Forms.CheckBox;
    SheetTreeImport: System.Windows.Forms.TabPage;
    Label3: System.Windows.Forms.Label;
    edImportFile: System.Windows.Forms.TextBox;
    btnImportFileChoose: System.Windows.Forms.Button;
    ListBox1: System.Windows.Forms.ListBox;
    OpenDialog2: System.Windows.Forms.OpenFileDialog;
    SheetFamilyGroups: System.Windows.Forms.TabPage;
    TreeView1: System.Windows.Forms.TreeView;
    SheetTreeCheck: System.Windows.Forms.TabPage;
    btnBaseRepair: System.Windows.Forms.Button;
    Panel1: System.Windows.Forms.Panel;
    Label4: System.Windows.Forms.Label;
    edMasterBase: System.Windows.Forms.TextBox;
    Label7: System.Windows.Forms.Label;
    edUpdateBase: System.Windows.Forms.TextBox;
    btnUpdateSelect: System.Windows.Forms.Button;
    gbSyncType: System.Windows.Forms.GroupBox;
    RadioButton1: System.Windows.Forms.RadioButton;
    RadioButton2: System.Windows.Forms.RadioButton;
    mSyncRes: System.Windows.Forms.TextBox;
    SheetPatSearch: System.Windows.Forms.TabPage;
    btnPatSearch: System.Windows.Forms.Button;
    Panel3: System.Windows.Forms.Panel;
    Label8: System.Windows.Forms.Label;
    edMinGens: System.Windows.Forms.NumericUpDown;
    SheetPlaceManage: System.Windows.Forms.TabPage;
    Panel4: System.Windows.Forms.Panel;
    rgTreeMergeType: System.Windows.Forms.GroupBox;
    btnHelp: System.Windows.Forms.Button;
    btnSetPatriarch: System.Windows.Forms.Button;
    chkOnlyNP: System.Windows.Forms.CheckBox;
    btnIntoList: System.Windows.Forms.Button;
    RadioButton3: System.Windows.Forms.RadioButton;
    RadioButton4: System.Windows.Forms.RadioButton;
    RadioButton5: System.Windows.Forms.RadioButton;
    RadioButton6: System.Windows.Forms.RadioButton;
    RadioButton7: System.Windows.Forms.RadioButton;
    RadioButton8: System.Windows.Forms.RadioButton;

    FBase: TfmBase;

    FSplitList: TList;
    FTree: TGEDCOMTree;

    FRec1, FRec2: TGEDCOMRecord;
    FRMMode: TMergeMode;
    FRMSkip: TStringList;
    FRMIndex: Integer;

    Memo1: TGKHyperView;
    Memo2: TGKHyperView;

    FPlaces: TStringList;
    ListPlaces: TGKListView;

    FChecksList: TObjectList;
    ListChecks: TGKListView;

    ListPatriarchs: TGKListView;

    function GetIndivName(iRec: TGEDCOMIndividualRecord; only_np: Boolean; var aName: string): Boolean;
    procedure SearchDups();
    procedure RecordMerge(aRecBase, aRecCopy: TGEDCOMRecord);
    procedure SetRec1(const Value: TGEDCOMRecord);
    procedure SetRec2(const Value: TGEDCOMRecord);

    procedure Select(aPerson: TGEDCOMIndividualRecord; aMode: TGenEngine.TTreeWalkMode);
    procedure CheckRelations();
    procedure UpdateSplitLists();

    procedure TreeCompare(aMainTree: TGEDCOMTree; aFileName: string);

    procedure CheckGroups();

    procedure PrepareChecksList();
    procedure CheckBase();
    procedure ListChecksDblClick(sender: System.Object; e: System.EventArgs);

    procedure PreparePatriarchsList();
    procedure ListPatriarchsDblClick(sender: System.Object; e: System.EventArgs);

    procedure PreparePlacesList();
    procedure PlacesClear();
    procedure CheckPlaces();
    procedure ListPlacesDblClick(sender: System.Object; e: System.EventArgs);

    procedure InitializeComponent;

    procedure btnFileChoose_Click(sender: System.Object; e: System.EventArgs);
    procedure btnSelectFamily_Click(sender: System.Object; e: System.EventArgs);
    procedure btnSelectAncestors_Click(sender: System.Object; e: System.EventArgs);
    procedure btnSelectDescendants_Click(sender: System.Object; e: System.EventArgs);
    procedure btnDelete_Click(sender: System.Object; e: System.EventArgs);
    procedure btnSave_Click(sender: System.Object; e: System.EventArgs);
    procedure btnSearch_Click(sender: System.Object; e: System.EventArgs);
    procedure btnRec1Select_Click(sender: System.Object; e: System.EventArgs);
    procedure btnRec2Select_Click(sender: System.Object; e: System.EventArgs);
    procedure btnMergeToLeft_Click(sender: System.Object; e: System.EventArgs);
    procedure btnMergeToRight_Click(sender: System.Object; e: System.EventArgs);
    procedure btnSkip_Click(sender: System.Object; e: System.EventArgs);
    procedure btnImportFileChoose_Click(sender: System.Object; e: System.EventArgs);
    procedure TreeView1_DoubleClick(sender: System.Object; e: System.EventArgs);
    procedure btnBaseRepair_Click(sender: System.Object; e: System.EventArgs);
    procedure btnPatSearch_Click(sender: System.Object; e: System.EventArgs);
    procedure btnSetPatriarch_Click(sender: System.Object; e: System.EventArgs);
    procedure btnIntoList_Click(sender: System.Object; e: System.EventArgs);
    procedure btnHelp_Click(sender: System.Object; e: System.EventArgs);
    procedure PageControl_SelectedIndexChanged(sender: System.Object; e: System.EventArgs);
    procedure btnUpdateSelect_Click(sender: System.Object; e: System.EventArgs);
    procedure btnSelectAll_Click(sender: System.Object; e: System.EventArgs);
    procedure RadioButton3_Click(sender: System.Object; e: System.EventArgs);
    procedure RadioButton8_Click(sender: System.Object; e: System.EventArgs);
  strict protected
    procedure Dispose(Disposing: Boolean); override;
  public
    constructor Create(aBase: TfmBase);

    property Base: TfmBase read FBase;

    procedure SetLang();
  end;

implementation

uses
  GKMain, GKSexCheck;

{==============================================================================}

{ TfmTreeTools.TCheckObj }

function TfmTreeTools.TCheckObj.GetRecName(): string;
begin
  Result := '';

  if (FRec is TGEDCOMIndividualRecord)
  then Result := TGenEngine.GetNameStr(TGEDCOMIndividualRecord(FRec));
end;

{==============================================================================}

procedure TfmTreeTools.InitializeComponent;
type
  TArrayOfInteger = array of Integer;
begin
  Self.PageControl := System.Windows.Forms.TabControl.Create;
  Self.SheetTreeCompare := System.Windows.Forms.TabPage.Create;
  Self.Label1 := System.Windows.Forms.Label.Create;
  Self.ListCompare := System.Windows.Forms.TextBox.Create;
  Self.edCompareFile := System.Windows.Forms.TextBox.Create;
  Self.btnFileChoose := System.Windows.Forms.Button.Create;
  Self.SheetTreeMerge := System.Windows.Forms.TabPage.Create;
  Self.Label4 := System.Windows.Forms.Label.Create;
  Self.Label7 := System.Windows.Forms.Label.Create;
  Self.edMasterBase := System.Windows.Forms.TextBox.Create;
  Self.edUpdateBase := System.Windows.Forms.TextBox.Create;
  Self.btnUpdateSelect := System.Windows.Forms.Button.Create;
  Self.gbSyncType := System.Windows.Forms.GroupBox.Create;
  Self.RadioButton1 := System.Windows.Forms.RadioButton.Create;
  Self.RadioButton2 := System.Windows.Forms.RadioButton.Create;
  Self.mSyncRes := System.Windows.Forms.TextBox.Create;
  Self.rgTreeMergeType := System.Windows.Forms.GroupBox.Create;
  Self.RadioButton3 := System.Windows.Forms.RadioButton.Create;
  Self.RadioButton4 := System.Windows.Forms.RadioButton.Create;
  Self.SheetTreeSplit := System.Windows.Forms.TabPage.Create;
  Self.btnSelectAll := System.Windows.Forms.Button.Create;
  Self.ListSelected := System.Windows.Forms.ListBox.Create;
  Self.ListSkipped := System.Windows.Forms.ListBox.Create;
  Self.btnSelectFamily := System.Windows.Forms.Button.Create;
  Self.btnSelectAncestors := System.Windows.Forms.Button.Create;
  Self.btnSelectDescendants := System.Windows.Forms.Button.Create;
  Self.btnDelete := System.Windows.Forms.Button.Create;
  Self.btnSave := System.Windows.Forms.Button.Create;
  Self.SheetTreeImport := System.Windows.Forms.TabPage.Create;
  Self.Label3 := System.Windows.Forms.Label.Create;
  Self.edImportFile := System.Windows.Forms.TextBox.Create;
  Self.btnImportFileChoose := System.Windows.Forms.Button.Create;
  Self.ListBox1 := System.Windows.Forms.ListBox.Create;
  Self.SheetRecMerge := System.Windows.Forms.TabPage.Create;
  Self.PageControl1 := System.Windows.Forms.TabControl.Create;
  Self.SheetMerge := System.Windows.Forms.TabPage.Create;
  Self.Lab1 := System.Windows.Forms.Label.Create;
  Self.Lab2 := System.Windows.Forms.Label.Create;
  Self.btnSearch := System.Windows.Forms.Button.Create;
  Self.Edit1 := System.Windows.Forms.TextBox.Create;
  Self.Edit2 := System.Windows.Forms.TextBox.Create;
  Self.btnRec1Select := System.Windows.Forms.Button.Create;
  Self.btnRec2Select := System.Windows.Forms.Button.Create;
  Self.btnMergeToLeft := System.Windows.Forms.Button.Create;
  Self.btnMergeToRight := System.Windows.Forms.Button.Create;
  Self.btnSkip := System.Windows.Forms.Button.Create;
  Self.ProgressBar1 := System.Windows.Forms.ProgressBar.Create;
  Self.SheetOptions := System.Windows.Forms.TabPage.Create;
  Self.rgMode := System.Windows.Forms.GroupBox.Create;
  Self.RadioButton8 := System.Windows.Forms.RadioButton.Create;
  Self.RadioButton7 := System.Windows.Forms.RadioButton.Create;
  Self.RadioButton6 := System.Windows.Forms.RadioButton.Create;
  Self.RadioButton5 := System.Windows.Forms.RadioButton.Create;
  Self.GroupBox1 := System.Windows.Forms.GroupBox.Create;
  Self.Label5 := System.Windows.Forms.Label.Create;
  Self.Label6 := System.Windows.Forms.Label.Create;
  Self.rbDirectMatching := System.Windows.Forms.RadioButton.Create;
  Self.rbIndistinctMatching := System.Windows.Forms.RadioButton.Create;
  Self.edNameAccuracy := System.Windows.Forms.NumericUpDown.Create;
  Self.edYearInaccuracy := System.Windows.Forms.NumericUpDown.Create;
  Self.chkBirthYear := System.Windows.Forms.CheckBox.Create;
  Self.chkOnlyNP := System.Windows.Forms.CheckBox.Create;
  Self.SheetFamilyGroups := System.Windows.Forms.TabPage.Create;
  Self.TreeView1 := System.Windows.Forms.TreeView.Create;
  Self.SheetTreeCheck := System.Windows.Forms.TabPage.Create;
  Self.btnBaseRepair := System.Windows.Forms.Button.Create;
  Self.Panel1 := System.Windows.Forms.Panel.Create;
  Self.SheetPatSearch := System.Windows.Forms.TabPage.Create;
  Self.Label8 := System.Windows.Forms.Label.Create;
  Self.btnPatSearch := System.Windows.Forms.Button.Create;
  Self.Panel3 := System.Windows.Forms.Panel.Create;
  Self.edMinGens := System.Windows.Forms.NumericUpDown.Create;
  Self.btnSetPatriarch := System.Windows.Forms.Button.Create;
  Self.SheetPlaceManage := System.Windows.Forms.TabPage.Create;
  Self.Panel4 := System.Windows.Forms.Panel.Create;
  Self.btnIntoList := System.Windows.Forms.Button.Create;
  Self.btnClose := System.Windows.Forms.Button.Create;
  Self.OpenDialog1 := System.Windows.Forms.OpenFileDialog.Create;
  Self.SaveDialog1 := System.Windows.Forms.SaveFileDialog.Create;
  Self.OpenDialog2 := System.Windows.Forms.OpenFileDialog.Create;
  Self.btnHelp := System.Windows.Forms.Button.Create;
  Self.PageControl.SuspendLayout;
  Self.SheetTreeCompare.SuspendLayout;
  Self.SheetTreeMerge.SuspendLayout;
  Self.gbSyncType.SuspendLayout;
  Self.rgTreeMergeType.SuspendLayout;
  Self.SheetTreeSplit.SuspendLayout;
  Self.SheetTreeImport.SuspendLayout;
  Self.SheetRecMerge.SuspendLayout;
  Self.PageControl1.SuspendLayout;
  Self.SheetMerge.SuspendLayout;
  Self.SheetOptions.SuspendLayout;
  Self.rgMode.SuspendLayout;
  Self.GroupBox1.SuspendLayout;
  (System.ComponentModel.ISupportInitialize(Self.edNameAccuracy)).BeginInit;
  (System.ComponentModel.ISupportInitialize(Self.edYearInaccuracy)).BeginInit;
  Self.SheetFamilyGroups.SuspendLayout;
  Self.SheetTreeCheck.SuspendLayout;
  Self.SheetPatSearch.SuspendLayout;
  (System.ComponentModel.ISupportInitialize(Self.edMinGens)).BeginInit;
  Self.SheetPlaceManage.SuspendLayout;
  Self.SuspendLayout;
  // 
  // PageControl
  // 
  Self.PageControl.Controls.Add(Self.SheetTreeCompare);
  Self.PageControl.Controls.Add(Self.SheetTreeMerge);
  Self.PageControl.Controls.Add(Self.SheetTreeSplit);
  Self.PageControl.Controls.Add(Self.SheetTreeImport);
  Self.PageControl.Controls.Add(Self.SheetRecMerge);
  Self.PageControl.Controls.Add(Self.SheetFamilyGroups);
  Self.PageControl.Controls.Add(Self.SheetTreeCheck);
  Self.PageControl.Controls.Add(Self.SheetPatSearch);
  Self.PageControl.Controls.Add(Self.SheetPlaceManage);
  Self.PageControl.Location := System.Drawing.Point.Create(8, 8);
  Self.PageControl.Name := 'PageControl';
  Self.PageControl.SelectedIndex := 0;
  Self.PageControl.Size := System.Drawing.Size.Create(721, 449);
  Self.PageControl.TabIndex := 0;
  Include(Self.PageControl.SelectedIndexChanged, Self.PageControl_SelectedIndexChanged);
  // 
  // SheetTreeCompare
  // 
  Self.SheetTreeCompare.Controls.Add(Self.Label1);
  Self.SheetTreeCompare.Controls.Add(Self.ListCompare);
  Self.SheetTreeCompare.Controls.Add(Self.edCompareFile);
  Self.SheetTreeCompare.Controls.Add(Self.btnFileChoose);
  Self.SheetTreeCompare.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetTreeCompare.Name := 'SheetTreeCompare';
  Self.SheetTreeCompare.Size := System.Drawing.Size.Create(713, 423);
  Self.SheetTreeCompare.TabIndex := 0;
  Self.SheetTreeCompare.Text := 'Сравнить базы данных';
  // 
  // Label1
  // 
  Self.Label1.Location := System.Drawing.Point.Create(8, 16);
  Self.Label1.Name := 'Label1';
  Self.Label1.Size := System.Drawing.Size.Create(35, 13);
  Self.Label1.TabIndex := 0;
  Self.Label1.Text := 'Файл';
  // 
  // ListCompare
  // 
  Self.ListCompare.Location := System.Drawing.Point.Create(8, 40);
  Self.ListCompare.Multiline := True;
  Self.ListCompare.Name := 'ListCompare';
  Self.ListCompare.ReadOnly := True;
  Self.ListCompare.Size := System.Drawing.Size.Create(697, 376);
  Self.ListCompare.TabIndex := 0;
  Self.ListCompare.Text := '';
  // 
  // edCompareFile
  // 
  Self.edCompareFile.Location := System.Drawing.Point.Create(48, 8);
  Self.edCompareFile.Name := 'edCompareFile';
  Self.edCompareFile.ReadOnly := True;
  Self.edCompareFile.Size := System.Drawing.Size.Create(568, 21);
  Self.edCompareFile.TabIndex := 1;
  Self.edCompareFile.Text := '';
  // 
  // btnFileChoose
  // 
  Self.btnFileChoose.Location := System.Drawing.Point.Create(624, 6);
  Self.btnFileChoose.Name := 'btnFileChoose';
  Self.btnFileChoose.Size := System.Drawing.Size.Create(81, 25);
  Self.btnFileChoose.TabIndex := 2;
  Self.btnFileChoose.Text := 'Выбрать...';
  Include(Self.btnFileChoose.Click, Self.btnFileChoose_Click);
  // 
  // SheetTreeMerge
  // 
  Self.SheetTreeMerge.Controls.Add(Self.Label4);
  Self.SheetTreeMerge.Controls.Add(Self.Label7);
  Self.SheetTreeMerge.Controls.Add(Self.edMasterBase);
  Self.SheetTreeMerge.Controls.Add(Self.edUpdateBase);
  Self.SheetTreeMerge.Controls.Add(Self.btnUpdateSelect);
  Self.SheetTreeMerge.Controls.Add(Self.gbSyncType);
  Self.SheetTreeMerge.Controls.Add(Self.mSyncRes);
  Self.SheetTreeMerge.Controls.Add(Self.rgTreeMergeType);
  Self.SheetTreeMerge.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetTreeMerge.Name := 'SheetTreeMerge';
  Self.SheetTreeMerge.Size := System.Drawing.Size.Create(713, 423);
  Self.SheetTreeMerge.TabIndex := 1;
  Self.SheetTreeMerge.Text := 'Объединить базы данных';
  // 
  // Label4
  // 
  Self.Label4.Location := System.Drawing.Point.Create(8, 8);
  Self.Label4.Name := 'Label4';
  Self.Label4.Size := System.Drawing.Size.Create(80, 13);
  Self.Label4.TabIndex := 0;
  Self.Label4.Text := 'Мастер-база';
  // 
  // Label7
  // 
  Self.Label7.Location := System.Drawing.Point.Create(8, 56);
  Self.Label7.Name := 'Label7';
  Self.Label7.Size := System.Drawing.Size.Create(100, 13);
  Self.Label7.TabIndex := 1;
  Self.Label7.Text := 'Обновление базы';
  // 
  // edMasterBase
  // 
  Self.edMasterBase.BackColor := System.Drawing.SystemColors.Control;
  Self.edMasterBase.Location := System.Drawing.Point.Create(8, 24);
  Self.edMasterBase.Name := 'edMasterBase';
  Self.edMasterBase.ReadOnly := True;
  Self.edMasterBase.Size := System.Drawing.Size.Create(609, 21);
  Self.edMasterBase.TabIndex := 0;
  Self.edMasterBase.Text := '[текущая база данных]';
  // 
  // edUpdateBase
  // 
  Self.edUpdateBase.Location := System.Drawing.Point.Create(8, 72);
  Self.edUpdateBase.Name := 'edUpdateBase';
  Self.edUpdateBase.ReadOnly := True;
  Self.edUpdateBase.Size := System.Drawing.Size.Create(609, 21);
  Self.edUpdateBase.TabIndex := 1;
  Self.edUpdateBase.Text := '';
  // 
  // btnUpdateSelect
  // 
  Self.btnUpdateSelect.Location := System.Drawing.Point.Create(624, 70);
  Self.btnUpdateSelect.Name := 'btnUpdateSelect';
  Self.btnUpdateSelect.Size := System.Drawing.Size.Create(81, 25);
  Self.btnUpdateSelect.TabIndex := 2;
  Self.btnUpdateSelect.Text := 'Выбрать...';
  Include(Self.btnUpdateSelect.Click, Self.btnUpdateSelect_Click);
  // 
  // gbSyncType
  // 
  Self.gbSyncType.Controls.Add(Self.RadioButton1);
  Self.gbSyncType.Controls.Add(Self.RadioButton2);
  Self.gbSyncType.Enabled := False;
  Self.gbSyncType.Location := System.Drawing.Point.Create(376, 104);
  Self.gbSyncType.Name := 'gbSyncType';
  Self.gbSyncType.Size := System.Drawing.Size.Create(329, 65);
  Self.gbSyncType.TabIndex := 3;
  Self.gbSyncType.TabStop := False;
  Self.gbSyncType.Text := 'Синхронизация';
  // 
  // RadioButton1
  // 
  Self.RadioButton1.Checked := True;
  Self.RadioButton1.Location := System.Drawing.Point.Create(16, 16);
  Self.RadioButton1.Name := 'RadioButton1';
  Self.RadioButton1.Size := System.Drawing.Size.Create(289, 17);
  Self.RadioButton1.TabIndex := 0;
  Self.RadioButton1.TabStop := True;
  Self.RadioButton1.Text := 'Доверенный источник (безусловная синхронизация)';
  // 
  // RadioButton2
  // 
  Self.RadioButton2.Enabled := False;
  Self.RadioButton2.Location := System.Drawing.Point.Create(16, 40);
  Self.RadioButton2.Name := 'RadioButton2';
  Self.RadioButton2.Size := System.Drawing.Size.Create(289, 17);
  Self.RadioButton2.TabIndex := 1;
  Self.RadioButton2.Text := 'Проверка всех элементов баз данных';
  // 
  // mSyncRes
  // 
  Self.mSyncRes.Location := System.Drawing.Point.Create(8, 176);
  Self.mSyncRes.Multiline := True;
  Self.mSyncRes.Name := 'mSyncRes';
  Self.mSyncRes.ReadOnly := True;
  Self.mSyncRes.Size := System.Drawing.Size.Create(697, 240);
  Self.mSyncRes.TabIndex := 4;
  Self.mSyncRes.Text := '';
  // 
  // rgTreeMergeType
  // 
  Self.rgTreeMergeType.Controls.Add(Self.RadioButton3);
  Self.rgTreeMergeType.Controls.Add(Self.RadioButton4);
  Self.rgTreeMergeType.Location := System.Drawing.Point.Create(8, 104);
  Self.rgTreeMergeType.Name := 'rgTreeMergeType';
  Self.rgTreeMergeType.Size := System.Drawing.Size.Create(361, 65);
  Self.rgTreeMergeType.TabIndex := 5;
  Self.rgTreeMergeType.TabStop := False;
  Self.rgTreeMergeType.Text := 'Тип объединения';
  // 
  // RadioButton3
  // 
  Self.RadioButton3.Checked := True;
  Self.RadioButton3.Location := System.Drawing.Point.Create(16, 16);
  Self.RadioButton3.Name := 'RadioButton3';
  Self.RadioButton3.Size := System.Drawing.Size.Create(328, 17);
  Self.RadioButton3.TabIndex := 2;
  Self.RadioButton3.TabStop := True;
  Self.RadioButton3.Text := 'Простое слияние данных';
  Include(Self.RadioButton3.Click, Self.RadioButton3_Click);
  // 
  // RadioButton4
  // 
  Self.RadioButton4.Location := System.Drawing.Point.Create(16, 40);
  Self.RadioButton4.Name := 'RadioButton4';
  Self.RadioButton4.Size := System.Drawing.Size.Create(328, 17);
  Self.RadioButton4.TabIndex := 3;
  Self.RadioButton4.Text := 'Синхронизация (незавершено, только для тестиров' +
  'ания)';
  Include(Self.RadioButton4.Click, Self.RadioButton3_Click);
  // 
  // SheetTreeSplit
  // 
  Self.SheetTreeSplit.Controls.Add(Self.btnSelectAll);
  Self.SheetTreeSplit.Controls.Add(Self.ListSelected);
  Self.SheetTreeSplit.Controls.Add(Self.ListSkipped);
  Self.SheetTreeSplit.Controls.Add(Self.btnSelectFamily);
  Self.SheetTreeSplit.Controls.Add(Self.btnSelectAncestors);
  Self.SheetTreeSplit.Controls.Add(Self.btnSelectDescendants);
  Self.SheetTreeSplit.Controls.Add(Self.btnDelete);
  Self.SheetTreeSplit.Controls.Add(Self.btnSave);
  Self.SheetTreeSplit.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetTreeSplit.Name := 'SheetTreeSplit';
  Self.SheetTreeSplit.Size := System.Drawing.Size.Create(713, 423);
  Self.SheetTreeSplit.TabIndex := 2;
  Self.SheetTreeSplit.Text := 'Разделить базу данных';
  // 
  // btnSelectAll
  // 
  Self.btnSelectAll.Location := System.Drawing.Point.Create(8, 352);
  Self.btnSelectAll.Name := 'btnSelectAll';
  Self.btnSelectAll.Size := System.Drawing.Size.Create(120, 25);
  Self.btnSelectAll.TabIndex := 0;
  Self.btnSelectAll.Text := 'Выбрать все связи';
  Include(Self.btnSelectAll.Click, Self.btnSelectAll_Click);
  // 
  // ListSelected
  // 
  Self.ListSelected.Location := System.Drawing.Point.Create(8, 8);
  Self.ListSelected.Name := 'ListSelected';
  Self.ListSelected.Size := System.Drawing.Size.Create(345, 329);
  Self.ListSelected.TabIndex := 1;
  // 
  // ListSkipped
  // 
  Self.ListSkipped.Location := System.Drawing.Point.Create(360, 8);
  Self.ListSkipped.Name := 'ListSkipped';
  Self.ListSkipped.Size := System.Drawing.Size.Create(345, 329);
  Self.ListSkipped.TabIndex := 2;
  // 
  // btnSelectFamily
  // 
  Self.btnSelectFamily.Location := System.Drawing.Point.Create(136, 352);
  Self.btnSelectFamily.Name := 'btnSelectFamily';
  Self.btnSelectFamily.Size := System.Drawing.Size.Create(120, 25);
  Self.btnSelectFamily.TabIndex := 3;
  Self.btnSelectFamily.Text := 'Выбрать семью';
  Include(Self.btnSelectFamily.Click, Self.btnSelectFamily_Click);
  // 
  // btnSelectAncestors
  // 
  Self.btnSelectAncestors.Location := System.Drawing.Point.Create(8, 384);
  Self.btnSelectAncestors.Name := 'btnSelectAncestors';
  Self.btnSelectAncestors.Size := System.Drawing.Size.Create(120, 25);
  Self.btnSelectAncestors.TabIndex := 4;
  Self.btnSelectAncestors.Text := 'Выбрать предков';
  Include(Self.btnSelectAncestors.Click, Self.btnSelectAncestors_Click);
  // 
  // btnSelectDescendants
  // 
  Self.btnSelectDescendants.Location := System.Drawing.Point.Create(136, 384);
  Self.btnSelectDescendants.Name := 'btnSelectDescendants';
  Self.btnSelectDescendants.Size := System.Drawing.Size.Create(120, 25);
  Self.btnSelectDescendants.TabIndex := 5;
  Self.btnSelectDescendants.Text := 'Выбрать потомков';
  Include(Self.btnSelectDescendants.Click, Self.btnSelectDescendants_Click);
  // 
  // btnDelete
  // 
  Self.btnDelete.Location := System.Drawing.Point.Create(600, 352);
  Self.btnDelete.Name := 'btnDelete';
  Self.btnDelete.Size := System.Drawing.Size.Create(105, 25);
  Self.btnDelete.TabIndex := 6;
  Self.btnDelete.Text := 'Удалить';
  Include(Self.btnDelete.Click, Self.btnDelete_Click);
  // 
  // btnSave
  // 
  Self.btnSave.Location := System.Drawing.Point.Create(600, 384);
  Self.btnSave.Name := 'btnSave';
  Self.btnSave.Size := System.Drawing.Size.Create(105, 25);
  Self.btnSave.TabIndex := 7;
  Self.btnSave.Text := 'Сохранить...';
  Include(Self.btnSave.Click, Self.btnSave_Click);
  // 
  // SheetTreeImport
  // 
  Self.SheetTreeImport.Controls.Add(Self.Label3);
  Self.SheetTreeImport.Controls.Add(Self.edImportFile);
  Self.SheetTreeImport.Controls.Add(Self.btnImportFileChoose);
  Self.SheetTreeImport.Controls.Add(Self.ListBox1);
  Self.SheetTreeImport.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetTreeImport.Name := 'SheetTreeImport';
  Self.SheetTreeImport.Size := System.Drawing.Size.Create(713, 423);
  Self.SheetTreeImport.TabIndex := 4;
  Self.SheetTreeImport.Text := 'Импорт росписей';
  // 
  // Label3
  // 
  Self.Label3.Location := System.Drawing.Point.Create(8, 16);
  Self.Label3.Name := 'Label3';
  Self.Label3.Size := System.Drawing.Size.Create(31, 13);
  Self.Label3.TabIndex := 0;
  Self.Label3.Text := 'Файл';
  // 
  // edImportFile
  // 
  Self.edImportFile.Location := System.Drawing.Point.Create(40, 8);
  Self.edImportFile.Name := 'edImportFile';
  Self.edImportFile.ReadOnly := True;
  Self.edImportFile.Size := System.Drawing.Size.Create(577, 21);
  Self.edImportFile.TabIndex := 0;
  Self.edImportFile.Text := '';
  // 
  // btnImportFileChoose
  // 
  Self.btnImportFileChoose.Location := System.Drawing.Point.Create(624, 6);
  Self.btnImportFileChoose.Name := 'btnImportFileChoose';
  Self.btnImportFileChoose.Size := System.Drawing.Size.Create(81, 25);
  Self.btnImportFileChoose.TabIndex := 1;
  Self.btnImportFileChoose.Text := 'Выбрать...';
  Include(Self.btnImportFileChoose.Click, Self.btnImportFileChoose_Click);
  // 
  // ListBox1
  // 
  Self.ListBox1.Location := System.Drawing.Point.Create(8, 40);
  Self.ListBox1.Name := 'ListBox1';
  Self.ListBox1.Size := System.Drawing.Size.Create(697, 368);
  Self.ListBox1.TabIndex := 2;
  // 
  // SheetRecMerge
  // 
  Self.SheetRecMerge.Controls.Add(Self.PageControl1);
  Self.SheetRecMerge.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetRecMerge.Name := 'SheetRecMerge';
  Self.SheetRecMerge.Size := System.Drawing.Size.Create(713, 423);
  Self.SheetRecMerge.TabIndex := 3;
  Self.SheetRecMerge.Text := 'Объединить дубликаты';
  // 
  // PageControl1
  // 
  Self.PageControl1.Controls.Add(Self.SheetMerge);
  Self.PageControl1.Controls.Add(Self.SheetOptions);
  Self.PageControl1.Location := System.Drawing.Point.Create(8, 8);
  Self.PageControl1.Name := 'PageControl1';
  Self.PageControl1.SelectedIndex := 0;
  Self.PageControl1.Size := System.Drawing.Size.Create(689, 393);
  Self.PageControl1.TabIndex := 0;
  // 
  // SheetMerge
  // 
  Self.SheetMerge.Controls.Add(Self.Lab1);
  Self.SheetMerge.Controls.Add(Self.Lab2);
  Self.SheetMerge.Controls.Add(Self.btnSearch);
  Self.SheetMerge.Controls.Add(Self.Edit1);
  Self.SheetMerge.Controls.Add(Self.Edit2);
  Self.SheetMerge.Controls.Add(Self.btnRec1Select);
  Self.SheetMerge.Controls.Add(Self.btnRec2Select);
  Self.SheetMerge.Controls.Add(Self.btnMergeToLeft);
  Self.SheetMerge.Controls.Add(Self.btnMergeToRight);
  Self.SheetMerge.Controls.Add(Self.btnSkip);
  Self.SheetMerge.Controls.Add(Self.ProgressBar1);
  Self.SheetMerge.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetMerge.Name := 'SheetMerge';
  Self.SheetMerge.Size := System.Drawing.Size.Create(681, 367);
  Self.SheetMerge.TabIndex := 0;
  Self.SheetMerge.Text := 'Объединение';
  // 
  // Lab1
  // 
  Self.Lab1.Location := System.Drawing.Point.Create(8, 8);
  Self.Lab1.Name := 'Lab1';
  Self.Lab1.Size := System.Drawing.Size.Create(24, 13);
  Self.Lab1.TabIndex := 0;
  Self.Lab1.Text := 'XXX1';
  // 
  // Lab2
  // 
  Self.Lab2.Location := System.Drawing.Point.Create(344, 8);
  Self.Lab2.Name := 'Lab2';
  Self.Lab2.Size := System.Drawing.Size.Create(24, 13);
  Self.Lab2.TabIndex := 1;
  Self.Lab2.Text := 'XXX2';
  // 
  // btnSearch
  // 
  Self.btnSearch.Location := System.Drawing.Point.Create(8, 312);
  Self.btnSearch.Name := 'btnSearch';
  Self.btnSearch.Size := System.Drawing.Size.Create(75, 25);
  Self.btnSearch.TabIndex := 0;
  Self.btnSearch.Text := 'Автопоиск';
  Include(Self.btnSearch.Click, Self.btnSearch_Click);
  // 
  // Edit1
  // 
  Self.Edit1.Location := System.Drawing.Point.Create(8, 24);
  Self.Edit1.Name := 'Edit1';
  Self.Edit1.ReadOnly := True;
  Self.Edit1.Size := System.Drawing.Size.Create(241, 21);
  Self.Edit1.TabIndex := 1;
  Self.Edit1.Text := '';
  // 
  // Edit2
  // 
  Self.Edit2.Location := System.Drawing.Point.Create(344, 24);
  Self.Edit2.Name := 'Edit2';
  Self.Edit2.ReadOnly := True;
  Self.Edit2.Size := System.Drawing.Size.Create(241, 21);
  Self.Edit2.TabIndex := 2;
  Self.Edit2.Text := '';
  // 
  // btnRec1Select
  // 
  Self.btnRec1Select.Location := System.Drawing.Point.Create(256, 22);
  Self.btnRec1Select.Name := 'btnRec1Select';
  Self.btnRec1Select.Size := System.Drawing.Size.Create(81, 25);
  Self.btnRec1Select.TabIndex := 3;
  Self.btnRec1Select.Text := 'Выбрать...';
  Include(Self.btnRec1Select.Click, Self.btnRec1Select_Click);
  // 
  // btnRec2Select
  // 
  Self.btnRec2Select.Location := System.Drawing.Point.Create(592, 22);
  Self.btnRec2Select.Name := 'btnRec2Select';
  Self.btnRec2Select.Size := System.Drawing.Size.Create(81, 25);
  Self.btnRec2Select.TabIndex := 4;
  Self.btnRec2Select.Text := 'Выбрать...';
  Include(Self.btnRec2Select.Click, Self.btnRec2Select_Click);
  // 
  // btnMergeToLeft
  // 
  Self.btnMergeToLeft.Location := System.Drawing.Point.Create(256, 312);
  Self.btnMergeToLeft.Name := 'btnMergeToLeft';
  Self.btnMergeToLeft.Size := System.Drawing.Size.Create(81, 25);
  Self.btnMergeToLeft.TabIndex := 7;
  Self.btnMergeToLeft.Text := '<<<';
  Include(Self.btnMergeToLeft.Click, Self.btnMergeToLeft_Click);
  // 
  // btnMergeToRight
  // 
  Self.btnMergeToRight.Location := System.Drawing.Point.Create(344, 312);
  Self.btnMergeToRight.Name := 'btnMergeToRight';
  Self.btnMergeToRight.Size := System.Drawing.Size.Create(81, 25);
  Self.btnMergeToRight.TabIndex := 8;
  Self.btnMergeToRight.Text := '>>>';
  Include(Self.btnMergeToRight.Click, Self.btnMergeToRight_Click);
  // 
  // btnSkip
  // 
  Self.btnSkip.Location := System.Drawing.Point.Create(88, 312);
  Self.btnSkip.Name := 'btnSkip';
  Self.btnSkip.Size := System.Drawing.Size.Create(75, 25);
  Self.btnSkip.TabIndex := 9;
  Self.btnSkip.Text := 'Пропустить';
  Include(Self.btnSkip.Click, Self.btnSkip_Click);
  // 
  // ProgressBar1
  // 
  Self.ProgressBar1.Location := System.Drawing.Point.Create(8, 344);
  Self.ProgressBar1.Name := 'ProgressBar1';
  Self.ProgressBar1.Size := System.Drawing.Size.Create(665, 16);
  Self.ProgressBar1.Step := 1;
  Self.ProgressBar1.TabIndex := 10;
  // 
  // SheetOptions
  // 
  Self.SheetOptions.Controls.Add(Self.rgMode);
  Self.SheetOptions.Controls.Add(Self.GroupBox1);
  Self.SheetOptions.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetOptions.Name := 'SheetOptions';
  Self.SheetOptions.Size := System.Drawing.Size.Create(681, 367);
  Self.SheetOptions.TabIndex := 1;
  Self.SheetOptions.Text := 'Настройки';
  // 
  // rgMode
  // 
  Self.rgMode.Controls.Add(Self.RadioButton8);
  Self.rgMode.Controls.Add(Self.RadioButton7);
  Self.rgMode.Controls.Add(Self.RadioButton6);
  Self.rgMode.Controls.Add(Self.RadioButton5);
  Self.rgMode.Location := System.Drawing.Point.Create(8, 8);
  Self.rgMode.Name := 'rgMode';
  Self.rgMode.Size := System.Drawing.Size.Create(225, 97);
  Self.rgMode.TabIndex := 0;
  Self.rgMode.TabStop := False;
  Self.rgMode.Text := 'Записи';
  // 
  // RadioButton8
  // 
  Self.RadioButton8.Location := System.Drawing.Point.Create(16, 72);
  Self.RadioButton8.Name := 'RadioButton8';
  Self.RadioButton8.Size := System.Drawing.Size.Create(192, 16);
  Self.RadioButton8.TabIndex := 3;
  Self.RadioButton8.Text := 'Источники';
  Include(Self.RadioButton8.Click, Self.RadioButton8_Click);
  // 
  // RadioButton7
  // 
  Self.RadioButton7.Location := System.Drawing.Point.Create(16, 56);
  Self.RadioButton7.Name := 'RadioButton7';
  Self.RadioButton7.Size := System.Drawing.Size.Create(192, 16);
  Self.RadioButton7.TabIndex := 2;
  Self.RadioButton7.Text := 'Семьи';
  Include(Self.RadioButton7.Click, Self.RadioButton8_Click);
  // 
  // RadioButton6
  // 
  Self.RadioButton6.Location := System.Drawing.Point.Create(16, 40);
  Self.RadioButton6.Name := 'RadioButton6';
  Self.RadioButton6.Size := System.Drawing.Size.Create(192, 16);
  Self.RadioButton6.TabIndex := 1;
  Self.RadioButton6.Text := 'Заметки';
  Include(Self.RadioButton6.Click, Self.RadioButton8_Click);
  // 
  // RadioButton5
  // 
  Self.RadioButton5.Location := System.Drawing.Point.Create(16, 24);
  Self.RadioButton5.Name := 'RadioButton5';
  Self.RadioButton5.Size := System.Drawing.Size.Create(192, 16);
  Self.RadioButton5.TabIndex := 0;
  Self.RadioButton5.Text := 'Персоны';
  Include(Self.RadioButton5.Click, Self.RadioButton8_Click);
  // 
  // GroupBox1
  // 
  Self.GroupBox1.Controls.Add(Self.Label5);
  Self.GroupBox1.Controls.Add(Self.Label6);
  Self.GroupBox1.Controls.Add(Self.rbDirectMatching);
  Self.GroupBox1.Controls.Add(Self.rbIndistinctMatching);
  Self.GroupBox1.Controls.Add(Self.edNameAccuracy);
  Self.GroupBox1.Controls.Add(Self.edYearInaccuracy);
  Self.GroupBox1.Controls.Add(Self.chkBirthYear);
  Self.GroupBox1.Controls.Add(Self.chkOnlyNP);
  Self.GroupBox1.Location := System.Drawing.Point.Create(8, 112);
  Self.GroupBox1.Name := 'GroupBox1';
  Self.GroupBox1.Size := System.Drawing.Size.Create(297, 161);
  Self.GroupBox1.TabIndex := 1;
  Self.GroupBox1.TabStop := False;
  Self.GroupBox1.Text := 'Поиск персон';
  // 
  // Label5
  // 
  Self.Label5.Location := System.Drawing.Point.Create(8, 104);
  Self.Label5.Name := 'Label5';
  Self.Label5.Size := System.Drawing.Size.Create(98, 13);
  Self.Label5.TabIndex := 0;
  Self.Label5.Text := 'Точность имени, %';
  // 
  // Label6
  // 
  Self.Label6.Location := System.Drawing.Point.Create(120, 104);
  Self.Label6.Name := 'Label6';
  Self.Label6.Size := System.Drawing.Size.Create(88, 13);
  Self.Label6.TabIndex := 1;
  Self.Label6.Text := 'Погрешность лет';
  // 
  // rbDirectMatching
  // 
  Self.rbDirectMatching.Checked := True;
  Self.rbDirectMatching.Location := System.Drawing.Point.Create(8, 16);
  Self.rbDirectMatching.Name := 'rbDirectMatching';
  Self.rbDirectMatching.Size := System.Drawing.Size.Create(153, 17);
  Self.rbDirectMatching.TabIndex := 0;
  Self.rbDirectMatching.TabStop := True;
  Self.rbDirectMatching.Text := 'Прямое сравнение';
  // 
  // rbIndistinctMatching
  // 
  Self.rbIndistinctMatching.Location := System.Drawing.Point.Create(8, 32);
  Self.rbIndistinctMatching.Name := 'rbIndistinctMatching';
  Self.rbIndistinctMatching.Size := System.Drawing.Size.Create(153, 17);
  Self.rbIndistinctMatching.TabIndex := 1;
  Self.rbIndistinctMatching.Text := 'Нечеткое сравнение';
  // 
  // edNameAccuracy
  // 
  Self.edNameAccuracy.Location := System.Drawing.Point.Create(8, 120);
  Self.edNameAccuracy.Name := 'edNameAccuracy';
  Self.edNameAccuracy.Size := System.Drawing.Size.Create(89, 21);
  Self.edNameAccuracy.TabIndex := 2;
  Self.edNameAccuracy.Value := System.Decimal.Create(TArrayOfInteger.Create(40, 
          0, 0, 0));
  // 
  // edYearInaccuracy
  // 
  Self.edYearInaccuracy.Location := System.Drawing.Point.Create(120, 120);
  Self.edYearInaccuracy.Name := 'edYearInaccuracy';
  Self.edYearInaccuracy.Size := System.Drawing.Size.Create(89, 21);
  Self.edYearInaccuracy.TabIndex := 4;
  Self.edYearInaccuracy.Value := System.Decimal.Create(TArrayOfInteger.Create(3, 
          0, 0, 0));
  // 
  // chkBirthYear
  // 
  Self.chkBirthYear.Location := System.Drawing.Point.Create(8, 80);
  Self.chkBirthYear.Name := 'chkBirthYear';
  Self.chkBirthYear.Size := System.Drawing.Size.Create(265, 17);
  Self.chkBirthYear.TabIndex := 6;
  Self.chkBirthYear.Text := 'Учитывать год рождения';
  // 
  // chkOnlyNP
  // 
  Self.chkOnlyNP.Location := System.Drawing.Point.Create(8, 56);
  Self.chkOnlyNP.Name := 'chkOnlyNP';
  Self.chkOnlyNP.Size := System.Drawing.Size.Create(265, 17);
  Self.chkOnlyNP.TabIndex := 7;
  Self.chkOnlyNP.Text := 'Только по имени/отчеству (только женщины)';
  // 
  // SheetFamilyGroups
  // 
  Self.SheetFamilyGroups.Controls.Add(Self.TreeView1);
  Self.SheetFamilyGroups.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetFamilyGroups.Name := 'SheetFamilyGroups';
  Self.SheetFamilyGroups.Size := System.Drawing.Size.Create(713, 423);
  Self.SheetFamilyGroups.TabIndex := 5;
  Self.SheetFamilyGroups.Text := 'Проверка связности семей';
  // 
  // TreeView1
  // 
  Self.TreeView1.ImageIndex := -1;
  Self.TreeView1.Location := System.Drawing.Point.Create(8, 8);
  Self.TreeView1.Name := 'TreeView1';
  Self.TreeView1.SelectedImageIndex := -1;
  Self.TreeView1.Size := System.Drawing.Size.Create(697, 401);
  Self.TreeView1.TabIndex := 0;
  Include(Self.TreeView1.DoubleClick, Self.TreeView1_DoubleClick);
  // 
  // SheetTreeCheck
  // 
  Self.SheetTreeCheck.Controls.Add(Self.btnBaseRepair);
  Self.SheetTreeCheck.Controls.Add(Self.Panel1);
  Self.SheetTreeCheck.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetTreeCheck.Name := 'SheetTreeCheck';
  Self.SheetTreeCheck.Size := System.Drawing.Size.Create(713, 423);
  Self.SheetTreeCheck.TabIndex := 6;
  Self.SheetTreeCheck.Text := 'Проверка базы данных';
  // 
  // btnBaseRepair
  // 
  Self.btnBaseRepair.Location := System.Drawing.Point.Create(560, 382);
  Self.btnBaseRepair.Name := 'btnBaseRepair';
  Self.btnBaseRepair.Size := System.Drawing.Size.Create(145, 25);
  Self.btnBaseRepair.TabIndex := 0;
  Self.btnBaseRepair.Text := 'Исправить';
  Include(Self.btnBaseRepair.Click, Self.btnBaseRepair_Click);
  // 
  // Panel1
  // 
  Self.Panel1.Location := System.Drawing.Point.Create(0, 0);
  Self.Panel1.Name := 'Panel1';
  Self.Panel1.Size := System.Drawing.Size.Create(713, 369);
  Self.Panel1.TabIndex := 1;
  // 
  // SheetPatSearch
  // 
  Self.SheetPatSearch.Controls.Add(Self.Label8);
  Self.SheetPatSearch.Controls.Add(Self.btnPatSearch);
  Self.SheetPatSearch.Controls.Add(Self.Panel3);
  Self.SheetPatSearch.Controls.Add(Self.edMinGens);
  Self.SheetPatSearch.Controls.Add(Self.btnSetPatriarch);
  Self.SheetPatSearch.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetPatSearch.Name := 'SheetPatSearch';
  Self.SheetPatSearch.Size := System.Drawing.Size.Create(713, 423);
  Self.SheetPatSearch.TabIndex := 7;
  Self.SheetPatSearch.Text := 'Поиск патриархов';
  // 
  // Label8
  // 
  Self.Label8.Location := System.Drawing.Point.Create(8, 392);
  Self.Label8.Name := 'Label8';
  Self.Label8.Size := System.Drawing.Size.Create(166, 13);
  Self.Label8.TabIndex := 0;
  Self.Label8.Text := 'Поколений потомков не менее';
  // 
  // btnPatSearch
  // 
  Self.btnPatSearch.Location := System.Drawing.Point.Create(632, 384);
  Self.btnPatSearch.Name := 'btnPatSearch';
  Self.btnPatSearch.Size := System.Drawing.Size.Create(75, 25);
  Self.btnPatSearch.TabIndex := 0;
  Self.btnPatSearch.Text := 'Поиск';
  Include(Self.btnPatSearch.Click, Self.btnPatSearch_Click);
  // 
  // Panel3
  // 
  Self.Panel3.Location := System.Drawing.Point.Create(0, 0);
  Self.Panel3.Name := 'Panel3';
  Self.Panel3.Size := System.Drawing.Size.Create(713, 369);
  Self.Panel3.TabIndex := 1;
  // 
  // edMinGens
  // 
  Self.edMinGens.Location := System.Drawing.Point.Create(184, 384);
  Self.edMinGens.Name := 'edMinGens';
  Self.edMinGens.Size := System.Drawing.Size.Create(57, 21);
  Self.edMinGens.TabIndex := 2;
  Self.edMinGens.Value := System.Decimal.Create(TArrayOfInteger.Create(2, 0, 
          0, 0));
  // 
  // btnSetPatriarch
  // 
  Self.btnSetPatriarch.Location := System.Drawing.Point.Create(503, 384);
  Self.btnSetPatriarch.Name := 'btnSetPatriarch';
  Self.btnSetPatriarch.Size := System.Drawing.Size.Create(123, 25);
  Self.btnSetPatriarch.TabIndex := 4;
  Self.btnSetPatriarch.Text := 'Установить признак';
  Include(Self.btnSetPatriarch.Click, Self.btnSetPatriarch_Click);
  // 
  // SheetPlaceManage
  // 
  Self.SheetPlaceManage.Controls.Add(Self.Panel4);
  Self.SheetPlaceManage.Controls.Add(Self.btnIntoList);
  Self.SheetPlaceManage.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetPlaceManage.Name := 'SheetPlaceManage';
  Self.SheetPlaceManage.Size := System.Drawing.Size.Create(713, 423);
  Self.SheetPlaceManage.TabIndex := 8;
  Self.SheetPlaceManage.Text := 'Управление местами';
  // 
  // Panel4
  // 
  Self.Panel4.Location := System.Drawing.Point.Create(0, 0);
  Self.Panel4.Name := 'Panel4';
  Self.Panel4.Size := System.Drawing.Size.Create(713, 369);
  Self.Panel4.TabIndex := 0;
  // 
  // btnIntoList
  // 
  Self.btnIntoList.Location := System.Drawing.Point.Create(8, 384);
  Self.btnIntoList.Name := 'btnIntoList';
  Self.btnIntoList.Size := System.Drawing.Size.Create(128, 25);
  Self.btnIntoList.TabIndex := 1;
  Self.btnIntoList.Text := 'Внести в справочник';
  Include(Self.btnIntoList.Click, Self.btnIntoList_Click);
  // 
  // btnClose
  // 
  Self.btnClose.DialogResult := System.Windows.Forms.DialogResult.Cancel;
  Self.btnClose.Location := System.Drawing.Point.Create(648, 480);
  Self.btnClose.Name := 'btnClose';
  Self.btnClose.Size := System.Drawing.Size.Create(81, 25);
  Self.btnClose.TabIndex := 1;
  Self.btnClose.Text := 'Закрыть';
  // 
  // OpenDialog1
  // 
  Self.OpenDialog1.Filter := 'GEDCOM|*.ged|Все файлы (*.*)|*.*';
  // 
  // SaveDialog1
  // 
  Self.SaveDialog1.DefaultExt := 'ged';
  Self.SaveDialog1.Filter := 'GEDCOM|*.ged';
  // 
  // OpenDialog2
  // 
  Self.OpenDialog2.Filter := 'Все поддерживаемые форматы (*.txt, *.csv, *.do' +
  'c, *.xls)|*.txt;*.csv;*.doc;*.xls|Роспись в txt-формате (*.txt)|*.txt|Рос' +
  'пись в csv-формате (*.csv)|*.csv|Роспись в формате Word (*.doc)|*.doc|Рос' +
  'пись в формате Excel (*.xls)|*.xls';
  // 
  // btnHelp
  // 
  Self.btnHelp.Location := System.Drawing.Point.Create(552, 480);
  Self.btnHelp.Name := 'btnHelp';
  Self.btnHelp.Size := System.Drawing.Size.Create(81, 25);
  Self.btnHelp.TabIndex := 2;
  Self.btnHelp.Text := 'Справка';
  Include(Self.btnHelp.Click, Self.btnHelp_Click);
  // 
  // TfmTreeTools
  // 
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.CancelButton := Self.btnClose;
  Self.ClientSize := System.Drawing.Size.Create(737, 513);
  Self.Controls.Add(Self.PageControl);
  Self.Controls.Add(Self.btnClose);
  Self.Controls.Add(Self.btnHelp);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedDialog;
  Self.KeyPreview := True;
  Self.MaximizeBox := False;
  Self.MinimizeBox := False;
  Self.Name := 'TfmTreeTools';
  Self.ShowInTaskbar := False;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := 'Инструменты';
  Self.PageControl.ResumeLayout(False);
  Self.SheetTreeCompare.ResumeLayout(False);
  Self.SheetTreeMerge.ResumeLayout(False);
  Self.gbSyncType.ResumeLayout(False);
  Self.rgTreeMergeType.ResumeLayout(False);
  Self.SheetTreeSplit.ResumeLayout(False);
  Self.SheetTreeImport.ResumeLayout(False);
  Self.SheetRecMerge.ResumeLayout(False);
  Self.PageControl1.ResumeLayout(False);
  Self.SheetMerge.ResumeLayout(False);
  Self.SheetOptions.ResumeLayout(False);
  Self.rgMode.ResumeLayout(False);
  Self.GroupBox1.ResumeLayout(False);
  (System.ComponentModel.ISupportInitialize(Self.edNameAccuracy)).EndInit;
  (System.ComponentModel.ISupportInitialize(Self.edYearInaccuracy)).EndInit;
  Self.SheetFamilyGroups.ResumeLayout(False);
  Self.SheetTreeCheck.ResumeLayout(False);
  Self.SheetPatSearch.ResumeLayout(False);
  (System.ComponentModel.ISupportInitialize(Self.edMinGens)).EndInit;
  Self.SheetPlaceManage.ResumeLayout(False);
  Self.ResumeLayout(False);
end;

constructor TfmTreeTools.Create(aBase: TfmBase);
begin
  inherited Create;
  InitializeComponent;

  FBase := aBase;

  FTree := Base.Tree;
  PageControl.SelectedIndex := 0;

  FSplitList := TList.Create;

  //
  Self.Memo1 := TGKHyperView.Create;
  Self.Memo1.Location := System.Drawing.Point.Create(8, 56);
  Self.Memo1.Size := System.Drawing.Size.Create(329, 248);
  Self.SheetMerge.Controls.Add(Self.Memo1);
  //
  Self.Memo2 := TGKHyperView.Create;
  Self.Memo2.Location := System.Drawing.Point.Create(344, 56);
  Self.Memo2.Size := System.Drawing.Size.Create(329, 248);
  Self.SheetMerge.Controls.Add(Self.Memo2);
  //

  FRMSkip := TStringList.Create;
  SetRec1(nil);
  SetRec2(nil);
  FRMMode := mmPerson;

  FPlaces := TStringList.Create;
  FPlaces.Sorted := True;

  FChecksList := TObjectList.Create(True);

  PrepareChecksList();
  PreparePatriarchsList();
  PreparePlacesList();

  SetLang();
end;

procedure TfmTreeTools.Dispose(Disposing: Boolean);
begin
  if Disposing then begin
    FChecksList.Free;

    PlacesClear();
    FPlaces.Free;

    FRMSkip.Free;

    FSplitList.Free;
  end;
  inherited Dispose(Disposing);
end;

procedure TfmTreeTools.SetLang();
begin
  btnClose.Text := LSList[LSID_DlgClose];
  btnHelp.Text := LSList[LSID_MIHelp];

  {rgOperation.Text := LSList[LSID_Operation];
  rgOperation.Items[0] := LSList[LSID_ToolOp_1];
  rgOperation.Items[1] := LSList[LSID_ToolOp_2];
  rgOperation.Items[2] := LSList[LSID_ToolOp_3];
  rgOperation.Items[3] := LSList[LSID_ToolOp_4];
  rgOperation.Items[4] := LSList[LSID_ToolOp_5];
  rgOperation.Items[5] := LSList[LSID_ToolOp_6];
  rgOperation.Items[6] := LSList[LSID_ToolOp_7];
  rgOperation.Items[7] := LSList[LSID_ToolOp_8];
  rgOperation.Items[8] := LSList[LSID_ToolOp_9];}

  Label1.Text := LSList[LSID_MIFile];
  btnFileChoose.Text := LSList[LSID_DlgSelect] + '...';

  {fixme!}
  //Label4
  //edMasterBase
  //Label7
  btnUpdateSelect.Text := LSList[LSID_DlgSelect] + '...';
  //rgTreeMergeType
  //gbSyncType

  btnSelectAll.Text := LSList[LSID_SelAll];
  btnSelectFamily.Text := LSList[LSID_SelFamily];
  btnSelectAncestors.Text := LSList[LSID_SelAncestors];
  btnSelectDescendants.Text := LSList[LSID_SelDescendants];
  btnDelete.Text := LSList[LSID_DoDelete];
  btnSave.Text := LSList[LSID_MIFileSave];

  SheetMerge.Text := LSList[LSID_RecMerge];
  SheetOptions.Text := LSList[LSID_MIOptions];
  btnRec1Select.Text := LSList[LSID_DlgSelect] + '...';
  btnRec2Select.Text := LSList[LSID_DlgSelect] + '...';
  btnSearch.Text := LSList[LSID_RM_Search];
  btnSkip.Text := LSList[LSID_RM_Skip];
  rgMode.Text := LSList[LSID_RM_Records];

  RadioButton5.Text := LSList[LSID_RPIndividuals];
  RadioButton6.Text := LSList[LSID_RPNotes];
  RadioButton7.Text := LSList[LSID_RPFamilies];
  RadioButton8.Text := LSList[LSID_RPSources];

  GroupBox1.Text := LSList[LSID_RM_SearchPersons];
  rbDirectMatching.Text := LSList[LSID_RM_DirectMatching];
  rbIndistinctMatching.Text := LSList[LSID_RM_IndistinctMatching];
  chkOnlyNP.Text := LSList[LSID_RM_OnlyNP];
  chkBirthYear.Text := LSList[LSID_RM_BirthYear];
  Label5.Text := LSList[LSID_RM_NameAccuracy];
  Label6.Text := LSList[LSID_RM_YearInaccuracy];

  Label3.Text := LSList[LSID_MIFile];
  btnImportFileChoose.Text := LSList[LSID_DlgSelect] + '...';

  btnBaseRepair.Text := LSList[LSID_Repair];

  Label8.Text := LSList[LSID_MinGenerations];
  btnSetPatriarch.Text := LSList[LSID_SetPatFlag];
  btnPatSearch.Text := LSList[LSID_Search];

  btnIntoList.Text := LSList[LSID_InsertIntoBook];
end;

procedure TfmTreeTools.TreeCompare(aMainTree: TGEDCOMTree; aFileName: string);
var
  tempTree: TGEDCOMTree;
  i, idx, k: Integer;
  iRec: TGEDCOMIndividualRecord;
  fams, names: TStringList;
  fam, nam, pat, tm: string;
  lst: TList;
begin
  ListCompare.Clear;

  tempTree := TGEDCOMTree.Create;
  tempTree.LoadFromFile(aFileName);
  fams := TStringList.Create;
  names := TStringList.Create;
  try
    ListCompare.AppendText(LSList[LSID_SearchMatches]+#13#10);

    for i := 0 to aMainTree.RecordsCount - 1 do
      if (aMainTree.Records[i] is TGEDCOMIndividualRecord) then begin
        iRec := aMainTree.Records[i] as TGEDCOMIndividualRecord;

        idx := names.AddObject(TGenEngine.GetNameStr(iRec), TList.Create);
        TList(names.Objects[idx]).Add(iRec);

        TGenEngine.GetNameParts(iRec, fam, nam, pat);
        fams.AddObject(TGenEngine.PrepareRusFamily(fam, (iRec.Sex = svFemale)), nil);
      end;

    for i := 0 to tempTree.RecordsCount - 1 do
      if (tempTree.Records[i] is TGEDCOMIndividualRecord) then begin
        iRec := tempTree.Records[i] as TGEDCOMIndividualRecord;
        tm := TGenEngine.GetNameStr(iRec);

        idx := names.IndexOf(tm);
        if (idx >= 0)
        then TList(names.Objects[idx]).Add(iRec);

        TGenEngine.GetNameParts(iRec, fam, nam, pat);
        tm := TGenEngine.PrepareRusFamily(fam, (iRec.Sex = svFemale));

        idx := fams.IndexOf(tm);
        if (idx >= 0)
        then fams.Objects[idx] := System.Object(1);
      end;

    for i := fams.Count - 1 downto 0 do
      if (fams.Objects[i] = nil) or (fams[i] = '?') then fams.Delete(i);

    for i := names.Count - 1 downto 0 do
      if (TList(names.Objects[i]).Count = 1) then begin
        TList(names.Objects[i]).Free;
        names.Delete(i);
      end;

    if (fams.Count <> 0) then begin
      ListCompare.AppendText(LSList[LSID_SimilarSurnames]+#13#10);
      for i := 0 to fams.Count - 1 do ListCompare.AppendText('    ' + fams[i]+#13#10);
    end;

    if (names.Count <> 0) then begin
      ListCompare.AppendText(LSList[LSID_SimilarNames]+#13#10);
      for i := 0 to names.Count - 1 do begin
        ListCompare.AppendText('    ' + names[i]+#13#10);
        lst := TList(names.Objects[i]);
        for k := 0 to lst.Count - 1 do begin
          iRec := TGEDCOMIndividualRecord(lst[k]);
          ListCompare.AppendText('      * ' + TGenEngine.GetNameStr(iRec) + ' ' + TGenEngine.GetLifeStr(iRec)+#13#10);
        end;
      end;
    end;
  finally
    for i := 0 to names.Count - 1 do System.Object(names.Objects[i]).Free;
    names.Free;

    fams.Free;
    tempTree.Destroy;
  end;
end;

procedure TfmTreeTools.PageControl_SelectedIndexChanged(sender: System.Object; e: System.EventArgs);
begin
  if (PageControl.SelectedTab = SheetFamilyGroups) then CheckGroups()
  else
  if (PageControl.SelectedTab = SheetTreeCheck) then CheckBase()
  else
  if (PageControl.SelectedTab = SheetPlaceManage) then CheckPlaces();
end;

procedure TfmTreeTools.btnFileChoose_Click(sender: System.Object; e: System.EventArgs);
begin
  if (OpenDialog1.ShowDialog() = System.Windows.Forms.DialogResult.OK) then begin
    edCompareFile.Text := OpenDialog1.FileName;
    TreeCompare(FTree, edCompareFile.Text);
  end;
end;

procedure TfmTreeTools.Select(aPerson: TGEDCOMIndividualRecord; aMode: TGenEngine.TTreeWalkMode);
begin
  FSplitList.Clear;
  TGenEngine.TreeWalk(aPerson, aMode, FSplitList);

  UpdateSplitLists();
end;

procedure TfmTreeTools.UpdateSplitLists();
var
  i, cnt: Integer;
  i_rec: TGEDCOMIndividualRecord;
begin
  ListSelected.BeginUpdate;
  ListSelected.Items.Clear;

  ListSkipped.BeginUpdate;
  ListSkipped.Items.Clear;

  try
    cnt := 0;
    for i := 0 to FTree.RecordsCount - 1 do
      if (FTree.Records[i] is TGEDCOMIndividualRecord) then begin
        Inc(cnt);

        i_rec := (FTree.Records[i] as TGEDCOMIndividualRecord);

        if (FSplitList.IndexOf(i_rec) < 0)
        then ListSkipped.Items.Add(i_rec.XRef + ' / ' + TGenEngine.GetNameStr(i_rec))
        else ListSelected.Items.Add(i_rec.XRef + ' / ' + TGenEngine.GetNameStr(i_rec));
      end;

    Text := FSplitList.Count.ToString() + ' / ' + cnt.ToString();
  finally
    ListSelected.EndUpdate;
    ListSkipped.EndUpdate;
  end;
end;

procedure TfmTreeTools.CheckRelations();

  procedure AddRel(aRec: TGEDCOMRecord);
  begin
    if (FSplitList.IndexOf(aRec) < 0)
    then FSplitList.Add(aRec);
  end;

  procedure CheckRecord(rec: TGEDCOMRecord);
  var
    i: Integer;
  begin
    for i := 0 to rec.MultimediaLinksCount - 1 do AddRel(rec.MultimediaLinks[i].Value);
    for i := 0 to rec.NotesCount - 1 do AddRel(rec.Notes[i].Value);
    for i := 0 to rec.SourceCitationsCount - 1 do AddRel(rec.SourceCitations[i].Value);
  end;

  procedure CheckTag(tag: TGEDCOMTagWithLists);
  var
    i: Integer;
  begin
    for i := 0 to tag.MultimediaLinksCount - 1 do AddRel(tag.MultimediaLinks[i].Value);
    for i := 0 to tag.NotesCount - 1 do AddRel(tag.Notes[i].Value);
    for i := 0 to tag.SourceCitationsCount - 1 do AddRel(tag.SourceCitations[i].Value);
  end;

  procedure CheckIndividual(iRec: TGEDCOMIndividualRecord);
  var
    i: Integer;
  begin
    CheckRecord(iRec);

    // PersonalNames
    for i := 0 to iRec.ChildToFamilyLinksCount - 1 do AddRel(iRec.ChildToFamilyLinks[i].Family);
    for i := 0 to iRec.SpouseToFamilyLinksCount - 1 do AddRel(iRec.SpouseToFamilyLinks[i].Family);
    for i := 0 to iRec.IndividualEventsCount - 1 do CheckTag(iRec.IndividualEvents[i].Detail);
    for i := 0 to iRec.IndividualOrdinancesCount - 1 do CheckTag(iRec.IndividualOrdinances[i]);
    for i := 0 to iRec.SubmittorsCount - 1 do AddRel(iRec.Submittors[i].Value);
    for i := 0 to iRec.AssociationsCount - 1 do AddRel(iRec.Associations[i].Value);
    for i := 0 to iRec.AliassesCount - 1 do AddRel(iRec.Aliasses[i].Value);
    for i := 0 to iRec.AncestorsInterestCount - 1 do AddRel(iRec.AncestorsInterest[i].Value);
    for i := 0 to iRec.DescendantsInterestCount - 1 do AddRel(iRec.DescendantsInterest[i].Value);
    // UserReferencesCount
    for i := 0 to iRec.GroupsCount - 1 do AddRel(iRec.Groups[i].Value);
  end;

  procedure CheckFamily(fRec: TGEDCOMFamilyRecord);
  var
    i: Integer;
  begin
    CheckRecord(fRec);

    for i := 0 to fRec.FamilyEventCount - 1 do CheckTag(fRec.FamilyEvents[i].Detail);
    AddRel(fRec.Submitter.Value);
    for i := 0 to fRec.SpouseSealingCount - 1 do CheckTag(fRec.SpouseSealing[i]);
  end;

  procedure CheckSource(sRec: TGEDCOMSourceRecord);
  var
    i: Integer;
  begin
    CheckRecord(sRec);

    for i := 0 to sRec.RepositoryCitationsCount - 1 do AddRel(sRec.RepositoryCitations[i].Value);
  end;

var
  i: Integer;
  rec: TGEDCOMRecord;
begin
  i := 0;
  while (i < FSplitList.Count) do begin
    rec := TGEDCOMRecord(FSplitList[i]);

    {fixme}
    case rec.RecordType of
      rtFamily: CheckFamily(rec as TGEDCOMFamilyRecord);
      rtIndividual: CheckIndividual(rec as TGEDCOMIndividualRecord);
      rtMultimedia: CheckRecord(rec);
      rtNote: CheckRecord(rec);
      rtRepository: CheckRecord(rec);
      rtSource: CheckSource(rec as TGEDCOMSourceRecord);
      rtSubmission: ;
      rtSubmitter: CheckRecord(rec);
      rtGroup: {!};
    end;

    Inc(i);
  end;
end;

procedure TfmTreeTools.btnSelectAll_Click(sender: System.Object; e: System.EventArgs);
begin
  Select(Base.GetSelectedPerson(), twmAll);
end;

procedure TfmTreeTools.btnSelectFamily_Click(sender: System.Object; e: System.EventArgs);
begin
  Select(Base.GetSelectedPerson(), twmFamily);
end;

procedure TfmTreeTools.btnSelectAncestors_Click(sender: System.Object; e: System.EventArgs);
begin
  Select(Base.GetSelectedPerson(), twmAncestors);
end;

procedure TfmTreeTools.btnSelectDescendants_Click(sender: System.Object; e: System.EventArgs);
begin
  Select(Base.GetSelectedPerson(), twmDescendants);
end;

procedure TfmTreeTools.btnDelete_Click(sender: System.Object; e: System.EventArgs);
var
  i: Integer;
  obj: System.Object;
begin
  for i := 0 to FSplitList.Count - 1 do begin
    obj := System.Object(FSplitList[i]);

    if (obj is TGEDCOMIndividualRecord)
    then Base.DeleteIndividualRecord(TGEDCOMIndividualRecord(obj), False);
  end;

  TGKUtils.ShowMessage(LSList[LSID_RecsDeleted]);

  FSplitList.Clear;
  UpdateSplitLists();

  Base.ListsRefresh();
end;

procedure TfmTreeTools.btnSave_Click(sender: System.Object; e: System.EventArgs);
var
  fs: System.IO.StreamWriter;
  subm: string;
  i: Integer;
  rec: TGEDCOMRecord;
begin
  if not(SaveDialog1.ShowDialog = System.Windows.Forms.DialogResult.OK) then Exit;

  CheckRelations();

  subm := FTree.Header.GetTagStringValue('SUBM');

  FTree.Header.Clear;
  FTree.Header.Source := TGenEngine.AppName;
  FTree.Header.ReceivingSystemName := TGenEngine.AppName;
  FTree.Header.CharacterSet := fmGEDKeeper.Options.DefCharacterSet;
  FTree.Header.Language := 'Russian';
  FTree.Header.GEDCOMVersion := '5.5';
  FTree.Header.GEDCOMForm := 'LINEAGE-LINKED';
  FTree.Header.FileName := System.IO.Path.GetFileName(SaveDialog1.FileName);
  FTree.Header.TransmissionDate.Date := DateTime.Now;

  if (subm <> '')
  then FTree.Header.SetTagStringValue('SUBM', subm);

  fs := System.IO.StreamWriter.Create(SaveDialog1.FileName, False, Encoding.GetEncoding(1251));
  try
    FTree.SaveHeaderToStream(fs);

    for i := 0 to FTree.RecordsCount - 1 do begin
      rec := TGEDCOMRecord(FTree.Records[i]);

      if (FSplitList.IndexOf(rec) >= 0)
      then rec.SaveToStream(fs);
    end;

    FTree.SaveFooterToStream(fs);

    FTree.Header.CharacterSet := csASCII;
  finally
    fs.Free;
  end;
end;

function TfmTreeTools.GetIndivName(iRec: TGEDCOMIndividualRecord; only_np: Boolean; var aName: string): Boolean;
var
  np: TGEDCOMPersonalName;
  f, n, p: string;
begin
  if (only_np) then begin
    TGenEngine.GetNameParts(iRec, f, n, p);
    aName := n + ' ' + p;
    Result := (Length(aName) > 3);
  end else begin
    np := iRec.PersonalNames[0];
    aName := np.StringValue;
    Result := (Length(np.FirstPart) > 3);
  end;
end;

procedure TfmTreeTools.SearchDups();
var
  only_np: Boolean;
  i, k: Integer;
  iRec, kRec: TGEDCOMRecord;
  iInd, kInd: TGEDCOMIndividualRecord;
  iName, kName: string;
  iNote, kNote: TGEDCOMNoteRecord;
  iFam, kFam: TGEDCOMFamilyRecord;
  iSrc, kSrc: TGEDCOMSourceRecord;
  res: Boolean;
  year1, year2, nameAccuracy, yearInaccuracy: Integer;
  ev: TGEDCOMCustomEvent;
begin
  nameAccuracy := Decimal.ToInt32(edNameAccuracy.Value);
  yearInaccuracy := Decimal.ToInt32(edYearInaccuracy.Value);
  only_np := chkOnlyNP.Checked;

  res := False;

  btnSkip.Enabled := False;
  try
    ProgressBar1.Minimum := 0;
    ProgressBar1.Maximum := FTree.RecordsCount;
    ProgressBar1.Value := FRMIndex;

    for i := FRMIndex to FTree.RecordsCount - 1 do begin
      FRMIndex := i;
      iRec := FTree.Records[i];

      case FRMMode of
        mmPerson: if (iRec is TGEDCOMIndividualRecord) then begin
          iInd := iRec as TGEDCOMIndividualRecord;
          if not(GetIndivName(iInd, only_np, iName)) then Continue;

          for k := i + 1 to FTree.RecordsCount - 1 do begin
            kRec := FTree.Records[k];

            if (kRec is TGEDCOMIndividualRecord) then begin
              kInd := kRec as TGEDCOMIndividualRecord;
              if not(GetIndivName(kInd, only_np, kName)) then Continue;

              if (iInd.Sex <> kInd.Sex)
              or (FRMSkip.IndexOf(iInd.XRef + '-' + kInd.XRef) >= 0)
              or (only_np and ((iInd.Sex <> svFemale) or (kInd.Sex <> svFemale)))
              then Continue;

              if (rbDirectMatching.Checked)
              then res := (iName = kName)
              else
              if (rbIndistinctMatching.Checked)
              then res := (TGenEngine.IndistinctMatching(4, iName, kName) > nameAccuracy);

              if (res) and (chkBirthYear.Checked) then begin
                ev := TGenEngine.GetIndividualEvent(iInd, 'BIRT');
                if (ev = nil)
                then year1 := 0
                else year1 := TGEDCOMDate(ev.Detail.Date.Value).Year;

                ev := TGenEngine.GetIndividualEvent(kInd, 'BIRT');
                if (ev = nil)
                then year2 := 0
                else year2 := TGEDCOMDate(ev.Detail.Date.Value).Year;

                res := res and ((year1 >= 0) and (year2 >= 0))
                           and (Abs(year1 - year2) <= yearInaccuracy);
              end;

              if (res) then begin
                SetRec1(iInd);
                SetRec2(kInd);
                Break;
              end;
            end;
          end;
        end;

        mmNote: if (iRec is TGEDCOMNoteRecord) then begin
          iNote := iRec as TGEDCOMNoteRecord;
          iName := iNote.Notes.Text;

          for k := i + 1 to FTree.RecordsCount - 1 do begin
            kRec := FTree.Records[k];

            if (kRec is TGEDCOMNoteRecord) then begin
              kNote := kRec as TGEDCOMNoteRecord;
              kName := kNote.Notes.Text;

              res := (iName = kName)
                and (FRMSkip.IndexOf(iNote.XRef + '-' + kNote.XRef) < 0);

              if (res) then begin
                SetRec1(iNote);
                SetRec2(kNote);
                Break;
              end;
            end;
          end;
        end;

        mmFamily: if (iRec is TGEDCOMFamilyRecord) then begin
          iFam := iRec as TGEDCOMFamilyRecord;
          iName := TGenEngine.GetFamilyStr(iFam);

          for k := i + 1 to FTree.RecordsCount - 1 do begin
            kRec := FTree.Records[k];

            if (kRec is TGEDCOMFamilyRecord) then begin
              kFam := kRec as TGEDCOMFamilyRecord;
              kName := TGenEngine.GetFamilyStr(kFam);

              res := (iName = kName)
                and (FRMSkip.IndexOf(iFam.XRef + '-' + kFam.XRef) < 0);

              if (res) then begin
                SetRec1(iFam);
                SetRec2(kFam);
                Break;
              end;
            end;
          end;
        end;

        mmSource: if (iRec is TGEDCOMSourceRecord) then begin
          iSrc := iRec as TGEDCOMSourceRecord;
          iName := iSrc.FiledByEntry;

          for k := i + 1 to FTree.RecordsCount - 1 do begin
            kRec := FTree.Records[k];

            if (kRec is TGEDCOMSourceRecord) then begin
              kSrc := kRec as TGEDCOMSourceRecord;
              kName := kSrc.FiledByEntry;

              res := (iName = kName)
                and (FRMSkip.IndexOf(iSrc.XRef + '-' + kSrc.XRef) < 0);

              if (res) then begin
                SetRec1(iSrc);
                SetRec2(kSrc);
                Break;
              end;
            end;
          end;
        end;
      end;

      if (res) then Break;

      ProgressBar1.Increment(1);
    end;
  finally
    btnSkip.Enabled := True;
  end;
end;

procedure TfmTreeTools.btnSearch_Click(sender: System.Object; e: System.EventArgs);
begin
  FRMIndex := 0;
  FRMSkip.Clear;

  SearchDups();
end;

procedure TfmTreeTools.btnSkip_Click(sender: System.Object; e: System.EventArgs);
begin
  if (FRec1 <> nil) and (FRec2 <> nil)
  then FRMSkip.Add(FRec1.XRef + '-' + FRec2.XRef);

  SearchDups();
end;

procedure TfmTreeTools.SetRec1(const Value: TGEDCOMRecord);
begin
  FRec1 := Value;

  btnMergeToLeft.Enabled := (FRec1 <> nil) and (FRec2 <> nil);
  btnMergeToRight.Enabled := (FRec1 <> nil) and (FRec2 <> nil);

  if (FRec1 = nil) then begin
    Lab1.Text := 'XXX1';
    Edit1.Text := '';
    Memo1.Lines.Clear;
  end else begin
    Lab1.Text := FRec1.XRef;

    case FRMMode of
      mmPerson: begin
        Edit1.Text := TGenEngine.GetNameStr(TGEDCOMIndividualRecord(FRec1));
        Base.ShowPersonInfo(TGEDCOMIndividualRecord(FRec1), Memo1.Lines);
      end;

      mmNote: begin
        Edit1.Text := TGEDCOMNoteRecord(FRec1).Notes[0];
        Base.ShowNoteInfo(TGEDCOMNoteRecord(FRec1), Memo1.Lines);
      end;

      mmFamily: begin
        Edit1.Text := TGenEngine.GetFamilyStr(TGEDCOMFamilyRecord(FRec1));
        Base.ShowFamilyInfo(TGEDCOMFamilyRecord(FRec1), Memo1.Lines);
      end;

      mmSource: begin
        Edit1.Text := TGEDCOMSourceRecord(FRec1).FiledByEntry;
        Base.ShowSourceInfo(TGEDCOMSourceRecord(FRec1), Memo1.Lines);
      end;
    end;
  end;
end;

procedure TfmTreeTools.SetRec2(const Value: TGEDCOMRecord);
begin
  FRec2 := Value;

  btnMergeToLeft.Enabled := (FRec1 <> nil) and (FRec2 <> nil);
  btnMergeToRight.Enabled := (FRec1 <> nil) and (FRec2 <> nil);

  if (FRec2 = nil) then begin
    Lab2.Text := 'XXX2';
    Edit2.Text := '';
    Memo2.Lines.Clear;
  end else begin
    Lab2.Text := FRec2.XRef;

    case FRMMode of
      mmPerson: begin
        Edit2.Text := TGenEngine.GetNameStr(TGEDCOMIndividualRecord(FRec2));
        Base.ShowPersonInfo(TGEDCOMIndividualRecord(FRec2), Memo2.Lines);
      end;

      mmNote: begin
        Edit2.Text := TGEDCOMNoteRecord(FRec2).Notes[0];
        Base.ShowNoteInfo(TGEDCOMNoteRecord(FRec2), Memo2.Lines);
      end;

      mmFamily: begin
        Edit2.Text := TGenEngine.GetFamilyStr(TGEDCOMFamilyRecord(FRec2));
        Base.ShowFamilyInfo(TGEDCOMFamilyRecord(FRec2), Memo2.Lines);
      end;

      mmSource: begin
        Edit2.Text := TGEDCOMSourceRecord(FRec2).FiledByEntry;
        Base.ShowSourceInfo(TGEDCOMSourceRecord(FRec2), Memo2.Lines);
      end;
    end;
  end;
end;

procedure TfmTreeTools.btnRec1Select_Click(sender: System.Object; e: System.EventArgs);
var
  irec: TGEDCOMRecord;
  sm: TGEDCOMRecord.TGEDCOMRecordType;
begin
  case FRMMode of
    mmPerson: sm := rtIndividual;
    mmNote: sm := rtNote;
  end;

  irec := Base.SelectRecord(sm, []);
  if (irec <> nil) then SetRec1(irec);
end;

procedure TfmTreeTools.btnRec2Select_Click(sender: System.Object; e: System.EventArgs);
var
  irec: TGEDCOMRecord;
  sm: TGEDCOMRecord.TGEDCOMRecordType;
begin
  case FRMMode of
    mmPerson: sm := rtIndividual;
    mmNote: sm := rtNote;
  end;

  irec := Base.SelectRecord(sm, []);
  if (irec <> nil) then SetRec2(irec);
end;

procedure TfmTreeTools.RecordMerge(aRecBase, aRecCopy: TGEDCOMRecord);
var
  repMap: TXRefReplaceMap;
  i: Integer;
begin
  repMap := TXRefReplaceMap.Create;
  try
    repMap.AddXRef(aRecCopy, aRecCopy.XRef, aRecBase.XRef);
    for i := 0 to FTree.RecordsCount - 1 do
      FTree.Records[i].ReplaceXRefs(repMap);

    case FRMMode of
      mmPerson: begin
        TGEDCOMIndividualRecord(aRecCopy).MoveTo(aRecBase);
        Base.DeleteIndividualRecord(TGEDCOMIndividualRecord(aRecCopy), False);
      end;

      mmNote: begin
        TGEDCOMNoteRecord(aRecCopy).MoveTo(aRecBase);
        Base.DeleteNoteRecord(TGEDCOMNoteRecord(aRecCopy), False);
      end;

      mmFamily: begin
        TGEDCOMFamilyRecord(aRecCopy).MoveTo(aRecBase);
        Base.DeleteFamilyRecord(TGEDCOMFamilyRecord(aRecCopy), False);
      end;

      mmSource: begin
        TGEDCOMSourceRecord(aRecCopy).MoveTo(aRecBase);
        Base.DeleteSourceRecord(TGEDCOMSourceRecord(aRecCopy), False);
      end;
    end;

    Base.ChangeRecord(aRecBase);
    Base.ListsRefresh();
  finally
    repMap.Free;
  end;
end;

procedure TfmTreeTools.btnMergeToLeft_Click(sender: System.Object; e: System.EventArgs);
begin
  RecordMerge(FRec1, FRec2);
  SetRec1(FRec1);
  SetRec2(nil);
end;

procedure TfmTreeTools.btnMergeToRight_Click(sender: System.Object; e: System.EventArgs);
begin
  RecordMerge(FRec2, FRec1);
  SetRec1(nil);
  SetRec2(FRec2);
end;

procedure TfmTreeTools.RadioButton8_Click(sender: System.Object; e: System.EventArgs);
begin
  if RadioButton5.Checked then FRMMode := mmPerson;
  if RadioButton6.Checked then FRMMode := mmNote;
  if RadioButton7.Checked then FRMMode := mmFamily;
  if RadioButton8.Checked then FRMMode := mmSource;

  btnRec1Select.Enabled := (FRMMode <> mmFamily);
  btnRec2Select.Enabled := (FRMMode <> mmFamily);
end;

procedure TfmTreeTools.btnImportFileChoose_Click(sender: System.Object; e: System.EventArgs);
var
  imp: TGKImporter;
begin
  if (OpenDialog2.ShowDialog() = System.Windows.Forms.DialogResult.OK) then begin
    edImportFile.Text := OpenDialog2.FileName;

    imp := TGKImporter.Create(Base.Engine, ListBox1.Items);
    try
      imp.TreeImportEx(edImportFile.Text);
    finally
      imp.Free;
    end;

    ListBox1.SelectedIndex := ListBox1.Items.Count - 1;
    Base.ListsRefresh(False);
  end;
end;

procedure TfmTreeTools.CheckGroups();
var
  i, k, group: Integer;
  iRec: TGEDCOMIndividualRecord;
  prepared: TList;
  root: TreeNode;
  pn: string;
begin
  TfmProgress.ProgressInit(FTree.RecordsCount, LSList[LSID_CheckFamiliesConnection]);
  prepared := TList.Create;
  try
    group := 0;
    TreeView1.Nodes.Clear();
    for i := 0 to FTree.RecordsCount - 1 do begin
      if (FTree.Records[i] is TGEDCOMIndividualRecord) then begin
        iRec := FTree.Records[i] as TGEDCOMIndividualRecord;
        if (prepared.IndexOf(iRec) >= 0) then Continue;

        Inc(group);
        FSplitList.Clear();
        TGenEngine.TreeWalk(iRec, twmAll, FSplitList);

        root := TreeView1.Nodes.Add(group.ToString() + ' '+LSList[LSID_Group].ToLower()+' (' + FSplitList.Count.ToString() + ')');
        for k := 0 to FSplitList.Count - 1 do begin
          iRec := TObject(FSplitList[k]) as TGEDCOMIndividualRecord;
          prepared.Add(iRec);

          pn := TGenEngine.GetNameStr(iRec);
          if (iRec.Patriarch) then pn := '(*) ' + pn;

          root.Nodes.Add(TGKTreeNode.Create(pn, iRec));
        end;
        root.ExpandAll();
      end;

      TfmProgress.ProgressStep();
      Application.DoEvents;
    end;
  finally
    FSplitList.Clear();
    prepared.Free;
    TfmProgress.ProgressDone();
  end;
end;

procedure TfmTreeTools.TreeView1_DoubleClick(sender: System.Object; e: System.EventArgs);
var
  node: TGKTreeNode;
  i_rec: TGEDCOMIndividualRecord;
begin
  node := TGKTreeNode(TreeView1.SelectedNode);
  if (node = nil) then Exit;

  i_rec := TGEDCOMIndividualRecord(node.Data);
  if (i_rec = nil) then Exit;

  Base.SelectRecordByXRef(i_rec.XRef);
  Close;
end;

procedure TfmTreeTools.PrepareChecksList();
begin
  Base.CreateListView(Panel1, ListChecks);
  ListChecks.Checkboxes := True;
  Include(ListChecks.DoubleClick, ListChecksDblClick);
  ListChecks.AddListColumn(LSList[LSID_Record], 400);
  ListChecks.AddListColumn(LSList[LSID_Problem], 200);
  ListChecks.AddListColumn(LSList[LSID_Solve], 200);
end;

procedure TfmTreeTools.CheckBase();
var
  i, iAge: Integer;
  iRec: TGEDCOMIndividualRecord;
  item: ListViewItem;
  dead_event: TGEDCOMCustomEvent;
  age: string;
  checkObj: TCheckObj;
  y_birth, y_death: Integer;
begin
  try
    TfmProgress.ProgressInit(FTree.RecordsCount, LSList[LSID_ToolOp_7]);

    FChecksList.Clear;
    for i := 0 to FTree.RecordsCount - 1 do begin
      TfmProgress.ProgressStep();

      if (FTree.Records[i] is TGEDCOMIndividualRecord) then begin
        iRec := FTree.Records[i] as TGEDCOMIndividualRecord;

        //
        dead_event := TGenEngine.GetIndividualEvent(iRec, 'DEAT');
        if (dead_event = nil) then begin
          age := TGenEngine.GetAge(iRec);
          if (age <> '') and (age <> '?') then begin
            iAge := Int32.Parse(age);
            if (iAge >= 130) then begin
              checkObj := TCheckObj.Create;
              checkObj.Rec := iRec;
              checkObj.Diag := cdPersonLonglived;
              checkObj.Solve := csSetIsDead;
              checkObj.Comment := System.String.Format(LSList[LSID_PersonLonglived], [age]);
              FChecksList.Add(checkObj);
            end;
          end;
        end;

        //
        if not(iRec.Sex in [svMale, svFemale]) then begin
          checkObj := TCheckObj.Create;
          checkObj.Rec := iRec;
          checkObj.Diag := cdPersonSexless;
          checkObj.Solve := csDefineSex;
          checkObj.Comment := LSList[LSID_PersonSexless];
          FChecksList.Add(checkObj);
        end;

        //
        y_birth := TGenEngine.GetIndependentYear(iRec, 'BIRT');
        y_death := TGenEngine.GetIndependentYear(iRec, 'DEAT');
        if (y_birth > -1) and (y_death > -1) and (y_death < y_birth)
        then begin
          checkObj := TCheckObj.Create;
          checkObj.Rec := iRec;
          checkObj.Diag := cdLiveYearsInvalid;
          checkObj.Solve := csSkip;
          checkObj.Comment := LSList[LSID_LiveYearsInvalid];
          FChecksList.Add(checkObj);
        end;

        //
        iAge := TGenEngine.GetMarriageAge(iRec);
        if (iAge > 0) and ((iAge <= 13) or (iAge >= 50)) then begin
          checkObj := TCheckObj.Create;
          checkObj.Rec := iRec;
          checkObj.Diag := cdStrangeSpouse;
          checkObj.Solve := csSkip;
          checkObj.Comment := System.String.Format(LSList[LSID_StrangeSpouse], [iAge.ToString()]);
          FChecksList.Add(checkObj);
        end;

        //
        iAge := TGenEngine.GetFirstbornAge(iRec);
        if (iAge > 0) and ((iAge <= 13) or (iAge >= 50)) then begin
          checkObj := TCheckObj.Create;
          checkObj.Rec := iRec;
          checkObj.Diag := cdStrangeParent;
          checkObj.Solve := csSkip;
          checkObj.Comment := System.String.Format(LSList[LSID_StrangeParent], [iAge.ToString()]);
          FChecksList.Add(checkObj);
        end;
      end;
    end;

    ///

    ListChecks.Items.Clear();
    for i := 0 to FChecksList.Count - 1 do begin
      checkObj := TCheckObj(FChecksList[i]);

      item := ListChecks.AddItem(checkObj.RecName, checkObj);
      item.SubItems.Add(checkObj.Comment);
    end;
  finally
    TfmProgress.ProgressDone();
  end;
end;

procedure TfmTreeTools.btnBaseRepair_Click(sender: System.Object; e: System.EventArgs);
var
  i: Integer;
  item: TExtListItem;
  iRec: TGEDCOMIndividualRecord;
  checkObj: TCheckObj;
begin
  try
    for i := 0 to ListChecks.Items.Count - 1 do begin
      item := TExtListItem(ListChecks.Items[i]);
      checkObj := TCheckObj(item.Data);

      if item.Checked then begin
        case checkObj.Diag of
          cdPersonLonglived: begin
            iRec := TGEDCOMIndividualRecord(checkObj.Rec);
            TGenEngine.CreateEventEx(FTree, iRec, 'DEAT', '', '');
            Base.ChangeRecord(iRec);
          end;

          cdPersonSexless: begin
            iRec := TGEDCOMIndividualRecord(checkObj.Rec);
            TfmSexCheck.CheckPersonSex(iRec, fmGEDKeeper.NamesTable);
            Base.ChangeRecord(iRec);
          end;

          cdLiveYearsInvalid: begin
            // dummy
          end;

          cdStrangeSpouse: begin
            // dummy
          end;

          cdStrangeParent: begin
            // dummy
          end;
        end;
      end;
    end;
  finally
    Base.ListsRefresh();
    CheckBase();
  end;
end;

procedure TfmTreeTools.ListChecksDblClick(sender: System.Object; e: System.EventArgs);
var
  item: TExtListItem;
  i_rec: TGEDCOMIndividualRecord;
begin
  item := ListChecks.SelectedItem();
  if (item = nil) then Exit;

  i_rec := TGEDCOMIndividualRecord(TCheckObj(item.Data).FRec);
  if (i_rec = nil) then Exit;

  Base.SelectRecordByXRef(i_rec.XRef);
  Close;
end;

procedure TfmTreeTools.btnPatSearch_Click(sender: System.Object; e: System.EventArgs);
var
  lst: TObjectList;

  function GetLinks(pObj: TGenEngine.TPatriarchObj): string;
  var
    i: Integer;
  begin
    Result := '';

    for i := 0 to lst.Count - 1 do begin
      if (i in pObj.ILinks) then begin
        if (Result <> '') then Result := Result + ', ';
        Result := Result + TGenEngine.GetNameStr(TGenEngine.TPatriarchObj(lst[i]).IRec);
      end;
    end;
  end;

var
  i: Integer;
  p_obj: TGenEngine.TPatriarchObj;
  item: TExtListItem;
  p_sign: string;
begin
  ListPatriarchs.BeginUpdate();
  lst := TObjectList.Create(True);
  try
    ListPatriarchs.Items.Clear();
    Base.Engine.GetPatriarchsList(True, False, lst, Decimal.ToInt32(edMinGens.Value));

    for i := 0 to lst.Count - 1 do begin
      p_obj := TGenEngine.TPatriarchObj(lst[i]);

      if not(p_obj.IRec.Patriarch) then p_sign := '' else p_sign := '[*] ';

      item := ListPatriarchs.AddItem(p_sign + TGenEngine.GetNameStr(p_obj.IRec), p_obj.IRec);
      item.SubItems.Add(p_obj.IBirthYear.ToString());
      item.SubItems.Add(p_obj.IDescendantsCount.ToString());
      item.SubItems.Add(p_obj.IDescGenerations.ToString());
      //item.SubItems.Add(GetLinks(p_obj));
    end;
  finally
    lst.Free;
    ListPatriarchs.EndUpdate();
  end;
end;

procedure TfmTreeTools.ListPatriarchsDblClick(sender: System.Object; e: System.EventArgs);
var
  item: TExtListItem;
  i_rec: TGEDCOMIndividualRecord;
begin
  item := ListPatriarchs.SelectedItem();
  if (item = nil) then Exit;

  i_rec := TGEDCOMIndividualRecord(item.Data);
  if (i_rec = nil) then Exit;

  Base.SelectRecordByXRef(i_rec.XRef);
  Close;
end;

procedure TfmTreeTools.btnSetPatriarch_Click(sender: System.Object; e: System.EventArgs);
var
  item: TExtListItem;
  i_rec: TGEDCOMIndividualRecord;
begin
  try
    item := ListPatriarchs.SelectedItem;
    if (item = nil) then Exit;

    i_rec := TGEDCOMIndividualRecord(item.Data);
    if (i_rec = nil) then Exit;

    i_rec.Patriarch := True;
  finally
    btnPatSearch_Click(nil, nil);
    Base.ListsRefresh();
  end;
end;

procedure TfmTreeTools.PreparePatriarchsList();
begin
  Base.CreateListView(Panel3, ListPatriarchs);
  Include(ListPatriarchs.DoubleClick, ListPatriarchsDblClick);
  ListPatriarchs.AddListColumn(LSList[LSID_Patriarch], 400);
  ListPatriarchs.AddListColumn(LSList[LSID_Birth], 90);
  ListPatriarchs.AddListColumn(LSList[LSID_Descendants], 90);
  ListPatriarchs.AddListColumn(LSList[LSID_Generations], 90);
end;

procedure TfmTreeTools.btnUpdateSelect_Click(sender: System.Object; e: System.EventArgs);
var
  tmt: Integer;
begin
  if (OpenDialog1.ShowDialog() = System.Windows.Forms.DialogResult.OK) then begin
    edUpdateBase.Text := OpenDialog1.FileName;

    if RadioButton3.Checked then tmt := 0
    else
    if RadioButton3.Checked then tmt := 1;

    case tmt of
      0: TGenEngine.TreeMerge(Base.Tree, edUpdateBase.Text, mSyncRes);
      1: TGenEngine.TreeSync(Base.Tree, edUpdateBase.Text, mSyncRes);
    end;

    Base.ListsRefresh();
  end;
end;

procedure TfmTreeTools.RadioButton3_Click(sender: System.Object; e: System.EventArgs);
begin
  gbSyncType.Enabled := RadioButton4.Checked;
end;

procedure TfmTreeTools.PreparePlacesList();
begin
  Base.CreateListView(Panel4, ListPlaces);
  Include(ListPlaces.DoubleClick, ListPlacesDblClick);
  ListPlaces.AddListColumn(LSList[LSID_Place], 400);
  ListPlaces.AddListColumn(LSList[LSID_LinksCount], 100);
end;

{ TfmTreeTools.TPlaceObj }

constructor TfmTreeTools.TPlaceObj.Create;
begin
  inherited Create;
  Facts := TList.Create;
end;

destructor TfmTreeTools.TPlaceObj.Destroy;
begin
  Facts.Free;
  inherited Destroy;
end;

procedure TfmTreeTools.PlacesClear();
var
  i: Integer;
begin
  for i := FPlaces.Count - 1 downto 0 do
    FPlaces.Objects[i].Free;

  FPlaces.Clear;
end;

procedure TfmTreeTools.CheckPlaces();

  procedure PrepareEvent(aEvent: TGEDCOMCustomEvent);
  var
    place_obj: TPlaceObj;
    place_str: string;
    loc: TGEDCOMLocationRecord;
    idx: Integer;
  begin
    place_str := aEvent.Detail.Place.StringValue;
    if (place_str = '') then Exit;    

    loc := TGEDCOMLocationRecord(aEvent.Detail.Place.Location.Value);
    if (loc <> nil) then place_str := '[*] ' + place_str;

    idx := FPlaces.IndexOf(place_str);
    if (idx >= 0) then begin
      place_obj := TPlaceObj(FPlaces.Objects[idx]);
    end else begin
      place_obj := TPlaceObj.Create;
      place_obj.Name := place_str;
      FPlaces.AddObject(place_str, place_obj);
    end;

    place_obj.Facts.Add(aEvent);
  end;

var
  i, k: Integer;
  iRec: TGEDCOMIndividualRecord;
  fRec: TGEDCOMFamilyRecord;
  item: TExtListItem;
  place_obj: TPlaceObj;
begin
  TfmProgress.ProgressInit(FTree.RecordsCount, LSList[LSID_PlacesPrepare]);
  ListPlaces.BeginUpdate();
  try
    PlacesClear();

    for i := 0 to FTree.RecordsCount - 1 do begin
      TfmProgress.ProgressStep();

      if (FTree.Records[i] is TGEDCOMIndividualRecord) then begin
        iRec := FTree.Records[i] as TGEDCOMIndividualRecord;
        for k := 0 to iRec.IndividualEventsCount - 1 do PrepareEvent(iRec.IndividualEvents[k]);
      end
      else
      if (FTree.Records[i] is TGEDCOMFamilyRecord) then begin
        fRec := FTree.Records[i] as TGEDCOMFamilyRecord;
        for k := 0 to fRec.FamilyEventCount - 1 do PrepareEvent(fRec.FamilyEvents[k]);
      end;
    end;

    ListPlaces.Items.Clear();
    for i := 0 to FPlaces.Count - 1 do begin
      place_obj := TPlaceObj(FPlaces.Objects[i]);

      item := ListPlaces.AddItem(FPlaces[i], place_obj);
      item.SubItems.Add(place_obj.Facts.Count.ToString());
    end;
  finally
    ListPlaces.EndUpdate();
    TfmProgress.ProgressDone();
  end;
end;

procedure TfmTreeTools.ListPlacesDblClick(sender: System.Object; e: System.EventArgs);
var
  item: TExtListItem;
  p_obj: TPlaceObj;
  loc: TGEDCOMLocationRecord;
  i: Integer;
  event: TGEDCOMCustomEvent;
begin
  item := ListPlaces.SelectedItem();
  if (item = nil) then Exit;

  p_obj := TPlaceObj(item.Data);
  if (p_obj = nil) then Exit;

  if (Pos('[*]', p_obj.Name) = 1) then begin
    TGKUtils.ShowMessage(LSList[LSID_PlaceAlreadyInBook]);
    Exit;
  end;

  loc := TGEDCOMLocationRecord(Base.SelectRecord(rtLocation, [p_obj.Name]));
  if (loc <> nil) then begin
    for i := 0 to p_obj.Facts.Count - 1 do begin
      event := TGEDCOMCustomEvent(p_obj.Facts[i]);
      event.Detail.Place.StringValue := loc.Name;
      event.Detail.Place.Location.Value := loc;
    end;
    ///
    CheckPlaces();
    Base.ListsRefresh(False);
  end;
end;

procedure TfmTreeTools.btnIntoList_Click(sender: System.Object; e: System.EventArgs);
begin
  ListPlacesDblClick(nil, nil);
end;

procedure TfmTreeTools.btnHelp_Click(sender: System.Object; e: System.EventArgs);
begin
  fmGEDKeeper.ShowHelpTopic(HelpTopics[PageControl.TabIndex]);
end;

end.
