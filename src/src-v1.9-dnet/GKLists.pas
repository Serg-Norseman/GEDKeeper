unit GKLists; {trans:fin}

{$I GEDKeeper2.inc}

{.$DEFINE FAST_LISTS} // вызывает глюки с сортировкой

interface

uses
  System.Drawing, System.ComponentModel, System.Windows.Forms, System.Resources,
  VCLStub, GedCom551, GKEngine, GKCommon, GKCtrls, GKUtils, GKLangs;

type
  TSheetList = class(System.Windows.Forms.ContainerControl)
  private
    type
      TModifyEvent = procedure (Sender: TObject; ItemData: TObject; Action: TGenEngine.TRecAction) of object;
      TListButton = (lbAdd, lbEdit, lbDelete, lbJump, lbMoveUp, lbMoveDown);
      TListButtonSet = TEnumSet;
  private
    FBtnAdd: System.Windows.Forms.ToolBarButton;
    FBtnDelete: System.Windows.Forms.ToolBarButton;
    FBtnEdit: System.Windows.Forms.ToolBarButton;
    FBtnLinkJump: System.Windows.Forms.ToolBarButton;
    FBtnMoveUp: System.Windows.Forms.ToolBarButton;
    FBtnMoveDown: System.Windows.Forms.ToolBarButton;
    FToolBar: System.Windows.Forms.ToolBar;

    FButtons: TListButtonSet;
    FList: TGKListView;
    FOnModify: TModifyEvent;
    FReadOnly: Boolean;

    procedure ButtonClick(sender: System.Object; e: System.Windows.Forms.ToolBarButtonClickEventArgs);
    procedure List_DoubleClick(sender: System.Object; e: System.EventArgs);
    procedure List_KeyDown(sender: System.Object; e: System.Windows.Forms.KeyEventArgs);
    procedure SheetShow(sender: System.Object; e: System.EventArgs);
    procedure SetButtons(const Value: TListButtonSet);

    function GetSelectedData(): TObject;

    {procedure LBDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure LBMeasureItem(Control: TWinControl; Index: Integer;
      var Height: Integer);}
    procedure SetReadOnly(const Value: Boolean);
  public
    constructor Create(AOwner: System.Windows.Forms.Control);
    destructor Destroy; override;

    property Buttons: TListButtonSet read FButtons write SetButtons;
    property List: TGKListView read FList;
    property ToolBar: System.Windows.Forms.ToolBar read FToolBar;

    procedure ItemAdd();
    procedure ItemEdit();
    procedure ItemDelete();
    procedure ItemJump();
    procedure ItemMoveUp();
    procedure ItemMoveDown();

    procedure Columns_BeginUpdate();
    procedure Columns_Clear();
    procedure Columns_EndUpdate();

    procedure AddColumn(aCaption: string; aWidth: Integer; aAutoSize: Boolean = False);

    property OnModify: TModifyEvent read FOnModify write FOnModify;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
  end;
  
{==============================================================================}

type
  TFilter = class(TObject)
  public
    type
      TGroupMode = (gmAll, gmNone, gmAny, gmSelected);
  end;

  TPersonsFilter = class(TFilter)
  private
    type
      TListFilterMode = (flCommon, flSelector);
  private
    Back_AliveBeforeDate: string;
    Back_GroupMode: TFilter.TGroupMode;
    Back_GroupRef: string;
    Back_LifeMode: TGenEngine.TLifeMode;
    Back_Name: string;
    Back_PatriarchOnly: Boolean;
    Back_Residence: string;
    Back_Sex: TGEDCOMObject.TGEDCOMSex;
    Back_SourceMode: TFilter.TGroupMode;
    Back_SourceRef: string;
    Back_EventVal: string;
  public
    AliveBeforeDate: string;
    GroupMode: TFilter.TGroupMode;
    GroupRef: string;
    LifeMode: TGenEngine.TLifeMode;
    Name: string;
    PatriarchOnly: Boolean;
    Residence: string;
    Sex: TGEDCOMObject.TGEDCOMSex;
    SourceMode: TFilter.TGroupMode;
    SourceRef: string;
    EventVal: string;

    List: TListFilterMode;
    ChildSelector: Boolean; // special mode
    TimeLineYear: Integer;

    constructor Create();

    procedure Clear();
    procedure Backup();
    procedure Restore();
  end;

  TListManager = class(TObject)
  private
    FTree: TGEDCOMTree;
  public
    constructor Create(aTree: TGEDCOMTree); virtual;
    destructor Destroy; override;

    procedure UpdateTitles(aList: TGKListView; isMain: Boolean);

    function  CheckFilter(aFilter: TPersonsFilter; aShieldState: TGenEngine.TShieldState): Boolean; virtual; abstract;
    procedure Fetch(aRec: TGEDCOMRecord); virtual; abstract;
    function  GetColumnValue(aColIndex: Integer; isMain: Boolean): string; virtual;
    procedure InitFilter(aFilter: TPersonsFilter); virtual;
    procedure UpdateItem(aItem: TExtListItem; isMain: Boolean); virtual; abstract;
    procedure UpdateColumns(aList: TGKListView; isMain: Boolean); virtual; abstract;
    procedure GetRow(aRec: TGEDCOMRecord; isMain: Boolean; var aRow: string); virtual;
  end;

  TIndividualListMan = class(TListManager)
  strict private
    type
      TColRec = record
        col_type: Byte;
        col_subtype: Byte;
      end;

      TColumnsMap = array [Byte] of TColRec;
      
  private
    FRec: TGEDCOMIndividualRecord;

    filter_grp: TGEDCOMGroupRecord;
    filter_abd: DateTime;
    filter_source: TGEDCOMSourceRecord;

    // timeline runtime
    FYearMin, FYearMax: Integer;

    // filter runtime
    age_year: Integer;

    function GetGroups(): string;
    function HasPlace(aFilter: TPersonsFilter): Boolean;
    function HasEventVal(aFilter: TPersonsFilter): Boolean;
    procedure SetColMap(aType, aSubType: Byte; var cols: Integer);
  protected
    FColumnsMap: TColumnsMap;
  public
    function  CheckFilter(aFilter: TPersonsFilter; aShieldState: TGenEngine.TShieldState): Boolean; override;
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function  GetColumnValue(aColIndex: Integer; isMain: Boolean): string; override;
    procedure GetRow(aRec: TGEDCOMRecord; isMain: Boolean; var aRow: string); override;
    procedure InitFilter(aFilter: TPersonsFilter); override;
    procedure UpdateItem(aItem: TExtListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TGKListView; isMain: Boolean); override;

    property YearMin: Integer read FYearMin;
    property YearMax: Integer read FYearMax;
  end;

  TFamilyListMan = class(TListManager)
  private
    FRec: TGEDCOMFamilyRecord;
  public
    function  CheckFilter(aFilter: TPersonsFilter; aShieldState: TGenEngine.TShieldState): Boolean; override;
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function  GetColumnValue(aColIndex: Integer; isMain: Boolean): string; override;
    procedure GetRow(aRec: TGEDCOMRecord; isMain: Boolean; var aRow: string); override;
    procedure UpdateItem(aItem: TExtListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TGKListView; isMain: Boolean); override;
  end;

  TNoteListMan = class(TListManager)
  private
    FRec: TGEDCOMNoteRecord;
  public
    function  CheckFilter(aFilter: TPersonsFilter; aShieldState: TGenEngine.TShieldState): Boolean; override;
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function  GetColumnValue(aColIndex: Integer; isMain: Boolean): string; override;
    procedure GetRow(aRec: TGEDCOMRecord; isMain: Boolean; var aRow: string); override;
    procedure UpdateItem(aItem: TExtListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TGKListView; isMain: Boolean); override;
  end;

  TMultimediaListMan = class(TListManager)
  private
    FRec: TGEDCOMMultimediaRecord;
  public
    function  CheckFilter(aFilter: TPersonsFilter; aShieldState: TGenEngine.TShieldState): Boolean; override;
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function  GetColumnValue(aColIndex: Integer; isMain: Boolean): string; override;
    procedure GetRow(aRec: TGEDCOMRecord; isMain: Boolean; var aRow: string); override;
    procedure UpdateItem(aItem: TExtListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TGKListView; isMain: Boolean); override;
  end;

  TSourceListMan = class(TListManager)
  private
    FRec: TGEDCOMSourceRecord;
  public
    function  CheckFilter(aFilter: TPersonsFilter; aShieldState: TGenEngine.TShieldState): Boolean; override;
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function  GetColumnValue(aColIndex: Integer; isMain: Boolean): string; override;
    procedure GetRow(aRec: TGEDCOMRecord; isMain: Boolean; var aRow: string); override;
    procedure UpdateItem(aItem: TExtListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TGKListView; isMain: Boolean); override;
  end;

  TRepositoryListMan = class(TListManager)
  private
    FRec: TGEDCOMRepositoryRecord;
  public
    function  CheckFilter(aFilter: TPersonsFilter; aShieldState: TGenEngine.TShieldState): Boolean; override;
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function  GetColumnValue(aColIndex: Integer; isMain: Boolean): string; override;
    procedure GetRow(aRec: TGEDCOMRecord; isMain: Boolean; var aRow: string); override;
    procedure UpdateItem(aItem: TExtListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TGKListView; isMain: Boolean); override;
  end;

  TGroupListMan = class(TListManager)
  private
    FRec: TGEDCOMGroupRecord;
  public
    function  CheckFilter(aFilter: TPersonsFilter; aShieldState: TGenEngine.TShieldState): Boolean; override;
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function  GetColumnValue(aColIndex: Integer; isMain: Boolean): string; override;
    procedure GetRow(aRec: TGEDCOMRecord; isMain: Boolean; var aRow: string); override;
    procedure UpdateItem(aItem: TExtListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TGKListView; isMain: Boolean); override;
  end;

  TResearchListMan = class(TListManager)
  private
    FRec: TGEDCOMResearchRecord;
  public
    function  CheckFilter(aFilter: TPersonsFilter; aShieldState: TGenEngine.TShieldState): Boolean; override;
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function  GetColumnValue(aColIndex: Integer; isMain: Boolean): string; override;
    procedure GetRow(aRec: TGEDCOMRecord; isMain: Boolean; var aRow: string); override;
    procedure UpdateItem(aItem: TExtListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TGKListView; isMain: Boolean); override;
  end;

  TTaskListMan = class(TListManager)
  private
    FRec: TGEDCOMTaskRecord;
  public
    function  CheckFilter(aFilter: TPersonsFilter; aShieldState: TGenEngine.TShieldState): Boolean; override;
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function  GetColumnValue(aColIndex: Integer; isMain: Boolean): string; override;
    procedure GetRow(aRec: TGEDCOMRecord; isMain: Boolean; var aRow: string); override;
    procedure UpdateItem(aItem: TExtListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TGKListView; isMain: Boolean); override;
  end;

  TCommunicationListMan = class(TListManager)
  private
    FRec: TGEDCOMCommunicationRecord;
  public
    function  CheckFilter(aFilter: TPersonsFilter; aShieldState: TGenEngine.TShieldState): Boolean; override;
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function  GetColumnValue(aColIndex: Integer; isMain: Boolean): string; override;
    procedure GetRow(aRec: TGEDCOMRecord; isMain: Boolean; var aRow: string); override;
    procedure UpdateItem(aItem: TExtListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TGKListView; isMain: Boolean); override;
  end;

  TLocationListMan = class(TListManager)
  private
    FRec: TGEDCOMLocationRecord;
  public
    function  CheckFilter(aFilter: TPersonsFilter; aShieldState: TGenEngine.TShieldState): Boolean; override;
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function  GetColumnValue(aColIndex: Integer; isMain: Boolean): string; override;
    procedure GetRow(aRec: TGEDCOMRecord; isMain: Boolean; var aRow: string); override;
    procedure UpdateItem(aItem: TExtListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TGKListView; isMain: Boolean); override;
  end;

{==============================================================================}

type
  TRecordsView = class(TGKListView)
  strict private
    type
      TRow = class
      public
        Index: Integer;
        Value: string;
        Age: Integer;
      end;
  private
    FContentList: TList;
    FFilteredCount: Integer;
    FIsMainList: Boolean;
    FListMan: TListManager;
    FRecordType: TGEDCOMRecord.TGEDCOMRecordType;
    FTotalCount: Integer;
    FTree: TGEDCOMTree;

    {$IFDEF FAST_LISTS}
    FCacheList: TObjectList;

    function CacheQuery(aIndex: Integer): string;
    procedure ListDataHint(Sender: TObject; StartIndex, EndIndex: Integer);
    {$ENDIF}

    //procedure ListCustomDrawItem(Sender: TCustomListView;
    //  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure List_ColumnClick(sender: System.Object; e: System.Windows.Forms.ColumnClickEventArgs);
    //procedure ListGetItemData(Sender: TObject; Item: TListItem);
    procedure SetRecordType(const Value: TGEDCOMRecord.TGEDCOMRecordType);
    function xCompare(Item1, Item2: TObject): Integer;
  public
    constructor Create(AOwner: System.Windows.Forms.Control); override;
    destructor Destroy; override;

    procedure UpdateContents(aShieldState: TGenEngine.TShieldState; aTitles: Boolean;
      aFilter: TPersonsFilter; aAutoSizeColumn: Integer = -1);
    procedure UpdateTitles();

    procedure DeleteRecord(aRec: TGEDCOMRecord);
    function  GetSelectedRecord(): TGEDCOMRecord;
    procedure SelectItemByRec(aRec: TGEDCOMRecord);

    property ContentList: TList read FContentList;
    property FilteredCount: Integer read FFilteredCount;
    property IsMainList: Boolean read FIsMainList write FIsMainList;
    property ListMan: TListManager read FListMan;
    property RecordType: TGEDCOMRecord.TGEDCOMRecordType read FRecordType write SetRecordType;
    property TotalCount: Integer read FTotalCount;
    property Tree: TGEDCOMTree read FTree write FTree;
  end;

implementation

uses GKMain;

{==============================================================================}

{ TSheetList }

constructor TSheetList.Create(AOwner: System.Windows.Forms.Control);
type
  TArrayOfSystem_Windows_Forms_ToolBarButton = array of System.Windows.Forms.ToolBarButton;
begin
  inherited Create();

  (AOwner as System.Windows.Forms.Control).SuspendLayout;

  Dock := System.Windows.Forms.DockStyle.Fill;
  (AOwner as System.Windows.Forms.Control).Controls.Add(Self);

  (AOwner as System.Windows.Forms.Control).ResumeLayout(False);

  // Alert
  {
  if (AOwner is TabPage)
  then Include(Self.Load, SheetShow);

  FActionAdd.ShortCut := TextToShortCut('Ctrl+I');
  FActionEdit.ShortCut := TextToShortCut('Ctrl+Enter');
  FActionDelete.ShortCut := TextToShortCut('Ctrl+D');
  }

  Self.SuspendLayout;

  FBtnMoveDown := System.Windows.Forms.ToolBarButton.Create();
  FBtnMoveDown.ImageIndex := 30;
  FBtnMoveDown.ToolTipText := LSList[LSID_RecordMoveDown];

  FBtnMoveUp := System.Windows.Forms.ToolBarButton.Create();
  FBtnMoveUp.ImageIndex := 29;
  FBtnMoveUp.ToolTipText := LSList[LSID_RecordMoveUp];

  FBtnLinkJump := System.Windows.Forms.ToolBarButton.Create();
  FBtnLinkJump.ImageIndex := 28;
  FBtnLinkJump.ToolTipText := LSList[LSID_RecordGoto];

  FBtnDelete := System.Windows.Forms.ToolBarButton.Create();
  FBtnDelete.ImageIndex := 5;
  FBtnDelete.ToolTipText := LSList[LSID_MIRecordDelete];

  FBtnEdit := System.Windows.Forms.ToolBarButton.Create();
  FBtnEdit.ImageIndex := 4;
  FBtnEdit.ToolTipText := LSList[LSID_MIRecordEdit];

  FBtnAdd := System.Windows.Forms.ToolBarButton.Create();
  FBtnAdd.ImageIndex := 3;
  FBtnAdd.ToolTipText := LSList[LSID_MIRecordAdd];

  FToolBar := System.Windows.Forms.ToolBar.Create();
  FToolBar.Appearance := System.Windows.Forms.ToolBarAppearance.Flat;
  FToolBar.Buttons.AddRange(TArrayOfSystem_Windows_Forms_ToolBarButton.Create(
    FBtnAdd, FBtnEdit, FBtnDelete, FBtnLinkJump, FBtnMoveUp, FBtnMoveDown));
  FToolBar.ImageList := fmGEDKeeper.ImageList_Buttons;
  FToolBar.ShowToolTips := True;
  Include(Self.FToolBar.ButtonClick, Self.ButtonClick);

  FList := TGKListView.Create(nil);
  FList.Location := System.Drawing.Point.Create(0, 0);
  FList.Size := System.Drawing.Size.Create(500, 290);
  FList.HideSelection := False;
  FList.LabelEdit := False;
  FList.FullRowSelect := True;
  FList.View := System.Windows.Forms.View.Details;
  Include(FList.DoubleClick, List_DoubleClick);
  Include(FList.KeyDown, List_KeyDown);

  FToolBar.Dock := System.Windows.Forms.DockStyle.Right;
  FList.Dock := System.Windows.Forms.DockStyle.Fill;

  Self.Controls.Add(FList);
  Self.Controls.Add(FToolBar);

  Self.Controls.SetChildIndex(FList, 0);
  Self.Controls.SetChildIndex(FToolBar, 1);

  Self.ResumeLayout(False);

  SetButtons(TEnumSet.Create([lbAdd, lbEdit, lbDelete]));
end;

destructor TSheetList.Destroy;
begin
  FList.Free;

  FBtnLinkJump.Free;
  FBtnMoveUp.Free;
  FBtnMoveDown.Free;
  FBtnDelete.Free;
  FBtnEdit.Free;
  FBtnAdd.Free;

  FToolBar.Free;

  inherited Destroy;
end;

procedure TSheetList.ButtonClick(sender: System.Object; e: System.Windows.Forms.ToolBarButtonClickEventArgs);
begin
  if (e.Button = FBtnAdd) then ItemAdd()
  else
  if (e.Button = FBtnEdit) then ItemEdit()
  else
  if (e.Button = FBtnDelete) then ItemDelete()
  else
  if (e.Button = FBtnLinkJump) then ItemJump()
  else
  if (e.Button = FBtnMoveUp) then ItemMoveUp()
  else
  if (e.Button = FBtnMoveDown) then ItemMoveDown();
end;

procedure TSheetList.List_KeyDown(sender: System.Object; e: System.Windows.Forms.KeyEventArgs);
begin
  if (e.Control) then
    case e.KeyCode of
      Keys.I: ItemAdd();
      Keys.D: ItemDelete();
      Keys.Return: ItemEdit();
    end;
end;

function TSheetList.GetSelectedData(): TObject;
begin
  Result := nil;

  if (TGKListView(FList).SelectedItem <> nil)
  then Result := TGKListView(FList).SelectedItem.Data;
end;

procedure TSheetList.ItemAdd();
begin
  if not(FReadOnly) and Assigned(FOnModify) then FOnModify(Self, nil, raAdd);
end;

procedure TSheetList.ItemEdit();
begin
  if not(FReadOnly) and Assigned(FOnModify) and (GetSelectedData() <> nil)
  then FOnModify(Self, GetSelectedData(), raEdit);
end;

procedure TSheetList.ItemDelete();
begin
  if not(FReadOnly) and Assigned(FOnModify) and (GetSelectedData() <> nil)
  then FOnModify(Self, GetSelectedData(), raDelete);
end;

procedure TSheetList.ItemJump();
begin
  if Assigned(FOnModify) and (GetSelectedData() <> nil)
  then FOnModify(Self, GetSelectedData(), raJump);
end;

procedure TSheetList.ItemMoveDown();
begin
  if not(FReadOnly) and Assigned(FOnModify) and (GetSelectedData() <> nil)
  then FOnModify(Self, GetSelectedData(), raMoveDown);
end;

procedure TSheetList.ItemMoveUp();
begin
  if not(FReadOnly) and Assigned(FOnModify) and (GetSelectedData() <> nil)
  then FOnModify(Self, GetSelectedData(), raMoveUp);
end;

procedure TSheetList.List_DoubleClick(sender: System.Object; e: System.EventArgs);
begin
  ItemEdit();
end;

procedure TSheetList.SheetShow(sender: System.Object; e: System.EventArgs);
begin
  FList.Focus();
end;

procedure TSheetList.SetButtons(const Value: TListButtonSet);
begin
  FButtons := Value;

  FBtnAdd.Visible := (FButtons.InSet(lbAdd));
  FBtnDelete.Visible := (FButtons.InSet(lbDelete));
  FBtnEdit.Visible := (FButtons.InSet(lbEdit));
  FBtnLinkJump.Visible := (FButtons.InSet(lbJump));
  FBtnMoveUp.Visible := (FButtons.InSet(lbMoveUp));
  FBtnMoveDown.Visible := (FButtons.InSet(lbMoveDown));

  FToolBar.Visible := not(FButtons.IsEmpty);  
end;

(*
procedure TSheetList.LBMeasureItem(Control: TWinControl; Index: Integer;
  var Height: Integer);
var
  item: string;
  rt: TRect;
  image: TImage;
  lst: TListBox;
  params: TDrawTextParams;
begin
  // Don't waste time with this on Index = -1
  if (Index > -1) then begin
    lst := TListBox(Control);
    // Create a temporary canvas to calculate the height
    image := TImage.Create(lst);
    try
      rt := lst.ClientRect;
      item := lst.Items.Strings[Index];
      image.Canvas.Font := lst.Font;

      {$IFNDEF DELPHI_NET}
      FillChar(params, SizeOf(TDrawTextParams), #0);
      {$ENDIF}

      params.cbSize := SizeOf(TDrawTextParams);
      params.iLeftMargin := 3;
      params.iRightMargin := 3;

      Height := DrawTextEx(image.Canvas.Handle,
        item, -1, rt, DT_WORDBREAK or DT_CALCRECT, params);
    finally
      image.Free;
    end;
  end;
end;

procedure TSheetList.LBDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  item: string;
  params: TDrawTextParams;
begin
  TListBox(Control).Canvas.FillRect(Rect);
  item := TListBox(Control).Items.Strings[Index];

  {$IFNDEF DELPHI_NET}
  FillChar(params, SizeOf(TDrawTextParams), #0);
  {$ENDIF}
  params.cbSize := SizeOf(TDrawTextParams);
  params.iLeftMargin := 3;
  params.iRightMargin := 3;

  DrawTextEx(TListBox(Control).Canvas.Handle, item, -1, Rect, DT_WORDBREAK, params);
end;
*)

procedure TSheetList.Columns_BeginUpdate();
begin
  //if (FList is TGKListView)
  //then (FList as TGKListView).Columns.BeginUpdate(); Alert
end;

procedure TSheetList.Columns_Clear();
begin
  if (FList is TGKListView)
  then (FList as TGKListView).Columns.Clear();
end;

procedure TSheetList.Columns_EndUpdate();
begin
  //if (FList is TGKListView)
  //then (FList as TGKListView).Columns.EndUpdate(); Alert
end;

procedure TSheetList.AddColumn(aCaption: string; aWidth: Integer; aAutoSize: Boolean);
begin
  FList.AddListColumn(aCaption, aWidth, aAutoSize);
end;

procedure TSheetList.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;

  FBtnAdd.Enabled := not(FReadOnly);
  FBtnDelete.Enabled := not(FReadOnly);
  FBtnEdit.Enabled := not(FReadOnly);
  FBtnMoveUp.Enabled := not(FReadOnly);
  FBtnMoveDown.Enabled := not(FReadOnly);

  if FReadOnly
  then FList.BackColor := System.Drawing.SystemColors.Control
  else FList.BackColor := System.Drawing.SystemColors.Window;
end;

{==============================================================================}

{ TPersonsFilter }

constructor TPersonsFilter.Create;
begin
  inherited Create;
  Clear();
end;

procedure TPersonsFilter.Backup();
begin
  Back_AliveBeforeDate := AliveBeforeDate;
  Back_GroupMode := GroupMode;
  Back_GroupRef := GroupRef;
  Back_LifeMode := LifeMode;
  Back_Name := Name;
  Back_PatriarchOnly := PatriarchOnly;
  Back_Residence := Residence;
  Back_Sex := Sex;
  Back_SourceMode := SourceMode;
  Back_SourceRef := SourceRef;
  Back_EventVal := EventVal;
end;

procedure TPersonsFilter.Restore();
begin
  AliveBeforeDate := Back_AliveBeforeDate;
  GroupMode := Back_GroupMode;
  GroupRef := Back_GroupRef;
  LifeMode := Back_LifeMode;
  Name := Back_Name;
  PatriarchOnly := Back_PatriarchOnly;
  Residence := Back_Residence;
  Sex := Back_Sex;
  SourceMode := Back_SourceMode;
  SourceRef := Back_SourceRef;
  EventVal := Back_EventVal;
end;

procedure TPersonsFilter.Clear();
begin
  GroupMode := gmAll;
  GroupRef := '';
  if (LifeMode <> lmTimeLine) then begin
    LifeMode := lmAll;
    TimeLineYear := -1;
  end;
  Name := '*';
  AliveBeforeDate := '';
  PatriarchOnly := False;
  Residence := '*';
  Sex := svNone;
  SourceMode := gmAll;
  SourceRef := '';
  EventVal := '*';
end;

{ TListManager }

constructor TListManager.Create(aTree: TGEDCOMTree);
begin
  inherited Create;
  FTree := aTree;
end;

destructor TListManager.Destroy;
begin
  inherited Destroy;
end;

function TListManager.GetColumnValue(aColIndex: Integer; isMain: Boolean): string;
begin
  Result := '';
end;

procedure TListManager.GetRow(aRec: TGEDCOMRecord; isMain: Boolean; var aRow: string);
begin
  Fetch(aRec);
  //aRow := GetXRefNum(aRec); // maybe preliminary zero
  aRow := TGenEngine.GetId(aRec).ToString();
end;

procedure TListManager.InitFilter(aFilter: TPersonsFilter);
begin
  //
end;

procedure TListManager.UpdateTitles(aList: TGKListView; isMain: Boolean);
begin
  //aList.Columns.BeginUpdate;Alert
  try
    aList.Columns.Clear;
    UpdateColumns(aList, isMain);
  finally
    //aList.Columns.EndUpdate;Alert
  end;
end;

{ TIndividualListMan }

procedure TIndividualListMan.Fetch(aRec: TGEDCOMRecord);
begin
  FRec := TGEDCOMIndividualRecord(aRec);
end;

procedure TIndividualListMan.InitFilter(aFilter: TPersonsFilter);
begin
  // filters
  if (aFilter.AliveBeforeDate = '') or (aFilter.AliveBeforeDate = '  .  .    ')
  then filter_abd := DateTime.Create(0)
  else filter_abd := DateTime.Parse(aFilter.AliveBeforeDate);

  if (aFilter.LifeMode <> lmTimeLine)
  then age_year := -1
  else age_year := aFilter.TimeLineYear;

  if (aFilter.GroupRef = '')
  then filter_grp := nil
  else filter_grp := FTree.XRefIndex_Find(aFilter.GroupRef) as TGEDCOMGroupRecord;

  if (aFilter.SourceRef = '')
  then filter_source := nil
  else filter_source := FTree.XRefIndex_Find(aFilter.SourceRef) as TGEDCOMSourceRecord;

  // timeline
  FYearMin := 10000;
  FYearMax := 0;
end;

function TIndividualListMan.HasPlace(aFilter: TPersonsFilter): Boolean;
var
  i: Integer;
  place: string;
  addr: Boolean;
begin
  Result := False;

  addr := fmGEDKeeper.Options.PlacesWithAddress;

  for i := 0 to FRec.IndividualEventsCount - 1 do begin
    place := TGenEngine.GetPlaceStr(FRec.IndividualEvents[i], addr);
    Result := TGenEngine.IsMatchesMask(place, aFilter.Residence);
    if (Result) then Exit;
  end;
end;

function TIndividualListMan.HasEventVal(aFilter: TPersonsFilter): Boolean;
var
  i: Integer;
begin
  Result := False;

  for i := 0 to FRec.IndividualEventsCount - 1 do begin
    Result := TGenEngine.IsMatchesMask(FRec.IndividualEvents[i].StringValue, aFilter.EventVal);
    if (Result) then Exit;
  end;
end;

function TIndividualListMan.CheckFilter(aFilter: TPersonsFilter; aShieldState: TGenEngine.TShieldState): Boolean;
var
  bdt, ddt: DateTime;
  isLive: Boolean;
  bdy, ddy, i, y: Integer;
  m, d: Word;
  ev, bd_ev, dd_ev: TGEDCOMCustomEvent;
begin
  Result := False;

  ///
  bd_ev := nil;
  dd_ev := nil;
  for i := 0 to FRec.IndividualEventsCount - 1 do begin
    ev := FRec.IndividualEvents[i];

    if (ev.Name = 'BIRT') and (bd_ev = nil) then begin
      bd_ev := ev;

      // timeline begin
      TGenEngine.GetIndependentDate(ev.Detail.Date.Value, y, m, d);
      if (y > 0) then begin
        if (FYearMin > y) then FYearMin := y;
        if (FYearMax < y) then FYearMax := y;
      end;
      // timeline end
    end
    else
    if (ev.Name = 'DEAT') and (dd_ev = nil) then dd_ev := ev;
  end;
  ///

  if (FRec.Restriction = rnPrivacy) and (aShieldState <> ssNone)
  then Exit;

  if ((aFilter.Sex <> svNone) and (FRec.Sex <> aFilter.Sex))
  or ((aFilter.Name <> '*') and not(TGenEngine.IsMatchesMask(TGenEngine.GetNameStr(FRec), aFilter.Name)))
  or ((aFilter.Residence <> '*') and not(HasPlace(aFilter)))
  or ((aFilter.EventVal <> '*') and not(HasEventVal(aFilter)))
  or ((aFilter.PatriarchOnly and not(FRec.Patriarch)))
  then Exit;

  isLive := (dd_ev = nil);

  case aFilter.LifeMode of
    lmAll: ;
    lmOnlyAlive:
      if not(isLive) then Exit;
    lmOnlyDead:
      if (isLive) then Exit;
    lmAliveBefore: begin
      if (bd_ev = nil) then bdt := DateTime.Create(0) else bdt := TGenEngine.GEDCOMDateToDate(bd_ev.Detail.Date.Value);
      if (dd_ev = nil) then ddt := DateTime.Create(0) else ddt := TGenEngine.GEDCOMDateToDate(dd_ev.Detail.Date.Value);

      if not(((bdt.Ticks = 0) or ((bdt.Ticks <> 0) and (bdt < filter_abd)))
         and ((ddt.Ticks = 0) or ((ddt.Ticks <> 0) and (ddt > filter_abd))))
      then Exit;
    end;
    lmTimeLine: begin
      if (bd_ev = nil) then bdy := -1 else TGenEngine.GetIndependentDate(bd_ev.Detail.Date.Value, bdy, m, d);
      if (dd_ev = nil) then ddy := -1 else TGenEngine.GetIndependentDate(dd_ev.Detail.Date.Value, ddy, m, d);

      if (age_year > 0)
      and not(((bdy > 0) and (bdy < age_year))
          and ((ddy <= 0) or ((ddy > 0) and (ddy > age_year))))
      then Exit;
    end;
  end;

  case aFilter.GroupMode of
    gmAll: ;
    gmNone: if (FRec.GroupsCount <> 0) then Exit;
    gmAny: if (FRec.GroupsCount = 0) then Exit;
    gmSelected: if (FRec.IndexOfGroup(filter_grp) < 0) then Exit;
  end;

  case aFilter.SourceMode of
    gmAll: ;
    gmNone: if (FRec.SourceCitationsCount <> 0) then Exit;
    gmAny: if (FRec.SourceCitationsCount = 0) then Exit;
    gmSelected: if (FRec.IndexOfSource(filter_source) < 0) then Exit;
  end;

  // special mode
  if aFilter.ChildSelector and (FRec.ChildToFamilyLinksCount <> 0)
  then Exit;

  Result := True;
end;

function TIndividualListMan.GetGroups(): string;
var
  idx: Integer;
  grp: TGEDCOMGroupRecord;
begin
  Result := '';

  for idx := 0 to FRec.GroupsCount - 1 do begin
    grp := TGEDCOMGroupRecord(FRec.Groups[idx].Value);
    if (grp <> nil) then begin
      Result := Result + grp.Name;

      if (idx < FRec.GroupsCount - 1)
      then Result := Result + '; ';
    end;
  end;
end;

procedure TIndividualListMan.UpdateItem(aItem: TExtListItem; isMain: Boolean);
var
  i: Integer;
  pct: TGlobalOptions.TPersonColumnType;
  columns: TGlobalOptions.TPersonColumnsList;
  f, n, p: string;
  ev: TGEDCOMCustomEvent;
  bd_ev, dd_ev: TGEDCOMCustomEvent;
  residence, religion, nationality, education, occupation,
  caste, mili, mili_ind, mili_dis, mili_rank: string;
begin
  columns := fmGEDKeeper.Options.ListPersonsColumns;

  bd_ev := nil;
  dd_ev := nil;

  residence := '';
  religion := '';
  nationality := '';
  education := '';
  occupation := '';
  caste := '';
  mili := '';
  mili_ind := '';
  mili_dis := '';
  mili_rank := '';

  for i := 0 to FRec.IndividualEventsCount - 1 do begin
    ev := FRec.IndividualEvents[i];

    if (ev.Name = 'BIRT') and (bd_ev = nil) then bd_ev := ev
    else
    if (ev.Name = 'DEAT') and (dd_ev = nil) then dd_ev := ev
    else
    if (ev.Name = 'RESI') and (residence = '')
    then residence := TGenEngine.GetPlaceStr(ev, fmGEDKeeper.Options.PlacesWithAddress)
    else
    if (ev.Name = 'RELI') and (religion = '')
    then religion := ev.StringValue
    else
    if (ev.Name = 'NATI') and (nationality = '')
    then nationality := ev.StringValue
    else
    if (ev.Name = 'EDUC') and (education = '')
    then education := ev.StringValue
    else
    if (ev.Name = 'OCCU') and (occupation = '')
    then occupation := ev.StringValue
    else
    if (ev.Name = 'CAST') and (caste = '')
    then caste := ev.StringValue
    else
    if (ev.Name = '_MILI') and (mili = '')
    then mili := ev.StringValue
    else
    if (ev.Name = '_MILI_IND') and (mili_ind = '')
    then mili_ind := ev.StringValue
    else
    if (ev.Name = '_MILI_DIS') and (mili_dis = '')
    then mili_dis := ev.StringValue
    else
    if (ev.Name = '_MILI_RANK') and (mili_Rank = '')
    then mili_Rank := ev.StringValue;
  end;

  for i := 0 to High(columns) do begin
    pct := columns[i].colType;
    if not(columns[i].colActive) then Continue;

    case pct of
      pctPatriarch: begin
        if (FRec.Patriarch)
        then aItem.SubItems.Add('*')
        else aItem.SubItems.Add(' ');
      end;

      pctName: begin
        if (fmGEDKeeper.Options.DefNameFormat in [nfF_NP, nfF_N_P])
        then TGenEngine.GetNameParts(FRec, f, n, p);

        case fmGEDKeeper.Options.DefNameFormat of
          nfFNP: begin
            aItem.SubItems.Add(TGenEngine.GetNameStr(FRec));
          end;
          nfF_NP: begin
            aItem.SubItems.Add(f);
            aItem.SubItems.Add(n + ' ' + p);
          end;
          nfF_N_P: begin
            aItem.SubItems.Add(f);
            aItem.SubItems.Add(n);
            aItem.SubItems.Add(p);
          end;
        end;
      end;

      pctNick:
        aItem.SubItems.Add(TGenEngine.GetNickStr(FRec));

      pctSex:
        aItem.SubItems.Add(TGenEngine.SexStr(FRec.Sex)[1]);

      pctBirthDate:
        aItem.SubItems.Add(TGenEngine.GEDCOMEventToDateStr(bd_ev, fmGEDKeeper.Options.DefDateFormat));

      pctDeathDate:
        aItem.SubItems.Add(TGenEngine.GEDCOMEventToDateStr(dd_ev, fmGEDKeeper.Options.DefDateFormat));

      pctBirthPlace:
        aItem.SubItems.Add(TGenEngine.GetPlaceStr(bd_ev, False));

      pctDeathPlace:
        aItem.SubItems.Add(TGenEngine.GetPlaceStr(dd_ev, False));

      pctResidence:
        aItem.SubItems.Add(residence);

      pctAge:
        if (isMain) then aItem.SubItems.Add(TGenEngine.GetAge(FRec, age_year));

      pctLifeExpectancy:
        if (isMain) then aItem.SubItems.Add(TGenEngine.GetLifeExpectancy(FRec));

      pctDaysForBirth:
        if (isMain) then aItem.SubItems.Add(TGenEngine.GetDaysForBirth(FRec));

      pctGroups:
        if (isMain) then aItem.SubItems.Add(GetGroups());

      pctReligion:
        if (isMain) then aItem.SubItems.Add(religion);

      pctNationality:
        if (isMain) then aItem.SubItems.Add(nationality);

      pctEducation:
        if (isMain) then aItem.SubItems.Add(education);

      pctOccupation:
        if (isMain) then aItem.SubItems.Add(occupation);

      pctCaste:
        if (isMain) then aItem.SubItems.Add(caste);

      //
      pctMili:
        if (isMain) then aItem.SubItems.Add(mili);
      pctMiliInd:
        if (isMain) then aItem.SubItems.Add(mili_ind);
      pctMiliDis:
        if (isMain) then aItem.SubItems.Add(mili_dis);
      pctMiliRank:
        if (isMain) then aItem.SubItems.Add(mili_rank);
      //

      pctChangeDate:
        if (isMain) then aItem.SubItems.Add(FRec.ChangeDate.ToString());

      pctBookmark: if (isMain) then begin
        if (FRec.Bookmark)
        then aItem.SubItems.Add('*')
        else aItem.SubItems.Add(' ');
      end;
    end;
  end;
end;

procedure TIndividualListMan.GetRow(aRec: TGEDCOMRecord; isMain: Boolean; var aRow: string);
var
  i: Integer;
  pct: TGlobalOptions.TPersonColumnType;
  columns: TGlobalOptions.TPersonColumnsList;
  f, n, p: string;
  ev: TGEDCOMCustomEvent;
  bd_ev, dd_ev: TGEDCOMCustomEvent;
  residence, religion, nationality, education, occupation,
  caste, mili, mili_ind, mili_dis, mili_rank: string;
begin
  inherited;

  columns := fmGEDKeeper.Options.ListPersonsColumns;

  bd_ev := nil;
  dd_ev := nil;

  residence := '';
  religion := '';
  nationality := '';
  education := '';
  occupation := '';
  caste := '';
  mili := '';
  mili_ind := '';
  mili_dis := '';
  mili_rank := '';

  for i := 0 to FRec.IndividualEventsCount - 1 do begin
    ev := FRec.IndividualEvents[i];

    if (ev.Name = 'BIRT') and (bd_ev = nil) then bd_ev := ev
    else
    if (ev.Name = 'DEAT') and (dd_ev = nil) then dd_ev := ev
    else
    if (ev.Name = 'RESI') and (residence = '')
    then residence := TGenEngine.GetPlaceStr(ev, fmGEDKeeper.Options.PlacesWithAddress)
    else
    if (ev.Name = 'RELI') and (religion = '')
    then religion := ev.StringValue
    else
    if (ev.Name = 'NATI') and (nationality = '')
    then nationality := ev.StringValue
    else
    if (ev.Name = 'EDUC') and (education = '')
    then education := ev.StringValue
    else
    if (ev.Name = 'OCCU') and (occupation = '')
    then occupation := ev.StringValue
    else
    if (ev.Name = 'CAST') and (caste = '')
    then caste := ev.StringValue
    else
    if (ev.Name = '_MILI') and (mili = '')
    then mili := ev.StringValue
    else
    if (ev.Name = '_MILI_IND') and (mili_ind = '')
    then mili_ind := ev.StringValue
    else
    if (ev.Name = '_MILI_DIS') and (mili_dis = '')
    then mili_dis := ev.StringValue
    else
    if (ev.Name = '_MILI_RANK') and (mili_Rank = '')
    then mili_Rank := ev.StringValue;
  end;

  for i := 0 to High(columns) do begin
    pct := columns[i].colType;
    if not(columns[i].colActive) then Continue;

    case pct of
      pctPatriarch: begin
        if (FRec.Patriarch)
        then aRow := aRow+#0+'*'
        else aRow := aRow+#0+' ';
      end;

      pctName: begin
        if (fmGEDKeeper.Options.DefNameFormat in [nfF_NP, nfF_N_P])
        then TGenEngine.GetNameParts(FRec, f, n, p);

        case fmGEDKeeper.Options.DefNameFormat of
          nfFNP: begin
            aRow := aRow+#0+TGenEngine.GetNameStr(FRec);
          end;
          nfF_NP: begin
            aRow := aRow+#0+f;
            aRow := aRow+#0+n + ' ' + p;
          end;
          nfF_N_P: begin
            aRow := aRow+#0+f;
            aRow := aRow+#0+n;
            aRow := aRow+#0+p;
          end;
        end;
      end;

      pctNick:
        aRow := aRow+#0+TGenEngine.GetNickStr(FRec);

      pctSex:
        aRow := aRow+#0+TGenEngine.SexStr(FRec.Sex)[1];

      pctBirthDate:
        aRow := aRow+#0+TGenEngine.GEDCOMEventToDateStr(bd_ev, fmGEDKeeper.Options.DefDateFormat);

      pctDeathDate:
        aRow := aRow+#0+TGenEngine.GEDCOMEventToDateStr(dd_ev, fmGEDKeeper.Options.DefDateFormat);

      pctBirthPlace:
        aRow := aRow+#0+TGenEngine.GetPlaceStr(bd_ev, False);

      pctDeathPlace:
        aRow := aRow+#0+TGenEngine.GetPlaceStr(dd_ev, False);

      pctResidence:
        aRow := aRow+#0+residence;

      pctAge:
        if (isMain) then aRow := aRow+#0+TGenEngine.GetAge(FRec, age_year);

      pctLifeExpectancy:
        if (isMain) then aRow := aRow+#0+TGenEngine.GetLifeExpectancy(FRec);

      pctDaysForBirth:
        if (isMain) then aRow := aRow+#0+TGenEngine.GetDaysForBirth(FRec);

      pctGroups:
        if (isMain) then aRow := aRow+#0+GetGroups();

      pctReligion:
        if (isMain) then aRow := aRow+#0+religion;

      pctNationality:
        if (isMain) then aRow := aRow+#0+nationality;

      pctEducation:
        if (isMain) then aRow := aRow+#0+education;

      pctOccupation:
        if (isMain) then aRow := aRow+#0+occupation;

      pctCaste:
        if (isMain) then aRow := aRow+#0+caste;

      //
      pctMili:
        if (isMain) then aRow := aRow+#0+mili;
      pctMiliInd:
        if (isMain) then aRow := aRow+#0+mili_ind;
      pctMiliDis:
        if (isMain) then aRow := aRow+#0+mili_dis;
      pctMiliRank:
        if (isMain) then aRow := aRow+#0+mili_rank;
      //

      pctChangeDate:
        if (isMain) then aRow := aRow+#0+FRec.ChangeDate.ToString();

      pctBookmark: if (isMain) then begin
        if (FRec.Bookmark)
        then aRow := aRow+#0+'*'
        else aRow := aRow+#0+' ';
      end;
    end;
  end;
end;

function TIndividualListMan.GetColumnValue(aColIndex: Integer; isMain: Boolean): string;
var
  pct: TGlobalOptions.TPersonColumnType;
  sub_index: Integer;
  f, n, p: string;
  ev: TGEDCOMCustomEvent;
begin
  pct := TGlobalOptions.TPersonColumnType(FColumnsMap[aColIndex].col_type);
  sub_index := FColumnsMap[aColIndex].col_subtype;

  case pct of
    pctPatriarch: begin
      if (FRec.Patriarch)
      then Result := '*'
      else Result := ' ';
    end;

    pctName: begin
      if (fmGEDKeeper.Options.DefNameFormat in [nfF_NP, nfF_N_P])
      then TGenEngine.GetNameParts(FRec, f, n, p);

      case fmGEDKeeper.Options.DefNameFormat of
        nfFNP: begin
          Result := TGenEngine.GetNameStr(FRec);
        end;
        nfF_NP: begin
          case sub_index of
            0: Result := f;
            1: Result := n + ' ' + p;
          end;
        end;
        nfF_N_P: begin
          case sub_index of
            0: Result := f;
            1: Result := n;
            2: Result := p;
          end;
        end;
      end;
    end;

    pctNick: Result := TGenEngine.GetNickStr(FRec);
    pctSex: Result := TGenEngine.SexStr(FRec.Sex)[1];

    pctBirthDate, pctBirthPlace: begin
      ev := TGenEngine.GetIndividualEvent(FRec, 'BIRT');

      case pct of
        pctBirthDate: Result := TGenEngine.GEDCOMEventToDateStr(ev, fmGEDKeeper.Options.DefDateFormat);
        pctBirthPlace: Result := TGenEngine.GetPlaceStr(ev, False);
      end;
    end;

    pctDeathDate, pctDeathPlace: begin
      ev := TGenEngine.GetIndividualEvent(FRec, 'DEAT');

      case pct of
        pctDeathDate: Result := TGenEngine.GEDCOMEventToDateStr(ev, fmGEDKeeper.Options.DefDateFormat);
        pctDeathPlace: Result := TGenEngine.GetPlaceStr(ev, False);
      end;
    end;

    pctResidence: Result := TGenEngine.GetResidencePlace(FRec, fmGEDKeeper.Options.PlacesWithAddress);

    pctAge: Result := TGenEngine.GetAge(FRec);
    pctLifeExpectancy: Result := TGenEngine.GetLifeExpectancy(FRec);
    pctDaysForBirth: Result := TGenEngine.GetDaysForBirth(FRec);
    pctGroups: Result := GetGroups();

    pctReligion: Result := TGenEngine.GetAttributeValue(FRec, 'RELI');
    pctNationality: Result := TGenEngine.GetAttributeValue(FRec, 'NATI');
    pctEducation: Result := TGenEngine.GetAttributeValue(FRec, 'EDUC');
    pctOccupation: Result := TGenEngine.GetAttributeValue(FRec, 'OCCU');
    pctCaste: Result := TGenEngine.GetAttributeValue(FRec, 'CAST');

    pctMili: Result := TGenEngine.GetAttributeValue(FRec, '_MILI');
    pctMiliInd: Result := TGenEngine.GetAttributeValue(FRec, '_MILI_IND');
    pctMiliDis: Result := TGenEngine.GetAttributeValue(FRec, '_MILI_DIS');
    pctMiliRank: Result := TGenEngine.GetAttributeValue(FRec, '_MILI_RANK');

    pctChangeDate: Result := FRec.ChangeDate.ToString();

    pctBookmark: begin
      if (FRec.Bookmark)
      then Result := '*'
      else Result := ' ';
    end;
  end;
end;

procedure TIndividualListMan.SetColMap(aType, aSubType: Byte; var cols: Integer);
begin
  Inc(cols);
  FColumnsMap[cols].col_type := aType;
  FColumnsMap[cols].col_subtype := aSubType;
end;

procedure TIndividualListMan.UpdateColumns(aList: TGKListView; isMain: Boolean);
var
  i, cols: Integer;
  columns: TGlobalOptions.TPersonColumnsList;
  col_type: TGlobalOptions.TPersonColumnType;
  asz: Boolean;
begin
  {if (isMain)
  then }columns := fmGEDKeeper.Options.ListPersonsColumns{
  else columns := DefPersonColumns};

  cols := 0;

  aList.AddListColumn('№', 50);

  for i := 0 to High(columns) do
    if columns[i].colActive then begin
      col_type := columns[i].colType;

      case col_type of
        pctPatriarch: begin
          aList.AddListColumn(LSList[LSID_Patriarch], 25);
          SetColMap(Ord(col_type), 0, cols);
        end;

        pctName: begin
          asz := False;

          case fmGEDKeeper.Options.DefNameFormat of
            nfFNP: begin
              aList.AddListColumn(LSList[LSID_FullName], 300, asz);
              SetColMap(Ord(col_type), 0, cols);
            end;

            nfF_NP: begin
              aList.AddListColumn(LSList[LSID_Surname], 150, asz);
              SetColMap(Ord(col_type), 0, cols);
              aList.AddListColumn(LSList[LSID_Name]+','+LSList[LSID_Patronymic], 150, asz);
              SetColMap(Ord(col_type), 1, cols);
            end;

            nfF_N_P: begin
              aList.AddListColumn(LSList[LSID_Surname], 150, asz);
              SetColMap(Ord(col_type), 0, cols);
              aList.AddListColumn(LSList[LSID_Name], 100, asz);
              SetColMap(Ord(col_type), 1, cols);
              aList.AddListColumn(LSList[LSID_Patronymic], 150, asz);
              SetColMap(Ord(col_type), 2, cols);
            end;
          end;
        end;

        pctNick, pctSex, pctBirthDate, pctDeathDate,
        pctBirthPlace, pctDeathPlace, pctResidence: begin
          aList.AddListColumn(LSList[TGlobalOptions.PersonColumnsName[col_type].Name], TGlobalOptions.PersonColumnsName[col_type].DefWidth);
          SetColMap(Ord(col_type), 0, cols);
        end;

        pctAge, pctLifeExpectancy, pctDaysForBirth, pctGroups,
        pctReligion, pctNationality, pctEducation, pctOccupation, pctCaste,
        pctMili, pctMiliInd, pctMiliDis, pctMiliRank,
        pctChangeDate, pctBookmark: if isMain then begin
          aList.AddListColumn(LSList[TGlobalOptions.PersonColumnsName[col_type].Name], TGlobalOptions.PersonColumnsName[col_type].DefWidth);
          SetColMap(Ord(col_type), 0, cols);
        end;
      end;
    end;
end;

{ TFamilyListMan }

function TFamilyListMan.CheckFilter(aFilter: TPersonsFilter; aShieldState: TGenEngine.TShieldState): Boolean;
begin
  Result := False;

  if (FRec.Restriction = rnPrivacy) and (aShieldState <> ssNone)
  then Exit;

  if (aFilter.List = flSelector)
  and ((aFilter.Name <> '*') and not(TGenEngine.IsMatchesMask(TGenEngine.GetFamilyStr(FRec), aFilter.Name)))
  then Exit;

  Result := True;
end;

procedure TFamilyListMan.Fetch(aRec: TGEDCOMRecord);
begin
  FRec := TGEDCOMFamilyRecord(aRec);
end;

procedure TFamilyListMan.UpdateItem(aItem: TExtListItem; isMain: Boolean);
begin
  aItem.SubItems.Add(TGenEngine.GetFamilyStr(FRec));
  aItem.SubItems.Add(TGenEngine.GetMarriageDate(FRec, fmGEDKeeper.Options.DefDateFormat));

  if isMain
  then aItem.SubItems.Add(FRec.ChangeDate.ToString());
end;

procedure TFamilyListMan.GetRow(aRec: TGEDCOMRecord; isMain: Boolean;
  var aRow: string);
begin
  inherited;

  aRow := aRow+#0+TGenEngine.GetFamilyStr(FRec);
  aRow := aRow+#0+TGenEngine.GetMarriageDate(FRec, fmGEDKeeper.Options.DefDateFormat);

  if isMain
  then aRow := aRow+#0+FRec.ChangeDate.ToString();
end;

function TFamilyListMan.GetColumnValue(aColIndex: Integer; isMain: Boolean): string;
begin
  case aColIndex of
    1: Result := TGenEngine.GetFamilyStr(FRec);
    2: Result := TGenEngine.GetMarriageDate(FRec, fmGEDKeeper.Options.DefDateFormat);
    3: Result := FRec.ChangeDate.ToString();
    else Result := '';
  end;
end;

procedure TFamilyListMan.UpdateColumns(aList: TGKListView; isMain: Boolean);
begin
  aList.AddListColumn('№', 50);
  aList.AddListColumn(LSList[LSID_Spouses], 300);
  aList.AddListColumn(LSList[LSID_MarriageDate], 100);
  if isMain then begin
    aList.AddListColumn(LSList[LSID_Changed], 150);
  end;
end;

{ TNoteListMan }

function TNoteListMan.CheckFilter(aFilter: TPersonsFilter;
  aShieldState: TGenEngine.TShieldState): Boolean;
begin
  Result := True;
end;

procedure TNoteListMan.Fetch(aRec: TGEDCOMRecord);
begin
  FRec := TGEDCOMNoteRecord(aRec);
end;

procedure TNoteListMan.UpdateItem(aItem: TExtListItem; isMain: Boolean);
var
  st: string;
begin
  if (FRec.Notes.Count > 0) then begin
    st := FRec.Notes[0].Trim();
    if (st = '') and (FRec.Notes.Count > 1)
    then st := FRec.Notes[1].Trim();
  end else st := '';

  aItem.SubItems.Add(st);

  if isMain
  then aItem.SubItems.Add(FRec.ChangeDate.ToString());
end;

procedure TNoteListMan.GetRow(aRec: TGEDCOMRecord; isMain: Boolean;
  var aRow: string);
var
  st: string;
begin
  inherited;

  if (FRec.Notes.Count > 0) then begin
    st := FRec.Notes[0].Trim();
    if (st = '') and (FRec.Notes.Count > 1)
    then st := FRec.Notes[1].Trim();
  end else st := '';

  aRow := aRow+#0+st;

  if isMain
  then aRow := aRow+#0+FRec.ChangeDate.ToString();
end;

function TNoteListMan.GetColumnValue(aColIndex: Integer; isMain: Boolean): string;
var
  st: string;
begin
  case aColIndex of
    1: begin
      if (FRec.Notes.Count > 0) then begin
        st := FRec.Notes[0].Trim();
        if (st = '') and (FRec.Notes.Count > 1)
        then st := FRec.Notes[1].Trim();
      end else st := '';

      Result := st;
    end;
    2: Result := FRec.ChangeDate.ToString();
    else Result := '';
  end;
end;

procedure TNoteListMan.UpdateColumns(aList: TGKListView; isMain: Boolean);
begin
  aList.AddListColumn('№', 50);
  aList.AddListColumn(LSList[LSID_Note], 400);

  if isMain then begin
    aList.AddListColumn(LSList[LSID_Changed], 150);
  end;
end;

{ TMultimediaListMan }

function TMultimediaListMan.CheckFilter(aFilter: TPersonsFilter;
  aShieldState: TGenEngine.TShieldState): Boolean;
var
  file_ref: TGEDCOMFileReferenceWithTitle;
begin
  Result := False;

  file_ref := FRec.FileReferences[0];

  if (aFilter.List = flSelector)
  and ((aFilter.Name <> '*') and not(TGenEngine.IsMatchesMask(file_ref.Title, aFilter.Name)))
  then Exit;

  Result := True;
end;

procedure TMultimediaListMan.Fetch(aRec: TGEDCOMRecord);
begin
  FRec := TGEDCOMMultimediaRecord(aRec);
end;

procedure TMultimediaListMan.UpdateItem(aItem: TExtListItem; isMain: Boolean);
var
  file_ref: TGEDCOMFileReferenceWithTitle;
begin
  file_ref := FRec.FileReferences[0];

  if (file_ref = nil) then begin
    aItem.SubItems.Add('error ' + FRec.XRef);
    Exit;
  end;

  aItem.SubItems.Add(file_ref.Title);
  aItem.SubItems.Add(LSList[TGenEngine.MediaTypes[file_ref.MediaType]]);

  if isMain then begin
    aItem.SubItems.Add(file_ref.StringValue);
    aItem.SubItems.Add(FRec.ChangeDate.ToString());
  end;
end;

procedure TMultimediaListMan.GetRow(aRec: TGEDCOMRecord; isMain: Boolean;
  var aRow: string);
var
  file_ref: TGEDCOMFileReferenceWithTitle;
begin
  inherited;

  file_ref := FRec.FileReferences[0];

  if (file_ref = nil) then begin
    aRow := aRow+#0+'error ' + FRec.XRef;
    Exit;
  end;

  aRow := aRow+#0+file_ref.Title;
  aRow := aRow+#0+LSList[TGenEngine.MediaTypes[file_ref.MediaType]];

  if isMain then begin
    aRow := aRow+#0+file_ref.StringValue;
    aRow := aRow+#0+FRec.ChangeDate.ToString();
  end;
end;

function TMultimediaListMan.GetColumnValue(aColIndex: Integer; isMain: Boolean): string;
var
  file_ref: TGEDCOMFileReferenceWithTitle;
begin
  file_ref := FRec.FileReferences[0];

  case aColIndex of
    1: Result := file_ref.Title;
    2: Result := LSList[TGenEngine.MediaTypes[file_ref.MediaType]];
    3: Result := file_ref.StringValue;
    4: Result := FRec.ChangeDate.ToString();
    else Result := '';
  end;
end;

procedure TMultimediaListMan.UpdateColumns(aList: TGKListView; isMain: Boolean);
begin
  aList.AddListColumn('№', 50);
  aList.AddListColumn(LSList[LSID_Title], 150);
  aList.AddListColumn(LSList[LSID_Type], 85);

  if isMain then begin
    aList.AddListColumn(LSList[LSID_File], 300);
    aList.AddListColumn(LSList[LSID_Changed], 150);
  end;
end;

{ TSourceListMan }

function TSourceListMan.CheckFilter(aFilter: TPersonsFilter;
  aShieldState: TGenEngine.TShieldState): Boolean;
begin
  Result := False;

  if (aFilter.List = flSelector)
  and ((aFilter.Name <> '*') and not(TGenEngine.IsMatchesMask(FRec.FiledByEntry, aFilter.Name)))
  then Exit;

  Result := True;
end;

procedure TSourceListMan.Fetch(aRec: TGEDCOMRecord);
begin
  FRec := TGEDCOMSourceRecord(aRec);
end;

procedure TSourceListMan.UpdateItem(aItem: TExtListItem; isMain: Boolean);
begin
  aItem.SubItems.Add(FRec.FiledByEntry.Trim());

  if isMain then begin
    aItem.SubItems.Add(FRec.Originator.Text.Trim());
    aItem.SubItems.Add(FRec.Title.Text.Trim());
    aItem.SubItems.Add(FRec.ChangeDate.ToString());
  end;
end;

procedure TSourceListMan.GetRow(aRec: TGEDCOMRecord; isMain: Boolean;
  var aRow: string);
begin
  inherited;

  aRow := aRow+#0+FRec.FiledByEntry.Trim();

  if isMain then begin
    aRow := aRow+#0+FRec.Originator.Text.Trim();
    aRow := aRow+#0+FRec.Title.Text.Trim();
    aRow := aRow+#0+FRec.ChangeDate.ToString();
  end;
end;

function TSourceListMan.GetColumnValue(aColIndex: Integer; isMain: Boolean): string;
begin
  case aColIndex of
    1: Result := FRec.FiledByEntry.Trim();
    2: Result := FRec.Originator.Text.Trim();
    3: Result := FRec.Title.Text.Trim();
    4: Result := FRec.ChangeDate.ToString();
    else Result := '';
  end;
end;

procedure TSourceListMan.UpdateColumns(aList: TGKListView; isMain: Boolean);
begin
  aList.AddListColumn('№', 50);
  aList.AddListColumn(LSList[LSID_ShortTitle], 120);

  if isMain then begin
    aList.AddListColumn(LSList[LSID_Author], 200);
    aList.AddListColumn(LSList[LSID_Title], 200);
    aList.AddListColumn(LSList[LSID_Changed], 150);
  end;
end;

{ TRepositoryListMan }

function TRepositoryListMan.CheckFilter(aFilter: TPersonsFilter;
  aShieldState: TGenEngine.TShieldState): Boolean;
begin
  Result := False;

  if (aFilter.List = flSelector)
  and ((aFilter.Name <> '*') and not(TGenEngine.IsMatchesMask(FRec.RepositoryName, aFilter.Name)))
  then Exit;

  Result := True;
end;

procedure TRepositoryListMan.Fetch(aRec: TGEDCOMRecord);
begin
  FRec := TGEDCOMRepositoryRecord(aRec);
end;

procedure TRepositoryListMan.UpdateColumns(aList: TGKListView; isMain: Boolean);
begin
  aList.AddListColumn('№', 50);
  aList.AddListColumn(LSList[LSID_Repository], 400);

  if isMain then begin
    aList.AddListColumn(LSList[LSID_Changed], 150);
  end;
end;

procedure TRepositoryListMan.UpdateItem(aItem: TExtListItem; isMain: Boolean);
begin
  aItem.SubItems.Add(FRec.RepositoryName);

  if isMain
  then aItem.SubItems.Add(FRec.ChangeDate.ToString());
end;

procedure TRepositoryListMan.GetRow(aRec: TGEDCOMRecord; isMain: Boolean;
  var aRow: string);
begin
  inherited;

  aRow := aRow+#0+FRec.RepositoryName;

  if isMain
  then aRow := aRow+#0+FRec.ChangeDate.ToString();
end;

function TRepositoryListMan.GetColumnValue(aColIndex: Integer; isMain: Boolean): string;
begin
  case aColIndex of
    1: Result := FRec.RepositoryName;
    2: Result := FRec.ChangeDate.ToString();
    else Result := '';
  end;
end;

{ TGroupListMan }

function TGroupListMan.CheckFilter(aFilter: TPersonsFilter;
  aShieldState: TGenEngine.TShieldState): Boolean;
begin
  Result := False;

  if (aFilter.List = flSelector)
  and ((aFilter.Name <> '*') and not(TGenEngine.IsMatchesMask(FRec.Name, aFilter.Name)))
  then Exit;

  Result := True;
end;

procedure TGroupListMan.Fetch(aRec: TGEDCOMRecord);
begin
  FRec := TGEDCOMGroupRecord(aRec);
end;

procedure TGroupListMan.UpdateColumns(aList: TGKListView; isMain: Boolean);
begin
  aList.AddListColumn('№', 50);
  aList.AddListColumn(LSList[LSID_Group], 400);

  if isMain then begin
    aList.AddListColumn(LSList[LSID_Changed], 150);
  end;
end;

procedure TGroupListMan.UpdateItem(aItem: TExtListItem; isMain: Boolean);
begin
  aItem.SubItems.Add(FRec.Name);

  if isMain
  then aItem.SubItems.Add(FRec.ChangeDate.ToString());
end;

procedure TGroupListMan.GetRow(aRec: TGEDCOMRecord; isMain: Boolean;
  var aRow: string);
begin
  inherited;

  aRow := aRow+#0+FRec.Name;

  if isMain
  then aRow := aRow+#0+FRec.ChangeDate.ToString();
end;

function TGroupListMan.GetColumnValue(aColIndex: Integer; isMain: Boolean): string;
begin
  case aColIndex of
    1: Result := FRec.Name;
    2: Result := FRec.ChangeDate.ToString();
    else Result := '';
  end;
end;

{ TResearchListMan }

function TResearchListMan.CheckFilter(aFilter: TPersonsFilter;
  aShieldState: TGenEngine.TShieldState): Boolean;
begin
  Result := False;

  if (aFilter.List = flSelector)
  and ((aFilter.Name <> '*') and not(TGenEngine.IsMatchesMask(FRec.Name, aFilter.Name)))
  then Exit;

  Result := True;
end;

procedure TResearchListMan.Fetch(aRec: TGEDCOMRecord);
begin
  FRec := TGEDCOMResearchRecord(aRec);
end;

procedure TResearchListMan.UpdateColumns(aList: TGKListView; isMain: Boolean);
begin
  aList.AddListColumn('№', 50);
  aList.AddListColumn(LSList[LSID_Title], 300);
  aList.AddListColumn(LSList[LSID_Priority], 90);
  aList.AddListColumn(LSList[LSID_Status], 90);
  aList.AddListColumn(LSList[LSID_StartDate], 90);
  aList.AddListColumn(LSList[LSID_StopDate], 90);
  aList.AddListColumn(LSList[LSID_Percent], 90);

  if isMain then begin
    aList.AddListColumn(LSList[LSID_Changed], 150);
  end;
end;

procedure TResearchListMan.UpdateItem(aItem: TExtListItem; isMain: Boolean);
begin
  aItem.SubItems.Add(FRec.Name);
  aItem.SubItems.Add(LSList[TGenEngine.PriorityNames[FRec.Priority]]);
  aItem.SubItems.Add(LSList[TGenEngine.StatusNames[FRec.Status]]);
  aItem.SubItems.Add(TGenEngine.GEDCOMDateToStr(FRec.StartDate, fmGEDKeeper.Options.DefDateFormat));
  aItem.SubItems.Add(TGenEngine.GEDCOMDateToStr(FRec.StopDate, fmGEDKeeper.Options.DefDateFormat));
  aItem.SubItems.Add(FRec.Percent.ToString());

  if isMain
  then aItem.SubItems.Add(FRec.ChangeDate.ToString());
end;

procedure TResearchListMan.GetRow(aRec: TGEDCOMRecord; isMain: Boolean;
  var aRow: string);
begin
  inherited;

  aRow := aRow+#0+FRec.Name;
  aRow := aRow+#0+LSList[TGenEngine.PriorityNames[FRec.Priority]];
  aRow := aRow+#0+LSList[TGenEngine.StatusNames[FRec.Status]];
  aRow := aRow+#0+TGenEngine.GEDCOMDateToStr(FRec.StartDate, fmGEDKeeper.Options.DefDateFormat);
  aRow := aRow+#0+TGenEngine.GEDCOMDateToStr(FRec.StopDate, fmGEDKeeper.Options.DefDateFormat);
  aRow := aRow+#0+FRec.Percent.ToString();

  if isMain
  then aRow := aRow+#0+FRec.ChangeDate.ToString();
end;

function TResearchListMan.GetColumnValue(aColIndex: Integer; isMain: Boolean): string;
begin
  case aColIndex of
    1: Result := FRec.Name;
    2: Result := LSList[TGenEngine.PriorityNames[FRec.Priority]];
    3: Result := LSList[TGenEngine.StatusNames[FRec.Status]];
    4: Result := TGenEngine.GEDCOMDateToStr(FRec.StartDate, fmGEDKeeper.Options.DefDateFormat);
    5: Result := TGenEngine.GEDCOMDateToStr(FRec.StopDate, fmGEDKeeper.Options.DefDateFormat);
    6: Result := FRec.Percent.ToString();
    7: Result := FRec.ChangeDate.ToString();
    else Result := '';
  end;
end;

{ TTaskListMan }

function TTaskListMan.CheckFilter(aFilter: TPersonsFilter;
  aShieldState: TGenEngine.TShieldState): Boolean;
begin
  Result := False;

  if (aFilter.List = flSelector)
  and ((aFilter.Name <> '*') and not(TGenEngine.IsMatchesMask(TGenEngine.GetTaskGoalStr(FTree, FRec), aFilter.Name)))
  then Exit;

  Result := True;
end;

procedure TTaskListMan.Fetch(aRec: TGEDCOMRecord);
begin
  FRec := TGEDCOMTaskRecord(aRec);
end;

procedure TTaskListMan.UpdateColumns(aList: TGKListView; isMain: Boolean);
begin
  aList.AddListColumn('№', 50);
  aList.AddListColumn(LSList[LSID_Goal], 300);
  aList.AddListColumn(LSList[LSID_Priority], 90);
  aList.AddListColumn(LSList[LSID_StartDate], 90);
  aList.AddListColumn(LSList[LSID_StopDate], 90);

  if isMain then begin
    aList.AddListColumn(LSList[LSID_Changed], 150);
  end;
end;

procedure TTaskListMan.UpdateItem(aItem: TExtListItem; isMain: Boolean);
begin
  aItem.SubItems.Add(TGenEngine.GetTaskGoalStr(FTree, FRec));
  aItem.SubItems.Add(LSList[TGenEngine.PriorityNames[FRec.Priority]]);
  aItem.SubItems.Add(TGenEngine.GEDCOMDateToStr(FRec.StartDate, fmGEDKeeper.Options.DefDateFormat));
  aItem.SubItems.Add(TGenEngine.GEDCOMDateToStr(FRec.StopDate, fmGEDKeeper.Options.DefDateFormat));

  if isMain
  then aItem.SubItems.Add(FRec.ChangeDate.ToString());
end;

procedure TTaskListMan.GetRow(aRec: TGEDCOMRecord; isMain: Boolean;
  var aRow: string);
begin
  inherited;

  aRow := aRow+#0+TGenEngine.GetTaskGoalStr(FTree, FRec);
  aRow := aRow+#0+LSList[TGenEngine.PriorityNames[FRec.Priority]];
  aRow := aRow+#0+TGenEngine.GEDCOMDateToStr(FRec.StartDate, fmGEDKeeper.Options.DefDateFormat);
  aRow := aRow+#0+TGenEngine.GEDCOMDateToStr(FRec.StopDate, fmGEDKeeper.Options.DefDateFormat);

  if isMain
  then aRow := aRow+#0+FRec.ChangeDate.ToString();
end;

function TTaskListMan.GetColumnValue(aColIndex: Integer; isMain: Boolean): string;
begin
  case aColIndex of
    1: Result := TGenEngine.GetTaskGoalStr(FTree, FRec);
    2: Result := LSList[TGenEngine.PriorityNames[FRec.Priority]];
    3: Result := TGenEngine.GEDCOMDateToStr(FRec.StartDate, fmGEDKeeper.Options.DefDateFormat);
    4: Result := TGenEngine.GEDCOMDateToStr(FRec.StopDate, fmGEDKeeper.Options.DefDateFormat);
    5: Result := FRec.ChangeDate.ToString();
    else Result := '';
  end;
end;

{ TCommunicationListMan }

function TCommunicationListMan.CheckFilter(aFilter: TPersonsFilter;
  aShieldState: TGenEngine.TShieldState): Boolean;
begin
  Result := False;

  if (aFilter.List = flSelector)
  and ((aFilter.Name <> '*') and not(TGenEngine.IsMatchesMask(FRec.Name, aFilter.Name)))
  then Exit;

  Result := True;
end;

procedure TCommunicationListMan.Fetch(aRec: TGEDCOMRecord);
begin
  FRec := TGEDCOMCommunicationRecord(aRec);
end;

procedure TCommunicationListMan.UpdateColumns(aList: TGKListView; isMain: Boolean);
begin
  aList.AddListColumn('№', 50);
  aList.AddListColumn(LSList[LSID_Theme], 300);
  aList.AddListColumn(LSList[LSID_Corresponder], 200);
  aList.AddListColumn(LSList[LSID_Type], 90);
  aList.AddListColumn(LSList[LSID_Date], 90);

  if isMain then begin
    aList.AddListColumn(LSList[LSID_Changed], 150);
  end;
end;

procedure TCommunicationListMan.UpdateItem(aItem: TExtListItem; isMain: Boolean);
begin
  aItem.SubItems.Add(FRec.Name);
  aItem.SubItems.Add(TGenEngine.GetCorresponderStr(FTree, FRec, False));
  aItem.SubItems.Add(LSList[TGenEngine.CommunicationNames[FRec.CommunicationType]]);
  aItem.SubItems.Add(TGenEngine.GEDCOMDateToStr(FRec.Date, fmGEDKeeper.Options.DefDateFormat));

  if isMain
  then aItem.SubItems.Add(FRec.ChangeDate.ToString());
end;

procedure TCommunicationListMan.GetRow(aRec: TGEDCOMRecord; isMain: Boolean;
  var aRow: string);
begin
  inherited;

  aRow := aRow+#0+FRec.Name;
  aRow := aRow+#0+TGenEngine.GetCorresponderStr(FTree, FRec, False);
  aRow := aRow+#0+LSList[TGenEngine.CommunicationNames[FRec.CommunicationType]];
  aRow := aRow+#0+TGenEngine.GEDCOMDateToStr(FRec.Date, fmGEDKeeper.Options.DefDateFormat);

  if isMain
  then aRow := aRow+#0+FRec.ChangeDate.ToString();
end;

function TCommunicationListMan.GetColumnValue(aColIndex: Integer; isMain: Boolean): string;
begin
  case aColIndex of
    1: Result := FRec.Name;
    2: Result := TGenEngine.GetCorresponderStr(FTree, FRec, False);
    3: Result := LSList[TGenEngine.CommunicationNames[FRec.CommunicationType]];
    4: Result := TGenEngine.GEDCOMDateToStr(FRec.Date, fmGEDKeeper.Options.DefDateFormat);
    5: Result := FRec.ChangeDate.ToString();
    else Result := '';
  end;
end;

{ TLocationListMan }

function TLocationListMan.CheckFilter(aFilter: TPersonsFilter;
  aShieldState: TGenEngine.TShieldState): Boolean;
begin
  Result := False;

  if (aFilter.List = flSelector)
  and ((aFilter.Name <> '*') and not(TGenEngine.IsMatchesMask(FRec.Name, aFilter.Name)))
  then Exit;

  Result := True;
end;

procedure TLocationListMan.Fetch(aRec: TGEDCOMRecord);
begin
  FRec := TGEDCOMLocationRecord(aRec);
end;

procedure TLocationListMan.UpdateColumns(aList: TGKListView; isMain: Boolean);
begin
  aList.AddListColumn('№', 50);
  aList.AddListColumn(LSList[LSID_Title], 300);
  aList.AddListColumn(LSList[LSID_Latitude], 120);
  aList.AddListColumn(LSList[LSID_Longitude], 120);

  if isMain then begin
    aList.AddListColumn(LSList[LSID_Changed], 150);
  end;
end;

procedure TLocationListMan.UpdateItem(aItem: TExtListItem; isMain: Boolean);
begin
  aItem.SubItems.Add(FRec.Name);
  aItem.SubItems.Add(FRec.Map.Lati);
  aItem.SubItems.Add(FRec.Map.Long);

  if isMain
  then aItem.SubItems.Add(FRec.ChangeDate.ToString());
end;

procedure TLocationListMan.GetRow(aRec: TGEDCOMRecord; isMain: Boolean;
  var aRow: string);
begin
  inherited;

  aRow := aRow+#0+FRec.Name;
  aRow := aRow+#0+FRec.Map.Lati;
  aRow := aRow+#0+FRec.Map.Long;

  if isMain
  then aRow := aRow+#0+FRec.ChangeDate.ToString();
end;

function TLocationListMan.GetColumnValue(aColIndex: Integer; isMain: Boolean): string;
begin
  case aColIndex of
    1: Result := FRec.Name;
    2: Result := FRec.Map.Lati;
    3: Result := FRec.Map.Long;
    4: Result := FRec.ChangeDate.ToString();
    else Result := '';
  end;
end;

{ TRecordsView }

constructor TRecordsView.Create(AOwner: System.Windows.Forms.Control);
begin
  inherited Create(nil);
  FContentList := TList.Create;

  //Alert
  {OwnerData := True;
  OnData := ListGetItemData;
  OnMeasureItem := ListMeasureItem;
  OnCustomDrawItem := ListCustomDrawItem;}

  Include(Self.ColumnClick, List_ColumnClick);

  {$IFDEF FAST_LISTS}
  OnDataHint := ListDataHint;
  FCacheList := TObjectList.Create(True);
  {$ENDIF}

  FListMan := nil;
  FRecordType := rtNone;
end;

destructor TRecordsView.Destroy;
begin
  {$IFDEF FAST_LISTS}
  FCacheList.Destroy;
  {$ENDIF}

  if Assigned(FListMan) then FreeAndNil(FListMan);

  FContentList.Free;
  inherited Destroy;
end;

function TRecordsView.xCompare(Item1, Item2: TObject): Integer;
var
  val1, val2: string;
  f: Integer;
begin
  //Alert
  {if (SortColumn = 0) then begin
    val1 := GetXRefNum(TGEDCOMRecord(Item1));
    val2 := GetXRefNum(TGEDCOMRecord(Item2));
  end else begin
    FListMan.Fetch(TGEDCOMRecord(Item1));
    val1 := FListMan.GetColumnValue(SortColumn, FIsMainList);
    FListMan.Fetch(TGEDCOMRecord(Item2));
    val2 := FListMan.GetColumnValue(SortColumn, FIsMainList);
  end;

  if (SortDirection = sdAscending) then f := 1 else f := -1;

  Result := agCompare(val1, val2) * f;}
end;

procedure TRecordsView.List_ColumnClick(sender: System.Object; e: System.Windows.Forms.ColumnClickEventArgs);
var
  rec: TGEDCOMRecord;
begin
  rec := GetSelectedRecord();

  {$IFNDEF DELPHI_NET}
  MergeSort(FContentList, xCompare); // 16.5ms
  {$ELSE}
  TGKUtils.QuickSort(FContentList, xCompare); // 32ms
  {$ENDIF}

  SelectItemByRec(rec);
  Invalidate();
end;

{$IFDEF FAST_LISTS}
function TRecordsView.CacheQuery(aIndex: Integer): string;
var
  i, max, max_index: Integer;
  row, founded: TRow;
  row_value: string;
  rec: TGEDCOMRecord;
begin
  founded := nil;
  max := 0;
  max_index := -1;

  for i := 0 to FCacheList.Count - 1 do begin
    row := TRow(FCacheList[i]);

    if (row.Index = aIndex) then begin
      founded := row;
      row.Age := 0;
    end else row.Age := row.Age + 1;

    if (max < row.Age) then begin
      max := row.Age;
      max_index := i;
    end;
  end;

  if (founded <> nil)
  then row_value := founded.Value
  else begin
    if (FCacheList.Count = 100) and (max_index >= 0)
    then FCacheList.Delete(max_index);

    rec := TGEDCOMRecord(FContentList[aIndex]);
    FListMan.GetRow(rec, FIsMainList, row_value);

    row := TRow.Create;
    row.Index := aIndex;
    row.Value := row_value;
    row.Age := 0;
    FCacheList.Add(row);
  end;

  Result := row_value;
end;

procedure TRecordsView.ListDataHint(Sender: TObject; StartIndex, EndIndex: Integer);
var
  idx: Integer;
begin
  try
    for idx := StartIndex to EndIndex do
      if (idx >= 0) and (idx < FContentList.Count)
      then CacheQuery(idx);
  except
    on E: Exception do LogWrite('ListDataHint(): ' + E.Message);
  end;
end;
{$ENDIF}

(*
procedure TRecordsView.ListGetItemData(Sender: TObject; Item: TListItem);
var
  i: Integer;
  {$IFNDEF FAST_LISTS}
  rec: TGEDCOMRecord;
  {$ELSE}
  row, st: string;
  {$ENDIF}
begin
  (*try
    if Assigned(Item) then begin
      {$IFNDEF FAST_LISTS}
      i := Item.Index;
      if (i >= 0) and (i < FContentList.Count) then begin
        rec := TGEDCOMRecord(FContentList[i]);
        Item.Caption := IntToStr(GetId(rec));
        FListMan.Fetch(rec);
        FListMan.UpdateItem(Item, FIsMainList);
      end;
      {$ELSE}
      row := CacheQuery(Item.Index);

      for i := 1 to GetTokensCount(row, #0) do begin
        st := GetToken(row, #0, i);

        if (i = 1)
        then Item.Caption := st
        else Item.SubItems.Add(st);
      end;
      {$ENDIF}
    end;
  except
    on E: Exception do LogWrite('ListGetItemData(): ' + E.Message);
  end;
end;

procedure TRecordsView.ListCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  rec: TGEDCOMRecord;
  i_rec: TGEDCOMIndividualRecord;
begin
  //Alert
  {if (Item = nil) then Exit;

  rec := TGEDCOMRecord(FContentList[Item.Index]);
  if (rec = nil) then Exit;

  if (rec is TGEDCOMIndividualRecord)
  and not((cdsFocused in State) and (cdsSelected in State))
  then begin
    i_rec := TGEDCOMIndividualRecord(rec);

    if (i_rec.ChildToFamilyLinksCount = 0) and (fmGEDKeeper.Options.ListPersons_HighlightUnparented)
    then Canvas.Brush.Color := $00CACAFF
    else
    if (i_rec.SpouseToFamilyLinksCount = 0) and (fmGEDKeeper.Options.ListPersons_HighlightUnmarried)
    then Canvas.Brush.Color := $00CAFFFF;
  end;}
end;
*)

procedure TRecordsView.DeleteRecord(aRec: TGEDCOMRecord);
// warning! need for correct work virtual lists
begin
  FContentList.Remove(aRec);
end;

procedure TRecordsView.SelectItemByRec(aRec: TGEDCOMRecord);
var
  idx: Integer;
  item: TExtListItem;
begin
  for idx := 0 to Items.Count - 1 do begin
    item := TExtListItem(Items[idx]);

    if (item.Data = aRec) then begin
      Self.SelectedItems.Clear;

      //SelectedIndices[0] := idx;
      item.Selected := True;
      item.EnsureVisible();
    end;
  end;

  //Alert
  {idx := FContentList.IndexOf(aRec);
  if (idx >= 0) then begin
    item := Items[idx];
    Selected := item;
    item.MakeVisible(False);
  end;}
end;

procedure TRecordsView.SetRecordType(const Value: TGEDCOMRecord.TGEDCOMRecordType);
begin
  FRecordType := Value;

  if Assigned(FListMan) then FreeAndNil(FListMan);

  case FRecordType of
    rtNone: ;
    rtIndividual: FListMan := TIndividualListMan.Create(FTree);
    rtFamily: FListMan := TFamilyListMan.Create(FTree);
    rtNote: FListMan := TNoteListMan.Create(FTree);
    rtMultimedia: FListMan := TMultimediaListMan.Create(FTree);
    rtSource: FListMan := TSourceListMan.Create(FTree);
    rtRepository: FListMan := TRepositoryListMan.Create(FTree);
    rtGroup: FListMan := TGroupListMan.Create(FTree);
    rtResearch: FListMan := TResearchListMan.Create(FTree);
    rtTask: FListMan := TTaskListMan.Create(FTree);
    rtCommunication: FListMan := TCommunicationListMan.Create(FTree);
    rtLocation: FListMan := TLocationListMan.Create(FTree);
    rtSubmission: FListMan := nil;
    rtSubmitter: FListMan := nil;
  end;
end;

function TRecordsView.GetSelectedRecord(): TGEDCOMRecord;
var
  item: TExtListItem;
begin
  //Alert
  //Result := nil;

  item := SelectedItem();
  if (item = nil)
  then Result := nil
  else Result := TGEDCOMRecord(item.Data);

  {if (ItemIndex < 0) or (ItemIndex >= FContentList.Count)
  then Result := nil
  else Result := TGEDCOMRecord(FContentList[ItemIndex]);}
end;

procedure TRecordsView.UpdateTitles();
begin
  FListMan.UpdateTitles(Self, FIsMainList);
end;

procedure TRecordsView.UpdateContents(aShieldState: TGenEngine.TShieldState; aTitles: Boolean;
  aFilter: TPersonsFilter; aAutoSizeColumn: Integer = -1);
var
  i: Integer;
  rec: TGEDCOMRecord;
  item: TExtListItem;
begin
  try
    FTotalCount := 0;
    FFilteredCount := 0;

    if (aTitles) and (FListMan <> nil)
    then FListMan.UpdateTitles(Self, FIsMainList);

    BeginUpdate();
    try
      // фильтрация
      FListMan.InitFilter(aFilter);
      FContentList.Clear;
      for i := 0 to FTree.RecordsCount - 1 do begin
        rec := FTree.Records[i];

        if (rec.RecordType = FRecordType) then begin
          Inc(FTotalCount);

          FListMan.Fetch(rec);
          if FListMan.CheckFilter(aFilter, aShieldState)
          then FContentList.Add(rec);
        end;
      end;
      FFilteredCount := FContentList.Count;
      //Items.Count := FContentList.Count; Alert

      Items.Clear;
      for i := 0 to FContentList.Count - 1 do begin
        rec := TGEDCOMRecord(FContentList[i]);

        item := AddItem(TGenEngine.GetId(rec).ToString(), rec);
        FListMan.Fetch(rec);
        FListMan.UpdateItem(item, FIsMainList);
      end;

      {$IFNDEF DELPHI_NET}
      MergeSort(FContentList, xCompare);
      {$ELSE}
      TGKUtils.QuickSort(FContentList, xCompare);
      {$ENDIF}

      if (aAutoSizeColumn >= 0)
      then Self.ResizeColumn(aAutoSizeColumn);
    finally
      EndUpdate();
    end;
  except
    on E: Exception do TGKUtils.LogWrite('UpdateContents(): ' + E.Message);
  end;
end;

end.
