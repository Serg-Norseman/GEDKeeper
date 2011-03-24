unit GKLists;

{$I GEDKeeper.inc}

interface

uses
  Types, Windows, Classes, Graphics, Controls, ComCtrls, ActnList, Menus,
  StdCtrls, GedCom551, GKEngine, GKCommon, GKCtrls;

type
  TModifyEvent = procedure (Sender: TObject; ItemData: TObject; Action: TRecAction) of object;

  TListButtons = set of (lbAdd, lbEdit, lbDelete, lbJump, lbMoveUp, lbMoveDown);

  TListMode = (lmView, lmBox);

  TSheetList = class(TCustomControl)
  private
    //FActionList: TActionList;
    FActionAdd: TAction;
    FActionDelete: TAction;
    FActionEdit: TAction;
    FActionJump: TAction;
    FActionMoveUp: TAction;
    FActionMoveDown: TAction;

    FBtnAdd: TToolButton;
    FBtnDelete: TToolButton;
    FBtnEdit: TToolButton;
    FBtnLinkJump: TToolButton;
    FBtnMoveUp: TToolButton;
    FBtnMoveDown: TToolButton;

    FList: TCustomListControl;
    FToolBar: TToolBar;

    FOnModify: TModifyEvent;
    FButtons: TListButtons;

    FListMode: TListMode;
    FReadOnly: Boolean;

    procedure ButtonClick(Sender: TObject);
    procedure ListDblClick(Sender: TObject);
    procedure ListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    //procedure SetActionList(const Value: TActionList);
    procedure SheetShow(Sender: TObject);
    procedure SetButtons(const Value: TListButtons);

    function GetSelectedData(): TObject;

    procedure LBDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure LBMeasureItem(Control: TWinControl; Index: Integer;
      var Height: Integer);
    procedure SetReadOnly(const Value: Boolean);
  protected
  public
    constructor Create(AOwner: TComponent; aListMode: TListMode = lmView);
    destructor Destroy; override;

    //property ActionList: TActionList read FActionList write SetActionList;
    property Buttons: TListButtons read FButtons write SetButtons;
    property List: TCustomListControl read FList;
    property ToolBar: TToolBar read FToolBar;

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
  TGroupMode = (gmAll, gmNone, gmAny, gmSelected);

  TListFilterMode = (flCommon, flSelector);

  TPersonsFilter = class(TObject)
  private
    Back_AliveBeforeDate: string;
    Back_GroupMode: TGroupMode;
    Back_GroupRef: string;
    Back_LifeMode: TLifeMode;
    Back_Name: string;
    Back_PatriarchOnly: Boolean;
    Back_Residence: string;
    Back_Sex: TGEDCOMSex;
    Back_SourceMode: TGroupMode;
    Back_SourceRef: string;
    Back_EventVal: string;
  public
    AliveBeforeDate: string;
    GroupMode: TGroupMode;
    GroupRef: string;
    LifeMode: TLifeMode;
    Name: string;
    PatriarchOnly: Boolean;
    Residence: string;
    Sex: TGEDCOMSex;
    SourceMode: TGroupMode;
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

    function  CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean; virtual; abstract;
    procedure Fetch(aRec: TGEDCOMRecord); virtual; abstract;
    function  GetColumnValue(aColIndex: Integer; isMain: Boolean): string; virtual;
    procedure InitFilter(aFilter: TPersonsFilter); virtual;
    procedure UpdateItem(aItem: TListItem; isMain: Boolean); virtual; abstract;
    procedure UpdateColumns(aList: TGKListView; isMain: Boolean); virtual; abstract;
  end;

  TColumnsMap = array [Byte] of record
    col_type: Byte;
    col_subtype: Byte;
  end;

  TIndividualListMan = class(TListManager)
  private
    FRec: TGEDCOMIndividualRecord;

    filter_grp: TGEDCOMGroupRecord;
    filter_abd: TDateTime;
    filter_source: TGEDCOMSourceRecord;

    // timeline runtime
    FYearMin, FYearMax: Integer;

    // filter runtime
    age_year: Integer;

    function GetGroups(): string;
  protected
    FColumnsMap: TColumnsMap;
  public
    function  CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean; override;
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function  GetColumnValue(aColIndex: Integer; isMain: Boolean): string; override;
    procedure InitFilter(aFilter: TPersonsFilter); override;
    procedure UpdateItem(aItem: TListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TGKListView; isMain: Boolean); override;

    property YearMin: Integer read FYearMin;
    property YearMax: Integer read FYearMax;
  end;

  TFamilyListMan = class(TListManager)
  private
    FRec: TGEDCOMFamilyRecord;
  public
    function  CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean; override;
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function  GetColumnValue(aColIndex: Integer; isMain: Boolean): string; override;
    procedure UpdateItem(aItem: TListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TGKListView; isMain: Boolean); override;
  end;

  TNoteListMan = class(TListManager)
  private
    FRec: TGEDCOMNoteRecord;
  public
    function  CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean; override;
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function  GetColumnValue(aColIndex: Integer; isMain: Boolean): string; override;
    procedure UpdateItem(aItem: TListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TGKListView; isMain: Boolean); override;
  end;

  TMultimediaListMan = class(TListManager)
  private
    FRec: TGEDCOMMultimediaRecord;
  public
    function  CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean; override;
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function  GetColumnValue(aColIndex: Integer; isMain: Boolean): string; override;
    procedure UpdateItem(aItem: TListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TGKListView; isMain: Boolean); override;
  end;

  TSourceListMan = class(TListManager)
  private
    FRec: TGEDCOMSourceRecord;
  public
    function  CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean; override;
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function  GetColumnValue(aColIndex: Integer; isMain: Boolean): string; override;
    procedure UpdateItem(aItem: TListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TGKListView; isMain: Boolean); override;
  end;

  TRepositoryListMan = class(TListManager)
  private
    FRec: TGEDCOMRepositoryRecord;
  public
    function  CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean; override;
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function  GetColumnValue(aColIndex: Integer; isMain: Boolean): string; override;
    procedure UpdateItem(aItem: TListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TGKListView; isMain: Boolean); override;
  end;

  TGroupListMan = class(TListManager)
  private
    FRec: TGEDCOMGroupRecord;
  public
    function  CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean; override;
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function  GetColumnValue(aColIndex: Integer; isMain: Boolean): string; override;
    procedure UpdateItem(aItem: TListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TGKListView; isMain: Boolean); override;
  end;

  TResearchListMan = class(TListManager)
  private
    FRec: TGEDCOMResearchRecord;
  public
    function  CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean; override;
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function  GetColumnValue(aColIndex: Integer; isMain: Boolean): string; override;
    procedure UpdateItem(aItem: TListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TGKListView; isMain: Boolean); override;
  end;

  TTaskListMan = class(TListManager)
  private
    FRec: TGEDCOMTaskRecord;
  public
    function  CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean; override;
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function  GetColumnValue(aColIndex: Integer; isMain: Boolean): string; override;
    procedure UpdateItem(aItem: TListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TGKListView; isMain: Boolean); override;
  end;

  TCommunicationListMan = class(TListManager)
  private
    FRec: TGEDCOMCommunicationRecord;
  public
    function  CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean; override;
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function  GetColumnValue(aColIndex: Integer; isMain: Boolean): string; override;
    procedure UpdateItem(aItem: TListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TGKListView; isMain: Boolean); override;
  end;

  TLocationListMan = class(TListManager)
  private
    FRec: TGEDCOMLocationRecord;
  public
    function  CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean; override;
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function  GetColumnValue(aColIndex: Integer; isMain: Boolean): string; override;
    procedure UpdateItem(aItem: TListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TGKListView; isMain: Boolean); override;
  end;

{==============================================================================}

type
  TRecordsView = class(TGKListView)
  private
    FContentList: TList;
    FFilteredCount: Integer;
    FIsMainList: Boolean;
    FListMan: TListManager;
    FRecordType: TGEDCOMRecordType;
    FTotalCount: Integer;
    FTree: TGEDCOMTree;

    procedure ListMeasureItem(Control: TWinControl; var AHeight: UINT);

    procedure ListCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure ListColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListGetItemData(Sender: TObject; Item: TListItem);
    procedure SetRecordType(const Value: TGEDCOMRecordType);
    function xCompare(Item1, Item2: TObject): Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure UpdateContents(aShieldState: TShieldState; aTitles: Boolean;
      aFilter: TPersonsFilter; aAutoSizeColumn: Integer = -1);
    procedure UpdateTitles();

    procedure DeleteRecord(aRec: TGEDCOMRecord);
    function  GetSelectedRecord(): TGEDCOMRecord;
    procedure SelectItemByRec(aRec: TGEDCOMRecord);

    property ContentList: TList read FContentList;
    property FilteredCount: Integer read FFilteredCount;
    property IsMainList: Boolean read FIsMainList write FIsMainList;
    property ListMan: TListManager read FListMan;
    property RecordType: TGEDCOMRecordType read FRecordType write SetRecordType;
    property TotalCount: Integer read FTotalCount;
    property Tree: TGEDCOMTree read FTree write FTree;
  end;

procedure AddListColumn(aList: TCustomListControl; aCaption: string; aWidth: Integer;
  aAutoSize: Boolean = False);

procedure ResizeColumn(aList: TCustomListControl; aColumnIndex: Integer);
  
function GetSelectedRecord(aList: TCustomListView): TGEDCOMRecord;

implementation

uses
  {$IFDEF PROFILER}ZProfiler, {$ENDIF}
  SysUtils, ExtCtrls, GKUtils, GKMain;

{==============================================================================}

procedure AddListColumn(aList: TCustomListControl; aCaption: string; aWidth: Integer;
  aAutoSize: Boolean = False);
begin
  if not(aList is TCustomListView) then Exit;

  with TGKListView(aList).Columns.Add() do begin
    Caption := aCaption;
    Width := aWidth;
    AutoSize := aAutoSize;
  end;
end;

procedure ResizeColumn(aList: TCustomListControl; aColumnIndex: Integer);
var
  i, max_w, w: Integer;
  item: TListItem;
  view: TGKListView;
begin
  view := TGKListView(aList);

  if (aColumnIndex < 0) or (aColumnIndex >= view.Columns.Count) then Exit;

  max_w := 0;

  for i := 0 to view.Items.Count - 1 do begin
    item := view.Items[i];

    if (aColumnIndex = 0)
    then w := view.StringWidth(item.Caption)
    else w := view.StringWidth(item.SubItems[aColumnIndex - 1]);

    if (max_w < w) then max_w := w;
  end;

  if (max_w <> 0)
  then view.Columns[aColumnIndex].Width := max_w + 16;
end;

function GetSelectedRecord(aList: TCustomListView): TGEDCOMRecord;
begin
  if (aList is TRecordsView)
  then Result := TRecordsView(aList).GetSelectedRecord()
  else
    if (aList.Selected = nil)
    then Result := nil
    else Result := TGEDCOMRecord(aList.Selected.Data);
end;

{==============================================================================}

{ TSheetList }

constructor TSheetList.Create(AOwner: TComponent; aListMode: TListMode = lmView);
begin
  inherited Create(AOwner);
  Parent := TWinControl(AOwner);

  FListMode := aListMode;

  HandleNeeded;
  Align := alClient;

  if (AOwner is TTabSheet)
  then TTabSheet(AOwner).OnShow := SheetShow;

  FActionAdd := TAction.Create(Self);
  with FActionAdd do begin
    Name := 'actRecAdd';
    Category := 'Tools';
    Caption := 'Add';
    Hint := 'Добавить запись';
    ImageIndex := 3;
    ShortCut := TextToShortCut('Ctrl+I');
    OnExecute := ButtonClick;
  end;

  FActionEdit := TAction.Create(Self);
  with FActionEdit do begin
    Name := 'actRecEdit';
    Category := 'Tools';
    Caption := 'Edit';
    Hint := 'Изменить запись';
    ImageIndex := 4;
    ShortCut := TextToShortCut('Ctrl+Enter');
    OnExecute := ButtonClick;
  end;

  FActionDelete := TAction.Create(Self);
  with FActionDelete do begin
    Name := 'actRecDelete';
    Category := 'Tools';
    Caption := 'Delete';
    Hint := 'Удалить запись';
    ImageIndex := 5;
    ShortCut := TextToShortCut('Ctrl+D');
    OnExecute := ButtonClick;
  end;

  FActionJump := TAction.Create(Self);
  with FActionJump do begin
    Name := 'actLinkJump';
    Category := 'Tools';
    Caption := 'LinkJump';
    Hint := 'Перейти на запись';
    ImageIndex := 28;
    OnExecute := ButtonClick;
  end;

  FActionMoveUp := TAction.Create(Self);
  with FActionMoveUp do begin
    Name := 'actMoveUp';
    Category := 'Tools';
    Caption := 'MoveUp';
    Hint := 'Поместить выше';
    ImageIndex := 29;
    OnExecute := ButtonClick;
  end;

  FActionMoveDown := TAction.Create(Self);
  with FActionMoveDown do begin
    Name := 'actMoveDown';
    Category := 'Tools';
    Caption := 'MoveDown';
    Hint := 'Поместить ниже';
    ImageIndex := 30;
    OnExecute := ButtonClick;
  end;

  //

  FToolBar := TToolBar.Create(Self);
  FToolBar.Parent := Self;
  FToolBar.BorderWidth := 1;
  FToolBar.ButtonHeight := 28;
  FToolBar.ButtonWidth := 30;
  FToolBar.EdgeBorders := [];
  FToolBar.Flat := True;
  FToolBar.Images := fmGEDKeeper.ImageList_Buttons;
  FToolBar.ShowHint := True;
  FToolBar.Align := alTop;
  FToolBar.AutoSize := True;

  FBtnMoveDown := TToolButton.Create(Self);
  FBtnMoveDown.Parent := FToolBar;
  FBtnMoveDown.Wrap := True;
  FBtnMoveDown.Action := FActionMoveDown;

  FBtnMoveUp := TToolButton.Create(Self);
  FBtnMoveUp.Parent := FToolBar;
  FBtnMoveUp.Wrap := True;
  FBtnMoveUp.Action := FActionMoveUp;

  FBtnLinkJump := TToolButton.Create(Self);
  FBtnLinkJump.Parent := FToolBar;
  FBtnLinkJump.Wrap := True;
  FBtnLinkJump.Action := FActionJump;

  FBtnDelete := TToolButton.Create(Self);
  FBtnDelete.Parent := FToolBar;
  FBtnDelete.Wrap := True;
  FBtnDelete.Action := FActionDelete;

  FBtnEdit := TToolButton.Create(Self);
  FBtnEdit.Parent := FToolBar;
  FBtnEdit.Wrap := True;
  FBtnEdit.Action := FActionEdit;

  FBtnAdd := TToolButton.Create(Self);
  FBtnAdd.Parent := FToolBar;
  FBtnAdd.Wrap := True;
  FBtnAdd.Action := FActionAdd;

  case FListMode of
    lmView: begin
      FList := TGKListView.Create(Self);
      with TGKListView(FList) do begin
        Parent := Self;
        Align := alClient;
        HideSelection := False;
        ReadOnly := True;
        RowSelect := True;
        ViewStyle := vsReport;

        SortType := stText;
        SortColumn := 0;
        SortDirection := sdAscending;
        ShowSortSign := True;

        OnDblClick := ListDblClick;
        OnKeyDown := ListKeyDown;
      end;
    end;

    lmBox: begin
      FList := TListBox.Create(Self);
      with TListBox(FList) do begin
        Parent := Self;
        Align := alClient;
        Style := lbOwnerDrawVariable;

        OnDrawItem := LBDrawItem;
        OnMeasureItem := LBMeasureItem;

        OnDblClick := ListDblClick;
        OnKeyDown := ListKeyDown;
      end;
    end;
  end;

  FToolBar.Align := alRight;
  FToolBar.List := True;

  SetButtons([lbAdd..lbDelete]);
end;

destructor TSheetList.Destroy;
begin
  FList.Free;

  FBtnDelete.Free;
  FBtnEdit.Free;
  FBtnAdd.Free;

  FToolBar.Free;

  inherited Destroy;
end;

procedure TSheetList.ButtonClick(Sender: TObject);
begin
  if (Sender = FActionAdd) then ItemAdd()
  else
  if (Sender = FActionEdit) then ItemEdit()
  else
  if (Sender = FActionDelete) then ItemDelete()
  else
  if (Sender = FActionJump) then ItemJump()
  else
  if (Sender = FActionMoveUp) then ItemMoveUp()
  else
  if (Sender = FActionMoveDown) then ItemMoveDown();
end;

procedure TSheetList.ListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) then
    case Key of
      Ord('I'): ItemAdd();
      Ord('D'): ItemDelete();
      VK_RETURN: ItemEdit();
    end;
end;

function TSheetList.GetSelectedData(): TObject;
begin
  Result := nil;

  case FListMode of
    lmView: begin
      if (TGKListView(FList).Selected <> nil)
      then Result := TGKListView(FList).Selected.Data;
    end;
    lmBox: begin
      if (TListBox(FList).ItemIndex > -1)
      then Result := TListBox(FList).Items.Objects[TListBox(FList).ItemIndex];
    end;
  end;
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

procedure TSheetList.ListDblClick(Sender: TObject);
begin
  ItemEdit();
end;

(*procedure TSheetList.SetActionList(const Value: TActionList);
begin
  {if (Value = nil) then begin
    FActionAdd.ActionList := nil;
    FActionEdit.ActionList := nil;
    FActionDelete.ActionList := nil;
  end else begin
    FActionAdd.ActionList := Value;
    FActionEdit.ActionList := Value;
    FActionDelete.ActionList := Value;
  end;}

  FActionList := Value;
end;*)

procedure TSheetList.SheetShow(Sender: TObject);
begin
  FList.SetFocus();
end;

procedure TSheetList.SetButtons(const Value: TListButtons);
begin
  FButtons := Value;

  FActionAdd.Visible := (lbAdd in FButtons);
  FActionDelete.Visible := (lbDelete in FButtons);
  FActionEdit.Visible := (lbEdit in FButtons);
  FActionJump.Visible := (lbJump in FButtons);
  FActionMoveUp.Visible := (lbMoveUp in FButtons);
  FActionMoveDown.Visible := (lbMoveDown in FButtons);
end;

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
        {$IFNDEF DELPHI_NET}PChar{$ENDIF}(item), -1, rt, DT_WORDBREAK or DT_CALCRECT,
        {$IFNDEF DELPHI_NET}@{$ENDIF}params);
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

  DrawTextEx(TListBox(Control).Canvas.Handle,
    {$IFNDEF DELPHI_NET}PChar{$ENDIF}(item), -1, Rect, DT_WORDBREAK,
    {$IFNDEF DELPHI_NET}@{$ENDIF}params);
end;

procedure TSheetList.Columns_BeginUpdate();
begin
  if (FList is TGKListView)
  then (FList as TGKListView).Columns.BeginUpdate();
end;

procedure TSheetList.Columns_Clear();
begin
  if (FList is TGKListView)
  then (FList as TGKListView).Columns.Clear();
end;

procedure TSheetList.Columns_EndUpdate();
begin
  if (FList is TGKListView)
  then (FList as TGKListView).Columns.EndUpdate();
end;

procedure TSheetList.AddColumn(aCaption: string; aWidth: Integer;
  aAutoSize: Boolean);
begin
  if (FList is TGKListView)
  then AddListColumn((FList as TGKListView), aCaption, aWidth, aAutoSize);
end;

procedure TSheetList.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;

  FActionAdd.Enabled := not(FReadOnly);
  FActionDelete.Enabled := not(FReadOnly);
  FActionEdit.Enabled := not(FReadOnly);
  FActionMoveUp.Enabled := not(FReadOnly);
  FActionMoveDown.Enabled := not(FReadOnly);

  if FReadOnly then begin
    if (FList is TGKListView)
    then (FList as TGKListView).Color := clBtnFace
    else (FList as TListBox).Color := clBtnFace;
  end else begin
    if (FList is TGKListView)
    then (FList as TGKListView).Color := clWindow
    else (FList as TListBox).Color := clWindow;
  end;
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

procedure TListManager.InitFilter(aFilter: TPersonsFilter);
begin
  //
end;

procedure TListManager.UpdateTitles(aList: TGKListView; isMain: Boolean);
begin
  aList.Columns.BeginUpdate;
  try
    aList.Columns.Clear;
    UpdateColumns(aList, isMain);
  finally
    aList.Columns.EndUpdate;
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
  then filter_abd := 0
  else filter_abd := StrToDate(aFilter.AliveBeforeDate);

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

function TIndividualListMan.CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean;

  function HasPlace(): Boolean;
  var
    i: Integer;
    place: string;
    addr: Boolean;
  begin
    Result := False;

    addr := fmGEDKeeper.Options.PlacesWithAddress;

    for i := 0 to FRec.IndividualEventsCount - 1 do begin
      place := GetPlaceStr(FRec.IndividualEvents[i], addr);
      Result := IsMatchesMask(place, aFilter.Residence);
      if (Result) then Exit;
    end;
  end;

  function HasEventVal(): Boolean;
  var
    i: Integer;
  begin
    Result := False;

    for i := 0 to FRec.IndividualEventsCount - 1 do begin
      Result := IsMatchesMask(FRec.IndividualEvents[i].StringValue, aFilter.EventVal);
      if (Result) then Exit;
    end;
  end;

var
  bdt, ddt: TDateTime;
  isLive: Boolean;
  bdy, ddy: Integer;
  m, d: Word;
  ev: TGEDCOMCustomEvent;
  i, y: Integer;
  bd_ev, dd_ev: TGEDCOMCustomEvent;
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
      GetIndependentDate(ev.Detail.Date.Value, y, m, d);
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
  or ((aFilter.Name <> '*') and not(IsMatchesMask(GetNameStr(FRec), aFilter.Name)))
  or ((aFilter.Residence <> '*') and not(HasPlace()))
  or ((aFilter.EventVal <> '*') and not(HasEventVal()))
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
      if (bd_ev = nil) then bdt := 0.0 else bdt := GEDCOMDateToDate(bd_ev.Detail.Date.Value);
      if (dd_ev = nil) then ddt := 0.0 else ddt := GEDCOMDateToDate(dd_ev.Detail.Date.Value);

      if not(((bdt = 0) or ((bdt <> 0) and (bdt < filter_abd)))
         and ((ddt = 0) or ((ddt <> 0) and (ddt > filter_abd))))
      then Exit;
    end;
    lmTimeLine: begin
      if (bd_ev = nil) then bdy := -1 else GetIndependentDate(bd_ev.Detail.Date.Value, bdy, m, d);
      if (dd_ev = nil) then ddy := -1 else GetIndependentDate(dd_ev.Detail.Date.Value, ddy, m, d);

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

procedure TIndividualListMan.UpdateItem(aItem: TListItem; isMain: Boolean);
var
  i: Integer;
  pct: TPersonColumnType;
  columns: TPersonColumnsList;
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
    then residence := GetPlaceStr(ev, fmGEDKeeper.Options.PlacesWithAddress)
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
        then GetNameParts(FRec, f, n, p);

        case fmGEDKeeper.Options.DefNameFormat of
          nfFNP: begin
            aItem.SubItems.Add(GetNameStr(FRec));
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
        aItem.SubItems.Add(GetNickStr(FRec));

      pctSex:
        aItem.SubItems.Add(SexData[FRec.Sex].ViewSign);

      pctBirthDate:
        aItem.SubItems.Add(GEDCOMEventToDateStr(bd_ev, fmGEDKeeper.Options.DefDateFormat));

      pctDeathDate:
        aItem.SubItems.Add(GEDCOMEventToDateStr(dd_ev, fmGEDKeeper.Options.DefDateFormat));

      pctBirthPlace:
        aItem.SubItems.Add(GetPlaceStr(bd_ev, False));

      pctDeathPlace:
        aItem.SubItems.Add(GetPlaceStr(dd_ev, False));

      pctResidence:
        aItem.SubItems.Add(residence);

      pctAge:
        if (isMain) then aItem.SubItems.Add(GetAge(FRec, age_year));

      pctLifeExpectancy:
        if (isMain) then aItem.SubItems.Add(GetLifeExpectancy(FRec));

      pctDaysForBirth:
        if (isMain) then aItem.SubItems.Add(GetDaysForBirth(FRec));

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
        if (isMain) then aItem.SubItems.Add(GetChangeDate(FRec));

      pctBookmark: if (isMain) then begin
        if (FRec.Bookmark)
        then aItem.SubItems.Add('*')
        else aItem.SubItems.Add(' ');
      end;
    end;
  end;
end;

function TIndividualListMan.GetColumnValue(aColIndex: Integer; isMain: Boolean): string;
var
  pct: TPersonColumnType;
  sub_index: Integer;
  f, n, p: string;
  ev: TGEDCOMCustomEvent;
begin
  pct := TPersonColumnType(FColumnsMap[aColIndex].col_type);
  sub_index := FColumnsMap[aColIndex].col_subtype;

  case pct of
    pctPatriarch: begin
      if (FRec.Patriarch)
      then Result := '*'
      else Result := ' ';
    end;

    pctName: begin
      if (fmGEDKeeper.Options.DefNameFormat in [nfF_NP, nfF_N_P])
      then GetNameParts(FRec, f, n, p);

      case fmGEDKeeper.Options.DefNameFormat of
        nfFNP: begin
          Result := GetNameStr(FRec);
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

    pctNick: Result := GetNickStr(FRec);
    pctSex: Result := SexData[FRec.Sex].ViewSign;

    pctBirthDate, pctBirthPlace: begin
      ev := GetIndividualEvent(FRec, 'BIRT');

      case pct of
        pctBirthDate: Result := GEDCOMEventToDateStr(ev, fmGEDKeeper.Options.DefDateFormat);
        pctBirthPlace: Result := GetPlaceStr(ev, False);
      end;
    end;

    pctDeathDate, pctDeathPlace: begin
      ev := GetIndividualEvent(FRec, 'DEAT');

      case pct of
        pctDeathDate: Result := GEDCOMEventToDateStr(ev, fmGEDKeeper.Options.DefDateFormat);
        pctDeathPlace: Result := GetPlaceStr(ev, False);
      end;
    end;

    pctResidence: Result := GetResidencePlace(FRec, fmGEDKeeper.Options.PlacesWithAddress);

    pctAge: Result := GetAge(FRec);
    pctLifeExpectancy: Result := GetLifeExpectancy(FRec);
    pctDaysForBirth: Result := GetDaysForBirth(FRec);
    pctGroups: Result := GetGroups();

    pctReligion: Result := GetAttributeValue(FRec, 'RELI');
    pctNationality: Result := GetAttributeValue(FRec, 'NATI');
    pctEducation: Result := GetAttributeValue(FRec, 'EDUC');
    pctOccupation: Result := GetAttributeValue(FRec, 'OCCU');
    pctCaste: Result := GetAttributeValue(FRec, 'CAST');

    pctMili: Result := GetAttributeValue(FRec, '_MILI');
    pctMiliInd: Result := GetAttributeValue(FRec, '_MILI_IND');
    pctMiliDis: Result := GetAttributeValue(FRec, '_MILI_DIS');
    pctMiliRank: Result := GetAttributeValue(FRec, '_MILI_RANK');

    pctChangeDate: Result := GetChangeDate(FRec);

    pctBookmark: begin
      if (FRec.Bookmark)
      then Result := '*'
      else Result := ' ';
    end;
  end;
end;

procedure TIndividualListMan.UpdateColumns(aList: TGKListView; isMain: Boolean);
var
  cols: Integer;

  procedure SetColMap(aType, aSubType: Byte);
  begin
    Inc(cols);
    FColumnsMap[cols].col_type := aType;
    FColumnsMap[cols].col_subtype := aSubType;
  end;

var
  i: Integer;
  columns: TPersonColumnsList;
  col_type: TPersonColumnType;
  asz: Boolean;
begin
  {if (isMain)
  then }columns := fmGEDKeeper.Options.ListPersonsColumns{
  else columns := DefPersonColumns};

  cols := 0;

  AddListColumn(aList, '№', 50);

  for i := 0 to High(columns) do
    if columns[i].colActive then begin
      col_type := columns[i].colType;

      case col_type of
        pctPatriarch: begin
          AddListColumn(aList, 'П', 25);
          SetColMap(Ord(col_type), 0);
        end;

        pctName: begin
          asz := False;

          case fmGEDKeeper.Options.DefNameFormat of
            nfFNP: begin
              AddListColumn(aList, 'Фамилия,Имя,Отчество', 300, asz);
              SetColMap(Ord(col_type), 0);
            end;

            nfF_NP: begin
              AddListColumn(aList, 'Фамилия', 150, asz);
              SetColMap(Ord(col_type), 0);
              AddListColumn(aList, 'Имя,Отчество', 150, asz);
              SetColMap(Ord(col_type), 1);
            end;

            nfF_N_P: begin
              AddListColumn(aList, 'Фамилия', 150, asz);
              SetColMap(Ord(col_type), 0);
              AddListColumn(aList, 'Имя', 100, asz);
              SetColMap(Ord(col_type), 1);
              AddListColumn(aList, 'Отчество', 150, asz);
              SetColMap(Ord(col_type), 2);
            end;
          end;
        end;

        pctNick, pctSex, pctBirthDate, pctDeathDate,
        pctBirthPlace, pctDeathPlace, pctResidence: begin
          AddListColumn(aList, PersonColumnsName[col_type].Name, PersonColumnsName[col_type].DefWidth);
          SetColMap(Ord(col_type), 0);
        end;

        pctAge, pctLifeExpectancy, pctDaysForBirth, pctGroups,
        pctReligion, pctNationality, pctEducation, pctOccupation, pctCaste,
        pctMili, pctMiliInd, pctMiliDis, pctMiliRank,
        pctChangeDate, pctBookmark: if isMain then begin
          AddListColumn(aList, PersonColumnsName[col_type].Name, PersonColumnsName[col_type].DefWidth);
          SetColMap(Ord(col_type), 0);
        end;
      end;
    end;
end;

{ TFamilyListMan }

function TFamilyListMan.CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean;
begin
  Result := False;

  if (FRec.Restriction = rnPrivacy) and (aShieldState <> ssNone)
  then Exit;

  if (aFilter.List = flSelector)
  and ((aFilter.Name <> '*') and not(IsMatchesMask(GetFamilyStr(FRec), aFilter.Name)))
  then Exit;

  Result := True;
end;

procedure TFamilyListMan.Fetch(aRec: TGEDCOMRecord);
begin
  FRec := TGEDCOMFamilyRecord(aRec);
end;

procedure TFamilyListMan.UpdateItem(aItem: TListItem; isMain: Boolean);
begin
  aItem.SubItems.Add(GetFamilyStr(FRec));
  aItem.SubItems.Add(GetMarriageDate(FRec, fmGEDKeeper.Options.DefDateFormat));

  if isMain
  then aItem.SubItems.Add(GetChangeDate(FRec));
end;

function TFamilyListMan.GetColumnValue(aColIndex: Integer; isMain: Boolean): string;
begin
  case aColIndex of
    1: Result := GetFamilyStr(FRec);
    2: Result := GetMarriageDate(FRec, fmGEDKeeper.Options.DefDateFormat);
    3: Result := GetChangeDate(FRec);
    else Result := '';
  end;
end;

procedure TFamilyListMan.UpdateColumns(aList: TGKListView; isMain: Boolean);
begin
  AddListColumn(aList, '№', 50);
  AddListColumn(aList, 'Супруги', 300);
  AddListColumn(aList, 'Дата брака', 100);
  if isMain then begin
    AddListColumn(aList, 'Изменено', 150);
  end;
end;

{ TNoteListMan }

function TNoteListMan.CheckFilter(aFilter: TPersonsFilter;
  aShieldState: TShieldState): Boolean;
begin
  Result := True;
end;

procedure TNoteListMan.Fetch(aRec: TGEDCOMRecord);
begin
  FRec := TGEDCOMNoteRecord(aRec);
end;

procedure TNoteListMan.UpdateItem(aItem: TListItem; isMain: Boolean);
var
  st: string;
begin
  if (FRec.Notes.Count > 0) then begin
    st := Trim(FRec.Notes[0]);
    if (st = '') and (FRec.Notes.Count > 1)
    then st := Trim(FRec.Notes[1]);
  end else st := '';

  aItem.SubItems.Add(st);

  if isMain
  then aItem.SubItems.Add(GetChangeDate(FRec));
end;

function TNoteListMan.GetColumnValue(aColIndex: Integer; isMain: Boolean): string;
var
  st: string;
begin
  case aColIndex of
    1: begin
      if (FRec.Notes.Count > 0) then begin
        st := Trim(FRec.Notes[0]);
        if (st = '') and (FRec.Notes.Count > 1)
        then st := Trim(FRec.Notes[1]);
      end else st := '';

      Result := st;
    end;
    2: Result := GetChangeDate(FRec);
    else Result := '';
  end;
end;

procedure TNoteListMan.UpdateColumns(aList: TGKListView; isMain: Boolean);
begin
  AddListColumn(aList, '№', 50);
  AddListColumn(aList, 'Заметка', 400);

  if isMain then begin
    AddListColumn(aList, 'Изменено', 150);
  end;
end;

{ TMultimediaListMan }

function TMultimediaListMan.CheckFilter(aFilter: TPersonsFilter;
  aShieldState: TShieldState): Boolean;
var
  file_ref: TGEDCOMFileReferenceWithTitle;
begin
  Result := False;

  file_ref := FRec.FileReferences[0];

  if (aFilter.List = flSelector)
  and ((aFilter.Name <> '*') and not(IsMatchesMask(file_ref.Title, aFilter.Name)))
  then Exit;

  Result := True;
end;

procedure TMultimediaListMan.Fetch(aRec: TGEDCOMRecord);
begin
  FRec := TGEDCOMMultimediaRecord(aRec);
end;

procedure TMultimediaListMan.UpdateItem(aItem: TListItem; isMain: Boolean);
var
  file_ref: TGEDCOMFileReferenceWithTitle;
begin
  file_ref := FRec.FileReferences[0];

  if (file_ref = nil) then begin
    aItem.SubItems.Add('error ' + FRec.XRef);
    Exit;
  end;

  aItem.SubItems.Add(file_ref.Title);
  aItem.SubItems.Add(MediaTypes[file_ref.MediaType].Name);

  if isMain then begin
    aItem.SubItems.Add(file_ref.StringValue);
    aItem.SubItems.Add(GetChangeDate(FRec));
  end;
end;

function TMultimediaListMan.GetColumnValue(aColIndex: Integer; isMain: Boolean): string;
var
  file_ref: TGEDCOMFileReferenceWithTitle;
begin
  file_ref := FRec.FileReferences[0];

  case aColIndex of
    1: Result := file_ref.Title;
    2: Result := MediaTypes[file_ref.MediaType].Name;
    3: Result := file_ref.StringValue;
    4: Result := GetChangeDate(FRec);
    else Result := '';
  end;
end;

procedure TMultimediaListMan.UpdateColumns(aList: TGKListView; isMain: Boolean);
begin
  AddListColumn(aList, '№', 50);
  AddListColumn(aList, 'Название', 150);
  AddListColumn(aList, 'Тип', 85);

  if isMain then begin
    AddListColumn(aList, 'Файл', 300);
    AddListColumn(aList, 'Изменено', 150);
  end;
end;

{ TSourceListMan }

function TSourceListMan.CheckFilter(aFilter: TPersonsFilter;
  aShieldState: TShieldState): Boolean;
begin
  Result := False;

  if (aFilter.List = flSelector)
  and ((aFilter.Name <> '*') and not(IsMatchesMask(FRec.FiledByEntry, aFilter.Name)))
  then Exit;

  Result := True;
end;

procedure TSourceListMan.Fetch(aRec: TGEDCOMRecord);
begin
  FRec := TGEDCOMSourceRecord(aRec);
end;

procedure TSourceListMan.UpdateItem(aItem: TListItem; isMain: Boolean);
begin
  aItem.SubItems.Add(Trim(FRec.FiledByEntry));

  if isMain then begin
    aItem.SubItems.Add(Trim(FRec.Originator.Text));
    aItem.SubItems.Add(Trim(FRec.Title.Text));
    aItem.SubItems.Add(GetChangeDate(FRec));
  end;
end;

function TSourceListMan.GetColumnValue(aColIndex: Integer; isMain: Boolean): string;
begin
  case aColIndex of
    1: Result := Trim(FRec.FiledByEntry);
    2: Result := Trim(FRec.Originator.Text);
    3: Result := Trim(FRec.Title.Text);
    4: Result := GetChangeDate(FRec);
    else Result := '';
  end;
end;

procedure TSourceListMan.UpdateColumns(aList: TGKListView; isMain: Boolean);
begin
  AddListColumn(aList, '№', 50);
  AddListColumn(aList, 'Краткое название', 120);

  if isMain then begin
    AddListColumn(aList, 'Автор', 200);
    AddListColumn(aList, 'Название', 200);
    AddListColumn(aList, 'Изменено', 150);
  end;
end;

{ TRepositoryListMan }

function TRepositoryListMan.CheckFilter(aFilter: TPersonsFilter;
  aShieldState: TShieldState): Boolean;
begin
  Result := False;

  if (aFilter.List = flSelector)
  and ((aFilter.Name <> '*') and not(IsMatchesMask(FRec.RepositoryName, aFilter.Name)))
  then Exit;

  Result := True;
end;

procedure TRepositoryListMan.Fetch(aRec: TGEDCOMRecord);
begin
  FRec := TGEDCOMRepositoryRecord(aRec);
end;

procedure TRepositoryListMan.UpdateColumns(aList: TGKListView; isMain: Boolean);
begin
  AddListColumn(aList, '№', 50);
  AddListColumn(aList, 'Архив', 400);

  if isMain then begin
    AddListColumn(aList, 'Изменено', 150);
  end;
end;

procedure TRepositoryListMan.UpdateItem(aItem: TListItem; isMain: Boolean);
begin
  aItem.SubItems.Add(FRec.RepositoryName);

  if isMain
  then aItem.SubItems.Add(GetChangeDate(FRec));
end;

function TRepositoryListMan.GetColumnValue(aColIndex: Integer; isMain: Boolean): string;
begin
  case aColIndex of
    1: Result := FRec.RepositoryName;
    2: Result := GetChangeDate(FRec);
    else Result := '';
  end;
end;

{ TGroupListMan }

function TGroupListMan.CheckFilter(aFilter: TPersonsFilter;
  aShieldState: TShieldState): Boolean;
begin
  Result := False;

  if (aFilter.List = flSelector)
  and ((aFilter.Name <> '*') and not(IsMatchesMask(FRec.Name, aFilter.Name)))
  then Exit;

  Result := True;
end;

procedure TGroupListMan.Fetch(aRec: TGEDCOMRecord);
begin
  FRec := TGEDCOMGroupRecord(aRec);
end;

procedure TGroupListMan.UpdateColumns(aList: TGKListView; isMain: Boolean);
begin
  AddListColumn(aList, '№', 50);
  AddListColumn(aList, 'Группа', 400);

  if isMain then begin
    AddListColumn(aList, 'Изменено', 150);
  end;
end;

procedure TGroupListMan.UpdateItem(aItem: TListItem; isMain: Boolean);
begin
  aItem.SubItems.Add(FRec.Name);

  if isMain
  then aItem.SubItems.Add(GetChangeDate(FRec));
end;

function TGroupListMan.GetColumnValue(aColIndex: Integer; isMain: Boolean): string;
begin
  case aColIndex of
    1: Result := FRec.Name;
    2: Result := GetChangeDate(FRec);
    else Result := '';
  end;
end;

{ TResearchListMan }

function TResearchListMan.CheckFilter(aFilter: TPersonsFilter;
  aShieldState: TShieldState): Boolean;
begin
  Result := False;

  if (aFilter.List = flSelector)
  and ((aFilter.Name <> '*') and not(IsMatchesMask(FRec.Name, aFilter.Name)))
  then Exit;

  Result := True;
end;

procedure TResearchListMan.Fetch(aRec: TGEDCOMRecord);
begin
  FRec := TGEDCOMResearchRecord(aRec);
end;

procedure TResearchListMan.UpdateColumns(aList: TGKListView; isMain: Boolean);
begin
  AddListColumn(aList, '№', 50);
  AddListColumn(aList, 'Исследование', 300);
  AddListColumn(aList, 'Приоритет', 90);
  AddListColumn(aList, 'Состояние', 90);
  AddListColumn(aList, 'Запущено', 90);
  AddListColumn(aList, 'Завершено', 90);
  AddListColumn(aList, 'Процент', 90);

  if isMain then begin
    AddListColumn(aList, 'Изменено', 150);
  end;
end;

procedure TResearchListMan.UpdateItem(aItem: TListItem; isMain: Boolean);
begin
  aItem.SubItems.Add(FRec.Name);
  aItem.SubItems.Add(PriorityNames[FRec.Priority]);
  aItem.SubItems.Add(StatusNames[FRec.Status]);
  aItem.SubItems.Add(GEDCOMDateToStr(FRec.StartDate, fmGEDKeeper.Options.DefDateFormat));
  aItem.SubItems.Add(GEDCOMDateToStr(FRec.StopDate, fmGEDKeeper.Options.DefDateFormat));
  aItem.SubItems.Add(IntToStr(FRec.Percent));

  if isMain
  then aItem.SubItems.Add(GetChangeDate(FRec));
end;

function TResearchListMan.GetColumnValue(aColIndex: Integer; isMain: Boolean): string;
begin
  case aColIndex of
    1: Result := FRec.Name;
    2: Result := PriorityNames[FRec.Priority];
    3: Result := StatusNames[FRec.Status];
    4: Result := GEDCOMDateToStr(FRec.StartDate, fmGEDKeeper.Options.DefDateFormat);
    5: Result := GEDCOMDateToStr(FRec.StopDate, fmGEDKeeper.Options.DefDateFormat);
    6: Result := IntToStr(FRec.Percent);
    7: Result := GetChangeDate(FRec);
    else Result := '';
  end;
end;

{ TTaskListMan }

function TTaskListMan.CheckFilter(aFilter: TPersonsFilter;
  aShieldState: TShieldState): Boolean;
begin
  Result := False;

  if (aFilter.List = flSelector)
  and ((aFilter.Name <> '*') and not(IsMatchesMask(GetTaskGoalStr(FTree, FRec), aFilter.Name)))
  then Exit;

  Result := True;
end;

procedure TTaskListMan.Fetch(aRec: TGEDCOMRecord);
begin
  FRec := TGEDCOMTaskRecord(aRec);
end;

procedure TTaskListMan.UpdateColumns(aList: TGKListView; isMain: Boolean);
begin
  AddListColumn(aList, '№', 50);
  AddListColumn(aList, 'Цель', 300);
  AddListColumn(aList, 'Приоритет', 90);
  AddListColumn(aList, 'Запущено', 90);
  AddListColumn(aList, 'Завершено', 90);

  if isMain then begin
    AddListColumn(aList, 'Изменено', 150);
  end;
end;

procedure TTaskListMan.UpdateItem(aItem: TListItem; isMain: Boolean);
begin
  aItem.SubItems.Add(GetTaskGoalStr(FTree, FRec));
  aItem.SubItems.Add(PriorityNames[FRec.Priority]);
  aItem.SubItems.Add(GEDCOMDateToStr(FRec.StartDate, fmGEDKeeper.Options.DefDateFormat));
  aItem.SubItems.Add(GEDCOMDateToStr(FRec.StopDate, fmGEDKeeper.Options.DefDateFormat));

  if isMain
  then aItem.SubItems.Add(GetChangeDate(FRec));
end;

function TTaskListMan.GetColumnValue(aColIndex: Integer; isMain: Boolean): string;
begin
  case aColIndex of
    1: Result := GetTaskGoalStr(FTree, FRec);
    2: Result := PriorityNames[FRec.Priority];
    3: Result := GEDCOMDateToStr(FRec.StartDate, fmGEDKeeper.Options.DefDateFormat);
    4: Result := GEDCOMDateToStr(FRec.StopDate, fmGEDKeeper.Options.DefDateFormat);
    5: Result := GetChangeDate(FRec);
    else Result := '';
  end;
end;

{ TCommunicationListMan }

function TCommunicationListMan.CheckFilter(aFilter: TPersonsFilter;
  aShieldState: TShieldState): Boolean;
begin
  Result := False;

  if (aFilter.List = flSelector)
  and ((aFilter.Name <> '*') and not(IsMatchesMask(FRec.Name, aFilter.Name)))
  then Exit;

  Result := True;
end;

procedure TCommunicationListMan.Fetch(aRec: TGEDCOMRecord);
begin
  FRec := TGEDCOMCommunicationRecord(aRec);
end;

procedure TCommunicationListMan.UpdateColumns(aList: TGKListView; isMain: Boolean);
begin
  AddListColumn(aList, '№', 50);
  AddListColumn(aList, 'Тема', 300);
  AddListColumn(aList, 'Корреспондент', 200);
  AddListColumn(aList, 'Тип', 90);
  AddListColumn(aList, 'Дата', 90);

  if isMain then begin
    AddListColumn(aList, 'Изменено', 150);
  end;
end;

procedure TCommunicationListMan.UpdateItem(aItem: TListItem; isMain: Boolean);
begin
  aItem.SubItems.Add(FRec.Name);
  aItem.SubItems.Add(GetCorresponderStr(FTree, FRec, False));
  aItem.SubItems.Add(CommunicationNames[FRec.CommunicationType]);
  aItem.SubItems.Add(GEDCOMDateToStr(FRec.Date, fmGEDKeeper.Options.DefDateFormat));

  if isMain
  then aItem.SubItems.Add(GetChangeDate(FRec));
end;

function TCommunicationListMan.GetColumnValue(aColIndex: Integer; isMain: Boolean): string;
begin
  case aColIndex of
    1: Result := FRec.Name;
    2: Result := GetCorresponderStr(FTree, FRec, False);
    3: Result := CommunicationNames[FRec.CommunicationType];
    4: Result := GEDCOMDateToStr(FRec.Date, fmGEDKeeper.Options.DefDateFormat);
    5: Result := GetChangeDate(FRec);
    else Result := '';
  end;
end;

{ TLocationListMan }

function TLocationListMan.CheckFilter(aFilter: TPersonsFilter;
  aShieldState: TShieldState): Boolean;
begin
  Result := False;

  if (aFilter.List = flSelector)
  and ((aFilter.Name <> '*') and not(IsMatchesMask(FRec.Name, aFilter.Name)))
  then Exit;

  Result := True;
end;

procedure TLocationListMan.Fetch(aRec: TGEDCOMRecord);
begin
  FRec := TGEDCOMLocationRecord(aRec);
end;

procedure TLocationListMan.UpdateColumns(aList: TGKListView; isMain: Boolean);
begin
  AddListColumn(aList, '№', 50);
  AddListColumn(aList, 'Название', 300);
  AddListColumn(aList, 'Широта', 120);
  AddListColumn(aList, 'Долгота', 120);

  if isMain then begin
    AddListColumn(aList, 'Изменено', 150);
  end;
end;

procedure TLocationListMan.UpdateItem(aItem: TListItem; isMain: Boolean);
begin
  aItem.SubItems.Add(FRec.Name);
  aItem.SubItems.Add(FRec.Map.Lati);
  aItem.SubItems.Add(FRec.Map.Long);

  if isMain
  then aItem.SubItems.Add(GetChangeDate(FRec));
end;

function TLocationListMan.GetColumnValue(aColIndex: Integer; isMain: Boolean): string;
begin
  case aColIndex of
    1: Result := FRec.Name;
    2: Result := FRec.Map.Lati;
    3: Result := FRec.Map.Long;
    4: Result := GetChangeDate(FRec);
    else Result := '';
  end;
end;

{ TRecordsView }

constructor TRecordsView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FContentList := TList.Create;

  OwnerData := True;
  OnData := ListGetItemData;
  OnColumnClick := ListColumnClick;
  OnCustomDrawItem := ListCustomDrawItem;

  //OwnerDraw := True;
  //OnMeasureItem := ListMeasureItem;

  FListMan := nil;
  FRecordType := rtNone;
end;

destructor TRecordsView.Destroy;
begin
  if Assigned(FListMan) then FreeAndNil(FListMan);

  FContentList.Free;
  inherited Destroy;
end;

function TRecordsView.xCompare(Item1, Item2: TObject): Integer;
var
  val1, val2: string;
  f: Integer;
begin
  if (SortColumn = 0) then begin
    val1 := GetXRefNum(TGEDCOMRecord(Item1));
    val2 := GetXRefNum(TGEDCOMRecord(Item2));
  end else begin
    FListMan.Fetch(TGEDCOMRecord(Item1));
    val1 := FListMan.GetColumnValue(SortColumn, FIsMainList);
    FListMan.Fetch(TGEDCOMRecord(Item2));
    val2 := FListMan.GetColumnValue(SortColumn, FIsMainList);
  end;

  if (SortDirection = sdAscending) then f := 1 else f := -1;

  Result := agCompare(val1, val2) * f;
end;

procedure TRecordsView.ListColumnClick(Sender: TObject; Column: TListColumn);
var
  rec: TGEDCOMRecord;
begin
  {$IFDEF PROFILER}Profiler.Mark(12, True);{$ENDIF}

  rec := GetSelectedRecord();
  {$IFNDEF DELPHI_NET}
  MergeSort(FContentList, xCompare); // 16.5ms
  {$ELSE}
  QuickSort(FContentList, xCompare); // 32ms
  {$ENDIF}
  SelectItemByRec(rec);

  Invalidate();

  {$IFDEF PROFILER}Profiler.Mark(12, False);{$ENDIF}
end;

procedure TRecordsView.ListGetItemData(Sender: TObject; Item: TListItem);
var
  idx: Integer;
  rec: TGEDCOMRecord;
begin
  try
    {$IFDEF PROFILER}Profiler.Mark(11, True);{$ENDIF}

    if Assigned(Item) then begin
      idx := Item.Index;
      if (idx >= 0) and (idx < FContentList.Count) then begin
        rec := TGEDCOMRecord(FContentList[idx]);
        Item.Caption := IntToStr(GetId(rec));
        FListMan.Fetch(rec);
        FListMan.UpdateItem(Item, FIsMainList);
      end;
    end;

    {$IFDEF PROFILER}Profiler.Mark(11, False);{$ENDIF}
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
  if (Item = nil) then Exit;

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
  end;
end;

procedure TRecordsView.DeleteRecord(aRec: TGEDCOMRecord);
// warning! need for correct work virtual lists
begin
  FContentList.Remove(aRec);
end;

procedure TRecordsView.SelectItemByRec(aRec: TGEDCOMRecord);
var
  idx: Integer;
  item: TListItem;
begin
  idx := FContentList.IndexOf(aRec);
  if (idx >= 0) then begin
    item := Items[idx];
    Selected := item;
    item.MakeVisible(False);
  end;
end;

procedure TRecordsView.SetRecordType(const Value: TGEDCOMRecordType);
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
begin
  if (ItemIndex < 0) or (ItemIndex >= FContentList.Count)
  then Result := nil
  else Result := TGEDCOMRecord(FContentList[ItemIndex]);
end;

procedure TRecordsView.UpdateTitles();
begin
  FListMan.UpdateTitles(Self, FIsMainList);
end;

procedure TRecordsView.UpdateContents(aShieldState: TShieldState; aTitles: Boolean;
  aFilter: TPersonsFilter; aAutoSizeColumn: Integer = -1);
var
  i: Integer;
  rec: TGEDCOMRecord;
begin
  {$IFDEF PROFILER}Profiler.Mark(8, True);{$ENDIF}

  try
    FTotalCount := 0;
    FFilteredCount := 0;

    if (aTitles) and (FListMan <> nil)
    then FListMan.UpdateTitles(Self, FIsMainList);

    Items.BeginUpdate();
    try
      // фильтрация
      {$IFDEF PROFILER}Profiler.Mark(6, True);{$ENDIF}
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
      Items.Count := FContentList.Count;
      {$IFDEF PROFILER}Profiler.Mark(6, False);{$ENDIF}

      {$IFNDEF DELPHI_NET}
      MergeSort(FContentList, xCompare);
      {$ELSE}
      QuickSort(FContentList, xCompare);
      {$ENDIF}

      {$IFDEF PROFILER}Profiler.Mark(7, True);{$ENDIF}
      if (aAutoSizeColumn >= 0)
      then ResizeColumn(Self, aAutoSizeColumn);
      {$IFDEF PROFILER}Profiler.Mark(7, False);{$ENDIF}
    finally
      Items.EndUpdate();
    end;
  except
    on E: Exception do LogWrite('UpdateContents(): ' + E.Message);
  end;

  {$IFDEF PROFILER}Profiler.Mark(8, False);{$ENDIF}
end;

procedure TRecordsView.ListMeasureItem(Control: TWinControl; var AHeight: UINT);
begin
  AHeight := 50;
end;

end.
