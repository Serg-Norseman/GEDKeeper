unit GKSheetList;

interface

uses
  Types, Classes, Graphics, Controls, ComCtrls, ActnList, Menus, StdCtrls,
  bsCtrls, GKCommon;

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

procedure AddListColumn(aList: TCustomListControl; aCaption: string; aWidth: Integer;
  aAutoSize: Boolean = False);

procedure ResizeColumn(aList: TBSListView; aColumnIndex: Integer);
  
implementation

uses
  Windows, GKMain, ExtCtrls;

procedure AddListColumn(aList: TCustomListControl; aCaption: string; aWidth: Integer;
  aAutoSize: Boolean = False);
begin
  if not(aList is TBSListView) then Exit;

  with TBSListView(aList).Columns.Add() do begin
    Caption := aCaption;
    Width := aWidth;
    AutoSize := aAutoSize;
  end;
end;

procedure ResizeColumn(aList: TBSListView; aColumnIndex: Integer);
var
  i, max_w, w: Integer;
  item: TListItem;
begin
  if (aColumnIndex < 0) or (aColumnIndex >= aList.Columns.Count) then Exit;

  max_w := 0;

  for i := 0 to aList.Items.Count - 1 do begin
    item := aList.Items[i];

    if (aColumnIndex = 0)
    then w := aList.StringWidth(item.Caption)
    else w := aList.StringWidth(item.SubItems[aColumnIndex - 1]);

    if (max_w < w) then max_w := w;
  end;

  if (max_w <> 0)
  then aList.Columns[aColumnIndex].Width := max_w + 16;
end;

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
  FToolBar.Images := fmGEDKeeper.ImageList1;
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
      FList := TBSListView.Create(Self);
      with TBSListView(FList) do begin
        Parent := Self;
        Align := alClient;
        HideSelection := False;
        ReadOnly := True;
        RowSelect := True;
        SortType := stText;
        ViewStyle := vsReport;
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
      if (TListView(FList).Selected <> nil)
      then Result := TListView(FList).Selected.Data;
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

      FillChar(params, SizeOf(TDrawTextParams), #0);
      params.cbSize := SizeOf(TDrawTextParams);
      params.iLeftMargin := 3;
      params.iRightMargin := 3;

      Height := DrawTextEx(image.Canvas.Handle, PChar(item), -1, rt, DT_WORDBREAK or DT_CALCRECT, @params);
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

  FillChar(params, SizeOf(TDrawTextParams), #0);
  params.cbSize := SizeOf(TDrawTextParams);
  params.iLeftMargin := 3;
  params.iRightMargin := 3;

  DrawTextEx(TListBox(Control).Canvas.Handle, PChar(item), -1, Rect, DT_WORDBREAK, @params);
end;

procedure TSheetList.Columns_BeginUpdate();
begin
  if (FList is TListView)
  then (FList as TListView).Columns.BeginUpdate();
end;

procedure TSheetList.Columns_Clear();
begin
  if (FList is TListView)
  then (FList as TListView).Columns.Clear();
end;

procedure TSheetList.Columns_EndUpdate();
begin
  if (FList is TListView)
  then (FList as TListView).Columns.EndUpdate();
end;

procedure TSheetList.AddColumn(aCaption: string; aWidth: Integer;
  aAutoSize: Boolean);
begin
  if (FList is TBSListView)
  then AddListColumn((FList as TBSListView), aCaption, aWidth, aAutoSize);
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
    if (FList is TListView)
    then (FList as TListView).Color := clBtnFace
    else (FList as TListBox).Color := clBtnFace;
  end else begin
    if (FList is TListView)
    then (FList as TListView).Color := clWindow
    else (FList as TListBox).Color := clWindow;
  end;
end;

end.
