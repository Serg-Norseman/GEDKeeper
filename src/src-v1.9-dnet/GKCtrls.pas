unit GKCtrls; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.Drawing, System.Windows.Forms, System.Collections, GKUtils, VCLStub;

type
  TComboItem = class(System.Object)
  public
    Caption: string;
    Data: System.Object;

    constructor Create(aCaption: string; aData: System.Object);

    function ToString: string; override;
  end;

  TTaggedComboItem = class(System.Object)
  public
    Caption: string;
    Tag: Integer;

    constructor Create(aCaption: string; aTag: Integer);

    function ToString: string; override;
  end;

  TGKTreeNode = class(TreeNode)
  public
    Data: System.Object;

    constructor Create(text: string; data: System.Object);
  end;

  TGKMenuItem = class(System.Windows.Forms.MenuItem)
  public
    Tag: Integer;

    constructor Create(text: string; tag: Integer);
  end;

{==============================================================================}

type
  TExtListItem = class(ListViewItem)
  public
    Data: System.Object;
  end;

  TGKListView = class(System.Windows.Forms.ListView)
  strict private
    type
      TListViewColumnSorter = class(System.Object, IComparer)
      private
        FSortColumn: Integer;
        FSortOrder: SortOrder;
        FObjCompare: CaseInsensitiveComparer;
      public
        constructor Create;

        function Compare(x, y: System.Object): Integer;

        property SortColumn: Integer read FSortColumn write FSortColumn;
        property Order: SortOrder read FSortOrder write FSortOrder;
      end;

    var
      lvwColumnSorter: TListViewColumnSorter;

    procedure ColClick(sender: System.Object; e: System.Windows.Forms.ColumnClickEventArgs);
  public
    constructor Create(AOwner: System.Windows.Forms.Control); override;

    procedure AddListColumn(aCaption: string; aWidth: Integer;
      aAutoSize: Boolean = False);
    function AddItem(title: string; data: System.Object): TExtListItem;
    function SelectedItem(): TExtListItem;

    procedure ResizeColumn(aColumnIndex: Integer);
  end;

{==============================================================================}

{
 It work something like <HTML TAGS>

 ~~  = ~
 ~u~ = Underline
 ~b~ = Bold
 ~i~ = Italic
 ~s~ = Strike Out
 ~+x~ = Increment Font Size by x
 ~-x~ = Decrement Font Size by x
 ~0~ = Reset font to default
 ~r~ = (horizontal) Rule like <hr>

 ~ColorIdent~ = Set Font color to ColorIdent (clBlue, clYellow...)

 ~@LinkName~ = name a link (like html tag <a name=LinkName> )

 ~^LinkName:Caption~ = use FontLink to draw Caption
                       (like html tag <a href="#linkname>Caption</a>)

 Note :
  There is no ~/tag~ because "ubis" are "flip-flop" tags :
   "normal text ~i~ Italic ~bu~ Italic,Bold,UnderLine ~ib~ UnderLine ~u~ normal"

  If you specify a colorIdent, it should be the last tag :
   ~ub+5clBlue~ : UnderLine, Bold, size+5, Color Blue.
   ~clBlueub+5~ : color "clBlueub+5" !!!
}

type
  TGKHyperView = class(System.Windows.Forms.Panel)
  public
    type
      TLinkEvent = procedure(Sender: System.Object; LinkName: string) of object;

  strict private
    type
      TLink = class(System.Object)
      public
        Name: string;
        Rect: TRect;

        constructor Create(Aname: string; x, y, w, h: Integer);
        function GotMouse(x, y: Integer): Boolean;
      end;

      THeights = array of Integer;
      TRuleStyle = (rsLowered, rsRaised);

    var
      FAcceptFontChange: boolean;
      FBorderWidth: Integer;
      FColor: System.Drawing.Color;
      FDefBrush: System.Drawing.SolidBrush;
      FDwnColor: System.Drawing.Color;
      FFont: System.Drawing.Font;
      FHeightCount: Word;
      FHeights: THeights;
      FLeftPos: Integer;
      FLines: TStringList;
      FLink: Integer;
      FLinkColor: System.Drawing.Color;
      FLinks: TList;
      EOnLink: TLinkEvent;
      FPageHeight: Integer;
      FPageWidth: Integer;
      FRange: TPoint;
      FRuleStyle: TRuleStyle;
      FTextFont: System.Drawing.Font;
      FTopPos: Integer;
      FWheelAccumulator: Integer;
      FUpColor: System.Drawing.Color;

    procedure FontChanged(Sender: System.Object);
    procedure LinesChanged(Sender: System.Object);
    procedure SetBorderWidth(Value: Integer);
    procedure SetColor(Value: System.Drawing.Color);
    procedure SetFont(Value: System.Drawing.Font);
    procedure SetLeftPos(Value: Integer);
    procedure SetLines(Value: TStringList);
    procedure SetTopPos(Value: Integer);
    procedure SetRuleStyle(Value: TRuleStyle);

    function GetHeight(grx: System.Drawing.Graphics; s: string): Integer;
    function GetWidth(grx: System.Drawing.Graphics; s: string): Integer;
    function GetFontSize(s: string; var i{dx}: Integer): Integer;
    function GetFontColor(s: string; var i{dx}: Integer): System.Drawing.Color;
    procedure OutText(grx: System.Drawing.Graphics; ss: string; resize: Boolean; var x, y, hMax, xMax: Integer);
    procedure DoPaint(aResize: Boolean);

    procedure ArrangeText();
    procedure ClearLinks();
    procedure SetFontSize(ASize: Double);
    procedure SetFontStyle(AStyle: System.Drawing.FontStyle);
    procedure GotoLink(Link: Word);
    procedure ScrollRange();
  strict protected
    procedure OnKeyDown(e: KeyEventArgs); override;
    procedure OnMouseDown(e: MouseEventArgs); override;
    procedure OnMouseMove(e: MouseEventArgs); override;
    procedure OnMouseWheel(e: MouseEventArgs); override;
    procedure OnPaint(pe: PaintEventArgs); override;
    procedure WndProc(var m: System.Windows.Forms.Message); override;
  public
    constructor Create;
    destructor Destroy; override;

    property LeftPos: Integer read FLeftPos write SetLeftPos;
    property TopPos: Integer read FTopPos write SetTopPos;
  published
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth;
    property Color: System.Drawing.Color read FColor write SetColor;
    property Font: System.Drawing.Font read FFont write SetFont;
    property Lines: TStringList read FLines write SetLines;
    property OnLink: TLinkEvent read EOnLink write EOnLink;
    property RuleStyle: TRuleStyle read FRuleStyle write SetRuleStyle;
  end;

{==============================================================================}

implementation

{ TComboItem }

constructor TComboItem.Create(aCaption: string; aData: System.Object);
begin
  inherited Create;

  Self.Caption := aCaption;
  Self.Data := aData;
end;

function TComboItem.ToString: string;
begin
  Result := Caption;
end;

{ TTaggedComboItem }

constructor TTaggedComboItem.Create(aCaption: string; aTag: Integer);
begin
  inherited Create;

  Self.Caption := aCaption;
  Self.Tag := aTag;
end;

function TTaggedComboItem.ToString: string;
begin
  Result := Caption;
end;

{ TGKTreeNode }

constructor TGKTreeNode.Create(text: string; data: System.Object);
begin
  inherited Create(text);
  Self.Data := data;
end;

{ TGKMenuItem }

constructor TGKMenuItem.Create(text: string; tag: Integer);
begin
  inherited Create(text);
  Self.Tag := tag;
end;

{==============================================================================}

{ TGKListView.TListViewColumnSorter }

constructor TGKListView.TListViewColumnSorter.Create();
begin
  inherited Create;
  FSortColumn := 0;
  FSortOrder := SortOrder.None;
  FObjCompare := CaseInsensitiveComparer.Create();
end;

function TGKListView.TListViewColumnSorter.Compare(x, y: System.Object): Integer;
var
  comp_res: Integer;
  item1, item2: ListViewItem;
begin
  item1 := ListViewItem(x);
  item2 := ListViewItem(y);

  comp_res := TGKUtils.agCompare(item1.SubItems[FSortColumn].Text, item2.SubItems[FSortColumn].Text);

  if (FSortOrder = SortOrder.Ascending)
  then Result := comp_res
  else
  if (FSortOrder = SortOrder.Descending)
  then Result := -comp_res
  else Result := 0;
end;

{ TGKListView }

constructor TGKListView.Create(AOwner: System.Windows.Forms.Control);
begin
  inherited Create;
  lvwColumnSorter := TListViewColumnSorter.Create;
  Self.ListViewItemSorter := lvwColumnSorter;
  Include(Self.ColumnClick, Self.ColClick);
end;

function TGKListView.SelectedItem(): TExtListItem;
begin
  if (Self.SelectedItems.Count <= 0)
  then Result := nil
  else Result := TExtListItem(Self.SelectedItems[0]);
end;

procedure TGKListView.AddListColumn(aCaption: string; aWidth: Integer;
  aAutoSize: Boolean = False);
var
  col: System.Windows.Forms.ColumnHeader;
begin
  if aAutoSize then aWidth := -1;
  col := Columns.Add(aCaption, aWidth, System.Windows.Forms.HorizontalAlignment.Left);
end;

function TGKListView.AddItem(title: string; data: System.Object): TExtListItem;
begin
  Result := TExtListItem.Create(title);
  Result.Data := data;

  Items.Add(Result);
end;

procedure TGKListView.ResizeColumn(aColumnIndex: Integer);
var
  i, max_w, w: Integer;
  //item: TListItem;
  view: TGKListView;
begin
  //aList.AutoResizeColumn(aColumnIndex, ColumnHeaderAutoResizeStyle.?); .net2up

  // Alert
  {view := TGKListView(aList);

  if (aColumnIndex < 0) or (aColumnIndex >= view.Columns.Count) then Exit;

  max_w := 0;

  for i := 0 to view.Items.Count - 1 do begin
    item := view.Items[i];

    if (aColumnIndex = 0)
    then w := view.StringWidth(item.Caption)
    else begin
      if (aColumnIndex - 1 < item.SubItems.Count)
      then w := view.StringWidth(item.SubItems[aColumnIndex - 1])
      else w := 0;
    end;

    if (max_w < w) then max_w := w;
  end;

  if (max_w <> 0)
  then view.Columns[aColumnIndex].Width := max_w + 16;}
end;

procedure TGKListView.ColClick(sender: System.Object; e: System.Windows.Forms.ColumnClickEventArgs);
begin
  if (e.Column = lvwColumnSorter.SortColumn) then begin
    if (lvwColumnSorter.Order = SortOrder.Ascending)
    then lvwColumnSorter.Order := SortOrder.Descending
    else lvwColumnSorter.Order := SortOrder.Ascending;
  end else begin
    lvwColumnSorter.SortColumn := e.Column;
    lvwColumnSorter.Order := SortOrder.Ascending;
  end;

  Sort();
end;

{==============================================================================}

{ TLink }

constructor TGKHyperView.TLink.Create(AName: string; x, y, w, h: Integer);
begin
  inherited Create;
  Name := AName;
  Rect.Left := x;
  Rect.Top := y;
  Rect.Right := x + w;
  Rect.Bottom := y + h;
end;

function TGKHyperView.TLink.GotMouse(x, y: Integer): Boolean;
begin
  Result := (x >= Rect.Left) and (x <= Rect.Right)
        and (y >= Rect.Top) and (y <= Rect.Bottom);
end;

{ TGKHyperView }

constructor TGKHyperView.Create;
begin
  inherited Create();
  TabStop := True;
  BorderStyle := System.Windows.Forms.BorderStyle.Fixed3D;

  FHeightCount := 0;
  FAcceptFontChange := False;
  FLines := TStringList.Create;
  FLines.OnChange := LinesChanged;

  SetLength(FHeights, 0);

  FTopPos := 0;
  FLeftPos := 0;
  //FFont := System.Drawing.Font.Create;
  //FFont.OnChange := FontChanged;
  FColor := System.Drawing.SystemColors.Control;
  FLinks := TList.Create;
  FLinkColor := System.Drawing.Color.Blue;
  FLink := -1;
  FUpColor := System.Drawing.Color.Gray;
  FDwnColor := System.Drawing.Color.White;
end;

destructor TGKHyperView.Destroy;
begin
  ClearLinks();
  FLinks.Free;
  FFont.Free;

  FHeights := nil;

  FLines.Free;
  inherited Destroy;
end;

procedure TGKHyperView.SetBorderWidth(Value: Integer);
begin
  if (FBorderWidth <> Value) then begin
    FBorderWidth := Value;
    Invalidate;
  end;
end;

procedure TGKHyperView.SetColor(Value: System.Drawing.Color);
begin
  if (FColor <> Value) then begin
    FColor := Value;
    Invalidate();
  end;
end;

procedure TGKHyperView.SetLines(Value: TStringList);
begin
  FLines.Assign(Value);
end;

procedure TGKHyperView.LinesChanged(Sender: System.Object);
begin
  if (FHeightCount <> FLines.Count) then begin
    FHeightCount := FLines.Count;
    SetLength(FHeights, FHeightCount);
  end;
  ArrangeText();
  TopPos := 0;

  Invalidate();
end;

procedure TGKHyperView.SetFont(Value: System.Drawing.Font);
begin
  //FFont.Assign(Value);
  ArrangeText();
  Invalidate();
end;

procedure TGKHyperView.SetTopPos(Value: Integer);
var
  dummy, R: TRect;
begin
  if (Value < 0) then Value := 0
  else
  if (Value > FRange.y) then Value := FRange.y;

  if (FTopPos <> Value) then begin
    dummy.Empty();
    ScrollWindowEx(Handle.ToInt32(), 0, FTopPos - Value, dummy, dummy, 0, R, 0);
    InvalidateRect(Handle.ToInt32(), R, False);

    FTopPos := Value;
    SetScrollPos(Handle.ToInt32(), sb_Vert, FTopPos, True);
  end;
end;

procedure TGKHyperView.SetLeftPos(Value: Integer);
var
  dummy, R: TRect;
begin
  if (Value < 0) then Value := 0
  else
  if (Value > FRange.x) then Value := FRange.x;

  if (FLeftPos <> Value) then begin
    dummy.Empty();
    ScrollWindowEx(Handle.ToInt32(), FLeftPos-Value, 0, dummy, dummy, 0, R, 0);
    InvalidateRect(Handle.ToInt32(), R, False);

    FLeftPos := Value;
    SetScrollPos(Handle.ToInt32(), sb_Horz, FLeftPos, True);
  end;
end;

procedure TGKHyperView.SetRuleStyle(Value: TRuleStyle);
begin
  if (FRuleStyle <> Value) then begin
    FRuleStyle := Value;
    if (FRuleStyle = rsRaised) then begin
      FUpColor := System.Drawing.Color.White;
      FDwnColor := System.Drawing.Color.Gray;
    end else begin
      FUpColor := System.Drawing.Color.Gray;
      FDwnColor := System.Drawing.Color.White;
    end;

    Invalidate();
  end;
end;

procedure TGKHyperView.FontChanged(Sender: System.Object);
begin
  if FAcceptFontChange then begin
    ArrangeText();
    Invalidate();
  end;
end;

procedure TGKHyperView.ScrollRange();
begin
  if (FPageWidth < ClientRectangle.Width) then begin
    FRange.x := 0;
    LeftPos := 0;
  end else FRange.x := FPageWidth - ClientRectangle.Width;

  SetScrollRange(Handle.ToInt32(), sb_Horz, 0, FRange.x, False);

  FRange.y := FPageHeight;
  SetScrollRange(Handle.ToInt32(), sb_Vert, 0, FRange.y, False);
end;

procedure TGKHyperView.ArrangeText();
begin
  FTextFont := System.Drawing.Font(Parent.Font.Clone());
  FDefBrush := System.Drawing.SolidBrush.Create(System.Drawing.Color.Black);

  DoPaint(True);
  DoPaint(False);

  ScrollRange();
end;

procedure TGKHyperView.OnPaint(pe: PaintEventArgs);
begin
  inherited OnPaint(pe);
  DoPaint(False);
end;

procedure TGKHyperView.SetFontStyle(AStyle: System.Drawing.FontStyle);
var
  fontStyle: System.Drawing.FontStyle;
begin
  fontStyle := FTextFont.Style;

  if ((fontStyle and AStyle) = System.Drawing.FontStyle.Regular)
  then fontStyle := fontStyle or AStyle
  else fontStyle := fontStyle and not AStyle;

  FTextFont := System.Drawing.Font.Create(FTextFont, fontStyle);
end;

procedure TGKHyperView.SetFontSize(ASize: Double);
begin
  FTextFont := System.Drawing.Font.Create(
                FTextFont.Name, ASize, FTextFont.Style, FTextFont.&Unit,
                FTextFont.GdiCharSet, FTextFont.GdiVerticalFont);
end;

function TGKHyperView.GetHeight(grx: System.Drawing.Graphics; s: string): Integer;
var
  s_sizes: System.Drawing.Size;
begin
  s_sizes := grx.MeasureString(s, FTextFont).ToSize();
  Result := s_sizes.Height;
end;

function TGKHyperView.GetWidth(grx: System.Drawing.Graphics; s: string): Integer;
var
  s_sizes: System.Drawing.Size;
begin
  s_sizes := grx.MeasureString(s, FTextFont).ToSize();
  Result := s_sizes.Width;
end;

procedure TGKHyperView.OutText(grx: System.Drawing.Graphics; ss: string; resize: Boolean; var x, y, hMax, xMax: Integer);
var
  h: Integer;
  s_sizes: System.Drawing.Size;
begin
  if (y >= -hMax) and (ss <> '') then begin
    if not resize then grx.DrawString(ss, FTextFont, FDefBrush, x, y);

    s_sizes := grx.MeasureString(ss, FTextFont).ToSize();

    Inc(x, s_sizes.Width);

    if resize then begin
      if (x > xMax) then xMax := x;
      h := s_sizes.Height;
      if (h > hMax) then hMax := h;
    end;
  end;
end;

function TGKHyperView.GetFontSize(s: string; var i{dx}: Integer): Integer;
begin
  Result := 0;
  while (s[i+1] in ['0'..'9']) do begin
    Inc(i);
    Result := 10 * Result + Ord(s[i]) - Ord('0');
  end;
end;

function TGKHyperView.GetFontColor(s: string; var i{dx}: Integer): System.Drawing.Color;
var
  ss: string;
begin
  ss := s[i];
  while (s[i+1] <> '~') do begin
    Inc(i);
    ss := ss + s[i];
  end;
  Result := System.Drawing.Color.FromName(ss);
end;

procedure TGKHyperView.DoPaint(aResize: Boolean);
var
  grx: System.Drawing.Graphics;
  s, ss, sn: string;
  x, y, i, hMax, xMax, Line: Integer;
  SaveColor: System.Drawing.Color;
  R3d: TRect;
begin
  grx := Self.CreateGraphics();
  try
    FAcceptFontChange := False;
    ClearLinks();

    // Background
    if not(aResize)
    then grx.FillRectangle(SolidBrush.Create(System.Drawing.SystemColors.Control), ClientRectangle);

    if aResize then begin
      y := 0;
      xMax := 0;
    end else y := FBorderWidth - FTopPos; // start position

    // For each line from the starting line to the last line 
    for Line := 0 to FLines.Count - 1 do begin
      if aResize then begin
        x := 0;
        hMax := GetHeight(grx, 'A');
      end else begin
        hMax := FHeights[Line]; // Line height
        x := FBorderWidth - FLeftPos; // Left position
      end;

      s := FLines[Line];  

      i := 1;
      ss := '';
      while (i <= Length(s)) do begin
        if (s[i] = '~') then begin
          if (s[i+1] = '~') then ss := ss + '~';

          OutText(grx, ss, aResize, x, y, hMax, xMax);
          Inc(i);

          while (s[i] <> '~') do begin
            case System.Char.ToUpper(s[i]) of
              { reset }
              '0' : FTextFont := System.Drawing.Font(Parent.Font.Clone());

              { attributs }
              'U' : SetFontStyle(System.Drawing.FontStyle.Underline);
              'B' : SetFontStyle(System.Drawing.FontStyle.Bold);
              'I' : SetFontStyle(System.Drawing.FontStyle.Italic);
              'S' : SetFontStyle(System.Drawing.FontStyle.StrikeOut);

              { Font Size }
              '+' : SetFontSize(FTextFont.Size + GetFontSize(s, i));
              '-' : SetFontSize(FTextFont.Size - GetFontSize(s, i));

              { Color }
              //'C' : Font.Color := FontColor();

              { Link Name:Caption }
              '^' : begin
                sn := '';
                while (s[i+1] <> ':') do begin { get Link Name }
                  Inc(i);
                  sn := sn + s[i];
                end;
                Inc(i);
                ss := '';
                while (s[i+1] <> '~') do begin { get Caption }
                  Inc(i);
                  ss := ss + s[i];
                end;
                SaveColor := FDefBrush.Color;
                FDefBrush.Color := FLinkColor;
                SetFontStyle(System.Drawing.FontStyle.Underline);

                if not aResize then
                  FLinks.Add(TLink.Create(sn, x, y, GetWidth(grx, ss), hMax));

                OutText(grx, ss, aResize, x, y, hMax, xMax);

                FDefBrush.Color := SaveColor;
                SetFontStyle(System.Drawing.FontStyle.Underline);
              end;

              { Horizontal Rule }
              'R': if not aResize then begin
                raise Exception.Create('need to realize');
                (*R3d.Left := x - FLeftPos;
                R3d.Top := y + hMax div 2 - 1;
                R3d.Bottom := R3d.Top + 2;
                R3d.Right := R3d.Left + FPageWidth - x - FBorderWidth;
                Frame3D(Canvas, R3d, FUpColor, FDwnColor, 1);*)
              end;

              { skip others }
              else while (s[i+1] <> '~') do Inc(i);
            end;

            Inc(i);
          end;

          ss := '';
        end else ss := ss + s[i];

        Inc(i);
      end;

      OutText(grx, ss, aResize, x, y, hMax, xMax);
      Inc(y, hMax);
      if aResize then FHeights[Line] := hMax;
    end;

    if aResize then begin
      FPageWidth  := xMax + 2 * FBorderWidth;
      FPageHeight := y;
    end;

    FAcceptFontChange := True;
  finally
    grx.Dispose;
  end;
end;

procedure TGKHyperView.ClearLinks();
var
  i: Integer;
begin
  for i := 0 to FLinks.Count - 1 do TLink(FLinks[i]).Free;
  FLinks.Clear;
end;

procedure TGKHyperView.OnMouseMove(e: MouseEventArgs);
var
  i: Integer;
begin
  inherited OnMouseMove(e);

  for i := 0 to FLinks.Count - 1 do
    if TLink(FLinks[i]).GotMouse(e.X, e.Y) then begin
      FLink := i;
      Cursor := Cursors.Hand;
      Exit;
    end;

  if FLink >= 0 then begin
    Cursor := Cursors.Default;
    FLink := -1;
  end;
end;

procedure TGKHyperView.OnMouseDown(e: MouseEventArgs);
begin
  inherited OnMouseDown(e);
  if not Focused then Focus;

  if (FLink >= 0) then GotoLink(FLink);
end;

procedure TGKHyperView.GotoLink(Link: Word);
var
  l: string;
  i, h: Integer;
begin
  l := '~@' + TLink(FLinks[Link]).Name + '~';
  h := FBorderWidth;
  for i := 0 to FLines.Count - 1 do begin
    if Pos(l, FLines[i]) > 0 then begin
      TopPos := h;
      Exit;
    end;
    Inc(h, FHeights[i]);
  end;

  if Assigned(EOnLink)
  then EOnLink(Self, TLink(FLinks[Link]).Name)
end;

procedure TGKHyperView.WndProc(var m: System.Windows.Forms.Message);
var
  new_pos: Integer;
  wParam: Cardinal;
  scrType: ScrollEventType;
  ScrollInfo: TScrollInfo;
begin
  inherited WndProc(m);

  case m.Msg of
    WM_SIZE: begin
      ScrollRange();
    end;

    WM_GETDLGCODE: begin
      m.Result := IntPtr(m.Result.ToInt32() or DLGC_WANTARROWS or DLGC_WANTTAB or DLGC_WANTCHARS);
    end;

    WM_HSCROLL: begin
      wParam := m.WParam.ToInt32();
      scrType := TGKUtils.GetScrollEventType(wParam and $ffff);

      case scrType of
        ScrollEventType.SmallDecrement: LeftPos := LeftPos - (ClientRectangle.Width div 20);
        ScrollEventType.SmallIncrement: LeftPos := LeftPos + (ClientRectangle.Width div 20);
        ScrollEventType.LargeDecrement: LeftPos := LeftPos - (ClientRectangle.Width div 2);
        ScrollEventType.LargeIncrement: LeftPos := LeftPos + (ClientRectangle.Width div 2);
        ScrollEventType.First         : LeftPos := 0;
        ScrollEventType.Last          : LeftPos := FPageWidth;
        ScrollEventType.ThumbTrack,
        ScrollEventType.ThumbPosition : begin
          ScrollInfo.cbSize := SizeOf(ScrollInfo);
          ScrollInfo.fMask := SIF_ALL;
          GetScrollInfo(Handle.ToInt32, SB_HORZ, ScrollInfo);
          LeftPos := ScrollInfo.nTrackPos;
        end;
      end;
    end;

    WM_VSCROLL: begin
      wParam := m.WParam.ToInt32();
      scrType := TGKUtils.GetScrollEventType(wParam and $ffff);

      case scrType of
        ScrollEventType.SmallDecrement: TopPos := TopPos - (ClientRectangle.Height div 20);
        ScrollEventType.SmallIncrement: TopPos := TopPos + (ClientRectangle.Height div 20);
        ScrollEventType.LargeDecrement: TopPos := TopPos - (ClientRectangle.Height div 2);
        ScrollEventType.LargeIncrement: TopPos := TopPos + (ClientRectangle.Height div 2);
        ScrollEventType.First         : TopPos := 0;
        ScrollEventType.Last          : TopPos := FPageHeight;
        ScrollEventType.ThumbTrack,
        ScrollEventType.ThumbPosition : begin
          ScrollInfo.cbSize := SizeOf(ScrollInfo);
          ScrollInfo.fMask := SIF_ALL;
          GetScrollInfo(Handle.ToInt32, SB_VERT, ScrollInfo);
          TopPos := ScrollInfo.nTrackPos;
        end;
      end;
    end;
  end;
end;

procedure TGKHyperView.OnMouseWheel(e: MouseEventArgs);
begin
  inherited OnMouseWheel(e);

  if (e.Delta > 0)
  then TopPos := TopPos + 1
  else TopPos := TopPos - 1;
end;

procedure TGKHyperView.OnKeyDown(e: KeyEventArgs);
begin
  inherited OnKeyDown(e);

  case e.KeyCode of
    Keys.Up: TopPos := TopPos - (ClientRectangle.Height div 20);

    Keys.Down: TopPos := TopPos + (ClientRectangle.Height div 20);

    Keys.Right: LeftPos := LeftPos + (ClientRectangle.Width div 20);

    Keys.Left: LeftPos := LeftPos - (ClientRectangle.Width div 20);

    Keys.Prior: TopPos := TopPos - (ClientRectangle.Height div 2);

    Keys.Next: TopPos := TopPos + (ClientRectangle.Height div 2);

    Keys.Home: begin
      TopPos := 0;
      LeftPos := 0;
    end;

    Keys.&End: begin
      TopPos := FPageHeight - ClientRectangle.Height + 2 * FBorderWidth;
      LeftPos := FPageWidth - ClientRectangle.Width;
    end;
  end;
end;

end.
