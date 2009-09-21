unit GKCtrls;

{$I compiler.inc}

interface

uses
  Types, SysUtils, Classes,
  {$IFDEF OS_MSWIN}
  Windows, Messages, Graphics, Controls, ComCtrls, CommCtrl
  {$ENDIF}
  {$IFDEF OS_LINUX}
  QGraphics, QControls, QComCtrls
  {$ENDIF};

{=================================== Common ===================================}

{$DEFINE LV_RELEASE}

{$IFDEF FREEPASCAL}
  {$UNDEF LV_RELEASE}
{$ENDIF}

{$IFNDEF OS_MSWIN}
  {$UNDEF LV_RELEASE}
{$ENDIF}

type
  TSortDirection = (sdAscending, sdDescending);

  TBSListView = class(TListView)
  {$IFDEF LV_RELEASE}
  private
    FNewWndProc, FPrevWndProc, FNewHeadProc, FPrevHeadProc: Pointer;
    FSortColumn: word;
    FSortDirection: TSortDirection;
    FHeadHandle: integer;
    FHeadLBDown, FHeadOnDiv: boolean;
    FHeadLBCol: integer;
    FShowSortSign: boolean;
    procedure HookControl;
    procedure UnhookControl;
    procedure HookWndProc(var AMsg: TMessage);
    procedure HeadWndProc(var AMsg: TMessage);
    procedure SetSortColumn(Value: word);
    procedure SetSortDirection(Value: TSortDirection);
    procedure SetShowSortSign(Value: boolean);
    function GetSortType: TSortType;
    procedure SetSortType(Value: TSortType);
    procedure WMParentNotify(var Message: TWMParentNotify); message WM_PARENTNOTIFY;
    procedure RefreshHeader;
  protected
    procedure CreateWnd; override;
    procedure ColClick(Column: TListColumn); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property SortColumn: word read FSortColumn write SetSortColumn;
    property SortDirection: TSortDirection read FSortDirection write SetSortDirection;
    property ShowSortSign: boolean read FShowSortSign write SetShowSortSign default true;
    property SortType read GetSortType write SetSortType;
  end{$ENDIF LV_RELEASE};

{$IFDEF OS_MSWIN}
procedure SetRU();
procedure SetEN();
{$ENDIF}

procedure Register;

implementation

uses
  Math;

procedure Register;
begin
  RegisterComponents('BSL Controls', [TBSListView]);
end;

{$IFDEF OS_MSWIN}
// Установить русский язык
procedure SetRU();
var
  Layout: WideString;
begin
  Layout := '00000419';
  LoadKeyboardLayoutW(PWideChar(Layout), KLF_ACTIVATE);
end;

// Установить английский язык
procedure SetEN();
var
  Layout: WideString;
begin
  Layout := '00000409';
  LoadKeyboardLayoutW(PWideChar(Layout), KLF_ACTIVATE);
end;
{$ENDIF}

{$IFDEF LV_RELEASE}

function agCompareListItems(Str1, Str2: string): integer;

  function IsValidNumber(const S: string; var V: extended): boolean;
  var
    NumCode: integer;
  begin
    Val(S, V, NumCode);
    Result:=(NumCode = 0);
  end;

var
  Val1, Val2: Extended;
begin
  if IsValidNumber(Str1,Val1) and IsValidNumber(Str2,Val2) then
    if Val1 < Val2
    then Result:=-1
    else
      if Val1 > Val2
      then Result:=1
      else Result:=0
  else begin
    Result:=AnsiCompareStr(Str1,Str2);
    if (Str1<>'') and (Str2='') then Result:=-1;
    if (Str1='') and (Str2<>'') then Result:=1;
  end;
end;

function agDefaultListViewSort(Item1, Item2: TListItem; lParam: Integer): Integer; stdcall;
var
  Str1,Str2: string;
  Column: integer;
begin
  with Item1 do
    if Assigned(TListView(ListView).OnCompare) then
      TListView(ListView).OnCompare(ListView,Item1,Item2,lParam,Result)
    else begin
      Column:=LoWord(lParam);
      if Column = 0 then begin
        Str1:=Item1.Caption;
        Str2:=Item2.Caption;
      end else begin
        if Item1.SubItems.Count > Column-1 then Str1:=Item1.SubItems[Column-1] else Str1:='';
        if Item2.SubItems.Count > Column-1 then Str2:=Item2.SubItems[Column-1] else Str2:='';
      end;
      Result:=agCompareListItems(Str1,Str2) * ShortInt(HiWord(lParam));
    end;
end;

constructor TBSListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNewWndProc := MakeObjectInstance(HookWndProc);
  FNewHeadProc := MakeObjectInstance(HeadWndProc);
  FPrevWndProc:=nil;
  FPrevHeadProc:=nil;
  FSortColumn:=0;
  FSortDirection:=sdAscending;
  FHeadHandle:=0;
  FHeadLBDown:=false;
  FShowSortSign:=true;
end;

destructor TBSListView.Destroy;
begin
  if Assigned(FPrevHeadProc) and (Pointer(GetWindowLong(FHeadHandle,
    GWL_WNDPROC)) = FNewHeadProc) then
  begin
    SetWindowLong(FHeadHandle,GWL_WNDPROC,LongInt(FPrevHeadProc));
    FPrevHeadProc:=nil;
  end;
  UnhookControl;
  FreeObjectInstance(FNewWndProc);
  FreeObjectInstance(FNewHeadProc);
  inherited Destroy;
end;

procedure TBSListView.CreateWnd;
begin
  inherited;
  HookControl;
end;

procedure TBSListView.HookControl;
var
  P: Pointer;
begin
  if not(csDestroying in ComponentState) then
  begin
    HandleNeeded;
    P := Pointer(GetWindowLong(Handle, GWL_WNDPROC));
    if (P <> FNewWndProc) then begin
      FPrevWndProc := P;
      SetWindowLong(Handle, GWL_WNDPROC, LongInt(FNewWndProc));
    end;
  end;
end;

procedure TBSListView.UnhookControl;
begin
  if Assigned(FPrevWndProc) and HandleAllocated and
  (Pointer(GetWindowLong(Handle, GWL_WNDPROC)) = FNewWndProc) then
    SetWindowLong(Handle, GWL_WNDPROC, LongInt(FPrevWndProc));
  FPrevWndProc := nil;
end;

procedure TBSListView.HookWndProc(var AMsg: TMessage);
begin
  with AMsg do begin
    if (Msg = LVM_SORTITEMS) then begin
      if ViewStyle = vsReport then WParamLo:=FSortColumn else WParamLo:=0;
      if FSortDirection = sdAscending then WParamHi:=1 else WParamHi:=word(-1);
      LParam:=integer(@agDefaultListViewSort);
    end;
    Result:=CallWindowProc(FPrevWndProc,Handle,Msg,WParam,LParam);
  end;
end;

procedure TBSListView.SetSortColumn(Value: word);
begin
  if Columns.Count = 0 then
    Value:=0
  else
    if Value >= Columns.Count then Value:=Columns.Count-1;
  if FSortColumn <> Value then begin
    FSortColumn:=Value;
    RefreshHeader;
    if SortType <> stNone then AlphaSort;
  end;
end;

procedure TBSListView.SetSortDirection(Value: TSortDirection);
begin
  if FSortDirection <> Value then begin
    FSortDirection:=Value;
    RefreshHeader;
    if SortType <> stNone then AlphaSort;
  end;
end;

procedure TBSListView.SetShowSortSign(Value: boolean);
begin
  if FShowSortSign <> Value then begin
    FShowSortSign:=Value;
    RefreshHeader;
  end;
end;

function TBSListView.GetSortType: TSortType;
begin
  Result:=inherited SortType;
end;

procedure TBSListView.SetSortType(Value: TSortType);
begin
  if GetSortType <> Value then begin
    inherited SortType:=Value;
    RefreshHeader;
  end;
end;

procedure TBSListView.ColClick(Column: TListColumn);
begin
  if FSortColumn <> Column.Index then
    SortColumn:=Column.Index
  else
    if FSortDirection = sdAscending then
      SortDirection:=sdDescending
    else
      SortDirection:=sdAscending;
  inherited;
end;

procedure TBSListView.WMParentNotify(var Message: TWMParentNotify);
begin
  inherited;
  with Message do
    if (Event = WM_CREATE) and (FHeadHandle = 0) then begin
      FHeadHandle:=ChildWnd;
      FPrevHeadProc:=Pointer(GetWindowLong(FHeadHandle,GWL_WNDPROC));
      SetWindowLong(FHeadHandle,GWL_WNDPROC,LongInt(FNewHeadProc));
    end;
end;

procedure TBSListView.HeadWndProc(var AMsg: TMessage);

  procedure PaintSortSign;
  const
    SignWidth  = 8;
    SignHeight = 7;
    SignMargin = 15;
  var
    C: TCanvas;
    i, x1, y1, csx: Integer;
    Item: THDItem;
  begin
    if (ViewStyle <> vsReport) or (Columns.Count = 0) or (SortType = stNone) or
      not FShowSortSign then exit;

    if (FSortColumn >= Columns.Count) then Exit;

    C:=TCanvas.Create;
    C.Handle:=GetDC(FHeadHandle);
    try
      C.Font.Assign(Font);

      // calculate csx
      csx:=0;
      for i:=0 to FSortColumn-1 do begin
        FillChar(Item,SizeOf(Item),0);
        Item.mask:=HDI_WIDTH;
        Header_GetItem(FHeadHandle,i,Item);
        inc(csx,Item.cxy);
      end;
      // calculate x1
      FillChar(Item,SizeOf(Item),0);
      Item.mask:=HDI_WIDTH;
      Header_GetItem(FHeadHandle,FSortColumn,Item);
      if Columns[FSortColumn].Alignment = taRightJustify then
        x1:=csx+Item.cxy-C.TextWidth(Columns[FSortColumn].Caption)-SignWidth-SignMargin
      else
        x1:=csx+C.TextWidth(Columns[FSortColumn].Caption)+SignMargin;
      // check x1 limits
      if x1 < csx+3 then exit;
      if x1+SignWidth > csx+Item.cxy-3 then exit;
      // calculate y1
      y1:=((C.TextHeight(Columns[FSortColumn].Caption)-SignHeight) div 2)+1;

      if FHeadLBDown and not(FHeadOnDiv) then begin
        if FHeadLBCol <> FSortColumn then exit;
        inc(x1); inc(y1);
      end;

      if FSortDirection = sdAscending then begin
        // Highlight
        C.Pen.Color:=clBtnHighlight;
        C.MoveTo(x1+1,y1+SignHeight);
        C.LineTo(x1+SignWidth,y1+SignHeight);
        C.MoveTo(x1+(SignWidth div 2),y1);
        C.LineTo(x1+SignWidth,y1+SignHeight);
        // Shadow
        C.Pen.Color:=clBtnShadow;
        C.MoveTo(x1,y1+SignHeight-1);
        C.LineTo(x1+(SignWidth div 2),y1-1);
      end else begin
        inc(y1);
        // Shadow
        C.Pen.Color:=clBtnShadow;
        C.MoveTo(x1+1,y1);
        C.LineTo(x1+SignWidth-1,y1);
        C.MoveTo(x1+1,y1);
        C.LineTo(x1+(SignWidth div 2),y1+SignHeight);
        // Highlight
        C.Pen.Color:=clBtnHighlight;
        C.MoveTo(x1+SignWidth,y1);
        C.LineTo(x1+(SignWidth div 2),y1+SignHeight);
      end;
    finally
      ReleaseDC(FHeadHandle,C.Handle);
      C.Free;
    end;
  end;

var
  Info: THDHitTestInfo;
begin
  with AMsg do begin
    Result:=CallWindowProc(FPrevHeadProc,FHeadHandle,Msg,WParam,LParam);
    case Msg of
      WM_PAINT:
        PaintSortSign;
      WM_LBUTTONDOWN:
        begin
          FHeadLBDown:=true;
          Info.Point.X:=TWMLButtonDown(AMsg).Pos.X;
          Info.Point.Y:=TWMLButtonDown(AMsg).Pos.Y;
          FHeadLBCol:=SendMessage(FHeadHandle,HDM_HITTEST,0,Integer(@Info));
          if (Info.Flags and HHT_ONDIVIDER) = 0 then
            FHeadOnDiv:=false
          else
            FHeadOnDiv:=true;
        end;
      WM_LBUTTONUP:
        begin
          FHeadLBDown:=false;
          FHeadOnDiv:=false;
        end;
      WM_MOUSEMOVE:
        if FHeadOnDiv then PaintSortSign;
    end;
  end;
end;

procedure TBSListView.RefreshHeader;
begin
  if IsWindowVisible(FHeadHandle) then
    InvalidateRect(FHeadHandle,nil,True);
end;

{$ENDIF LV_RELEASE}

end.
