{$I DFS.INC}  { Standard defines for all Delphi Free Stuff components }

// Delphi 2 and C++B 1 have incorrectly declared InsertItem as private.
{$IFDEF DFS_COMPILER_3_UP}
  {$DEFINE DFS_FIXED_LIST_VIEW}
{$ENDIF}

{------------------------------------------------------------------------------}
{ TdfsEnhListView v3.72                                                        }
{------------------------------------------------------------------------------}
{ A list view control that provides enhanced functionality beyond the          }
{ standard list view.  For example, automatic sorting of simple data types,    }
{ owner draw event for vsReport mode, and more.  This does NOT require any     }
{ special version of COMCTL32.DLL.                                             }
{                                                                              }
{ Copyright 1998-2001, Brad Stowers.  All Rights Reserved.                     }
{                                                                              }
{ Copyright:                                                                   }
{ All Delphi Free Stuff (hereafter "DFS") source code is copyrighted by        }
{ Bradley D. Stowers (hereafter "author"), and shall remain the exclusive      }
{ property of the author.                                                      }
{                                                                              }
{ Distribution Rights:                                                         }
{ You are granted a non-exlusive, royalty-free right to produce and distribute }
{ compiled binary files (executables, DLLs, etc.) that are built with any of   }
{ the DFS source code unless specifically stated otherwise.                    }
{ You are further granted permission to redistribute any of the DFS source     }
{ code in source code form, provided that the original archive as found on the }
{ DFS web site (http://www.delphifreestuff.com) is distributed unmodified. For }
{ example, if you create a descendant of TDFSColorButton, you must include in  }
{ the distribution package the colorbtn.zip file in the exact form that you    }
{ downloaded it from http://www.delphifreestuff.com/mine/files/colorbtn.zip.   }
{                                                                              }
{ Restrictions:                                                                }
{ Without the express written consent of the author, you may not:              }
{   * Distribute modified versions of any DFS source code by itself. You must  }
{     include the original archive as you found it at the DFS site.            }
{   * Sell or lease any portion of DFS source code. You are, of course, free   }
{     to sell any of your own original code that works with, enhances, etc.    }
{     DFS source code.                                                         }
{   * Distribute DFS source code for profit.                                   }
{                                                                              }
{ Warranty:                                                                    }
{ There is absolutely no warranty of any kind whatsoever with any of the DFS   }
{ source code (hereafter "software"). The software is provided to you "AS-IS", }
{ and all risks and losses associated with it's use are assumed by you. In no  }
{ event shall the author of the softare, Bradley D. Stowers, be held           }
{ accountable for any damages or losses that may occur from use or misuse of   }
{ the software.                                                                }
{                                                                              }
{ Support:                                                                     }
{ Support is provided via the DFS Support Forum, which is a web-based message  }
{ system.  You can find it at http://www.delphifreestuff.com/discus/           }
{ All DFS source code is provided free of charge. As such, I can not guarantee }
{ any support whatsoever. While I do try to answer all questions that I        }
{ receive, and address all problems that are reported to me, you must          }
{ understand that I simply can not guarantee that this will always be so.      }
{                                                                              }
{ Clarifications:                                                              }
{ If you need any further information, please feel free to contact me directly.}
{ This agreement can be found online at my site in the "Miscellaneous" section.}
{------------------------------------------------------------------------------}
{ The lateset version of my components are always available on the web at:     }
{   http://www.delphifreestuff.com/                                            }
{ See ELV.txt for notes, known issues, and revision history.                   }
{------------------------------------------------------------------------------}
{ Date last modified:  June 28, 2001                                           }
{------------------------------------------------------------------------------}

unit EnhListView;

interface

uses
  Forms, Windows, Messages, Classes, Controls, ComCtrls, CommCtrl, SysUtils,
  ImgList, Graphics, Menus;

const
  DRAWTEXTEX_FLAGS = DT_NOPREFIX or DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS;
  DRAWTEXTEX_ALIGNMENT: array[TAlignment] of UINT = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WM_OWNERDRAWCOLUMNS = WM_USER + 143;

type
  TIntArray = array[0..(MaxInt div SizeOf(Integer)-1)] of Integer;
  PIntArray = ^TIntArray;

  TResizeMethod = (rmFitText, rmFitHeader);
  TAutoColumnSort = (acsNoSort, acsSort, acsSortToggle);
  TAutoSortStyle = (assSmart, assDefault);
  TSortAs = (saNone, saString, saNumeric, saDateTime);
  TLVStyle = (lvStandard, lvOwnerDrawFixed);
  TLVHDrawItemEvent = procedure(Control: TWinControl; var ACanvas: TCanvas;
     Index: Integer; var ARect: TRect; Selected: boolean;
     var DefaultDrawing: boolean) of object;
  TLVMeasureItemEvent = procedure(Control: TWinControl;
     var AHeight: UINT) of object;
  TLVDrawItemEvent = procedure(Control: TWinControl; var ACanvas: TCanvas;
     Index: Integer; ARect: TRect; State: TOwnerDrawState;
     var DefaultDrawing, FullRowSelect: boolean) of object;
  TLVDrawSubItemEvent = procedure(Control: TWinControl; var ACanvas: TCanvas;
     Index, SubItem: Integer; ARect: TRect; State: TOwnerDrawState;
     var DefaultDrawing: boolean) of object;
  TLVAfterDrawItemEvent = procedure(Control: TWinControl; var ACanvas: TCanvas;
     Index: Integer; ARect: TRect; State: TOwnerDrawState) of object;
  TLVSortItemsEvent = procedure(Sender: TObject; Item1, Item2: TListItem;
     SortColumn: integer; var SortAs: TSortAs; var CompResult: integer) of object;
  TLVSortStatusEvent = procedure(Sender: TObject; SortColumn: integer;
     Ascending: boolean) of object;
  TLVEditCanceled = procedure(Sender: TObject; Item: TListItem) of object;

  { The new class }
  TCustomEnhListView = class(TCustomListView)
  private
    FSortDirty: boolean;
    FUpdateCount: integer;
    FStyle: TLVStyle;
    FAutoColumnSort: TAutoColumnSort;
    FAutoSortStyle: TAutoSortStyle;
    FAutoResort: boolean;
    FAutoSortAscending: boolean;
    FTmpAutoSortAscending: boolean;
    FLastColumnClicked: Integer;
    FShowSortArrows: boolean;
    FReverseSortArrows: boolean;
    FSortUpBmp,
    FSortDownBmp: TBitmap;
    FCreatingWindowHandle: boolean;
    FNoColumnResize: boolean;
    FOldHeaderWndProc: pointer;
    FHeaderInstance: pointer;
    FSearchStr: string;
    FSearchTickCount: Double;
    FColumnSearch: boolean;

    FOnSortBegin: TLVSortStatusEvent;
    FOnSortFinished: TLVSortStatusEvent;
    FOnMeasureItem: TLVMeasureItemEvent;
    FOnDrawItem: TLVDrawItemEvent;
    FOnDrawSubItem: TLVDrawSubItemEvent;
    FOnAfterDefaultDrawItem: TLVAfterDrawItemEvent;
    FOnDrawHeader: TLVHDrawItemEvent;
    FOnSortItems: TLVSortItemsEvent;
    FOnEditCanceled: TLVEditCanceled;

    procedure HeaderWndProc(var Message: TMessage);
    { Message handlers }
    procedure CMSysColorChange(var Message: TWMSysColorChange); message CM_SYSCOLORCHANGE;
    procedure CMFontChanged(var Messsage: TMessage); message CM_FONTCHANGED;
    procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    procedure WMDrawHeader(var Message: TWMDrawItem); message WM_DRAWITEM;
    procedure WMNotify(var Message: TWMNotify); message WM_NOTIFY;
    procedure WMOwnerDrawColumns(var Message: TMessage); message WM_OWNERDRAWCOLUMNS;
    procedure WMParentNotify(var Message: TWMParentNotify); message WM_PARENTNOTIFY;
  protected
    { USE WITH CARE.  This can be nil }
    FCanvas: TCanvas;
    FHeaderHandle: HWND;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ResetOwnerDrawHeight;
    procedure InvalidateColumnHeader(Index: integer); virtual;
    procedure DoSort(ColumnIndex:integer; Descending: boolean); virtual;
    procedure SortBegin(ColumnIndex: integer; Ascending: boolean); virtual;
    procedure SortFinished(ColumnIndex: integer; Ascending: boolean); virtual;
    procedure SortItems(const Item1, Item2: TListItem; SortColumn: integer;
       var CompResult: integer); virtual;
    procedure MeasureItem(var Height: UINT); virtual;
    procedure DefaultDrawItem(Index: Integer; Rect: TRect;
       State: TOwnerDrawState; FullRowSelect: boolean); virtual;
    procedure DefaultDrawSubItem(Index, SubItem: integer; Rect: TRect;
       State: TOwnerDrawState); virtual;
    procedure ProcessDrawItemMsg(Index: Integer;
       Rect: TRect; State: TOwnerDrawState; var DefaultDrawing,
       FullRowSelect: boolean); virtual;
    function ActualColumnIndex(Index: integer): integer; virtual;
    function GetActualColumn(Index: integer): TListColumn; virtual;
    function GetSubItemText(Index, SubItem: integer): string; virtual;
    procedure DrawSubItem(Index, SubItem: Integer; Rect: TRect;
       State: TOwnerDrawState; var DefaultDrawing: boolean); virtual;
    procedure DrawItem(var Canvas: TCanvas; Index: Integer; Rect: TRect;
       State: TOwnerDrawState; var DefaultDrawing, FullRowSelect: boolean);
       reintroduce; overload; virtual;
    procedure AfterDrawItem(var Canvas: TCanvas; Index: Integer;
       Rect: TRect; State: TOwnerDrawState); virtual;
    procedure Edit(const Item: TLVItem); override;
    procedure EditCanceled(const Item: TLVItem); virtual;
    { Overriden ancestor methods }
    procedure ColClick(Column: TListColumn); override;
{$IFDEF DFS_FIXED_LIST_VIEW}
    procedure InsertItem(Item: TListItem); override;
{$ENDIF}
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure ProcessDrawHeaderMsg(Index: Integer; Rect: TRect;
       State: TOwnerDrawState; var DefaultDrawing: boolean); virtual;
    procedure DrawHeader(var Canvas: TCanvas; Index: Integer; var Rect: TRect;
       Selected: boolean; var DefaultDrawing: boolean); virtual;
    procedure DefaultDrawHeader(var Canvas: TCanvas; Index: Integer;
       var Rect: TRect; Selected: boolean); virtual;
    procedure SetOnDrawHeader(Value: TLVHDrawItemEvent); virtual;
    procedure SetColumnsOwnerDrawFlag(OwnerDrawn: boolean); virtual;
    procedure CreateSortBmps(var UpBmp, DownBmp: TBitmap); virtual;

    { Property methods }
    procedure SetAutoColumnSort(Value: TAutoColumnSort);
    procedure SetAutoSortStyle(Value: TAutoSortStyle);
    procedure SetCurrentSortAscending(Value: boolean);
    procedure SetAutoSortAscending(Value: boolean);
    procedure SetStyle(Value: TLVStyle);
    procedure SetShowSortArrows(Value: boolean);
    procedure SetReverseSortArrows(Value: boolean);
    procedure SetLastColumnClicked(Value: integer);
    procedure SetAutoResort(Value: boolean);

    function GetSmallImages: TCustomImageList;
    procedure SetSmallImages(Val: TCustomImageList);
    function GetCurrentColumnWidth(Index: integer): integer;

    procedure CreateParams(var Params: TCreateParams); override;
    procedure Loaded; override;

    { Should probably remain protected }
    property SortUpBmp: TBitmap read FSortUpBmp;
    property SortDownBmp: TBitmap read FSortDownBmp;

    { Should be made public by descendants as needed }
    property LastColumnClicked: Integer
      read FLastColumnClicked write SetLastColumnClicked;

    { Should be published by descendants as needed }
    property HeaderHandle: HWnd
      read FHeaderHandle;
    property AutoColumnSort: TAutoColumnSort
      read FAutoColumnSort write SetAutoColumnSort default acsNoSort;
    property AutoSortStyle: TAutoSortStyle
      read FAutoSortStyle write SetAutoSortStyle default assSmart;
    property AutoResort: boolean
      read FAutoResort write SetAutoResort default TRUE;
    property AutoSortAscending: boolean
      read FAutoSortAscending write SetAutoSortAscending default TRUE;
    property ColumnSearch: boolean
      read FColumnSearch write FColumnSearch default FALSE;
    property ShowSortArrows: boolean
      read FShowSortArrows write SetShowSortArrows default FALSE;
    property ReverseSortArrows: boolean
      read FReverseSortArrows write SetReverseSortArrows default FALSE;
    property CurrentSortAscending: boolean
      read FTmpAutoSortAscending write SetCurrentSortAscending;
    property Style: TLVStyle
      read FStyle write SetStyle default lvStandard;
    property CurrentColumnWidth[Index: integer]: integer
      read GetCurrentColumnWidth;

    property NoColumnResize: boolean
      read FNoColumnResize write FNoColumnResize;
    // We have to redeclare this so we can hook into the read/write methods.
    property SmallImages: TCustomImageList
      read GetSmallImages write SetSmallImages;

    { Events }
    property OnDrawHeader: TLVHDrawItemEvent
      read FOnDrawHeader write SetOnDrawHeader;
    property OnMeasureItem: TLVMeasureItemEvent
      read FOnMeasureItem write FOnMeasureItem;
    property OnDrawItem: TLVDrawItemEvent
      read FOnDrawItem write FOnDrawItem;
    property OnDrawSubItem: TLVDrawSubItemEvent
      read FOnDrawSubItem write FOnDrawSubItem;
    property OnAfterDefaultDrawItem: TLVAfterDrawItemEvent
      read FOnAfterDefaultDrawItem write FOnAfterDefaultDrawItem;
    property OnSortItems: TLVSortItemsEvent
      read FOnSortItems write FOnSortItems;
    property OnSortBegin: TLVSortStatusEvent
      read FOnSortBegin write FOnSortBegin;
    property OnSortFinished: TLVSortStatusEvent
      read FOnSortFinished write FOnSortFinished;
    property OnEditCanceled: TLVEditCanceled
      read FOnEditCanceled write FOnEditCanceled;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DefaultSort(ColumnIndex:integer; Descending: boolean); virtual;
    procedure Resort; virtual;
    // Use these as replacements for Items.BeginUpdate and EndUpdate.  They
    // call those methods, but they also inhibit autosorting until after the
    // last EndUpdate.
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;

    // Resize all columns.
    procedure ResizeColumns(ResizeMethod: TResizeMethod); virtual;

    // Move list item to new position.
    procedure MoveItem(OriginalIndex, NewIndex: Integer); virtual;

    function StringSelect(FindStr: string; ColumnIndex: Integer): boolean; virtual;
    function SubStringSelect(FindStr: string; ColumnIndex: Integer): boolean; virtual;

    // Accounts for re-ordered columns
    property ActualColumn[Index: integer]: TListColumn
      read GetActualColumn;
  published
  end;


  TdfsEnhListView = class(TCustomEnhListView)
  public
    property HeaderHandle;
    property CurrentSortAscending;
    property LastColumnClicked;
    property CurrentColumnWidth;
  published
    property AutoColumnSort;
    property AutoSortStyle;
    property AutoResort;
    property AutoSortAscending;

    property ColumnSearch;
    property NoColumnResize;
    property ReverseSortArrows;
    property ShowSortArrows;
    property Style;

    property OnMeasureItem;
    property OnDrawItem;
    property OnDrawSubItem;
    property OnAfterDefaultDrawItem;
    property OnDrawHeader;
    property OnSortItems;
    property OnSortBegin;
    property OnSortFinished;
    property OnEditCanceled;

    { Publish TCustomListView inherited protected properties }
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property Color;
    property ColumnClick;
    property OnClick;
    property OnDblClick;
    property Columns;
    property Constraints;
    property Ctl3D;
    property DragKind;
    property DragMode;
    property ReadOnly default False;
    property Enabled;
    property Font;
    property HideSelection;
    property IconOptions;
    property Items;
    property AllocBy;
    property MultiSelect;
    property OnChange;
    property OnChanging;
    property OnColumnClick;
    property OnDeletion;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEnter;
    property OnExit;
    property OnInsert;
    property OnDragDrop;
    property OnDragOver;
    property DragCursor;
    property OnStartDrag;
    property OnEndDrag;
    property OnGetImageIndex;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnSelectItem;
    property OnStartDock;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property ParentBiDiMode;
    property ShowHint;
    property PopupMenu;
    property ShowColumnHeaders;
    property TabOrder;
    property TabStop default True;
    property ViewStyle;
    property Visible;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property LargeImages;
    property SmallImages;
    property StateImages;
  end;

var
  { Default drawing variables }
  DefDraw_TextOffset: integer; // Offset for the text -- 5
  DefDraw_ImageOffset: integer; // Offset for image -- 2

implementation

uses
  Registry, ExtListView;

var
  FDirection,
  FSortColNum: integer;

function IsValidNumber(S: string; var V: extended): boolean;
var
  NumCode: integer;
  FirstSpace: integer;
begin
  FirstSpace := Pos(' ', S);
  if FirstSpace > 0 then
    S := Copy(S, 1, FirstSpace - 1);
  Val(S, V, NumCode);
  Result := (NumCode = 0);
  if not Result then
  begin
    // Remove all thousands seperators
    S := StringReplace(S, ThousandSeparator, '', [rfReplaceAll]);
    // change DecimalSeperator to '.' because Val only recognizes that, not
    // the locale specific decimal char.  Stupid Val.
    S := StringReplace(S, DecimalSeparator, '.', [rfReplaceAll]);
    // and try again
    Val(S, V, NumCode);
    Result := (NumCode = 0);
  End;
end;

// date conversion will fail if using long format, e.g. '1 January 1994'
function IsValidDateTime(const S: string; var D: TDateTime): boolean;
var
  i: integer;
  HasDate: boolean;
  HasTime: boolean;
begin
  // Check for two date seperators.  This is because some regions use a "-"
  //  to seperate dates, so if we just checked for one we would flag negative
  //  numbers as being dates.
  i := Pos(DateSeparator, S);
  HasDate := i > 0;
  if HasDate and (i <> Length(S)) then
    HasDate := Pos(DateSeparator, Copy(S, i+1, Length(S)-i)) > 0;
  HasTime := Pos(TimeSeparator, S) > 0;
  Result := HasDate or HasTime;
  if Result then
  begin
    try
      if HasDate and HasTime then
        D := StrToDateTime(S)
      else if HasDate then
        D := StrToDate(S)
      else if HasTime then
        D := StrToTime(S);
    except
      // Something failed to convert...
      D := 0;
      Result := FALSE;
    end;
  end;
end; { IsValidDateTime }

function __CustomSortProc1__(Item1, Item2: TListItem; Data: integer): integer;
   stdcall;
var
  Str1, Str2: string;
  Val1, Val2: extended;
  Date1, Date2: TDateTime;
  Diff: TDateTime;
begin
  if (Item1 = nil) or (Item2 = nil) then
  begin
    // something bad happening, I'm outta here
    Result := 0;
    exit;
  end;

  try
    if FSortColNum = -1 then
    begin
      Str1 := Item1.Caption;
      Str2 := Item2.Caption;
    end else begin
      if FSortColNum < Item1.SubItems.Count then
        Str1 := Item1.SubItems[FSortColNum]
      else
        Str1 := '';
      if FSortColNum < Item2.SubItems.Count then
        Str2 := Item2.SubItems[FSortColNum]
      else
        Str2 := '';
    end;

    if TCustomEnhListView(Data).AutoSortStyle = assSmart then
    begin
      if IsValidDateTime(Str1, Date1) and IsValidDateTime(Str2, Date2) then
      begin
        Diff := Date1 - Date2;
        if Diff < 0.0 then Result := -1
        else if Diff > 0.0 then Result := 1
        else Result := 0
      end else if IsValidNumber(Str1, Val1) and IsValidNumber(Str2, Val2) then
      begin
        if Val1 < Val2 then Result := -1
        else if Val1 > Val2 then Result := 1
        else Result := 0
      end else
        Result := AnsiCompareStr(Str1, Str2);
    end else
      Result := AnsiCompareStr(Str1, Str2);

    Result := FDirection * Result; // Set direction flag.
  except
    Result := 0;  // Something went bad in the comparison.  Say they are equal.
  end;
end;

function __CustomSortProc2__(Item1, Item2: TListItem; Data: integer): integer;
   stdcall;
var
  EvRes: integer;
begin
  EvRes := 0;
  TCustomEnhListView(Data).SortItems(Item1, Item2, FSortColNum, EvRes);
  Result := EvRes * FDirection;
end;



{ TCustomEnhListView }

// Override constructor to "zero out" our internal variable.
constructor TCustomEnhListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FSearchStr := '';
  FSearchTickCount := 0;
  FHeaderHandle := 0;
  FSortDirty := FALSE;
  FUpdateCount := 1; // inhibit sorting until finished creating.
  FAutoColumnSort := acsNoSort;
  FAutoResort := TRUE;
  FAutoSortStyle := assSmart;
  FAutoSortAscending := TRUE;
  FTmpAutoSortAscending := FAutoSortAscending;
  FLastColumnClicked := -1;
  FCanvas := nil;
  FStyle  := lvStandard;
  FSortUpBmp := nil;
  FSortDownBmp := nil;
  FShowSortArrows := FALSE;
  FReverseSortArrows := FALSE;

  FHeaderInstance := MakeObjectInstance(HeaderWndProc);
end;

destructor TCustomEnhListView.Destroy;
begin
  FSortUpBmp.Free;
  FSortDownBmp.Free;
  FCanvas.Free;
  if FHeaderHandle <> 0 then
    SetWindowLong(FHeaderHandle, GWL_WNDPROC, LongInt(FOldHeaderWndProc));
  FreeObjectInstance(FHeaderInstance);

  inherited Destroy;
end;

procedure TCustomEnhListView.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  if (FStyle = lvOwnerDrawFixed) then
  begin
    Params.Style := Params.Style or LVS_OWNERDRAWFIXED;
    if FCanvas = nil then
      FCanvas := TCanvas.Create;
  end else begin
    if (not assigned(FOnDrawHeader)) and (not FShowSortArrows) then
    begin
      FCanvas.Free;
      FCanvas := nil;
    end;
  end;
end;

procedure TCustomEnhListView.CreateWnd;
begin
//  if FCreatingWindowHandle then exit;

  FCreatingWindowHandle := TRUE;
  try
    inherited CreateWnd;
    // If we are loading object from stream (form file), we have to wait until
    // everything is loaded before populating the list.  If we are not loading,
    // i.e. the component was created dynamically or was just dropped on a form,
    // we need to reset the flag now.
    if not (csLoading in ComponentState) then
      FUpdateCount := 0;

    // Something very bizarre happens in either TCustomListView or in the
    // list view code itself in COMCTL32.DLL:  The first WM_MEASUREITEM value
    // is not honored if the listview has small images assigned to it.  Instead
    // the value is ignored and the height of the images are used.  I found that
    // by forcing Windows to ask for the item height a second time, it would
    // honor the value then.
    if Style = lvOwnerDrawFixed then
      ResetOwnerDrawHeight;
  finally
    FCreatingWindowHandle := FALSE;
  end;
end;

procedure TCustomEnhListView.Loaded;
begin
  inherited Loaded;

  if not FCreatingWindowHandle then
    HandleNeeded;

  FUpdateCount := 0;

  if Columns.Count > 0 then
    FLastColumnClicked := 0;
  Resort;

  // Something flaky going on.  Hard to explain, but this clears it up.
  PostMessage(Handle, WM_OWNERDRAWCOLUMNS, 0, 0);
end;

procedure TCustomEnhListView.WMDestroy(var Message: TWMDestroy);
begin
  inherited;
end;

procedure TCustomEnhListView.DoSort(ColumnIndex:integer; Descending: boolean);
begin
  FSortDirty := FALSE;
  LastColumnClicked := ColumnIndex;
  SortBegin(ColumnIndex, not Descending);
  if Descending then
    FDirection := 1
  else
    FDirection := -1;
  FSortColNum := ColumnIndex - 1;
  if assigned(FOnSortItems) then
    CustomSort(@__CustomSortProc2__, integer(Self))
  else
    CustomSort(@__CustomSortProc1__, integer(Self));
  SortFinished(ColumnIndex, not Descending);
end;

procedure TCustomEnhListView.DefaultSort(ColumnIndex: integer;
   Descending: boolean);
begin
  // Check if the sort order should be toggled
  if FAutoColumnSort = acsSortToggle then
    if LastColumnClicked = ColumnIndex then
      FTmpAutoSortAscending := not Descending
    else
      FTmpAutoSortAscending := Descending;

  InvalidateColumnHeader(ColumnIndex);
  DoSort(ColumnIndex, Descending);
end;

procedure TCustomEnhListView.SortItems(const Item1, Item2: TListItem;
   SortColumn: integer; var CompResult: integer);
var
  SortAs: TSortAs;
  Str1, Str2: string;
  F1, F2: extended;
  Date1, Date2, Diff: TDateTime;
begin
  // The only way to get in here is if FOnSortItems is assigned, so don't bother
  //  checking for nil
  SortAs := saNone;
  FonSortItems(Self, Item1, Item2, SortColumn, SortAs, CompResult);
  // Do they want us to sort it?
  if SortAs <> saNone then
  begin
    if SortColumn = -1 then
    begin
      Str1 := Item1.Caption;
      Str2 := Item2.Caption;
    end else begin
      if SortColumn < Item1.SubItems.Count then
        Str1 := Item1.SubItems[SortColumn]
      else
        Str1 := '';
      if SortColumn < Item2.SubItems.Count then
        Str2 := Item2.SubItems[SortColumn]
      else
        Str2 := '';
    end;

    case SortAs of
      saString: CompResult := AnsiCompareStr(Str1, Str2);
      saNumeric:
        begin
          if not IsValidNumber(Str1, F1) then
            F1 := 0;
          if not IsValidNumber(Str2, F2) then
            F2 := 0;
          if F1 < F2 then CompResult := -1
          else if F1 > F2 then CompResult := 1
          else CompResult := 0;
        end;
      saDateTime:
        begin
          if not IsValidDateTime(Str1, Date1) then
            Date1 := 0;
          if not IsValidDateTime(Str2, Date2) then
            Date1 := 0;
          Diff := Date1 - Date2;
          if Diff < 0.0 then CompResult := -1
          else if Diff > 0.0 then CompResult := 1
          else CompResult := 0
        end;
    end;
  end;
end;

procedure TCustomEnhListView.SortBegin(ColumnIndex: integer;
   Ascending: boolean);
begin
  if assigned(FOnSortBegin) then
    FOnSortBegin(Self, ColumnIndex, Ascending);
end;

procedure TCustomEnhListView.SortFinished(ColumnIndex: integer;
   Ascending: boolean);
begin
  if assigned(FOnSortFinished) then
    FOnSortFinished(Self, ColumnIndex, Ascending);
end;

procedure TCustomEnhListView.ColClick(Column: TListColumn);
begin
  // Check if the sort order should be toggled
  if FAutoColumnSort = acsSortToggle then
    if LastColumnClicked = Column.Index then
      FTmpAutoSortAscending := not FTmpAutoSortAscending
    else
      FTmpAutoSortAscending := FAutoSortAscending;

  inherited ColClick(Column);

  if (FAutoColumnSort <> acsNoSort) and (Column.Index < Columns.Count) then
    DoSort(Column.Index, FTmpAutoSortAscending);

  LastColumnClicked := Column.Index;
end;

{$IFDEF DFS_FIXED_LIST_VIEW}
procedure TCustomEnhListView.InsertItem(Item: TListItem);
begin
  inherited InsertItem(Item);
  if FAutoResort then
    Resort;
end;
{$ENDIF}


procedure TCustomEnhListView.Edit(const Item: TLVItem);
begin
  inherited Edit(Item);
  if FAutoResort then
    Resort;
end;

type
  THackListItems = class(TListItems)
  end;

procedure TCustomEnhListView.EditCanceled(const Item: TLVItem);
begin
  if Assigned(FOnEditCanceled) then
    with Item do
      FOnEditCanceled(Self, THackListItems(Items).GetItem(iItem));
end;

procedure TCustomEnhListView.CNNotify(var Message: TWMNotify);
begin
  inherited;

  with Message.NMHdr^ do
    case code of
{$IFNDEF DFS_FIXED_LIST_VIEW}
      LVN_INSERTITEM:
        if FAutoResort then
          Resort;
{$ENDIF}
      LVN_ENDLABELEDIT:
        with PLVDispInfo(Pointer(Message.NMHdr))^ do
          if (item.pszText = nil) and (item.IItem <> -1) then
            EditCanceled(item);
    end;
end;

procedure TCustomEnhListView.SetAutoColumnSort(Value: TAutoColumnSort);
begin
  if FAutoColumnSort <> Value then
  begin
    FAutoColumnSort := Value;
    if FAutoColumnSort <> acsNoSort then
      Resort;
  end;
end;

procedure TCustomEnhListView.SetAutoSortStyle(Value: TAutoSortStyle);
begin
  if FAutoSortStyle <> Value then
  begin
    FAutoSortStyle := Value;
    Resort;
  end;
end;

procedure TCustomEnhListView.SetAutoResort(Value: boolean);
begin
  if FAutoResort <> Value then
    FAutoResort := Value;
end;

procedure TCustomEnhListView.SetCurrentSortAscending(Value: boolean);
begin
  if FTmpAutoSortAscending <> Value then
  begin
    FTmpAutoSortAscending := Value;
    InvalidateColumnHeader(FLastColumnClicked);
  end;
end;

procedure TCustomEnhListView.SetAutoSortAscending(Value: Boolean);
begin
  if FAutoSortAscending <> Value then
  begin
    FAutoSortAscending := Value;
    FTmpAutoSortAscending := Value;
  end;
end;

procedure TCustomEnhListView.Resort;
begin
  FSortDirty := TRUE;
  if ((FAutoColumnSort <> acsNoSort) and (LastColumnClicked >= 0) and
     (LastColumnClicked < Columns.Count)) or (assigned(FOnSortItems)) then
  begin
    if FUpdateCount < 1 then
      DoSort(LastColumnClicked, FTmpAutoSortAscending);
  end;
end;

procedure TCustomEnhListView.BeginUpdate;
begin
  Items.BeginUpdate;
  inc(FUpdateCount);
end;


procedure TCustomEnhListView.EndUpdate;
begin
  dec(FUpdateCount);
  if FUpdateCount < 0 then
    FUpdateCount := 0; // In case someone gets overly happy with EndUpdate calls
  if FUpdateCount = 0 then
  begin
    // Need to resort?
    if FSortDirty then
      Resort;
  end;

  // Call this last so resort happens before screen redraw is re-enabled.
  Items.EndUpdate;
end;


procedure TCustomEnhListView.DrawItem(var Canvas: TCanvas; Index: Integer;
   Rect: TRect; State: TOwnerDrawState; var DefaultDrawing,
   FullRowSelect: boolean);
begin
  DefaultDrawing := not assigned(FOnDrawItem);
  if assigned(FOnDrawItem) then
    FOnDrawItem(Self, Canvas, Index, Rect, State, DefaultDrawing,FullRowSelect);
end;

procedure TCustomEnhListView.AfterDrawItem(var Canvas: TCanvas; Index: Integer;
   Rect: TRect; State: TOwnerDrawState);
begin
  if assigned(FOnAfterDefaultDrawItem) then
    FOnAfterDefaultDrawItem(Self, Canvas, Index, Rect, State);
end;

procedure TCustomEnhListView.CMSysColorChange(var Message: TWMSysColorChange);
begin
  // Need to recreate the sort arrow bmps to use the new system colors
  if ShowSortArrows then
    CreateSortBmps(FSortUpBmp, FSortDownBmp);
  inherited;
end;

procedure TCustomEnhListView.CMFontChanged(var Messsage: TMessage);
begin
  if HandleAllocated and (Style = lvOwnerDrawFixed) then
    RecreateWnd
  else
    inherited;
end;

procedure TCustomEnhListView.CNMeasureItem(var Message: TWMMeasureItem);
var
  DC: HDC;
  OldFont: HFONT;
  Size: TSize;
begin
  inherited;

  DC := CreateCompatibleDC(0);
  OldFont := SelectObject(DC, Font.Handle);
  try
    GetTextExtentPoint32(DC, 'Wy', 2, Size);
    // Owner drawing only happens in vsReport mode, so no need to check anything
    // besides that.
    // I'm checking SmallImages.Height here, but I don't think it'll do any
    // good.  From what I can tell, if you have SmallImages assigned, this
    // handler will get called but the value you give it is ignored and the
    // list uses it's normal item height.  Strange....
    if assigned(SmallImages) and (SmallImages.Height > Size.cy) then
      Message.MeasureItemStruct.itemHeight := SmallImages.Height
    else
      Message.MeasureItemStruct.itemHeight := Size.cy + 1;
  finally
    SelectObject(DC, OldFont);
    DeleteDC(DC);
  end;
  MeasureItem(Message.MeasureItemStruct.itemHeight);
  Message.Result := 1;
end;

procedure TCustomEnhListView.MeasureItem(var Height: UINT);
begin
  if assigned(FOnMeasureItem) then
    FOnMeasureItem(Self, Height);
end;


procedure TCustomEnhListView.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
  DoDefaultDrawing: boolean;
  FullRowSelect: boolean;
  SavedDC: integer;
begin { CNDrawItem }
  if FCanvas = nil then exit;

  with Message.DrawItemStruct^ do
  begin
    State := TOwnerDrawState(LongRec(itemState).Lo);

    SavedDC := SaveDC(hDC);
    FCanvas.Handle := hDC;
    try
      FCanvas.Font := Font;
      FCanvas.Brush := Brush;
      DoDefaultDrawing := FALSE;
      FullRowSelect := FALSE;
      ProcessDrawItemMsg(itemID, rcItem, State, DoDefaultDrawing, FullRowSelect);
    finally
      FCanvas.Handle := 0;
      RestoreDC(hDC, SavedDC);
    end;
  end;

  Message.Result := 1;
end;

function TCustomEnhListView.GetActualColumn(Index: integer): TListColumn;
begin
  // Delphi 2 and C++B 1 have a bug in TListColumn.GetWidth.  It returns zero
  // for the width if the handle hasn't been allocated yet instead of returning
  // the value of the internal storage variable like Delphi 3 does.  I've also
  // had some problems similar under Delphi 3, so I'm just always requiring the
  // handle to be valid.
  HandleNeeded;

  if Index >= Columns.Count then
    Result := nil
  else
    Result := Columns[Index];
end;

function TCustomEnhListView.GetSubItemText(Index, SubItem: integer): string;
begin
  if SubItem < 0 then
    Result := Items[Index].Caption
  else
    Result := Items[Index].SubItems[SubItem];
end;

// SubItem is -1 for Caption item
procedure TCustomEnhListView.DrawSubItem(Index, SubItem: Integer; Rect: TRect;
   State: TOwnerDrawState; var DefaultDrawing: boolean);
begin
  DefaultDrawing := not assigned(FOnDrawSubItem);
  if assigned(FOnDrawSubItem) then
    FOnDrawSubItem(Self, FCanvas, Index, SubItem, Rect, State, DefaultDrawing);
end;

procedure TCustomEnhListView.DefaultDrawSubItem(Index, SubItem: Integer;
   Rect: TRect; State: TOwnerDrawState);
var
  DoDefaultDrawing: boolean;
  SavedDC: integer;
begin
  DoDefaultDrawing := csDesigning in ComponentState;
  SavedDC := SaveDC(FCanvas.Handle);
  try
    if not (csDesigning in ComponentState) then
      DrawSubItem(Index, SubItem, Rect, State, DoDefaultDrawing);

    if DoDefaultDrawing then
    begin
      if SubItem >= 0 then
        InflateRect(Rect, -4, 0);
      if ActualColumn[SubItem+1].Alignment = taLeftJustify then
        Inc(Rect.Left, DefDraw_TextOffset);
      DrawTextEx(FCanvas.Handle, PChar(GetSubItemText(Index, SubItem)), -1, Rect,
         DRAWTEXTEX_FLAGS or
         DRAWTEXTEX_ALIGNMENT[ActualColumn[SubItem+1].Alignment], nil);
    end;
  finally
    RestoreDC(FCanvas.Handle, SavedDC);
  end;
end;

type
  THackImageList = class(TCustomImageList);

procedure TCustomEnhListView.DefaultDrawItem(Index: Integer; Rect: TRect;
   State: TOwnerDrawState; FullRowSelect: boolean);
const
  DrawingStyles: array[TDrawingStyle] of Longint = (ILD_FOCUS, ILD_SELECTED,
    ILD_NORMAL, ILD_TRANSPARENT);
  Images: array[TImageType] of Longint = (0, ILD_MASK);
var
  DS: TDrawingStyle;
  x: integer;
  OldBlend: TColor;
  Count: Integer;
  SubRect: TRect;
  ImgTop: integer;
begin
  if Items[Index] = nil then
    // something bad happening, I'm outta here
    exit;

  if Columns.Count > 0 then
  begin
    if (odSelected in State) then
    begin
      if Focused then
      begin
        FCanvas.Brush.Color := clHighlight;
        FCanvas.Font.Color := clHighlightText;
      end else begin
        if not HideSelection then
        begin
          FCanvas.Brush.Color := clBtnFace;
          FCanvas.Font.Color := clBtnText;
        end;
      end;
    end;
    SubRect := Rect;
    SubRect.Right := Rect.Left + CurrentColumnWidth[0]{ - 2};

    if assigned(StateImages) then
    begin
      StateImages.Draw(FCanvas, SubRect.Left + DefDraw_ImageOffSet,
         SubRect.Top + (SubRect.Bottom - SubRect.Top - StateImages.Height) div 2,
         Items[Index].StateIndex);
      Inc(SubRect.Left, StateImages.Width);
    end;

    if assigned(SmallImages) then
    begin
      OldBlend := SmallImages.BlendColor;
      SmallImages.BlendColor := clHighlight;
      ImgTop := SubRect.Top + (SubRect.Bottom - SubRect.Top -
        SmallImages.Height) div 2;

      { Changing DrawStyle causes an invalidate, which is very nasty since we
        are in the process of repainting here.  Continuous flickering.... }
      if Focused and ((odSelected in State) or Items[Index].Focused) then
        DS := dsSelected
      else
        DS := dsTransparent;
      // Draw OverlayImage
      if (Items[Index].OverlayIndex >= 0) and
         (Items[Index].OverlayIndex <= 3) then // vadid overlay index?
      begin
        x := IndexToOverlayMask(Items[Index].OverlayIndex+1);
        THackImageList(SmallImages).DoDraw(Items[Index].ImageIndex, FCanvas,
           SubRect.Left + DefDraw_ImageOffSet, ImgTop, DrawingStyles[DS] or
           Images[SmallImages.ImageType] or ILD_OVERLAYMASK and x, Enabled);
      end else
        THackImageList(SmallImages).DoDraw(Items[Index].ImageIndex, FCanvas,
           SubRect.Left + DefDraw_ImageOffSet, ImgTop,
           DrawingStyles[DS] or Images[SmallImages.ImageType], Enabled);

      SmallImages.BlendColor := OldBlend;
      if ActualColumn[0].Alignment = taLeftJustify then
        Inc(SubRect.Left, {DefDraw_TextOffset + }SmallImages.Width);
{    end else begin
      if ActualColumn[0].Alignment = taLeftJustify then
        Inc(SubRect.Left, DefDraw_TextOffset);}
    end;

    DefaultDrawSubItem(Index, -1, SubRect, State);

    // Already done column 0, start at 1.
    for Count := 1 to Columns.Count-1 do
    begin
      { Restore this through each iteration since they may screw with it in
        the OnDrawSubItem event. }
      if not FullRowSelect then
      begin
        FCanvas.Brush.Color := clWindow;
        FCanvas.Font.Color := clWindowText;
      end;

      if Count > Items[Index].SubItems.Count then
        continue; // Hidden item
      if ActualColumn[Count].Alignment = taLeftJustify then
      begin
        SubRect.Left := SubRect.Right;
        SubRect.Right := SubRect.Left + CurrentColumnWidth[Count];
//        Inc(SubRect.Left, DefDraw_TextOffset)
      end else begin
        SubRect.Left := SubRect.Right;// + DefDraw_TextOffset;
        SubRect.Right := SubRect.Left + CurrentColumnWidth[Count];
//        Dec(SubRect.Right, DefDraw_TextOffset);
      end;
      DefaultDrawSubItem(Index, Count-1, SubRect, State);
    end;
  end;
end;


procedure TCustomEnhListView.ProcessDrawItemMsg(Index: Integer; Rect: TRect;
   State: TOwnerDrawState; var DefaultDrawing, FullRowSelect: boolean);
var
  SubRect: TRect;
begin
  DefaultDrawing := csDesigning in ComponentState;
  if not (csDesigning in ComponentState) then
    DrawItem(FCanvas, Index, Rect, State, DefaultDrawing, FullRowSelect);

  if DefaultDrawing then
  begin
    FCanvas.FillRect(Rect);

    if (Index >= 0) then
    begin
      if (odSelected in State) then
      begin
        if (not HideSelection) or Focused then
        begin
          if Focused then
            FCanvas.Brush.Color := clHighlight
          else
            FCanvas.Brush.Color := clBtnFace;

          SubRect := Rect;
//          Inc(SubRect.Left, DefDraw_TextOffset - 2);
//          Dec(SubRect.Left, 2);
          if (not FullRowSelect) then
          begin
            if assigned(Items[Index]) then
              SubRect.Right := SubRect.Left +
                 FCanvas.TextWidth(Items[Index].Caption) + 8;
            if assigned(StateImages) then
              OffsetRect(SubRect, StateImages.Width, 0);
            if assigned(SmallImages) then
              OffsetRect(SubRect, SmallImages.Width, 0);
            // Don't let it go past first column width
            if (Columns.Count > 0) and
               (CurrentColumnWidth[0] < SubRect.Right) then
              SubRect.Right := CurrentColumnWidth[0];
          end else begin
            if assigned(StateImages) then
              Inc(SubRect.Left, StateImages.Width);
            if assigned(SmallImages) then
              Inc(SubRect.Left, SmallImages.Width);
          end;
          FCanvas.FillRect(SubRect);
        end;
      end;
      DefaultDrawItem(Index, Rect, State, FullRowSelect);
      if (odFocused in State) and Focused then
      begin
        SubRect := Rect;
//        Inc(SubRect.Left, DefDraw_TextOffset - 2);
//        Dec(SubRect.Left, 2);
        if (not FullRowSelect) then
        begin
          if assigned(Items[Index]) then
            SubRect.Right := SubRect.Left +
               FCanvas.TextWidth(Items[Index].Caption) + 8;
            if assigned(SmallImages) then
              OffsetRect(SubRect, SmallImages.Width, 0);
            if assigned(StateImages) then
              OffsetRect(SubRect, StateImages.Width, 0);
            // Don't let it go past first column width
            if (Columns.Count > 0) and
               (CurrentColumnWidth[0] < SubRect.Right) then
              SubRect.Right := CurrentColumnWidth[0];
        end else begin
          if assigned(StateImages) then
            Inc(SubRect.Left, StateImages.Width);
          if assigned(SmallImages) then
            Inc(SubRect.Left, SmallImages.Width);
        end;
        FCanvas.DrawFocusRect(SubRect);
      end;
    end else
      FCanvas.FillRect(Rect);

    if (not (csDesigning in ComponentState)) then
      AfterDrawItem(FCanvas, Index, Rect, State);
  end;
end;


procedure TCustomEnhListView.SetStyle(Value: TLVStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    if HandleAllocated then
      RecreateWnd;
  end;
end;

procedure TCustomEnhListView.SetReverseSortArrows(Value: boolean);
begin
  if Value <> FReverseSortArrows then
  begin
    FReverseSortArrows := Value;
    if ShowSortArrows then
    begin
      CreateSortBmps(FSortUpBmp, FSortDownBmp);
      InvalidateColumnHeader(FLastColumnClicked);
    end;
  end;
end;

procedure TCustomEnhListView.SetShowSortArrows(Value: boolean);
begin
  if Value <> FShowSortArrows then
    FShowSortArrows := Value;
  FSortUpBmp.Free;
  FSortDownBmp.Free;
  if FShowSortArrows then
  begin
    FSortUpBmp := TBitmap.Create;
    FSortDownBmp := TBitmap.Create;
    CreateSortBmps(FSortUpBmp, FSortDownBmp);
    if not (csReading in ComponentState) then
      SetColumnsOwnerDrawFlag(TRUE);
  end else begin
    FSortUpBmp := nil;
    FSortDownBmp := nil;

    if not (csReading in ComponentState) then
      SetColumnsOwnerDrawFlag(assigned(FOnDrawHeader))
  end;
  if HandleAllocated then
    Invalidate;
end;

procedure TCustomEnhListView.CreateSortBmps(var UpBmp, DownBmp: TBitmap);
var
  HeaderHeight: integer;
  MidPoint: integer;
  Bmp: TBitmap;
begin
  if UpBmp = nil then
    UpBmp := TBitmap.Create;
  if DownBmp = nil then
    DownBmp := TBitmap.Create;

  UpBmp.Canvas.Font.Assign(Font);
  HeaderHeight := UpBmp.Canvas.TextHeight('Wy') - 6;
  if HeaderHeight > 0 then
  begin
    if Odd(HeaderHeight) then
      Inc(HeaderHeight);
    UpBmp.Width := HeaderHeight;
    UpBmp.Height := HeaderHeight;
    DownBmp.Width := HeaderHeight;
    DownBmp.Height := HeaderHeight;
    MidPoint := HeaderHeight div 2;

    { Don't ask about the drawing.  I just fooled around until I got
      something I liked. }
    if FReverseSortArrows then
      Bmp := UpBmp
    else
      Bmp := DownBmp;
    with Bmp.Canvas do
    begin
      Brush.Color := clBtnFace;
      FillRect(Rect(0, 0, HeaderHeight, HeaderHeight));
      Pen.Color := clBtnShadow;
      MoveTo(MidPoint, HeaderHeight-2);
      LineTo(HeaderHeight-1, 0);
      Pixels[HeaderHeight-1, 0] := Pen.Color;
      Pen.Color := clBtnHighlight;
      MoveTo(HeaderHeight-2, 0);
      LineTo(0, 0);
      LineTo(MidPoint-1, HeaderHeight-2);
      Pixels[MidPoint-1, HeaderHeight-2] := Pen.Color;
    end;

    if FReverseSortArrows then
      Bmp := DownBmp
    else
      Bmp := UpBmp;
    with Bmp.Canvas do
    begin
      Brush.Color := clBtnFace;
      FillRect(Rect(0, 0, HeaderHeight, HeaderHeight));
      Pen.Color := clBtnHighlight;
      MoveTo(0, HeaderHeight-1);
      LineTo(MidPoint-1, 0);
      Pen.Color := clBtnShadow;
      MoveTo(MidPoint, 0);
      LineTo(HeaderHeight-1, HeaderHeight-1);
      LineTo(-1, HeaderHeight-1);
      Pixels[MidPoint, 0] := clBtnFace;
    end;
  end;
end;

procedure TCustomEnhListView.DestroyWnd;
begin
  if not FCreatingWindowHandle then
  begin
    inherited DestroyWnd;

    FHeaderHandle := 0;
  end;
end;

procedure TCustomEnhListView.DrawHeader(var Canvas: TCanvas; Index: Integer;
   var Rect: TRect; Selected: boolean; var DefaultDrawing: boolean);
begin
  DefaultDrawing := not assigned(FOnDrawHeader);
  if assigned(FOnDrawHeader) then
    FOnDrawHeader(Self, Canvas, Index, Rect, Selected, DefaultDrawing);
end;

procedure TCustomEnhListView.WMNotify(var Message: TWMNotify);
{$J+}
const
  RECURSE_FLAG: boolean = FALSE;
{$J-}
begin
  if NoColumnResize then
    case Message.NMHdr.code of
      HDN_BEGINTRACK, HDN_TRACK, HDN_BEGINTRACKW, HDN_TRACKW:
      begin
        Message.Result := 1;
        exit;
      end;
    end;

  inherited;
  // Note the recursion flag.  This is needed since the SetColumnsOwnerDrawFlag
  // call below will cause some HDN_xxx notification messages.
  if RECURSE_FLAG then
    exit;

  // For some reason, the SECOND time you drag a header width, it toasts the
  // column index in the draw item message.  Also seems to reset owner draw
  // info at times, too.  Anyway, the best fix I could come up with was to
  // always reset the owner draw flag.
  case Message.NMHdr.code of
    HDN_BEGINTRACK, HDN_ITEMCHANGED, HDN_BEGINTRACKW, HDN_ITEMCHANGEDW:
      begin
        if Message.NMHdr.code <> HDN_TRACK then
        begin
          RECURSE_FLAG := TRUE;
          try
            SetColumnsOwnerDrawFlag(assigned(FOnDrawHeader) or FShowSortArrows);
          finally
            RECURSE_FLAG := FALSE;
          end;
        end;
      end;
    HDN_DIVIDERDBLCLICK, HDN_DIVIDERDBLCLICKW:
      { D4 (and others probably) don't update column width when this happens. }
      begin
        with PHDNotify(Pointer(Message.NMHdr))^ do
          if Item < Columns.Count then
            Column[Item].Width := ListView_GetColumnWidth(Handle, Item);
      end;
  end;

(*  old way.  had some performance problems when used in conjunction with
    TToolbar97 component.  No idea why that would cause it, though.
  // For some reason, the SECOND time you drag a header width, it toasts the
  // column index in the draw item message.  Also seems to reset owner draw
  // info at times, too.  Anyway, the best fix I could come up with was to
  // always watch for a change in the header handle, and always reset the owner
  // draw flag.  Note the recursion flag.  This is needed since the
  // SetColumnsOwnerDrawFlag will cause some HDN_xxx notification messages.

  // Best way that I can find to snag the real header handle.  Kludgy at best,
  // but what else are you gonna do?
  case Message.NMHdr.code of
    HDN_LAST..HDN_FIRST:
      begin
        if Message.NMHdr.hwndFrom <> FHeaderHandle then
          FHeaderHandle := Message.NMHdr^.hwndFrom;

        if RECURSE_FLAG or (FUpdateCount > 0) then exit;

        RECURSE_FLAG := TRUE;
        try
          SetColumnsOwnerDrawFlag(assigned(FOnDrawHeader) or FShowSortArrows);
        finally
          RECURSE_FLAG := FALSE;
        end;
      end;
  end;
*)
end;

procedure TCustomEnhListView.WMDrawHeader(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
  DoDefaultDrawing: boolean;
  SavedDC: integer;
begin { CNDrawItem }
  if (FCanvas = nil) then exit;

  with Message.DrawItemStruct^ do begin
    Message.Result := 1;
    State := TOwnerDrawState(LongRec(itemState).Lo);

    SavedDC := SaveDC(hDC);
    FCanvas.Handle := hDC;
    try
      FCanvas.Font := Font;
      FCanvas.Brush := Brush;
      DoDefaultDrawing := False;
      ProcessDrawHeaderMsg(itemID, rcItem, State, DoDefaultDrawing);
    finally
      FCanvas.Handle := 0;
      RestoreDC(hDC, SavedDC);
    end;
  end;
end;

procedure TCustomEnhListView.ProcessDrawHeaderMsg(Index: Integer; Rect: TRect;
   State: TOwnerDrawState; var DefaultDrawing: boolean);
begin
  FCanvas.Font.Assign(Font);
  FCanvas.Brush.Assign(Brush);
  FCanvas.Brush.Style := bsClear;
  FCanvas.Brush.Color := clBtnFace;

  DefaultDrawing := csDesigning in ComponentState;
  if not (csDesigning in ComponentState) then
    DrawHeader(FCanvas, Index, Rect, odSelected in State, DefaultDrawing);

  if DefaultDrawing then
    DefaultDrawHeader(FCanvas, Index, Rect, odSelected in State);
end;

procedure TCustomEnhListView.DefaultDrawHeader(var Canvas: TCanvas;
   Index: Integer; var Rect: TRect; Selected: boolean);
var
  TheColumn: TListColumn;
  Offset: integer;
  R, CR: TRect;
  Bmp: TBitmap;
begin

(******************************************************************************)
(* NOTE:  This method is overriden and replaced in TExtListView.  That means  *)
(*   that if changes are made here, they will also need to be made in         *)
(*   ExtListView.pas' DefaultDrawHeader method.                               *)
(******************************************************************************)

  if not Selected then
    InflateRect(Rect, -2, -2);
  Canvas.FillRect(Rect);
  if Selected then
    InflateRect(Rect, -2, -2);

  if (Index >= 0) and (Index < Columns.Count) then begin
    // Don't use ActualColumn[] here!  That's for SubItem foolery, not header.
    TheColumn := Columns[Index];

    if Selected then begin
      inc(Rect.Top);
      inc(Rect.Left);
    end;

    R := Rect;

    case TheColumn.Alignment of
      taRightJustify: Dec(R.Right, 4);
      taLeftJustify: Inc(R.Left, 4);
      // taCenter needs no modification
    end;

    if FShowSortArrows and (LastColumnClicked = Index) and
       (AutoColumnSort <> acsNoSort) then
    begin
      if CurrentSortAscending
      then Bmp := FSortUpBmp
      else Bmp := FSortDownBmp;

      if (TheColumn.Alignment = taRightJustify)
      then Inc(R.Left, Bmp.Width + 8)
      else Dec(R.Right, Bmp.Width + 8);

      { How big of a rectangle do we have to work with for the text? }
      CR := R;
      DrawTextEx(FCanvas.Handle, PChar(TheColumn.Caption), -1, CR,
         DRAWTEXTEX_FLAGS or DT_CALCRECT or
         DRAWTEXTEX_ALIGNMENT[TheColumn.Alignment], nil);
      { Note that DT_CALCRECT does not adjust for alignment. We must do that }
      case TheColumn.Alignment of
        taRightJustify:
          R.Left := R.Right - (CR.Right - CR.Left);

        taCenter: begin
          R.Left := R.Left + (((R.Right - R.Left) - (CR.Right - CR.Left)) div 2);
          R.Right := R.Left + (CR.Right - CR.Left);
        end;
        else // taLeftJustify: doesn't matter, that is what DT_CALCRECT returns
          R := CR;
      end;

      if R.Left < Rect.Left then R.Left := Rect.Left;
      if R.Right > Rect.Right then R.Right := Rect.Right;

      if Selected then
        OffsetRect(R, 1, 1);

      // Draw the caption in the rect available
      DrawTextEx(FCanvas.Handle, PChar(TheColumn.Caption), -1, R,
         DRAWTEXTEX_FLAGS or DRAWTEXTEX_ALIGNMENT[TheColumn.Alignment], nil);

      // Draw the sort arrow bitmap
      Offset := (Rect.Bottom - Rect.Top - Bmp.Height) div 2;
      case TheColumn.Alignment of
        taRightJustify:
          // Only draw if we have enough room
          if (R.Left - Bmp.Width - 8) >= Rect.Left then
            Canvas.Draw(R.Left - Bmp.Width - 8, R.Top + Offset, Bmp);
      else // taLeftJustify, taCenter
        // Only draw if we have enough room
        if (R.Right + Bmp.Width + 8) <= Rect.Right then
          Canvas.Draw(R.Right + 8, R.Top + Offset, Bmp);
      end;
    end else begin
      if Selected then
        OffsetRect(R, 1, 1);

      DrawTextEx(FCanvas.Handle, PChar(TheColumn.Caption), -1, R,
         DRAWTEXTEX_FLAGS or DRAWTEXTEX_ALIGNMENT[TheColumn.Alignment], nil);
    end;
  end;
end;

procedure TCustomEnhListView.SetOnDrawHeader(Value: TLVHDrawItemEvent);
begin
  FOnDrawHeader := Value;
  SetColumnsOwnerDrawFlag(assigned(FOnDrawHeader) or FShowSortArrows);
end;

procedure TCustomEnhListView.SetColumnsOwnerDrawFlag(OwnerDrawn: boolean);
var
  Item: THDItem;
  x: integer;
begin
  if not HandleAllocated then exit;

  for x := 0 to Columns.Count-1 do
  begin
    Item.Mask := HDI_FORMAT;
    if Header_GetItem(HeaderHandle, x, Item) then
    begin
      if OwnerDrawn then
        Item.Fmt := Item.Fmt or HDF_OWNERDRAW
      else
        Item.Fmt := Item.Fmt and not HDF_OWNERDRAW;
      Header_SetItem(HeaderHandle, x, Item);
    end;
  end;

  if OwnerDrawn then
  begin
    if (FCanvas = nil) then
      FCanvas := TCanvas.Create;
  end else begin
    if (Style = lvStandard) and (FCanvas <> nil) then
    begin
      FCanvas.Free;
      FCanvas := nil;
    end;
  end;
end;

procedure TCustomEnhListView.SetLastColumnClicked(Value: integer);
var
  OldValue: integer;
begin
  if Value <> FLastColumnClicked then
  begin
    OldValue := FLastColumnClicked;
    FLastColumnClicked := Value;
    // If showing arrows and clicked column changes, we have to get rid of the
    // old sorting arrow by causing the header to be repainted.
    if FShowSortArrows then
      // Can't do this above because FLastColumnClicked is used to paint the
      // arrow
      InvalidateColumnHeader(OldValue);
  end;
end;

function TCustomEnhListView.ActualColumnIndex(Index: integer): integer;
begin
  Result := Index;
end;

procedure TCustomEnhListView.InvalidateColumnHeader(Index: integer);

  function RealColWidth(i: integer): integer;
  var
    Column: TLVColumn;
  begin
    Column.mask := LVCF_WIDTH;
    ListView_GetColumn(Handle, i, Column);
    Result := Column.cx;
  end;
  
var
  R: TRect;
  x: integer;
  w: integer;
begin
  if (Index < 0) or (Index >= Columns.Count) or (HeaderHandle = 0) then
    exit;

  w := RealColWidth(Index);
  // We have to turn this into the actual column index if drag-drop headers have
  // re-arranged stuff in the TExtListView descendant component.
  Index := ActualColumnIndex(Index);

  Windows.GetClientRect(HeaderHandle, R);
  for x := 0 to Columns.Count - 1 do
    if ActualColumnIndex(x) < Index then
      inc(R.Left, RealColWidth(x));
  R.Right := R.Left + w;

  // Adjust for shadow
  InflateRect(R, -2, -2);
  InvalidateRect(HeaderHandle, @R, FALSE);
end;

procedure TCustomEnhListView.WMOwnerDrawColumns(var Message: TMessage);
begin
  SetColumnsOwnerDrawFlag(assigned(FOnDrawHeader) or FShowSortArrows);
  Update;
end;

procedure TCustomEnhListView.ResizeColumns(ResizeMethod: TResizeMethod);
var
  i: integer;
begin
  BeginUpdate;
  Columns.BeginUpdate;
  try
    for i := 0 to Columns.Count - 1 do
      if ResizeMethod = rmFitText then
        Columns[i].Width := -1
      else
        Columns[i].Width := -2;
  finally
    EndUpdate;
    Columns.EndUpdate;
  end;
end;


function TCustomEnhListView.GetCurrentColumnWidth(Index: integer): integer;
var
  Column: TLVColumn;
begin
  if HandleAllocated then
  begin
    Column.mask := LVCF_WIDTH;
    ListView_GetColumn(Handle, ActualColumnIndex(Index), Column);
    Result := Column.cx;
  end else
    Result := ActualColumn[Index].Width;
end;


function TCustomEnhListView.GetSmallImages: TCustomImageList;
begin
  Result := inherited SmallImages;
end;

procedure TCustomEnhListView.SetSmallImages(Val: TCustomImageList);
begin
  inherited SmallImages := Val;

  // If owner drawn, we have to recreate the window so that the WM_MEASUREITEM
  // will get sent to us again, and we can handle it to account for image list
  // size.
  if HandleAllocated and (Style = lvOwnerDrawFixed) and (not (csLoading in
    ComponentState)) then
    ResetOwnerDrawHeight;
end;

procedure TCustomEnhListView.HeaderWndProc(var Message: TMessage);
  function DisallowColumnResize: boolean;
  var
    HTI: THDHitTestInfo;
    pt: TPoint;
  begin
    Result := NoColumnResize;
    if (not Result) and (Self is TCustomExtListView) then
    begin
      // get cursor position
      GetCursorPos(pt);
      // convert to coordinates on header control of the listview
      Windows.ScreentoClient(HeaderHandle, pt);
      // fill in hittest structure
      HTI.flags := HHT_ONHEADER Or HHT_ONDIVIDER;
      HTI.point.x := pt.x;
      HTI.point.y := pt.y;
      //  get the header's hit-test info
      SendMessage(HeaderHandle, HDM_HITTEST, LongInt(0),LongInt(@HTI));
      if (HTI.Item >=0) and (HTI.Item <
        TdfsExtListView(Self).ColumnsFormat.Count) then
        Result := not TdfsExtListView(Self).ColumnsFormat[HTI.Item].AllowResize;
    end;
  end;
var
  HTI: THDHitTestInfo;
  Icon: HICON;
begin
  try
    with Message do
    begin
      case Msg of
        WM_SETCURSOR:
          begin
            if DisallowColumnResize then
//            if NoColumnResize then
            begin
              Icon := GetClassLong(FHeaderHandle, GCL_HICON);
              if Icon = 0 then
                Icon := LoadCursor(0, IDC_ARROW);
              SetCursor(Icon);
              exit;
            end;
          end;
        WM_NCHITTEST:
          begin
            with TWMNCHitTest(Message) do
              if csDesigning in ComponentState then
              begin
                Result := Windows.HTTRANSPARENT;
                exit;
              end
              else if DisallowColumnResize then
              begin
                HTI.Point := Point(LoWord(Message.LParam), HiWord(Message.LParam));
                Windows.ScreenToClient(FHeaderHandle, HTI.Point);
                SendMessage(FHeaderHandle, HDM_HITTEST, 0, integer(@HTI));
                if ((HTI.Flags and HHT_ONHeader) = 0) then
                begin
                  Result := Windows.HTNOWHERE;
                  exit;
                end;
              end;
          end;
        WM_NCDESTROY:
          begin
            Result := CallWindowProc(FOldHeaderWndProc, FHeaderHandle, Msg, WParam, LParam);
            FHeaderHandle := 0;
            FOldHeaderWndProc := nil;
            Exit;
          end;
      end;
      Result := CallWindowProc(FOldHeaderWndProc, FHeaderHandle, Msg, WParam, LParam);
    end;
  except
    Application.HandleException(Self);
  end;
end;

procedure TCustomEnhListView.WMParentNotify(var Message: TWMParentNotify);
begin
  with Message do
    if (Event = WM_CREATE) and (FHeaderHandle = 0) then
    begin
      FHeaderHandle := ChildWnd;
      FOldHeaderWndProc := Pointer(GetWindowLong(FHeaderHandle, GWL_WNDPROC));
      SetWindowLong(FHeaderHandle, GWL_WNDPROC, LongInt(FHeaderInstance));
    end;
  inherited;
end;

procedure TCustomEnhListView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  // Ctrl + causes all columns to change size as if their dividers had been
  // double-clicked.  Can't have that.
  if NoColumnResize and (Key = VK_ADD) and (Shift = [ssCtrl]) then
    Key := VK_SUBTRACT;
end;

procedure TCustomEnhListView.ResetOwnerDrawHeight;
var
  r: TRect;
  wp: TWindowPos;
begin
  // Found this code on www.codeguru.com in an article talking about how to get
  // an owner draw listview to ask for the item height (WM_MEASUREITEM) again.  
	GetWindowRect(Handle, r);
	wp.hwnd := Handle;
	wp.cx := Width;
	wp.cy := Height;
	wp.flags := SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOOWNERZORDER or SWP_NOZORDER;
	SendMessage(Handle, WM_WINDOWPOSCHANGED, 0, LPARAM(@wp));
end;

procedure TCustomEnhListView.MoveItem(OriginalIndex, NewIndex: Integer);
var
  Selected, Focused: boolean;
  ListItem:  TListItem;
begin
  if ((OriginalIndex < 0) or (OriginalIndex > Items.Count)) or
    ((NewIndex < 0) or (NewIndex > Items.Count)) then
    Exit;
    
  BeginUpdate;
  try
    Selected := Items[OriginalIndex].Selected;
    Focused := Items[OriginalIndex].Focused;
    if NewIndex < OriginalIndex then
      inc(OriginalIndex);
    if (NewIndex > OriginalIndex) then
      ListItem := Items.Insert(NewIndex + 1)
    else
      ListItem := Items.Insert(NewIndex);
    ListItem.Assign(Items[OriginalIndex]);
    Items.Delete(OriginalIndex);
    ListItem.Selected := Selected;
    ListItem.Focused := Focused;
  finally
    EndUpdate;
  end;
end;

procedure TCustomEnhListView.KeyUp(var Key: Word; Shift: TShiftState);
var
  PrevSearch: string;
  Ascii: array[0..1] of char;
  KBState: TKeyboardState;
begin
  inherited;
  if ColumnSearch then
  begin
    GetKeyboardState(KBState);
    if (ToAscii(Key, 0, KBState, Ascii, 0) = 1) and (Ascii[0] in [#32..#127]) then
    begin
      PrevSearch := FSearchStr;                      // remember searchstring
      if GetTickCount > FSearchTickCount + 1000 then // last search over one second ago?
        PrevSearch := '';                            // reset searchstring
      FSearchStr := PrevSearch + Ascii[0];           // Append searchstring
      FSearchTickCount := GetTickCount;              // remember last search time
      Key := 0;                                      // prevent automatic search on first column
      if not StringSelect(FSearchStr, LastColumnClicked) then
      begin
        MessageBeep(MB_ICONSTOP);
        FSearchStr := PrevSearch;
      end;
    end;
  end;
end;

function TCustomEnhListView.StringSelect(FindStr: string; ColumnIndex: Integer): boolean;
var
  SearchLen,
  SearchIndex,
  SearchStart: Integer;
begin
  Result := FALSE;
  SearchLen := Length(FindStr);
  if Assigned(Selected) then   // determine starting item
    SearchStart := Selected.Index + 1
  else
    SearchStart := 1;

  // Searches from currently selected item to last item
  // and from first item to currently selected item until result(found)

  SearchIndex := 0;
  while (SearchIndex < Items.Count) and not Result do
  begin
    if ColumnIndex = 0 then                                // find main or subitem?
      Result := AnsiCompareText(Copy(Items[(SearchStart + SearchIndex) mod
        Items.Count].Caption, 0, SearchLen), FindStr) = 0
    else
      Result := AnsiCompareText(Copy(Items[(SearchStart + SearchIndex) mod
        Items.Count].SubItems[ColumnIndex - 1], 0, SearchLen), FindStr) = 0;
    Inc(SearchIndex);
  end;
  if Result then
  begin
    Selected := Items[(SearchStart + SearchIndex - 1) mod Items.Count];
    ItemFocused := Selected;
  end;
end;

function TCustomEnhListView.SubStringSelect(FindStr: string;
  ColumnIndex: Integer): boolean;
var
  SearchIndex,
  SearchStart: Integer;
begin
  Result := FALSE;
  if Assigned(Selected) then  // determine starting item
    SearchStart := Selected.Index + 1
  else
    SearchStart := 1;

  // Searches from currently selected item to last item
  // and from first item to currently selected item until result(found)

  SearchIndex := 0;
  while (SearchIndex < Items.Count) and not Result do
  begin
    if ColumnIndex = 0 then                                // find main or subitem?
      Result := Pos(FindStr, Items[(SearchStart + SearchIndex) mod
        Items.Count].Caption) > 0
    else
      Result := Pos(FindStr, Items[(SearchStart + SearchIndex) mod
        Items.Count].SubItems[ColumnIndex - 1]) > 0;
    Inc(SearchIndex);
  end;
  if Result then
  begin
    Selected := Items[(SearchStart + SearchIndex - 1) mod Items.Count];
    ItemFocused := Selected;
  end;
end;

procedure TCustomEnhListView.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = SmallImages) then
    SmallImages := nil;
  inherited Notification(AComponent, Operation);
end;

initialization
  DefDraw_TextOffset := 4;
  DefDraw_ImageOffset := 2;
end.

