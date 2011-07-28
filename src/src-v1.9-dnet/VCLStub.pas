unit VCLStub;

interface

uses
  System.Collections,
  System.Drawing,
  System.Globalization,
  System.IO,
  System.Text.RegularExpressions,
  System.Reflection,
  System.Runtime.InteropServices,
  System.Security,
  System.Security.Permissions,
  System.Text,
  System.Threading,
  System.Windows.Forms;

type
  TEnumSet = packed record
  private
    FValue: LongWord;
  public
    class function Create(): TEnumSet; overload; static;
    class function Create(e: array of Enum): TEnumSet; overload; static;

    procedure Include(e: Enum); overload;
    procedure Include(e: array of Enum); overload;

    procedure Exclude(e: Enum);
    function InSet(e: Enum): Boolean;

    function IsEmpty(): Boolean;

    function ToString(B: Byte): string; overload;
  end;

  TPoint = packed record
    X: LongInt;
    Y: LongInt;
  public
    class function Create(const AX, AY: LongInt): TPoint; static;
    class function Empty: TPoint; static;
    function ToString: string; override;

    function Left: LongInt;
    function Top: LongInt;

    function IsEmpty: Boolean;
    function Equals(const AX, AY: LongInt): Boolean; overload;
    function Equals(const Value: TPoint): Boolean; overload;
  end;

  TRect = packed record
    Left, Top, Right, Bottom: LongInt;
  strict private
  public
    class function Create(const ALeft, ATop, ARight, ABottom: LongInt): TRect; overload; static;
    class function Bounds(const ALeft, ATop, AWidth, AHeight: LongInt): TRect; static;
    class function Empty: TRect; static;
    function ToString: string; override;

    function ToRectangle(): System.Drawing.Rectangle;

    function GetWidth: LongInt;
    function GetHeight: LongInt;

    function IsEmpty: Boolean;
    function Contains(const X, Y: LongInt): Boolean; overload;
    function Contains(const Value: TPoint): Boolean; overload;

    function GetOffset(const X, Y: LongInt): TRect; overload;
    function GetOffset(const Value: TPoint): TRect; overload;
  end;

{==============================================================================}

type
  MakeIntResource = Integer;

const
  WM_SIZE             = $0005;
  WM_COPYDATA         = $004A;
  WM_GETDLGCODE       = $0087;
  WM_HSCROLL          = $0114;
  WM_VSCROLL          = $0115;
  WM_DROPFILES        = $0233;
  WM_USER             = $0400;

  SW_SHOW = 5;

  RT_BITMAP       = MakeIntResource(2);
  RT_RCDATA       = MakeIntResource(10);

const
  hhctrl = 'hhctrl.ocx';
  HH_DISPLAY_TOPIC = $0000;
  HH_HELP_CONTEXT = $000F;
  HH_CLOSE_ALL = $0012;

const
  { Dialog Codes }
  DLGC_WANTARROWS = 1;         { Control wants arrow keys         }
  DLGC_WANTTAB = 2;            { Control wants tab keys           }
  DLGC_WANTALLKEYS = 4;        { Control wants all keys           }
  DLGC_WANTMESSAGE = 4;        { Pass message to control          }
  DLGC_HASSETSEL = 8;          { Understands EM_SETSEL message    }
  DLGC_DEFPUSHBUTTON = $10;    { Default pushbutton               }
  DLGC_UNDEFPUSHBUTTON = $20;  { Non-default pushbutton           }
  DLGC_RADIOBUTTON = $40;      { Radio button                     }
  DLGC_WANTCHARS = $80;        { Want WM_CHAR messages            }
  DLGC_STATIC = $100;          { Static item: don't include       }
  DLGC_BUTTON = $2000;         { Button item: can be checked      }

  { Scroll Bar Constants }
  SB_HORZ = 0;
  SB_VERT = 1;
  SB_CTL = 2;
  SB_BOTH = 3;

  SIF_RANGE = 1;
  SIF_PAGE = 2;
  SIF_POS = 4;
  SIF_DISABLENOSCROLL = 8;
  SIF_TRACKPOS = $10;
  SIF_ALL = (SIF_RANGE or SIF_PAGE or SIF_POS or SIF_TRACKPOS);

type
  LOWORD = Word;
  DWORD = LongWord;
  UINT = LongWord;
  BOOL = LongBool;
  WPARAM = Longint;
  LPARAM = Longint;
  THandle = Integer;
  HGLOBAL = THandle;
  HRSRC = type THandle;
  HMODULE = type THandle;
  HDROP = Longint;
  HWND = type LongWord;
  HINST = type THandle;
  HRGN = type LongWord;
  LANGID = Word;
  HKL = type LongWord;

const
  INVALID_HANDLE_VALUE = THandle(-1);

type
  TScrollInfo = packed record
    cbSize: UINT;
    fMask: UINT;
    nMin: Integer;
    nMax: Integer;
    nPage: UINT;
    nPos: Integer;
    nTrackPos: Integer;
  end;

  TVSFixedFileInfo = packed record
    dwSignature: DWORD;        { e.g. $feef04bd }
    dwStrucVersion: DWORD;     { e.g. $00000042 = "0.42" }
    dwFileVersionMS: DWORD;    { e.g. $00030075 = "3.75" }
    dwFileVersionLS: DWORD;    { e.g. $00000031 = "0.31" }
    dwProductVersionMS: DWORD; { e.g. $00030010 = "3.10" }
    dwProductVersionLS: DWORD; { e.g. $00000031 = "0.31" }
    dwFileFlagsMask: DWORD;    { = $3F for version "0.42" }
    dwFileFlags: DWORD;        { e.g. VFF_DEBUG | VFF_PRERELEASE }
    dwFileOS: DWORD;           { e.g. VOS_DOS_WINDOWS16 }
    dwFileType: DWORD;         { e.g. VFT_DRIVER }
    dwFileSubtype: DWORD;      { e.g. VFT2_DRV_KEYBOARD }
    dwFileDateMS: DWORD;       { e.g. 0 }
    dwFileDateLS: DWORD;       { e.g. 0 }
  end;

const
  MAX_PATH = 260;

type
  [StructLayout(LayoutKind.Sequential)]
  TFileTime = record
    dwLowDateTime: DWORD;
    dwHighDateTime: DWORD;
  end;

  [StructLayout(LayoutKind.Sequential, CharSet=CharSet.Auto)]
  TWin32FindData = record
    dwFileAttributes: DWORD;
    ftCreationTime: TFileTime;
    ftLastAccessTime: TFileTime;
    ftLastWriteTime: TFileTime;
    nFileSizeHigh: DWORD;
    nFileSizeLow: DWORD;
    dwReserved0: DWORD;
    dwReserved1: DWORD;
    [MarshalAs(UnmanagedType.ByValTStr, SizeConst=MAX_PATH)]
    cFileName: string;
    [MarshalAs(UnmanagedType.ByValTStr, SizeConst=14)]
    cAlternateFileName: string;
  end;

  [StructLayout(LayoutKind.Sequential, CharSet=CharSet.Ansi)]
  TWin32FindDataA = record
    dwFileAttributes: DWORD;
    ftCreationTime: TFileTime;
    ftLastAccessTime: TFileTime;
    ftLastWriteTime: TFileTime;
    nFileSizeHigh: DWORD;
    nFileSizeLow: DWORD;
    dwReserved0: DWORD;
    dwReserved1: DWORD;
    [MarshalAs(UnmanagedType.ByValTStr, SizeConst=MAX_PATH)]
    cFileName: string;
    [MarshalAs(UnmanagedType.ByValTStr, SizeConst=14)]
    cAlternateFileName: string;
  end;

  [StructLayout(LayoutKind.Sequential, CharSet=CharSet.Unicode)]
  TWin32FindDataW = record
    dwFileAttributes: DWORD;
    ftCreationTime: TFileTime;
    ftLastAccessTime: TFileTime;
    ftLastWriteTime: TFileTime;
    nFileSizeHigh: DWORD;
    nFileSizeLow: DWORD;
    dwReserved0: DWORD;
    dwReserved1: DWORD;
    [MarshalAs(UnmanagedType.ByValTStr, SizeConst=MAX_PATH)]
    cFileName: string;
    [MarshalAs(UnmanagedType.ByValTStr, SizeConst=14)]
    cAlternateFileName: string;
  end;

[SuppressUnmanagedCodeSecurity, DllImport(hhctrl, CharSet = CharSet.Unicode, SetLastError = True, EntryPoint = 'HtmlHelpW')]
function HtmlHelp(hwndCaller: IntPtr; pszFile: string; uCommand: UInt; dwData: DWORD): HWND; external;

const
  shell32   = 'shell32.dll';
  user32    = 'user32.dll';
  version   = 'version.dll';
  kernel32  = 'kernel32.dll';

[SuppressUnmanagedCodeSecurity, DllImport(shell32, CharSet = CharSet.Auto, SetLastError = True, EntryPoint = 'ShellExecute')]
function ShellExecute(hWnd: HWND; Operation, FileName, Parameters,
  Directory: string; ShowCmd: Integer): HINST; external;

[SuppressUnmanagedCodeSecurity, DllImport(user32, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'ScrollWindowEx')]
function ScrollWindowEx(hWnd: HWND; dx, dy: Integer; const prcScroll, prcClip: TRect;
  hrgnUpdate: HRGN; out prcUpdate: TRect; flags: UINT): BOOL; external;

[SuppressUnmanagedCodeSecurity, DllImport(user32, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'SetScrollRange')]
function SetScrollRange(hWnd: HWND; nBar, nMinPos, nMaxPos: Integer; bRedraw: BOOL): BOOL; external;

[SuppressUnmanagedCodeSecurity, DllImport(user32, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'GetScrollInfo')]
function GetScrollInfo(hWnd: HWND; BarFlag: Integer; var ScrollInfo: TScrollInfo): BOOL; external;

[SuppressUnmanagedCodeSecurity, DllImport(user32, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'InvalidateRect')]
function InvalidateRect(hWnd: HWND; const lpRect: TRect; bErase: BOOL): BOOL; external;

[SuppressUnmanagedCodeSecurity, DllImport(user32, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'SetScrollPos')]
function SetScrollPos(hWnd: HWND; nBar, nPos: Integer; bRedraw: BOOL): Integer; external;

[SuppressUnmanagedCodeSecurity, DllImport(user32, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'GetKeyboardLayout')]
function GetKeyboardLayout(dwLayout: DWORD): HKL; external;

[SuppressUnmanagedCodeSecurity, DllImport(user32, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'ActivateKeyboardLayout')]
function ActivateKeyboardLayout(hkl: HKL; Flags: UINT): HKL; external;

[SuppressUnmanagedCodeSecurity, DllImport(shell32, CharSet = CharSet.Auto, SetLastError = True, EntryPoint = 'DragQueryFile')]
function DragQueryFile(Drop: HDROP; FileIndex: UINT; FileName: StringBuilder; cb: UINT): UINT; external;

[SuppressUnmanagedCodeSecurity, DllImport(version, CharSet = CharSet.Auto, SetLastError = True, EntryPoint = 'GetFileVersionInfoSize')]
function GetFileVersionInfoSize(lptstrFilename: string; out lpdwHandle: DWORD): DWORD; external;

[SuppressUnmanagedCodeSecurity, DllImport(version, CharSet = CharSet.Auto, SetLastError = True, EntryPoint = 'GetFileVersionInfo')]
function GetFileVersionInfo(lptstrFilename: string; dwHandle, dwLen: DWORD; [out] lpData: TBytes): BOOL; external;

[SuppressUnmanagedCodeSecurity, DllImport(version, CharSet = CharSet.Auto, SetLastError = True, EntryPoint = 'VerQueryValue')]
function VerQueryValue([in] pBlock: TBytes; lpSubBlock: string;
  out lplpBuffer: IntPtr; out puLen: UINT): BOOL; external;

[SuppressUnmanagedCodeSecurity, DllImport(shell32, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'DragAcceptFiles')]
procedure DragAcceptFiles(Wnd: HWND; Accept: BOOL); external;

[SuppressUnmanagedCodeSecurity, DllImport(user32, CharSet = CharSet.Auto, SetLastError = True, EntryPoint = 'PostMessage')]
function PostMessage(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL; external;

[SuppressUnmanagedCodeSecurity, DllImport(user32, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'EnableWindow')]
function EnableWindow(hWnd: HWND; bEnable: BOOL): BOOL; external;

[SuppressUnmanagedCodeSecurity, DllImport(kernel32, CharSet = CharSet.Auto, SetLastError = True, EntryPoint = 'FindResource')]
function FindResource(hModule: HMODULE; lpName, lpType: string): HRSRC; overload; external;

[SuppressUnmanagedCodeSecurity, DllImport(kernel32, CharSet = CharSet.Auto, SetLastError = True, EntryPoint = 'FindResource')]
function FindResource(hModule: HMODULE; lpName: string; lpType: Integer): HRSRC; overload; external;

[SuppressUnmanagedCodeSecurity, DllImport(kernel32, CharSet = CharSet.Auto, SetLastError = True, EntryPoint = 'FindResource')]
function FindResource(hModule: HMODULE; lpName, lpType: Integer): HRSRC; overload; external;

[SuppressUnmanagedCodeSecurity, DllImport(kernel32, CharSet = CharSet.Auto, SetLastError = True, EntryPoint = 'FindResource')]
function FindResource(hModule: HMODULE; lpName: Integer; lpType: string): HRSRC; overload; external;

[SuppressUnmanagedCodeSecurity, DllImport(kernel32, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'LoadResource')]
function LoadResource(hModule: HINST; hResInfo: HRSRC): HGLOBAL; external;

[SuppressUnmanagedCodeSecurity, DllImport(kernel32, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'SizeofResource')]
function SizeofResource(hModule: HINST; hResInfo: HRSRC): DWORD; external;

[SuppressUnmanagedCodeSecurity, DllImport(kernel32, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'FreeResource')]
function FreeResource(hResData: HGLOBAL): BOOL; external;

[SuppressUnmanagedCodeSecurity, DllImport(kernel32, CharSet = CharSet.Auto, SetLastError = True, EntryPoint = 'GetPrivateProfileString')]
function GetPrivateProfileString(lpAppName, lpKeyName, lpDefault: string;
  lpReturnedString: StringBuilder; nSize: DWORD; lpFileName: string): DWORD; overload; external;
[SuppressUnmanagedCodeSecurity, DllImport(kernel32, CharSet = CharSet.Auto, SetLastError = True, EntryPoint = 'GetPrivateProfileString')]
function GetPrivateProfileString(lpAppName: string; lpKeyName, lpDefault: IntPtr;
  [out] lpReturnedString: TBytes; nSize: DWORD; lpFileName: string): DWORD; overload; external;
[SuppressUnmanagedCodeSecurity, DllImport(kernel32, CharSet = CharSet.Auto, SetLastError = True, EntryPoint = 'GetPrivateProfileString')]
function GetPrivateProfileString(lpAppName, lpKeyName, lpDefault: IntPtr;
  [out] lpReturnedString: TBytes; nSize: DWORD; lpFileName: string): DWORD; overload; external;

[SuppressUnmanagedCodeSecurity, DllImport(kernel32, CharSet = CharSet.Auto, SetLastError = True, EntryPoint = 'WritePrivateProfileString')]
function WritePrivateProfileString(lpAppName, lpKeyName, lpString, lpFileName: string): BOOL; overload; external;
[SuppressUnmanagedCodeSecurity, DllImport(kernel32, CharSet = CharSet.Auto, SetLastError = True, EntryPoint = 'WritePrivateProfileString')]
function WritePrivateProfileString(lpAppName, lpKeyName: string; lpString: IntPtr; lpFileName: string): BOOL; overload; external;
[SuppressUnmanagedCodeSecurity, DllImport(kernel32, CharSet = CharSet.Auto, SetLastError = True, EntryPoint = 'WritePrivateProfileString')]
function WritePrivateProfileString(lpAppName: string; lpKeyName, lpString: IntPtr; lpFileName: string): BOOL; overload; external;
[SuppressUnmanagedCodeSecurity, DllImport(kernel32, CharSet = CharSet.Auto, SetLastError = True, EntryPoint = 'WritePrivateProfileString')]
function WritePrivateProfileString(lpAppName, lpKeyName, lpString: IntPtr; lpFileName: string): BOOL; overload; external;

[SuppressUnmanagedCodeSecurity, DllImport(kernel32, CharSet = CharSet.Auto, SetLastError = True, EntryPoint = 'FindFirstFile')]
function FindFirstFile(lpFileName: string; out lpFindFileData: TWIN32FindData): THandle; external;

[SuppressUnmanagedCodeSecurity, DllImport(kernel32, CharSet = CharSet.Auto, SetLastError = True, EntryPoint = 'FindNextFile')]
function FindNextFile(hFindFile: THandle; out lpFindFileData: TWIN32FindData): BOOL; external;

[SuppressUnmanagedCodeSecurity, DllImport(kernel32, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'FileTimeToLocalFileTime')]
function FileTimeToLocalFileTime(const lpFileTime: TFileTime; out lpLocalFileTime: TFileTime): BOOL; external;

[SuppressUnmanagedCodeSecurity, DllImport(kernel32, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'FileTimeToDosDateTime')]
function FileTimeToDosDateTime(const lpFileTime: TFileTime;
  out lpFatDate, lpFatTime: Word): BOOL; external;

[SuppressUnmanagedCodeSecurity, DllImport(kernel32, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'FindClose')]
function FindClose(hFindFile: THandle): BOOL; external;

{==============================================================================}

type
  TSeekOrigin = (soBeginning, soCurrent, soEnd);

const
  { TFileStream create mode }
  fmCreate         = $FFFF;
  fmOpenRead       = $0000;
  fmOpenWrite      = $0001;
  fmOpenReadWrite  = $0002;

  fmShareDenyWrite = $0020;
  fmShareDenyRead  = $0030; // write-only not supported on all platforms
  fmShareDenyNone  = $0040;

const
  { File attribute constants }
  faReadOnly  = $00000001;
  faHidden    = $00000002;
  faSysFile   = $00000004;
  faVolumeID  = $00000008 deprecated;  // not used in Win32
  faDirectory = $00000010;
  faArchive   = $00000020;
  faAnyFile   = $0000003F;

{==============================================================================}

type
  TListSortCompare = function (Item1, Item2: TObject): Integer;
  TListNotification = (lnAdded, lnExtracted, lnDeleted);
  EListError = class(Exception);

  // these operators are used in Assign and go beyond simply copying
  //   laCopy = dest becomes a copy of the source
  //   laAnd  = intersection of the two lists
  //   laOr   = union of the two lists
  //   laXor  = only those not in both lists
  // the last two operators can actually be thought of as binary operators but
  // their implementation has been optimized over their binary equivalent.
  //   laSrcUnique  = only those unique to source (same as laAnd followed by laXor)
  //   laDestUnique = only those unique to dest   (same as laOr followed by laXor)
  TListAssignOp = (laCopy, laAnd, laOr, laXor, laSrcUnique, laDestUnique);

  TList = class;

  TListEnumerator = class
  private
    FIndex: Integer;
    FList: TList;
  public
    constructor Create(AList: TList);
    function GetCurrent: TObject;
    function MoveNext: Boolean;
    property Current: TObject read GetCurrent;
  end;

  TList = class(TObject)
  private
  const
    SListCountError = 'List count out of bounds (%d)';
    SListIndexError = 'List index out of bounds (%d)';
    SListCapacityError = 'List capacity out of bounds (%d)';

  var
    FList: System.Collections.ArrayList;
  protected
    function Get(Index: Integer): TObject;
    function GetCount: Integer;
    function GetCapacity: Integer;
    procedure Grow; virtual;
    procedure Put(Index: Integer; Item: TObject);
    procedure Notify(Instance: TObject; Action: TListNotification); virtual;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
  public
    constructor Create;
    function Add(Item: TObject): Integer;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
                                      
    class procedure Error(const Msg: string; Data: Integer); overload; 
    procedure Exchange(Index1, Index2: Integer);
    function Extract(Item: TObject): TObject;
    function First: TObject;
    function GetEnumerator: TListEnumerator;
    function IndexOf(Item: TObject): Integer;
    procedure Insert(Index: Integer; Item: TObject);
    function Last: TObject;
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(Item: TObject): Integer;
    procedure Pack;
    procedure Sort(Compare: TListSortCompare);
    procedure Assign(ListA: TList; AOperator: TListAssignOp = laCopy; ListB: TList = nil);
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: TObject read Get write Put; default;
    property List: System.Collections.ArrayList read FList;
  end;

  TObjectList = class(TList)
  private
    FOwnsObjects: Boolean;
  protected
    procedure Notify(Instance: TObject; Action: TListNotification); override;
  public
    constructor Create; overload;
    constructor Create(AOwnsObjects: Boolean); overload;

    function FindInstanceOf(AClass: TClass; AExact: Boolean = True; AStartAt: Integer = 0): Integer;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

  TOrderedList = class(TObject)
  private
    FList: TList;
  protected
    procedure PushItem(AItem: TObject); virtual; abstract;
    function PopItem: TObject; virtual;
    function PeekItem: TObject; virtual;
    property List: TList read FList;
  public
    constructor Create;
    destructor Destroy; override;

    function Count: Integer;
    function AtLeast(ACount: Integer): Boolean;
    function Push(AItem: TObject): TObject;
    function Pop: TObject;
    function Peek: TObject;
  end;

  TStack = class(TOrderedList)
  protected
    procedure PushItem(AItem: TObject); override;
  public
    procedure Clear;
  end;

  TQueue = class(TOrderedList)
  protected
    procedure PushItem(AItem: TObject); override;
  end;

  TPersistent = System.MarshalByRefObject;

  TPersistentHelper = class helper (TObjectHelper) for TPersistent
  private
    procedure AssignError(Source: TPersistent);
  protected
    procedure AssignTo(Dest: TPersistent); virtual;
    function GetOwner: TPersistent; virtual;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); virtual;
    function GetNamePath: string; virtual;
  end;

{ TStrings class }

  TStrings = class;
  TStream = class;

  EStringListError = class(Exception);

  TStringsEnumerator = class
  private
    FIndex: Integer;
    FStrings: TStrings;
  public
    constructor Create(AStrings: TStrings);
    function GetCurrent: string;
    function MoveNext: Boolean;
    property Current: string read GetCurrent;
  end;

  TStrings = class(TPersistent)
  public
    type
      TStringsDefined = set of (sdDelimiter, sdQuoteChar, sdNameValueSeparator,
        sdLineBreak, sdStrictDelimiter);
      
  private
    FDefined: TStringsDefined;
    FLineBreak: string;
    FNameValueSeparator: Char;
    FStrictDelimiter: Boolean;
    FUpdateCount: Integer;

    function GetName(Index: Integer): string;
    function GetValue(const Name: string): string;
    procedure SetValue(const Name, Value: string);
    function GetLineBreak: string;
    procedure SetLineBreak(const Value: string);
    function GetNameValueSeparator: Char;
    procedure SetNameValueSeparator(const Value: Char);
    function GetStrictDelimiter: Boolean;
    procedure SetStrictDelimiter(const Value: Boolean);
    function GetValueFromIndex(Index: Integer): string;
    procedure SetValueFromIndex(Index: Integer; const Value: string);
  protected
    procedure Error(const Msg: string; Data: Integer); overload;
    //procedure Error(Msg: PResStringRec; Data: Integer); overload;
    function ExtractName(const S: string): string;
    function Get(Index: Integer): string; virtual; abstract;
    function GetCapacity: Integer; virtual;
    function GetCount: Integer; virtual; abstract;
    function GetObject(Index: Integer): TObject; virtual;
    function GetTextStr: string; virtual;
    procedure Put(Index: Integer; const S: string); virtual;
    procedure PutObject(Index: Integer; AObject: TObject); virtual;
    procedure SetCapacity(NewCapacity: Integer); virtual;
    procedure SetTextStr(const Value: string); virtual;
    procedure SetUpdateState(Updating: Boolean); virtual;
    property UpdateCount: Integer read FUpdateCount;
    function CompareStrings(const S1, S2: string): Integer; virtual;
  public
    function Add(const S: string): Integer; virtual;
    function AddObject(const S: string; AObject: TObject): Integer; virtual;

    procedure Append(const S: string);
    procedure AddStrings(Strings: TStrings); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure Clear; virtual; abstract;
    procedure Delete(Index: Integer); virtual; abstract;
    procedure EndUpdate;
    function Equals(Strings: TStrings): Boolean;
    procedure Exchange(Index1, Index2: Integer); virtual;
    function GetEnumerator: TStringsEnumerator;
    function IndexOf(const S: string): Integer; virtual;
    function IndexOfName(const Name: string): Integer; virtual;
    function IndexOfObject(AObject: TObject): Integer; virtual;

    procedure Insert(Index: Integer; const S: string); virtual; abstract;
    procedure InsertObject(Index: Integer; const S: string; AObject: TObject); virtual;
    procedure LoadFromFile(const FileName: string); overload; virtual;
    procedure LoadFromFile(const FileName: string; Encoding: System.Text.Encoding); overload; virtual;
    procedure LoadFromStream(Stream: TStream); overload; virtual;
    procedure LoadFromStream(Stream: TStream; Encoding: System.Text.Encoding); overload; virtual;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    procedure SaveToFile(const FileName: string); overload; virtual;
    procedure SaveToFile(const FileName: string; Encoding: System.Text.Encoding); overload; virtual;
    procedure SaveToStream(Stream: TStream); overload; virtual;
    procedure SaveToStream(Stream: TStream; Encoding: System.Text.Encoding); overload; virtual;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;
    property LineBreak: string read GetLineBreak write SetLineBreak;
    property Names[Index: Integer]: string read GetName;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property Values[const Name: string]: string read GetValue write SetValue;
    property ValueFromIndex[Index: Integer]: string read GetValueFromIndex write SetValueFromIndex;
    property NameValueSeparator: Char read GetNameValueSeparator write SetNameValueSeparator;
    property StrictDelimiter: Boolean read GetStrictDelimiter write SetStrictDelimiter;
    property Strings[Index: Integer]: string read Get write Put; default;
    property Text: string read GetTextStr write SetTextStr;
  end;

{ TStringList class }

  TStringList = class;

  TStringListSortCompare = function(List: TStringList; Index1, Index2: Integer): Integer;
  TNotifyEvent = procedure(Sender: TObject) of object;

  TStringList = class(TStrings)
  public
    type
      TDuplicates = (dupIgnore, dupAccept, dupError);

  private
    type
      TStringItem = record
        FString: string;
        FObject: TObject;
      end;

    const
      SSortedListError = 'Operation not allowed on sorted list';
      SDuplicateString = 'String list does not allow duplicates';

  private
    FList: array of TStringItem;
    FCount: Integer;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    FCaseSensitive: Boolean;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;

    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure QuickSort(L, R: Integer; SCompare: TStringListSortCompare);
    procedure SetSorted(Value: Boolean);
    procedure SetCaseSensitive(const Value: Boolean);
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: Integer): string; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetUpdateState(Updating: Boolean); override;
    function CompareStrings(const S1, S2: string): Integer; override;
    procedure InsertItem(Index: Integer; const S: string; AObject: TObject); virtual;
  public
    function Add(const S: string): Integer; override;
    function AddObject(const S: string; AObject: TObject): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    function Find(const S: string; var Index: Integer): Boolean; virtual;
    function IndexOf(const S: string): Integer; override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure InsertObject(Index: Integer; const S: string;
      AObject: TObject); override;
    procedure Sort; virtual;
    procedure CustomSort(Compare: TStringListSortCompare); virtual;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;

{==============================================================================}

  EStreamError = class(Exception);
  EFilerError = class(EStreamError);
  EReadError = class(EFilerError);
  EWriteError = class(EFilerError);
  EInvalidImage = class(EFilerError);

  TStream = class(TObject)
  private
  const
    SReadError = 'Stream read error';
    SWriteError = 'Stream write error';

    function GetPosition: Int64;
    procedure SetPosition(const Pos: Int64);
    function GetSize: Int64;
    function Skip(Amount: Integer): Integer;
  protected
    procedure SetSize(NewSize: Int64); overload; virtual; abstract;
  public
    function Read(var Buffer: array of Byte; Offset, Count: Longint): Longint; overload; virtual; abstract;
    function Read(var Buffer: array of Byte; Count: Longint): Longint; overload;
    function Read(var Buffer: Byte): Longint; overload;
    function Read(var Buffer: Byte; Count: Longint): Longint; overload; platform;
    function Read(var Buffer: Boolean): Longint; overload;
    function Read(var Buffer: Boolean; Count: Longint): Longint; overload; platform;
    function Read(var Buffer: Char): Longint; overload;
    function Read(var Buffer: Char; Count: Longint): Longint; overload; platform;
    function Read(var Buffer: ShortInt): Longint; overload;
    function Read(var Buffer: ShortInt; Count: Longint): Longint; overload; platform;
    function Read(var Buffer: SmallInt): Longint; overload;
    function Read(var Buffer: SmallInt; Count: Longint): Longint; overload; platform;
    function Read(var Buffer: Word): Longint; overload;
    function Read(var Buffer: Word; Count: Longint): Longint; overload; platform;
    function Read(var Buffer: Integer): Longint; overload;
    function Read(var Buffer: Integer; Count: Longint): Longint; overload; platform;
    function Read(var Buffer: Cardinal): Longint; overload;
    function Read(var Buffer: Cardinal; Count: Longint): Longint; overload; platform;
    function Read(var Buffer: Int64): Longint; overload;
    function Read(var Buffer: Int64; Count: Longint): Longint; overload; platform;
    function Read(var Buffer: UInt64): Longint; overload;
    function Read(var Buffer: UInt64; Count: Longint): Longint; overload; platform;
    function Read(var Buffer: Single): Longint; overload;
    function Read(var Buffer: Single; Count: Longint): Longint; overload; platform;
    function Read(var Buffer: Double): Longint; overload;
    function Read(var Buffer: Double; Count: Longint): Longint; overload; platform;

    function Write(const Buffer: array of Byte; Offset, Count: Longint): Longint; overload; virtual; abstract;
    function Write(const Buffer: array of Byte; Count: Longint): Longint; overload;
    function Write(const Buffer: Byte): Longint; overload;
    function Write(const Buffer: Byte; Count: Longint): Longint; overload; platform;
    function Write(const Buffer: Boolean): Longint; overload;
    function Write(const Buffer: Boolean; Count: Longint): Longint; overload; platform;
    function Write(const Buffer: Char): Longint; overload;
    function Write(const Buffer: Char; Count: Longint): Longint; overload; platform;
    function Write(const Buffer: ShortInt): Longint; overload;
    function Write(const Buffer: ShortInt; Count: Longint): Longint; overload; platform;
    function Write(const Buffer: SmallInt): Longint; overload;
    function Write(const Buffer: SmallInt; Count: Longint): Longint; overload; platform;
    function Write(const Buffer: Word): Longint; overload;
    function Write(const Buffer: Word; Count: Longint): Longint; overload; platform;
    function Write(const Buffer: Integer): Longint; overload;
    function Write(const Buffer: Integer; Count: Longint): Longint; overload; platform;
    function Write(const Buffer: Cardinal): Longint; overload;
    function Write(const Buffer: Cardinal; Count: Longint): Longint; overload; platform;
    function Write(const Buffer: Int64): Longint; overload;
    function Write(const Buffer: Int64; Count: Longint): Longint; overload; platform;
    function Write(const Buffer: UInt64): Longint; overload;
    function Write(const Buffer: UInt64; Count: Longint): Longint; overload; platform;
    function Write(const Buffer: Single): Longint; overload;
    function Write(const Buffer: Single; Count: Longint): Longint; overload; platform;
    function Write(const Buffer: Double): Longint; overload;
    function Write(const Buffer: Double; Count: Longint): Longint; overload; platform;

    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; virtual; abstract;

    procedure ReadBuffer(Buffer: TBytes; Count: Longint); overload;
    procedure ReadBuffer(var Buffer: Byte); overload;
    procedure ReadBuffer(var Buffer: Byte; Count: Longint); overload; platform;
    procedure ReadBuffer(var Buffer: Boolean); overload;
    procedure ReadBuffer(var Buffer: Boolean; Count: Longint); overload; platform;
    procedure ReadBuffer(var Buffer: Char); overload;
    procedure ReadBuffer(var Buffer: Char; Count: Longint); overload; platform;
    procedure ReadBuffer(var Buffer: ShortInt); overload;
    procedure ReadBuffer(var Buffer: ShortInt; Count: Longint); overload; platform;
    procedure ReadBuffer(var Buffer: SmallInt); overload;
    procedure ReadBuffer(var Buffer: SmallInt; Count: Longint); overload; platform;
    procedure ReadBuffer(var Buffer: Word); overload;
    procedure ReadBuffer(var Buffer: Word; Count: Longint); overload; platform;
    procedure ReadBuffer(var Buffer: Integer); overload;
    procedure ReadBuffer(var Buffer: Integer; Count: Longint); overload; platform;
    procedure ReadBuffer(var Buffer: Cardinal); overload;
    procedure ReadBuffer(var Buffer: Cardinal; Count: Longint); overload; platform;
    procedure ReadBuffer(var Buffer: Int64); overload;
    procedure ReadBuffer(var Buffer: Int64; Count: Longint); overload; platform;
    procedure ReadBuffer(var Buffer: UInt64); overload;
    procedure ReadBuffer(var Buffer: UInt64; Count: Longint); overload; platform;
    procedure ReadBuffer(var Buffer: Single); overload;
    procedure ReadBuffer(var Buffer: Single; Count: Longint); overload; platform;
    procedure ReadBuffer(var Buffer: Double); overload;
    procedure ReadBuffer(var Buffer: Double; Count: Longint); overload; platform;

    procedure WriteBuffer(const Buffer: TBytes; Count: Longint); overload;
    procedure WriteBuffer(const Buffer: Byte); overload;
    procedure WriteBuffer(const Buffer: Byte; Count: Longint); overload; platform;
    procedure WriteBuffer(const Buffer: Boolean); overload;
    procedure WriteBuffer(const Buffer: Boolean; Count: Longint); overload; platform;
    procedure WriteBuffer(const Buffer: Char); overload;
    procedure WriteBuffer(const Buffer: Char; Count: Longint); overload; platform;
    procedure WriteBuffer(const Buffer: ShortInt); overload;
    procedure WriteBuffer(const Buffer: ShortInt; Count: Longint); overload; platform;
    procedure WriteBuffer(const Buffer: SmallInt); overload;
    procedure WriteBuffer(const Buffer: SmallInt; Count: Longint); overload; platform;
    procedure WriteBuffer(const Buffer: Word); overload;
    procedure WriteBuffer(const Buffer: Word; Count: Longint); overload; platform;
    procedure WriteBuffer(const Buffer: Integer); overload;
    procedure WriteBuffer(const Buffer: Integer; Count: Longint); overload; platform;
    procedure WriteBuffer(const Buffer: Cardinal); overload;
    procedure WriteBuffer(const Buffer: Cardinal; Count: Longint); overload; platform;
    procedure WriteBuffer(const Buffer: Int64); overload;
    procedure WriteBuffer(const Buffer: Int64; Count: Integer); overload; platform;
    procedure WriteBuffer(const Buffer: UInt64); overload;
    procedure WriteBuffer(const Buffer: UInt64; Count: Integer); overload; platform;
    procedure WriteBuffer(const Buffer: Single); overload;
    procedure WriteBuffer(const Buffer: Single; Count: Integer); overload; platform;
    procedure WriteBuffer(const Buffer: Double); overload;
    procedure WriteBuffer(const Buffer: Double; Count: Integer); overload; platform;

    function CopyFrom(Source: TStream; Count: Int64): Int64;

    class operator Implicit(const Value: TStream): System.IO.Stream;
    class operator Implicit(const Value: System.IO.Stream): TStream;

    property Position: Int64 read GetPosition write SetPosition;
    property Size: Int64 read GetSize write SetSize;
  end;

  TCLRStreamWrapper = class(TStream)
  protected
    FHandle: System.IO.Stream;
    procedure SetSize(NewSize: Int64); override;
  public
    constructor Create(AHandle: System.IO.Stream);
    destructor Destroy; override;

    function Read(var Buffer: array of Byte; Offset, Count: Longint): Longint; override;
    function Write(const Buffer: array of Byte; Offset, Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    property Handle: System.IO.Stream read FHandle;
  end;

  TCustomMemoryStream = class(TStream)
  private
    FSize, FPosition: Int64;
  protected
    FMemory: TBytes;
  public
    function Read(var Buffer: array of Byte; Offset, Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    property Memory: TBytes read FMemory;
  end;

  TMemoryStream = class(TCustomMemoryStream)
  private
    function GetCapacity: Longint;
    procedure SetCapacity(NewCapacity: Longint);
  protected
    function Realloc(var NewCapacity: Longint): TBytes; virtual;
    procedure SetSize(NewSize: Int64); override;

    property Capacity: Longint read GetCapacity write SetCapacity;
  public
    procedure Clear;
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(const FileName: string);
    function Write(const Buffer: array of Byte; Offset, Count: Longint): Longint; override;
  end;

  TFileStream = class(TCLRStreamWrapper)
  strict private
    FFileName: string;
  public
    constructor Create(const AFileName: string; Mode: Word); overload;
    constructor Create(const AFileName: string; Mode: Word; Rights: Cardinal); overload;
    property FileName: string read FFileName;
  end;

  EResNotFound = class(Exception);

  [SecurityPermission(SecurityAction.LinkDemand, UnmanagedCode=True)]
  TResourceStream = class(TCustomMemoryStream)
  private
  const
    SResNotFound = 'Resource %s not found';
    SCantWriteResourceStreamError = 'Can''t write to a read-only resource stream';

  var
    HResInfo: THandle;
    HGlobal: THandle;

    procedure Initialize(Instance, ResInfo: THandle; const Name: string);
  protected
    procedure SetSize(NewSize: Int64); override;
  public
    constructor Create(Instance: THandle; const ResName: string; ResType: Integer); overload;
    constructor Create(Instance: THandle; const ResName, ResType: string); overload;
    constructor CreateFromID(Instance: THandle; ResID, ResType: Integer); overload;
    constructor CreateFromID(Instance: THandle; ResID: Integer; ResType: string); overload;
    function Write(const Buffer: array of Byte; Offset, Count: Longint): Longint; override;
  end;

  TStreamToCLRStream = class(System.IO.Stream)
  protected
    FStream: TStream;
    constructor Create(Stream: TStream);
  public
    { overridden methods of System.IO.Stream }
    procedure Close; override;
    procedure Flush; override;
    function get_CanRead: Boolean; override;
    function get_CanSeek: Boolean; override;
    function get_CanWrite: Boolean; override;
    function get_Length: Int64; override;
    function get_Position: Int64; override;
    function Read(Buffer: TBytes; Offset: Integer; Count: Integer): Integer; override;
    function Seek(Offset: Int64; Origin: System.IO.SeekOrigin): Int64; override;
    procedure SetLength(Value: Int64); override;
    procedure set_Position(Value: Int64); override;
    procedure Write(Buffer: TBytes; Offset: Integer; Count: Integer); override;
    property CanRead: Boolean read get_CanRead;
    property CanSeek: Boolean read get_CanSeek;
    property CanWrite: Boolean read get_CanWrite;
    property Length: Int64 read get_Length;
    property Position: Int64 read get_Position write set_Position;
  public
    destructor Destroy; override;

    class function GetStream(Stream: TStream): System.IO.Stream; static;
  end;

{==============================================================================}

type
  EIniFileException = class(Exception);

  [SecurityPermission(SecurityAction.LinkDemand, UnmanagedCode=True)]
  [FileIOPermission(SecurityAction.LinkDemand, Unrestricted=True)]
  TIniFile = class(TObject)
  private
  const
    SIniFileWriteError = 'Unable to write to {0}';

  var
    FFileName: string;
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;

    function ReadInteger(const Section, Ident: string; Default: Longint): Longint;
    procedure WriteInteger(const Section, Ident: string; Value: Longint);

    function ReadBool(const Section, Ident: string; Default: Boolean): Boolean;
    procedure WriteBool(const Section, Ident: string; Value: Boolean);

    function ReadBinaryStream(const Section, Name: string; Value: TStream): Integer;
    procedure WriteBinaryStream(const Section, Name: string; Value: TStream);

    function ReadDateTime(const Section, Name: string; Default: DateTime): DateTime;
    procedure WriteDateTime(const Section, Name: string; Value: DateTime);

    function ReadFloat(const Section, Name: string; Default: Double): Double;
    procedure WriteFloat(const Section, Name: string; Value: Double);

    function SectionExists(const Section: string): Boolean;
    procedure ReadSections(const Section: string; Strings: TStrings); overload;
    function ValueExists(const Section, Ident: string): Boolean;

    function ReadString(const Section, Ident, Default: string): string;
    procedure WriteString(const Section, Ident, Value: string);

    procedure ReadSection(const Section: string; Strings: TStrings);
    procedure ReadSections(Strings: TStrings); overload;
    procedure ReadSectionValues(const Section: string; Strings: TStrings);
    procedure EraseSection(const Section: string);
    procedure DeleteKey(const Section, Ident: string);
    procedure UpdateFile;

    property FileName: string read FFileName;
  end;

{==============================================================================}

function StrArrayToStrings(StrArr: array of string): TStrings;

procedure FreeAndNil(var Obj);

function CreateDir(const Dir: string): Boolean;

function Floor(const X: Double): Integer;

function Ceil(const X: Double): Integer;

function HInstance: HINST;

type
  TInterfaceRef = type of interface;

function Supports(const Instance: TObject; const IID: TInterfaceRef; out Intf): Boolean;

function PosEx(const SubStr, S: string; Offset: Integer = 1): Integer;

function CompareText(const S1, S2: string): Integer; 

function CompareStr(const S1, S2: string): Integer;

function SameText(const S1, S2: string): Boolean;

function StrToIntDef(const S: string; Default: Integer): Integer;

function TrimLeft(const S: string): string;
function TrimRight(const S: string): string;

function ExtractFilePath(const FileName: string): string;

type
  TReplaceFlags = set of (rfReplaceAll, rfIgnoreCase);

function StringReplace(const S, OldPattern, NewPattern: string;
  Flags: TReplaceFlags): string;

type
  TSearchRec = record
    Attr: Integer;
    Name: string;
    ExcludeAttr: Integer;
    FindHandle: THandle;
    FindData: TWIN32FindData;
  end;

{ FindDelimiter returns the index in S of the leftmost whole
  character that matches any character in Delimiters (except null (#0)).
  The search starts at the character position specified by Offset,
  or at the beginning of the string if not specified (Offset = 1).
  Example: FindDelimiter('\.:', 'c:\filename.ext') returns 2. }

function FindDelimiter(const Delimiters, S: string; Offset: Integer): Integer;

function MatchesMask(const Filename, Mask: string): Boolean;

procedure StrDelete(var Dest: string; Index, Count: Integer); inline;
function StrCopy(const S: string; Index, Count: Integer): string; inline;

implementation

const
  SAssignError = 'Cannot assign a %s to a %s';

function StrArrayToStrings(StrArr: array of string): TStrings;
var
  i: Integer;
begin
  Result := TStringList.Create;
  for i := 0 to High(StrArr) do Result.Add(StrArr[i]);
end;

procedure FreeAndNil(var Obj);
begin
  TObject(Obj).Free;
  Obj := nil;
end;

function CreateDir(const Dir: string): Boolean;
var
  LInfo: System.IO.DirectoryInfo;
begin
  LInfo := System.IO.DirectoryInfo.Create(Dir);
  LInfo := LInfo.Parent;
  Result := (LInfo <> nil) and LInfo.Exists;
  if Result then
  begin
    LInfo := System.IO.Directory.CreateDirectory(Dir);
    Result := (LInfo <> nil) and LInfo.Exists;
  end;
end;

function Floor(const X: Double): Integer;
begin
  Result := System.Convert.ToInt32(System.Math.Floor(X));
end;

function Ceil(const X: Double): Integer;
begin
  Result := System.Convert.ToInt32(System.Math.Ceiling(X));
end;

function HInstance: HINST;
begin
  Result := HINST(Marshal.GetHInstance(Assembly.GetCallingAssembly.GetModules[0]));
end;

function Supports(const Instance: TObject; const IID: TInterfaceRef; out Intf): Boolean;
begin
  Result := Instance is IID;
  if Result then
    Intf := Instance as IID;
end;

function PosEx(const SubStr, S: string; Offset: Integer = 1): Integer;
begin
  if (Offset <= 0) or (S = nil) or (OffSet > Length(S)) then
    Result := 0
  else
  // CLR strings are zero relative
    Result := System.String(S).IndexOf(SubStr, Offset - 1) + 1;
end;

function CompareText(const S1, S2: string): Integer;
begin
  Result := System.String.Compare(S1, S2, True);
end;

function CompareStr(const S1, S2: string): Integer;
begin
  Result := System.String.Compare(S1, S2, False);
end;

function SameText(const S1, S2: string): Boolean;
begin
  Result := CompareText(S1, S2) = 0;
end;

function StrToIntDef(const S: string; Default: Integer): Integer;
var
  E: Integer;
begin
  Val(S, Result, E);
  if not(E = 0) then Result := Default;
end;

function TrimLeft(const S: string): string;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  if I > L then Result := ''
  else if I <> 1 then Result := Copy(S, I, Maxint)
  else Result := S;
end;

function TrimRight(const S: string): string;
var
  I, L: Integer;
begin
  L := Length(S);
  I := L;
  while (I > 0) and (S[I] <= ' ') do Dec(I);
  if I <> L then Result := Copy(S, 1, I)
  else Result := S;
end;

type
  TCharArray = array of Char;

function CharArrayOf(const AText: string): TCharArray;
begin
  if (AText <> nil)
  then Result := System.String(AText).ToCharArray
  else SetLength(Result, 0);
end;

function LastDelimiter(const Delimiters, S: string): Integer;
begin
  if S <> nil then
    Result := System.String(S).LastIndexOfAny(CharArrayOf(Delimiters)) + 1
  else
    Result := 0;
end;

function ExtractFilePath(const FileName: string): string;
var
  I: Integer;
begin
  I := LastDelimiter(System.IO.Path.DirectorySeparatorChar
                   + System.IO.Path.VolumeSeparatorChar, FileName);
  Result := Copy(FileName, 1, I);
end;

function StringReplace(const S, OldPattern, NewPattern: string;
  Flags: TReplaceFlags): string;
var
  SearchStr, Patt, NewStr: string;
  Offset: Integer;
  SB: StringBuilder;
begin
  if rfIgnoreCase in Flags then
  begin
    SearchStr := S.ToUpper;
    Patt := OldPattern.ToUpper;
  end else
  begin
    SearchStr := S;
    Patt := OldPattern;
  end;
  NewStr := S;
  SB := StringBuilder.Create;
  while SearchStr <> '' do
  begin
    Offset := Pos(Patt, SearchStr);
    if Offset = 0 then
    begin
      SB.Append(NewStr);
      Break;
    end;
    SB.Append(NewStr, 0, Offset - 1);
    SB.Append(NewPattern);
    NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);
    if not (rfReplaceAll in Flags) then
    begin
      SB.Append(NewStr);
      Break;
    end;
    SearchStr := Copy(SearchStr, Offset + Length(Patt), MaxInt);
  end;
  Result := SB.ToString;
end;

function MakeLong(A, B: Word): Longint; inline;
begin
  Result := A or B shl 16;
end;

const
  ParseBufSize = 4096;
  B2HConvert: array[0..15] of Byte = ($30, $31, $32, $33, $34, $35, $36, $37, $38, $39, $41, $42, $43, $44, $45, $46);

procedure BinToHex(const Buffer: array of Byte; BufOffset: Integer;
  var Text: array of Byte; TextOffset: Integer; Count: Integer);
var
  I: Integer;
begin
  for I := 0  to Count - 1 do
  begin
    Text[TextOffset + I * 2] := B2HConvert[Buffer[BufOffset + I] shr 4];
    Text[TextOffset + 1 + I * 2] := B2HConvert[Buffer[BufOffset + I] and $F];
  end;
end;

const
  H2BConvert: array['0'..'f'] of SmallInt =
    ( 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15);

function HexToBin(Text: TBytes; TextOffset: Integer;
  Buffer: TBytes; BufOffset: Integer; Count: Integer): Integer;
var
  I, C: Integer;
begin
  C := 0;
  for I := 0 to Count - 1 do begin
    if not (AnsiChar(Text[TextOffset + I * 2]) in [AnsiChar('0')..AnsiChar('f')]) or
       not (AnsiChar(Text[TextOffset + 1 + I * 2]) in [AnsiChar('0')..AnsiChar('f')]) then
      Break;
    Buffer[BufOffset + I] :=
      (H2BConvert[AnsiChar(Text[TextOffset + I * 2])] shl 4) or
       H2BConvert[AnsiChar(Text[TextOffset + 1 + I * 2])];
    Inc(C);
  end;
  Result := C;
end;

{==============================================================================}

function FindDelimiter(const Delimiters, S: string; Offset: Integer): Integer;
begin
  if (S <> nil) then begin
    if (Offset < 1) then Offset := 1;
    Result := System.String(S).IndexOfAny(CharArrayOf(Delimiters), Offset - 1) + 1;
  end else Result := 0;
end;

const
  SInvalidMask = '''{0}'' is an invalid mask at (%d)';

type
  EMaskException = class(Exception);

procedure InvalidMask(const Mask: string);
begin
  raise EMaskException.Create(System.&String.Format(SInvalidMask, [Mask]));
end;

function ConvertMaskToRegularExpression(const Mask: string): string;
var
  I, CurPos, Len: Integer;

  procedure CheckPos(I, Len: Integer; const Mask: string);
  begin
    if I = Len - 1 then
      InvalidMask(Mask);
  end;

begin
  Result := '';
  CurPos := 0;
  Len := Length(Mask);
  while (CurPos < Len) do begin
    I := FindDelimiter('*?[', Mask, CurPos + 1) - 1; // Do not localize
    if (I >= CurPos) then begin
      if (I > CurPos) then
        Result := Result +
          System.Text.RegularExpressions.RegEx.Escape(Copy(Mask, CurPos + 1, I - CurPos));
      case Mask[I + 1] of         // add one because pascal strings are 1-offset
        '?' : Result := Result + '.';
        '*' : Result := Result + '.*';
        '[' :
          begin
            CheckPos(I, Len, Mask);
            if Mask[I + 2] = '!' then
            begin
              Result := Result + '[^';
              Inc(I);
              CheckPos(I, Len, Mask);
            end
            else
              Result := Result + '[';
            CurPos := I + 1;
            while Mask[I + 1] <> ']' do
            begin
              I := FindDelimiter(']-', Mask, CurPos + 1) - 1; // Do not localize
              if I < 0 then
                InvalidMask(Mask);
              Result := Result +
                System.Text.RegularExpressions.RegEx.Escape(Copy(Mask, CurPos + 1, I - CurPos));
              if Mask[I + 1] = '-' then
              begin
                CheckPos(I, Len, Mask);
                Result := Result + '-';
                CurPos := I + 1;
              end;
            end;
            Result := Result + ']';
          end;
      end;
      CurPos := I + 1;
    end
    else Break;
  end;
  if (CurPos < Len) then
    Result := Result +
      System.Text.RegularExpressions.RegEx.Escape(Copy(Mask, CurPos + 1, Len - CurPos));
end;

function MatchesMask(const Filename, Mask: string): Boolean;
var
  FMask: System.Text.RegularExpressions.RegEx;
  Match: System.Text.RegularExpressions.Match;
  Groups: System.Text.RegularExpressions.GroupCollection;
  Group: System.Text.RegularExpressions.Group;
  Capture: System.Text.RegularExpressions.Capture;
  I, J: Integer;
begin
  FMask := System.Text.RegularExpressions.RegEx.Create(
    ConvertMaskToRegularExpression(Mask),
    System.Text.RegularExpressions.RegexOptions.IgnoreCase);
  try
    Result := False;
    Match := FMask.Match(FileName);
    Groups := Match.Groups;

    for I := 0 to Groups.Count - 1 do begin
      Group := Groups.get_Item(I);
      if Group.Success then begin
        for J := 0 to Group.Captures.Count - 1 do begin
          Capture := Group.Captures.Item[J];
          if SameText(Capture.Value, Filename) then begin
            Result := True;
            Exit;
          end;
        end;
      end;
    end;
  finally
    FMask.Free; // Finalize the mask
  end;
end;

procedure StrDelete(var Dest: string; Index, Count: Integer); inline;
begin
  Dest := Dest.Remove(Index - 1, Count);
end;

function StrCopy(const S: string; Index, Count: Integer): string; inline;
begin
  Result := S.Substring(Index - 1, Count);
end;

{==============================================================================}

{ TEnumSet }

class function TEnumSet.Create(): TEnumSet;
begin
  Result.FValue := 0;
end;

class function TEnumSet.Create(e: array of Enum): TEnumSet;
begin
  Result := TEnumSet.Create;
  Result.Include(e);
end;

function TEnumSet.ToString(B: Byte): string;
var
  i: Integer;
  bt: Byte;
  s: string;
begin
  bt := $01;
  s := '';
  for i := 1 to 8 do begin
    if (b and bt) > 0
    then s := '1' + s
    else s := '0' + s;
    bt := (bt shl 1);
  end;
  Result := s;
end;

procedure TEnumSet.Include(e: array of Enum);
var
  i: Integer;
begin
  for i := 0 to High(e) do Include(e[i]);    
end;

procedure TEnumSet.Include(e: Enum);
var
  pos: Byte;
begin
  pos := Byte(e);
  FValue := FValue or ($01 shl pos);
end;

procedure TEnumSet.Exclude(e: Enum);
var
  pos: Byte;
begin
  pos := Byte(e);
  FValue := FValue and (($01 shl pos) xor $FFFFFFFF);
end;

function TEnumSet.InSet(e: Enum): Boolean;
var
  pos: Byte;
  bt: Cardinal;
begin
  pos := Byte(e);
  bt := $01 shl pos;
  Result := (bt and FValue) > 0;
end;

function TEnumSet.IsEmpty(): Boolean;
begin
  Result := (FValue = 0);
end;

{ TPoint }

class function TPoint.Create(const AX, AY: LongInt): TPoint;
begin
  Result.X := AX;
  Result.Y := AY;
end;

class function TPoint.Empty: TPoint;
begin
  Result := Create(0, 0);
end;

function TPoint.ToString: string;
begin
  Result := '{X=' + X.ToString + // DO NOT LOCALIZE
            ',Y=' + Y.ToString + '}'; // DO NOT LOCALIZE
end;

function TPoint.Left: LongInt;
begin
  Result := X;
end;

function TPoint.Top: LongInt;
begin
  Result := Y;
end;

function TPoint.IsEmpty: Boolean;
begin
  Result := (X = 0) and (Y = 0);
end;

function TPoint.Equals(const AX, AY: LongInt): Boolean;
begin
  Result := (X = AX) and (Y = AY);
end;

function TPoint.Equals(const Value: TPoint): Boolean;
begin
  Result := (X = Value.X) and (Y = Value.Y);
end;

{ TRect }

class function TRect.Create(const ALeft, ATop, ARight, ABottom: LongInt): TRect;
begin
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Right := ARight;
  Result.Bottom := ABottom;
end;

class function TRect.Bounds(const ALeft, ATop, AWidth, AHeight: LongInt): TRect;
begin
  Result := TRect.Create(ALeft, ATop, ALeft + AWidth, ATop + AHeight);
end;

class function TRect.Empty: TRect;
begin
  Result := TRect.Create(0, 0, 0, 0);
end;

function TRect.ToString: string;
begin
  Result := '{X=' + Left.ToString + // DO NOT LOCALIZE
            ',Y=' + Top.ToString + // DO NOT LOCALIZE
            ',Width=' + GetWidth.ToString + // DO NOT LOCALIZE
            ',Height=' + GetHeight.ToString + '}'; // DO NOT LOCALIZE
end;

function TRect.ToRectangle(): System.Drawing.Rectangle;
begin
  Result := System.Drawing.Rectangle.Create(
    Left, Top, (Right - Left) + 1, (Bottom - Top) + 1);
end;

function TRect.GetWidth: LongInt;
begin
  Result := Right - Left;
end;

function TRect.GetHeight: LongInt;
begin
  Result := Bottom - Top;
end;

function TRect.IsEmpty: Boolean;
begin
  Result := (Right <= Left) or
            (Bottom <= Top);
end;

function TRect.Contains(const X, Y: LongInt): Boolean;
begin
  Result := (X >= Left) and
            (Y >= Top) and
            (X < Right) and
            (Y < Bottom);
end;

function TRect.Contains(const Value: TPoint): Boolean;
begin
  Result := Contains(Value.X, Value.Y);
end;

function TRect.GetOffset(const X, Y: LongInt): TRect;
begin
  Result := TRect.Create(Left + X, Top + Y, Right + X, Bottom + Y);
end;

function TRect.GetOffset(const Value: TPoint): TRect;
begin
  Result := GetOffset(Value.X, Value.Y);
end;

{==============================================================================}

{ TListComparer }

type
  TListComparer = class(TObject, IComparer)
  private
    FCompare: TListSortCompare;
  public
    function Compare(O1, O2: TObject): Integer;
    constructor Create(Compare: TListSortCompare);
  end;

{ TListComparer }

function TListComparer.Compare(O1, O2: TObject): Integer;
begin
  Result := FCompare(O1, O2);
end;

constructor TListComparer.Create(Compare: TListSortCompare);
begin
  inherited Create;
  FCompare := Compare;
end;

{ TListEnumerator }

constructor TListEnumerator.Create(AList: TList);
begin
  inherited Create;
  FIndex := -1;
  FList := AList;
end;

function TListEnumerator.GetCurrent: TObject;
begin
  Result := FList[FIndex];
end;

function TListEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FList.Count - 1;
  if Result then
    Inc(FIndex);
end;

{ TList }

constructor TList.Create;
begin
  inherited Create;
  FList := System.Collections.ArrayList.Create;
end;

function TList.Add(Item: TObject): Integer;
begin
  Result := FList.Add(Item);
  if Item <> nil then
    Notify(Item, lnAdded);
end;

procedure TList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TList.Delete(Index: Integer);
var
  Temp: TObject;
begin
  Temp := FList[Index];
  FList.RemoveAt(Index);
  if Temp <> nil then
    Notify(Temp, lnDeleted);
end;

class procedure TList.Error(const Msg: string; Data: Integer);
begin
  raise EListError.Create(System.&String.Format(Msg, [Data]));
end;

procedure TList.Exchange(Index1, Index2: Integer);
var
  Item: TObject;
begin
  Item := FList[Index1];
  FList[Index1] := FList[Index2];
  FList[Index2] := Item;
end;

function TList.First: TObject;
begin
  Result := Get(0);
end;

function TList.Get(Index: Integer): TObject;
begin
  Result := FList[Index];
end;

function TList.GetCapacity: Integer;
begin
  Result := FList.Capacity;
end;

function TList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TList.GetEnumerator: TListEnumerator;
begin
  Result := TListEnumerator.Create(Self);
end;

procedure TList.Grow;
var
  Delta: Integer;
  LCapacity: Integer;
begin
  LCapacity := FList.Capacity;
  if LCapacity > 64 then
    Delta := LCapacity div 4
  else
    if LCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  SetCapacity(LCapacity + Delta);
end;

function TList.IndexOf(Item: TObject): Integer;
begin
  Result := FList.IndexOf(TObject(Item));
end;

procedure TList.Insert(Index: Integer; Item: TObject);
begin
  FList.Insert(Index, Item);
  if Item <> nil then
    Notify(Item, lnAdded);
end;

function TList.Last: TObject;
begin
  Result := Get(Count - 1);
end;

procedure TList.Move(CurIndex, NewIndex: Integer);
var
  Item: TObject;
begin
  if CurIndex <> NewIndex then
  begin
    if (NewIndex < 0) or (NewIndex >= Count) then
      Error(SListIndexError, NewIndex);
    Item := Get(CurIndex);
    FList.RemoveAt(CurIndex);
    FList.Insert(NewIndex, Item);
  end;
end;

procedure TList.Put(Index: Integer; Item: TObject);
var
  Temp: TObject;
begin
  if (Index < 0) or (Index >= Count) then
    Error(SListIndexError, Index);
  if Item <> FList[Index] then
  begin
    Temp := FList[Index];
    FList[Index] := Item;
    if Temp <> nil then
      Notify(Temp, lnDeleted);
    if Item <> nil then
      Notify(Item, lnAdded);
  end;
end;

function TList.Remove(Item: TObject): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end;

procedure TList.Pack;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Items[I] = nil then
      Delete(I);
end;

procedure TList.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity < Count then
    Error(SListCapacityError, NewCapacity);
  FList.Capacity := NewCapacity;
end;

procedure TList.SetCount(NewCount: Integer);
var
  I, C: Integer;
  TempArray: array of System.Object;
begin
  if NewCount < 0 then
    Error(SListCountError, NewCount);
  C := FList.Count;
  if NewCount > C then
  begin
    SetLength(TempArray, NewCount - C);
    FList.AddRange(System.Object(TempArray) as ICollection);
  end
  else
  begin
    SetLength(TempArray, C - NewCount);
    FList.CopyTo(NewCount, TempArray, 0, C - NewCount);
    FList.RemoveRange(NewCount, C - NewCount);
    for I := 0 to Length(TempArray) - 1 do
      Notify(TempArray[I], lnDeleted);
  end;
end;

procedure TList.Sort(Compare: TListSortCompare);
begin
  FList.Sort(TListComparer.Create(Compare));
end;

function TList.Extract(Item: TObject): TObject;
var
  I: Integer;
begin
  Result := nil;
  I := IndexOf(Item);
  if I >= 0 then
  begin
    Result := Item;
    FList.RemoveAt(I);
    Notify(Result, lnExtracted);
  end;
end;

procedure TList.Notify(Instance: TObject; Action: TListNotification);
begin
end;

procedure TList.Assign(ListA: TList; AOperator: TListAssignOp; ListB: TList);
var
  I: Integer;
  LTemp, LSource: TList;
begin
  // ListB given?
  if ListB <> nil then
  begin
    LSource := ListB;
    Assign(ListA);
  end
  else
    LSource := ListA;

  // on with the show
  case AOperator of

    // 12345, 346 = 346 : only those in the new list
    laCopy:
      begin
        Clear;
        Capacity := LSource.Capacity;
        for I := 0 to LSource.Count - 1 do
          Add(LSource[I]);
      end;

    // 12345, 346 = 34 : intersection of the two lists
    laAnd:
      for I := Count - 1 downto 0 do
        if LSource.IndexOf(Items[I]) = -1 then
          Delete(I);

    // 12345, 346 = 123456 : union of the two lists
    laOr:
      for I := 0 to LSource.Count - 1 do
        if IndexOf(LSource[I]) = -1 then
          Add(LSource[I]);

    // 12345, 346 = 1256 : only those not in both lists
    laXor:
      begin
        LTemp := TList.Create; // Temp holder of 4 byte values
        LTemp.Capacity := LSource.Count;
        for I := 0 to LSource.Count - 1 do
          if IndexOf(LSource[I]) = -1 then
            LTemp.Add(LSource[I]);
        for I := Count - 1 downto 0 do
          if LSource.IndexOf(Items[I]) <> -1 then
            Delete(I);
        I := Count + LTemp.Count;
        if Capacity < I then
          Capacity := I;
        for I := 0 to LTemp.Count - 1 do
          Add(LTemp[I]);
      end;

    // 12345, 346 = 125 : only those unique to source
    laSrcUnique:
      for I := Count - 1 downto 0 do
        if LSource.IndexOf(Items[I]) <> -1 then
          Delete(I);

    // 12345, 346 = 6 : only those unique to dest
    laDestUnique:
      begin
        LTemp := TList.Create;
        LTemp.Capacity := LSource.Count;
        for I := LSource.Count - 1 downto 0 do
          if IndexOf(LSource[I]) = -1 then
            LTemp.Add(LSource[I]);
        Assign(LTemp);
      end;
  end;
end;

{ TObjectList }

constructor TObjectList.Create;
begin
  inherited Create;
  FOwnsObjects := True;
end;

constructor TObjectList.Create(AOwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObjects := AOwnsObjects;
end;

function TObjectList.FindInstanceOf(AClass: TClass; AExact: Boolean;
  AStartAt: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := AStartAt to Count - 1 do
    if (AExact
    and (Items[I].ClassType = AClass)) or (not AExact and Items[I].InheritsFrom(AClass))
    then begin
      Result := I;
      break;
    end;
end;

procedure TObjectList.Notify(Instance: TObject; Action: TListNotification);
begin
  if OwnsObjects then
    if Action = lnDeleted then
      Instance.Free;
  inherited Notify(Instance, Action);
end;

{ TOrderedList }

function TOrderedList.AtLeast(ACount: integer): boolean;
begin
  Result := List.Count >= ACount;
end;

function TOrderedList.Peek: TObject;
begin
  Result := PeekItem;
end;

function TOrderedList.Pop: TObject;
begin
  Result := PopItem;
end;

function TOrderedList.Push(AItem: TObject): TObject;
begin
  PushItem(AItem);
  Result := AItem;
end;

function TOrderedList.Count: Integer;
begin
  Result := List.Count;
end;

constructor TOrderedList.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TOrderedList.Destroy;
begin
  List.Free;
  inherited Destroy;
end;

function TOrderedList.PeekItem: TObject;
begin
  Result := List[List.Count-1];
end;

function TOrderedList.PopItem: TObject;
begin
  Result := PeekItem;
  List.Delete(List.Count-1);
end;

{ TStack }

procedure TStack.PushItem(AItem: TObject);
begin
  List.Add(AItem);
end;

procedure TStack.Clear;
begin
  List.Clear;
end;

{ TQueue }

procedure TQueue.PushItem(AItem: TObject);
begin
  List.Insert(0, AItem);
end;

{==============================================================================}

{ TPersistentHelper }

procedure TPersistentHelper.AssignError(Source: TPersistent);
var
  SourceName: string;
begin
  if Source <> nil then
    SourceName := Source.ClassName
  else
    SourceName := 'nil';

  raise EConvertError.Create(System.&String.Format(SAssignError, [SourceName, ClassName]));
end;

procedure TPersistentHelper.Assign(Source: TPersistent);
begin
  if Source <> nil then
    Source.AssignTo(Self)
  else
    AssignError(nil);
end;

procedure TPersistentHelper.AssignTo(Dest: TPersistent);
begin
  Dest.AssignError(Self);
end;

constructor TPersistentHelper.Create;
begin
  inherited Create;
  // This doesn't need to do anything. It is just present to hoist
  // the MarshalByRef constructor public
end;

function TPersistentHelper.GetNamePath: string;
var
  S: string;
begin
  Result := ClassName;
  if GetOwner <> nil then
  begin
    S := GetOwner.GetNamePath;
    if S <> '' then
      Result := S + '.' + Result;
  end;
end;

function TPersistentHelper.GetOwner: TPersistent;
begin
  Result := nil;
end;

{ TStringsEnumerator }

constructor TStringsEnumerator.Create(AStrings: TStrings);
begin
  inherited Create;
  FIndex := -1;
  FStrings := AStrings;
end;

function TStringsEnumerator.GetCurrent: string;
begin
  Result := FStrings[FIndex];
end;

function TStringsEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FStrings.Count - 1;
  if Result then
    Inc(FIndex);
end;

{ TStrings }

function TStrings.Add(const S: string): Integer;
begin
  Result := GetCount;
  Insert(Result, S);
end;

function TStrings.AddObject(const S: string; AObject: TObject): Integer;
begin
  Result := Add(S);
  PutObject(Result, AObject);
end;

procedure TStrings.Append(const S: string);
begin
  Add(S);
end;

procedure TStrings.AddStrings(Strings: TStrings);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Strings.Count - 1 do
      AddObject(Strings[I], Strings.Objects[I]);
  finally
    EndUpdate;
  end;
end;

procedure TStrings.Assign(Source: TPersistent);
begin
  if Source is TStrings then
  begin
    BeginUpdate;
    try
      Clear;
      FDefined := TStrings(Source).FDefined;
      FNameValueSeparator := TStrings(Source).FNameValueSeparator;
      FLineBreak := TStrings(Source).FLineBreak;
      FStrictDelimiter := TStrings(Source).FStrictDelimiter;
      AddStrings(TStrings(Source));
    finally
      EndUpdate;
    end;
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TStrings.BeginUpdate;
begin
  if FUpdateCount = 0 then
    SetUpdateState(True);
  Inc(FUpdateCount);
end;

procedure TStrings.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    SetUpdateState(False);
end;

function TStrings.Equals(Strings: TStrings): Boolean;
var
  I, Count: Integer;
begin
  Result := False;
  Count := GetCount;
  if Count <> Strings.GetCount then
    Exit;
  for I := 0 to Count - 1 do if Get(I) <> Strings.Get(I) then
    Exit;
  Result := True;
end;

procedure TStrings.Error(const Msg: string; Data: Integer);
begin
  raise EStringListError.Create(System.&String.Format(Msg, [Data]));
end;

procedure TStrings.Exchange(Index1, Index2: Integer);
var
  TempObject: TObject;
  TempString: string;
begin
  BeginUpdate;
  try
    TempString := Strings[Index1];
    TempObject := Objects[Index1];
    Strings[Index1] := Strings[Index2];
    Objects[Index1] := Objects[Index2];
    Strings[Index2] := TempString;
    Objects[Index2] := TempObject;
  finally
    EndUpdate;
  end;
end;

function TStrings.ExtractName(const S: string): string;
var
  P: Integer;
begin
  Result := S;
  P := Pos(NameValueSeparator, Result);
  if P <> 0 then
    SetLength(Result, P-1)
  else
    SetLength(Result, 0);
end;

function TStrings.GetCapacity: Integer;
begin  // descendants may optionally override/replace this default implementation
  Result := Count;
end;

function TStrings.GetEnumerator: TStringsEnumerator;
begin
  Result := TStringsEnumerator.Create(Self);
end;

function TStrings.GetName(Index: Integer): string;
begin
  Result := ExtractName(Get(Index));
end;

function TStrings.GetObject(Index: Integer): TObject;
begin
  Result := nil;
end;

function TStrings.GetTextStr: string;
var
  Buffer: StringBuilder;
  I, Count: Integer;
begin
  Count := GetCount;
  Buffer := StringBuilder.Create;
  for I := 0 to Count - 1 do
  begin
    Buffer.Append(Get(I));
    Buffer.Append(LineBreak);
  end;
  Result := Buffer.ToString;
end;

function TStrings.GetValue(const Name: string): string;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then
    Result := Copy(Get(I), Length(Name) + 2, MaxInt)
  else
    Result := '';
end;

function TStrings.IndexOf(const S: string): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if CompareStrings(Get(Result), S) = 0 then
      Exit;
  Result := -1;
end;

function TStrings.IndexOfName(const Name: string): Integer;
var
  P: Integer;
  S: string;
begin
  for Result := 0 to GetCount - 1 do
  begin
    S := Get(Result);
    P := Pos(NameValueSeparator, S);
    if (P <> 0) and (CompareStrings(Copy(S, 1, P - 1), Name) = 0) then
      Exit;
  end;
  Result := -1;
end;

function TStrings.IndexOfObject(AObject: TObject): Integer;
begin
  if AObject = nil then
  begin
    for Result := 0 to GetCount - 1 do
      if GetObject(Result) = nil then
        Exit;
  end
  else
  begin
    for Result := 0 to GetCount - 1 do
      if AObject.Equals(GetObject(Result)) then        
        Exit;
  end;

  Result := -1;
end;

procedure TStrings.InsertObject(Index: Integer; const S: string;
  AObject: TObject);
begin
  Insert(Index, S);
  PutObject(Index, AObject);
end;

procedure TStrings.LoadFromFile(const FileName: string);
begin
  LoadFromFile(FileName, nil);
end;

procedure TStrings.LoadFromFile(const FileName: string; Encoding: System.Text.Encoding);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream, Encoding);
  finally
    Stream.Free;
  end;
end;

procedure TStrings.LoadFromStream(Stream: TStream);
begin
  LoadFromStream(Stream, nil);
end;

procedure TStrings.LoadFromStream(Stream: TStream; Encoding: System.Text.Encoding);

  function ContainsPreamble(const Buffer, Signature: array of Byte): Boolean;
  var
    I: Integer;
  begin
    Result := True;
    if Length(Buffer) >= Length(Signature) then
    begin
      for I := 1 to Length(Signature) do
        if Buffer[I - 1] <> Signature [I - 1] then
        begin
          Result := False;
          Break;
        end;
    end
    else
      Result := False;
  end;

var
  Size: Integer;
  Buffer, Preamble: array of Byte;
begin
  BeginUpdate;
  try
    // Read bytes from stream
    Size := Stream.Size - Stream.Position;
    SetLength(Buffer, Size);
    Stream.Read(Buffer, Size);

    Size := 0;
    if Encoding = nil then
    begin
      // Find the appropraite encoding
      if ContainsPreamble(Buffer, System.Text.Encoding.Unicode.GetPreamble) then
        Encoding := System.Text.Encoding.Unicode
      else
        if ContainsPreamble(Buffer, System.Text.Encoding.BigEndianUnicode.GetPreamble) then
          Encoding := System.Text.Encoding.BigEndianUnicode
        else
          if ContainsPreamble(Buffer, System.Text.Encoding.UTF8.GetPreamble) then
            Encoding := System.Text.Encoding.UTF8
          else
            Encoding := System.Text.Encoding.Default;
      Size := Length(Encoding.GetPreamble);
    end
    else
    begin
      // Use specified encoding, ignore preamble bytes if present
      Preamble := Encoding.GetPreamble;
      if ContainsPreamble(Buffer, Preamble) then
        Size := Length(Preamble);
    end;
    SetTextStr(Encoding.GetString(Buffer, Size, Length(Buffer) - Size));
  finally
    EndUpdate;
  end;
end;

procedure TStrings.Move(CurIndex, NewIndex: Integer);
var
  TempObject: TObject;
  TempString: string;
begin
  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      TempString := Get(CurIndex);
      TempObject := GetObject(CurIndex);
      Delete(CurIndex);
      InsertObject(NewIndex, TempString, TempObject);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TStrings.Put(Index: Integer; const S: string);
var
  TempObject: TObject;
begin
  TempObject := GetObject(Index);
  Delete(Index);
  InsertObject(Index, S, TempObject);
end;

procedure TStrings.PutObject(Index: Integer; AObject: TObject);
begin
end;

procedure TStrings.SaveToFile(const FileName: string);
begin
  SaveToFile(FileName, nil);
end;

procedure TStrings.SaveToFile(const FileName: string; Encoding: System.Text.Encoding);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream, Encoding);
  finally
    Stream.Free;
  end;
end;

procedure TStrings.SaveToStream(Stream: TStream);
begin
  SaveToStream(Stream, nil);
end;

procedure TStrings.SaveToStream(Stream: TStream; Encoding: System.Text.Encoding);
var
  Buffer, Preamble: array of Byte;
begin
  if Encoding = nil then
    Encoding := System.Text.Encoding.Default;
  Buffer := Encoding.GetBytes(GetTextStr);
  Preamble := Encoding.GetPreamble;
  if Length(Preamble) > 0 then
    Stream.WriteBuffer(Preamble, Length(Preamble));
  Stream.WriteBuffer(Buffer, Length(Buffer));
end;

procedure TStrings.SetCapacity(NewCapacity: Integer);
begin
  // do nothing - descendants may optionally implement this method
end;

procedure TStrings.SetTextStr(const Value: string);
var
  P, Start, L: Integer;
begin
  BeginUpdate;
  try
    Clear;

    Start := 1;
    L := Length(LineBreak);
    P := Pos(LineBreak, Value);
    while P > 0 do
    begin
      Add(Copy(Value, Start, P - Start));
      Start := P + L;
      P := PosEx(LineBreak, Value, Start);
    end;
    if Start <= Length(Value) then
      Add(Copy(Value, Start, Length(Value) - Start + 1));
  finally
    EndUpdate;
  end;
end;

procedure TStrings.SetUpdateState(Updating: Boolean);
begin
end;

procedure TStrings.SetValue(const Name, Value: string);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then
      I := Add('');
    Put(I, Name + NameValueSeparator + Value);
  end
  else
  begin
    if I >= 0 then
      Delete(I);
  end;
end;

function TStrings.GetLineBreak: string;
begin
  if not (sdLineBreak in FDefined) then
    LineBreak := sLineBreak;
  Result := FLineBreak;
end;

function TStrings.GetStrictDelimiter: Boolean;
begin
  if not (sdStrictDelimiter in FDefined) then
    StrictDelimiter := False;
  Result := FStrictDelimiter;
end;

procedure TStrings.SetLineBreak(const Value: string);
begin
  if (FLineBreak <> Value) or not (sdLineBreak in FDefined) then
  begin
    Include(FDefined, sdLineBreak);
    FLineBreak := Value;
  end
end;

procedure TStrings.SetStrictDelimiter(const Value: Boolean);
begin
  if (FStrictDelimiter <> Value) or not (sdStrictDelimiter in FDefined) then
  begin
    Include(FDefined, sdStrictDelimiter);
    FStrictDelimiter := Value;
  end
end;

function TStrings.CompareStrings(const S1, S2: string): Integer;
begin
  Result := CompareText(S1, S2);
end;

function TStrings.GetNameValueSeparator: Char;
begin
  if not (sdNameValueSeparator in FDefined) then
    NameValueSeparator := '=';
  Result := FNameValueSeparator;
end;

procedure TStrings.SetNameValueSeparator(const Value: Char);
begin
  if (FNameValueSeparator <> Value) or not (sdNameValueSeparator in FDefined) then
  begin
    Include(FDefined, sdNameValueSeparator);
    FNameValueSeparator := Value;
  end
end;

function TStrings.GetValueFromIndex(Index: Integer): string;
begin
  if Index >= 0 then
    Result := Copy(Get(Index), Length(Names[Index]) + 2, MaxInt)
  else
    Result := '';
end;

procedure TStrings.SetValueFromIndex(Index: Integer; const Value: string);
begin
  if Value <> '' then
  begin
    if Index < 0 then
      Index := Add('');
    Put(Index, Names[Index] + NameValueSeparator + Value);
  end
  else
    if Index >= 0 then
      Delete(Index);
end;

{ TStringList }

function TStringList.Add(const S: string): Integer;
begin
  Result := AddObject(S, nil);
end;

function TStringList.AddObject(const S: string; AObject: TObject): Integer;
begin
  if not Sorted then
    Result := FCount
  else
    if Find(S, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError: Error(SDuplicateString, 0);
      end;
  InsertItem(Result, S, AObject);
end;

procedure TStringList.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TStringList.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

procedure TStringList.Clear;
begin
  if FCount <> 0 then
  begin
    Changing;
    FCount := 0;
    SetCapacity(0);
    Changed;
  end;
end;

procedure TStringList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    Error(TList.SListIndexError, Index);
  Changing;
  Dec(FCount);
  if Index < FCount then
    System.Array.Copy(System.Array(FList), Index + 1, System.Array(FList),
      Index, FCount - Index);
  Changed;
end;

procedure TStringList.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= FCount) then
    Error(TList.SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then
    Error(TList.SListIndexError, Index2);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

procedure TStringList.ExchangeItems(Index1, Index2: Integer);
var
  Temp: TStringItem;
begin
  Temp := FList[Index1];
  FList[Index1] := FList[Index2];
  FLIst[Index2] := Temp;
end;

function TStringList.Find(const S: string; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(FList[I].FString, S);
    if C < 0 then
      L := I + 1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then
          L := I;
      end;
    end;
  end;
  Index := L;
end;

function TStringList.Get(Index: Integer): string;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(TList.SListIndexError, Index);
  Result := FList[Index].FString;
end;

function TStringList.GetCapacity: Integer;
begin
  Result := Length(FList);
end;

function TStringList.GetCount: Integer;
begin
  Result := FCount;
end;

function TStringList.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(TList.SListIndexError, Index);
  Result := FList[Index].FObject;
end;

procedure TStringList.Grow;
var
  Delta: Integer;
  C: Integer;
begin
  C := Length(FList);
  if C > 64 then
    Delta := C div 4
  else if C > 8 then
    Delta := 16
  else
    Delta := 4;
  SetCapacity(C + Delta);
end;

function TStringList.IndexOf(const S: string): Integer;
begin
  if not Sorted then
    Result := inherited IndexOf(S)
  else if not Find(S, Result) then
    Result := -1;
end;

procedure TStringList.Insert(Index: Integer; const S: string);
begin
  InsertObject(Index, S, nil);
end;

procedure TStringList.InsertObject(Index: Integer; const S: string;
  AObject: TObject);
begin
  if Sorted then
    Error(SSortedListError, 0);
  if (Index < 0) or (Index > Count) then
    Error(TList.SListIndexError, Index);
  InsertItem(Index, S, AObject);
end;

procedure TStringList.InsertItem(Index: Integer; const S: string; AObject: TObject);
begin
  Changing;
  if FCount = Length(FList) then
    Grow;
  if Index < FCount then
    System.Array.Copy(System.Array(FList), Index, System.Array(FList),
      Index + 1, FCount - Index);
  with FList[Index] do
  begin
    FObject := AObject;
    FString := S;
  end;
  Inc(FCount);
  Changed;
end;

procedure TStringList.Put(Index: Integer; const S: string);
begin
  if Sorted then
    Error(SSortedListError, 0);
  if (Index < 0) or (Index >= FCount) then
    Error(TList.SListIndexError, Index);
  Changing;
  FList[Index].FString := S;
  Changed;
end;

procedure TStringList.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= FCount) then
    Error(TList.SListIndexError, Index);
  Changing;
  FList[Index].FObject := AObject;
  Changed;
end;

procedure TStringList.QuickSort(L, R: Integer; SCompare: TStringListSortCompare);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(Self, I, P) < 0 do
        Inc(I);
      while SCompare(Self, J, P) > 0 do
        Dec(J);
      if I <= J then
      begin
        ExchangeItems(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TStringList.SetCapacity(NewCapacity: Integer);
begin
  SetLength(FList, NewCapacity);
end;

procedure TStringList.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then
      Sort;
    FSorted := Value;
  end;
end;

procedure TStringList.SetUpdateState(Updating: Boolean);
begin
  if Updating then
    Changing
  else
    Changed;
end;

function StringListCompareStrings(List: TStringList; Index1,
  Index2: Integer): Integer;
begin
  Result := List.CompareStrings(List.FList[Index1].FString,
    List.FList[Index2].FString);
end;

procedure TStringList.Sort;
begin
  CustomSort(StringListCompareStrings);
end;

procedure TStringList.CustomSort(Compare: TStringListSortCompare);
begin
  if not Sorted and (FCount > 1) then
  begin
    Changing;
    QuickSort(0, FCount - 1, Compare);
    Changed;
  end;
end;

function TStringList.CompareStrings(const S1, S2: string): Integer;
begin
  if CaseSensitive then
    Result := CompareStr(S1, S2)
  else
    Result := CompareText(S1, S2);
end;

procedure TStringList.SetCaseSensitive(const Value: Boolean);
begin
  if Value <> FCaseSensitive then
  begin
    FCaseSensitive := Value;
    if Sorted then
      Sort;
  end;
end;

{==============================================================================}

{ TStream }

function TStream.GetPosition: Int64;
begin
  Result := Seek(0, soCurrent);
end;

procedure TStream.SetPosition(const Pos: Int64);
begin
  Seek(Pos, soBeginning);
end;

function TStream.GetSize: Int64;
var
  Pos: Int64;
begin
  Pos := Seek(0, soCurrent);
  Result := Seek(0, soEnd);
  Seek(Pos, soBeginning);
end;

procedure TStream.ReadBuffer(Buffer: TBytes; Count: Longint);
begin
  if (Count <> 0) and (Read(Buffer, Count) <> Count) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBuffer(var Buffer: Byte);
begin
  if Read(Buffer) <> SizeOf(Byte) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBuffer(var Buffer: Byte; Count: Longint);
begin
  if (Count <> 0) and (Read(Buffer, Count) <> Count) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBuffer(var Buffer: Boolean);
begin
  if Read(Buffer) <> SizeOf(Boolean) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBuffer(var Buffer: Boolean; Count: Longint);
begin
  if (Count <> 0) and (Read(Buffer, Count) <> Count) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBuffer(var Buffer: Char);
begin
  if Read(Buffer) <> SizeOf(Char) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBuffer(var Buffer: Char; Count: Longint);
begin
  if (Count <> 0) and (Read(Buffer, Count) <> Count) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBuffer(var Buffer: ShortInt);
begin
  if Read(Buffer) <> SizeOf(ShortInt) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBuffer(var Buffer: ShortInt; Count: Longint);
begin
  if (Count <> 0) and (Read(Buffer, Count) <> Count) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBuffer(var Buffer: SmallInt);
begin
  if Read(Buffer) <> SizeOf(SmallInt) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBuffer(var Buffer: SmallInt; Count: Longint);
begin
  if (Count <> 0) and (Read(Buffer, Count) <> Count) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBuffer(var Buffer: Word);
begin
  if Read(Buffer) <> SizeOf(Word) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBuffer(var Buffer: Word; Count: Longint);
begin
  if (Count <> 0) and (Read(Buffer, Count) <> Count) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBuffer(var Buffer: Integer);
begin
  if Read(Buffer) <> SizeOf(Integer) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBuffer(var Buffer: Integer; Count: Longint);
begin
  if (Count <> 0) and (Read(Buffer, Count) <> Count) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBuffer(var Buffer: Cardinal);
begin
  if Read(Buffer) <> SizeOf(Cardinal) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBuffer(var Buffer: Cardinal; Count: Longint);
begin
  if (Count <> 0) and (Read(Buffer, Count) <> Count) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBuffer(var Buffer: Int64);
begin
  if Read(Buffer) <> SizeOf(Int64) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBuffer(var Buffer: Int64; Count: Longint);
begin
  if (Count <> 0) and (Read(Buffer, Count) <> Count) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBuffer(var Buffer: UInt64);
begin
  if Read(Buffer) <> SizeOf(UInt64) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBuffer(var Buffer: UInt64; Count: Longint);
begin
  if (Count <> 0) and (Read(Buffer, Count) <> Count) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBuffer(var Buffer: Single);
begin
  if Read(Buffer) <> SizeOf(Single) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBuffer(var Buffer: Single; Count: Longint);
begin
  if (Count <> 0) and (Read(Buffer, Count) <> Count) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBuffer(var Buffer: Double);
begin
  if Read(Buffer) <> SizeOf(Double) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBuffer(var Buffer: Double; Count: Longint);
begin
  if (Count <> 0) and (Read(Buffer, Count) <> Count) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.WriteBuffer(const Buffer: TBytes; Count: Longint);
begin
  if (Count <> 0) and (Write(Buffer, Count) <> Count) then
    raise EWriteError.Create(SWriteError);
end;

procedure TStream.WriteBuffer(const Buffer: Byte);
begin
  if Write(Buffer) <> SizeOf(Byte) then
    raise EWriteError.Create(SWriteError);
end;

procedure TStream.WriteBuffer(const Buffer: Byte; Count: Longint);
begin
  if (Count <> 0) and (Write(Buffer, Count) <> Count) then
    raise EWriteError.Create(SWriteError);
end;

procedure TStream.WriteBuffer(const Buffer: Boolean);
begin
  if Write(Buffer) <> SizeOf(Boolean) then
    raise EWriteError.Create(SWriteError);
end;

procedure TStream.WriteBuffer(const Buffer: Boolean; Count: Longint);
begin
  if (Count <> 0) and (Write(Buffer, Count) <> Count) then
    raise EWriteError.Create(SWriteError);
end;

procedure TStream.WriteBuffer(const Buffer: Char);
begin
  if Write(Buffer) <> SizeOf(Char) then
    raise EWriteError.Create(SWriteError);
end;

procedure TStream.WriteBuffer(const Buffer: Char; Count: Longint);
begin
  if (Count <> 0) and (Write(Buffer, Count) <> Count) then
    raise EWriteError.Create(SWriteError);
end;

procedure TStream.WriteBuffer(const Buffer: ShortInt);
begin
  if Write(Buffer) <> SizeOf(ShortInt) then
    raise EWriteError.Create(SWriteError);
end;

procedure TStream.WriteBuffer(const Buffer: ShortInt; Count: Longint);
begin
  if (Count <> 0) and (Write(Buffer, Count) <> Count) then
    raise EWriteError.Create(SWriteError);
end;

procedure TStream.WriteBuffer(const Buffer: SmallInt);
begin
  if Write(Buffer) <> SizeOf(SmallInt) then
    raise EWriteError.Create(SWriteError);
end;

procedure TStream.WriteBuffer(const Buffer: SmallInt; Count: Longint);
begin
  if (Count <> 0) and (Write(Buffer, Count) <> Count) then
    raise EWriteError.Create(SWriteError);
end;

procedure TStream.WriteBuffer(const Buffer: Word);
begin
  if Write(Buffer) <> SizeOf(Word) then
    raise EWriteError.Create(SWriteError);
end;

procedure TStream.WriteBuffer(const Buffer: Word; Count: Longint);
begin
  if (Count <> 0) and (Write(Buffer, Count) <> Count) then
    raise EWriteError.Create(SWriteError);
end;

procedure TStream.WriteBuffer(const Buffer: Integer);
begin
  if Write(Buffer, 4) <> 4 then
    raise EWriteError.Create(SWriteError);
end;

procedure TStream.WriteBuffer(const Buffer: Integer; Count: Longint);
begin
  if (Count <> 0) and (Write(Buffer, Count) <> Count) then
    raise EWriteError.Create(SWriteError);
end;

procedure TStream.WriteBuffer(const Buffer: Cardinal);
begin
  if Write(Buffer) <> SizeOf(Cardinal) then
    raise EWriteError.Create(SWriteError);
end;

procedure TStream.WriteBuffer(const Buffer: Cardinal; Count: Longint);
begin
  if (Count <> 0) and (Write(Buffer, Count) <> Count) then
    raise EWriteError.Create(SWriteError);
end;

procedure TStream.WriteBuffer(const Buffer: Int64);
begin
  if Write(Buffer) <> SizeOf(Int64) then
    raise EWriteError.Create(SWriteError);
end;

procedure TStream.WriteBuffer(const Buffer: Int64; Count: Integer);
begin
  if (Count <> 0) and (Write(Buffer, Count) <> Count) then
    raise EWriteError.Create(SWriteError);
end;

procedure TStream.WriteBuffer(const Buffer: UInt64);
begin
  if Write(Buffer) <> SizeOf(UInt64) then
    raise EWriteError.Create(SWriteError);
end;

procedure TStream.WriteBuffer(const Buffer: UInt64; Count: Integer);
begin
  if (Count <> 0) and (Write(Buffer, Count) <> Count) then
    raise EWriteError.Create(SWriteError);
end;

procedure TStream.WriteBuffer(const Buffer: Single);
begin
  if Write(Buffer) <> SizeOf(Single) then
    raise EWriteError.Create(SWriteError);
end;

procedure TStream.WriteBuffer(const Buffer: Single; Count: Integer);
begin
  if (Count <> 0) and (Write(Buffer, Count) <> Count) then
    raise EWriteError.Create(SWriteError);
end;

procedure TStream.WriteBuffer(const Buffer: Double);
begin
  if Write(Buffer) <> SizeOf(Double) then
    raise EWriteError.Create(SWriteError);
end;

procedure TStream.WriteBuffer(const Buffer: Double; Count: Integer);
begin
  if (Count <> 0) and (Write(Buffer, Count) <> Count) then
    raise EWriteError.Create(SWriteError);
end;

{MaxBufSize moved out of CopyFrom for performance }
const
  MaxBufSize = $F000;

function TStream.CopyFrom(Source: TStream; Count: Int64): Int64;
var
  BufSize, N: Integer;
  Buffer: array of Byte;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end;
  Result := Count;
  if Count > MaxBufSize then
    BufSize := MaxBufSize
  else
    BufSize := Count;
  SetLength(Buffer, BufSize);
  while Count <> 0 do
  begin
    if Count > BufSize then
      N := BufSize
    else
      N := Count;
    Source.ReadBuffer(Buffer, N);
    WriteBuffer(Buffer, N);
    Dec(Count, N);
  end;
end;

class operator TStream.Implicit(const Value: TStream): System.IO.Stream;
begin
  if Value is TCLRStreamWrapper then
    Result := TCLRStreamWrapper(Value).Handle
  else
    Result := TStreamToCLRStream.Create(Value);
end;

class operator TStream.Implicit(const Value: System.IO.Stream): TStream;
begin
  Result := TCLRStreamWrapper.Create(Value);
end;

function TStream.Read(var Buffer: array of Byte; Count: Longint): Longint;
begin
  Result := Read(Buffer, 0, Count);
end;

function TStream.Read(var Buffer: Byte): Longint;
var
  Buf: array[0..0] of Byte;
begin
  Result := Read(Buf, 1);
  Buffer := Buf[0];
end;

function TStream.Read(var Buffer: Byte; Count: Longint): Longint;
var
  Buf: array[0..0] of Byte;
begin
  if Count <> 0 then
  begin
    Result := Read(Buf, 1);
    if Count > 1 then
      Inc(Result, Skip(Count - 1));
    Buffer := Buf[0];
  end
  else
    Result := 0;
end;

function TStream.Read(var Buffer: Boolean): Longint;
var
  Buf: array[0..0] of Byte;
begin
  Result := Read(Buf, 1);
  Buffer := Boolean(Buf[0]);
end;

function TStream.Read(var Buffer: Boolean; Count: Longint): Longint;
var
  Buf: array[0..0] of Byte;
begin
  if Count <> 0 then
  begin
    Result := Read(Buf, 1);
    if Count > 1 then
      Inc(Result, Skip(Count - 1));
    Buffer := Boolean(Buf[0]);
  end
  else
    Result := 0;
end;

function TStream.Read(var Buffer: Char): Longint;
var
  Buf: array[0..1] of Byte;
begin
  Result := Read(Buf, 2);
  Buffer := Char(Buf[0] or Buf[1] shl 8);
end;

function TStream.Read(var Buffer: Char; Count: Longint): Longint;
var
  Buf: array[0..1] of Byte;
  S: Integer;
begin
  S := 0;
  if Count > 2 then
  begin
    S := Count - 2;
    Count := 2;
  end;
  if Count <> 0 then
  begin
    Buf[1] := 0;
    Result := Read(Buf, Count);
    Buffer := Char(Buf[0] or Buf[1] shl 8);
    if S <> 0 then
      Inc(Result, Skip(S));
  end
  else
    Result := 0;
end;

function TStream.Read(var Buffer: ShortInt): Longint;
var
  Buf: array[0..0] of Byte;
begin
  Result := Read(Buf, 1);
  Buffer := ShortInt(Buf[0]);
end;

function TStream.Read(var Buffer: ShortInt; Count: Longint): Longint;
var
  Buf: array[0..0] of Byte;
begin
  if Count <> 0 then
  begin
    Result := Read(Buf, 1);
    if Count > 1 then
      Inc(Result, Skip(Count - 1));
    Buffer := ShortInt(Buf[0]);
  end
  else
    Result := 0;
end;

function TStream.Read(var Buffer: SmallInt): Longint;
var
  Buf: array[0..1] of Byte;
begin
  Result := Read(Buf, 2);
  Buffer := SmallInt(Buf[0] or (Buf[1] shl 8));
end;

function TStream.Read(var Buffer: SmallInt; Count: Longint): Longint;
var
  Buf: array[0..1] of Byte;
  S: Integer;
begin
  S := 0;
  if Count > 2 then
  begin
    S := Count - 2;
    Count := 2;
  end;
  if Count <> 0 then
  begin
    Buf[1] := 0;
    Result := Read(Buf, Count);
    Buffer := SmallInt(Buf[0] or (Buf[1] shl 8));
    if S <> 0 then
      Inc(Result, Skip(S));
  end
  else
    Result := 0;
end;

function TStream.Read(var Buffer: Word): Longint;
var
  Buf: array[0..1] of Byte;
begin
  Result := Read(Buf, 2);
  Buffer := Word(Buf[0] or (Buf[1] shl 8));
end;

function TStream.Read(var Buffer: Word; Count: Longint): Longint;
var
  Buf: array[0..1] of Byte;
  S: Integer;
begin
  S := 0;
  if Count > 2 then
  begin
    S := Count - 2;
    Count := 2;
  end;
  if Count <> 0 then
  begin
    Buf[1] := 0;
    Result := Read(Buf, Count);
    Buffer := Word(Buf[0] or (Buf[1] shl 8));
    if S <> 0 then
      Inc(Result, Skip(S));
  end
  else
    Result := 0;
end;

function TStream.Read(var Buffer: Integer): Longint;
var
  Buf: array[0..3] of Byte;
begin
  Result := Read(Buf, 4);
  Buffer := Integer(Buf[0] or (Buf[1] shl 8) or (Buf[2] shl 16) or (Buf[3] shl 24));
end;

function TStream.Read(var Buffer: Integer; Count: Longint): Longint;
var
  Buf: array[0..3] of Byte;
  S: Integer;
begin
  S := 0;
  if Count > 4 then
  begin
    S := Count - 4;
    Count := 4;
  end;
  if Count <> 0 then
  begin
    Buf[1] := 0;
    Buf[2] := 0;
    Buf[3] := 0;
    Result := Read(Buf, Count);
    Buffer := Integer(Buf[0] or (Buf[1] shl 8) or (Buf[2] shl 16) or (Buf[3] shl 24));
    if S <> 0 then
      Inc(Result, Skip(S));
  end
  else
    Result := 0;
end;

function TStream.Read(var Buffer: Cardinal): Longint;
var
  Buf: array[0..3] of Byte;
begin
  Result := Read(Buf, 4);
  Buffer := Cardinal(Buf[0] or (Buf[1] shl 8) or (Buf[2] shl 16) or (Buf[3] shl 24));
end;

function TStream.Read(var Buffer: Cardinal; Count: Longint): Longint;
var
  Buf: array[0..3] of Byte;
  S: Integer;
begin
  S := 0;
  if Count > 4 then
  begin
    S := Count - 4;
    Count := 4;
  end;
  if Count <> 0 then
  begin
    Buf[1] := 0;
    Buf[2] := 0;
    Buf[3] := 0;
    Result := Read(Buf, Count);
    Buffer := Cardinal(Buf[0] or (Buf[1] shl 8) or (Buf[2] shl 16) or (Buf[3] shl 24));
    if S <> 0 then
      Inc(Result, Skip(S));
  end
  else
    Result := 0;
end;

function TStream.Read(var Buffer: Int64): Longint;
var
  Buf: array[0..7] of Byte;
begin
  Result := Read(Buf, 8);
  Buffer := Int64(Buf[0]) or (Int64(Buf[1]) shl 8) or
      (Int64(Buf[2]) shl 16) or (Int64(Buf[3]) shl 24) or
      (Int64(Buf[4]) shl 32) or (Int64(Buf[5]) shl 40) or
      (Int64(Buf[6]) shl 48) or (Int64(Buf[7]) shl 56);
end;

function TStream.Read(var Buffer: Int64; Count: Longint): Longint;
var
  Buf: array[0..7] of Byte;
  S: Integer;
begin
  S := 0;
  if Count > 8 then
  begin
    S := Count - 8;
    Count := 8;
  end;
  if Count <> 0 then
  begin
    Buf[1] := 0;
    Buf[2] := 0;
    Buf[3] := 0;
    Buf[4] := 0;
    Buf[5] := 0;
    Buf[6] := 0;
    Buf[7] := 0;
    Result := Read(Buf, Count);
    Buffer := Int64(Buf[0]) or (Int64(Buf[1]) shl 8) or
        (Int64(Buf[2]) shl 16) or (Int64(Buf[3]) shl 24) or
        (Int64(Buf[4]) shl 32) or (Int64(Buf[5]) shl 40) or
        (Int64(Buf[6]) shl 48) or (Int64(Buf[7]) shl 56);
    if S <> 0 then
      Inc(Result, Skip(S));
  end
  else
    Result := 0;
end;

function TStream.Read(var Buffer: UInt64): Longint;
var
  Buf: array[0..7] of Byte;
begin
  Result := Read(Buf, 8);
  Buffer := Int64(Buf[0]) or (Int64(Buf[1]) shl 8) or
      (Int64(Buf[2]) shl 16) or (Int64(Buf[3]) shl 24) or
      (Int64(Buf[4]) shl 32) or (Int64(Buf[5]) shl 40) or
      (Int64(Buf[6]) shl 48) or (Int64(Buf[7]) shl 56);
end;

function TStream.Read(var Buffer: UInt64; Count: Longint): Longint;
var
  Buf: array[0..7] of Byte;
  S: Integer;
begin
  S := 0;
  if Count > 8 then
  begin
    S := Count - 8;
    Count := 8;
  end;
  if Count <> 0 then
  begin
    Buf[1] := 0;
    Buf[2] := 0;
    Buf[3] := 0;
    Buf[4] := 0;
    Buf[5] := 0;
    Buf[6] := 0;
    Buf[7] := 0;
    Result := Read(Buf, Count);
    Buffer := Int64(Buf[0]) or (Int64(Buf[1]) shl 8) or
        (Int64(Buf[2]) shl 16) or (Int64(Buf[3]) shl 24) or
        (Int64(Buf[4]) shl 32) or (Int64(Buf[5]) shl 40) or
        (Int64(Buf[6]) shl 48) or (Int64(Buf[7]) shl 56);
    if S <> 0 then
      Inc(Result, Skip(S));
  end
  else
    Result := 0;
end;

function TStream.Read(var Buffer: Single): Longint;
var
  Buf: array[0..3] of Byte;
begin
  Result := Read(Buf, 4);
  Buffer := BitConverter.ToSingle(Buf, 0);
end;

function TStream.Read(var Buffer: Single; Count: Longint): Longint;
var
  Buf: array[0..3] of Byte;
begin
  if Count <> 4 then
  begin
    Buffer := 0;
    Result := Skip(Count);
  end
  else
  begin
    Result := Read(Buf, 4);
    Buffer := BitConverter.ToSingle(Buf, 0);
  end;
end;

function TStream.Read(var Buffer: Double): Longint;
var
  Buf: array[0..7] of Byte;
begin
  Result := Read(Buf, 8);
  Buffer := BitConverter.ToDouble(Buf, 0);
end;

function TStream.Read(var Buffer: Double; Count: Longint): Longint;
var
  Buf: array[0..7] of Byte;
begin
  if Count = 8 then
  begin
    Result := Read(Buf, 8);
    Buffer := BitConverter.ToDouble(Buf, 0);
  end
  else
  begin
    Buffer := 0;
    Result := Skip(Count);
  end;
end;

function TStream.Skip(Amount: Integer): Integer;
var
  P: Integer;
begin
  P := Position;
  Result := Seek(Amount, soCurrent) - P;
end;

function TStream.Write(const Buffer: array of Byte; Count: Longint): Longint;
begin
  Result := Write(Buffer, 0, Count);
end;

function TStream.Write(const Buffer: Byte): Longint;
var
  Buf: array[0..0] of Byte;
begin
  Buf[0] := Buffer;
  Result := Write(Buf, 1);
end;

function TStream.Write(const Buffer: Byte; Count: Longint): Longint;
var
  Buf: array[0..0] of Byte;
  C: Integer;
begin
  C := Count;
  if C > 1 then
    C := 1;
  Buf[0] := Buffer;
  Result := Write(Buf, C);
  if C < Count then
    Inc(Result, Skip(Count - C));
end;

function TStream.Write(const Buffer: Boolean): Longint;
var
  Buf: array[0..0] of Byte;
begin
  Buf[0] := Byte(Buffer);
  Result := Write(Buf, 1);
end;

function TStream.Write(const Buffer: Boolean; Count: Longint): Longint;
var
  Buf: array[0..0] of Byte;
  C: Integer;
begin
  C := Count;
  if C > 1 then
    C := 1;
  Buf[0] := Byte(Buffer);
  Result := Write(Buf, C);
  if C < Count then
    Inc(Result, Skip(Count - C));
end;

function TStream.Write(const Buffer: Char): Longint;
var
  Buf: array[0..1] of Byte;
begin
  Buf[0] := Word(Buffer) and $FF;
  Buf[1] := (Word(Buffer) shr 8) and $FF;
  Result := Write(Buf, 2);
end;

function TStream.Write(const Buffer: Char; Count: Longint): Longint;
var
  Buf: array[0..1] of Byte;
  C: Integer;
begin
  C := Count;
  if C > 2 then
    C := 2;
  Buf[0] := Word(Buffer) and $FF;
  Buf[1] := (Word(Buffer) shr 8) and $FF;
  Result := Write(Buf, C);
  if C < Count then
    Inc(Result, Skip(Count - C));
end;

function TStream.Write(const Buffer: ShortInt): Longint;
var
  Buf: array[0..0] of Byte;
begin
  Buf[0] := Buffer;
  Result := Write(Buf, 1);
end;

function TStream.Write(const Buffer: ShortInt; Count: Longint): Longint;
var
  Buf: array[0..0] of Byte;
  C: Integer;
begin
  C := Count;
  if C > 1 then
    C := 1;
  Buf[0] := Buffer;
  Result := Write(Buf, C);
  if C < Count then
    Inc(Result, Skip(Count - C));
end;

function TStream.Write(const Buffer: SmallInt): Longint;
var
  Buf: array[0..1] of Byte;
begin
  Buf[0] := Buffer and $FF;
  Buf[1] := (Buffer shr 8) and $FF;
  Result := Write(Buf, 2);
end;

function TStream.Write(const Buffer: SmallInt; Count: Longint): Longint;
var
  Buf: array[0..1] of Byte;
  C: Integer;
begin
  C := Count;
  if C > 2 then
    C := 2;
  Buf[0] := Buffer and $FF;
  Buf[1] := (Buffer shr 8) and $FF;
  Result := Write(Buf, C);
  if C < Count then
    Inc(Result, Skip(Count - C));
end;

function TStream.Write(const Buffer: Word): Longint;
var
  Buf: array[0..1] of Byte;
begin
  Buf[0] := Word(Buffer) and $FF;
  Buf[1] := (Word(Buffer) shr 8) and $FF;
  Result := Write(Buf, 2);
end;

function TStream.Write(const Buffer: Word; Count: Longint): Longint;
var
  Buf: array[0..1] of Byte;
  C: Integer;
begin
  C := Count;
  if C > 2 then
    C := 2;
  Buf[0] := Word(Buffer) and $FF;
  Buf[1] := (Word(Buffer) shr 8) and $FF;
  Result := Write(Buf, C);
  if C < Count then
    Inc(Result, Skip(Count - C));
end;

function TStream.Write(const Buffer: Integer): Longint;
var
  Buf: array[0..3] of Byte;
begin
  Buf[0] := Buffer and $FF;
  Buf[1] := (Buffer shr 8) and $FF;
  Buf[2] := (Buffer shr 16) and $FF;
  Buf[3] := (Buffer shr 24) and $FF;
  Result := Write(Buf, 4);
end;

function TStream.Write(const Buffer: Integer; Count: Longint): Longint;
var
  Buf: array[0..3] of Byte;
  C: Integer;
begin
  C := Count;
  if C > 4 then
    C := 4;
  Buf[0] := Buffer and $FF;
  Buf[1] := (Buffer shr 8) and $FF;
  Buf[2] := (Buffer shr 16) and $FF;
  Buf[3] := (Buffer shr 24) and $FF;
  Result := Write(Buf, C);
  if C < Count then
    Inc(Result, Skip(Count - C));
end;

function TStream.Write(const Buffer: Cardinal): Longint;
var
  Buf: array[0..3] of Byte;
begin
  Buf[0] := Buffer and $FF;
  Buf[1] := (Buffer shr 8) and $FF;
  Buf[2] := (Buffer shr 16) and $FF;
  Buf[3] := (Buffer shr 24) and $FF;
  Result := Write(Buf, 4);
end;

function TStream.Write(const Buffer: Cardinal; Count: Longint): Longint;
var
  Buf: array[0..3] of Byte;
  C: Integer;
begin
  C := Count;
  if C > 4 then
    C := 4;
  Buf[0] := Buffer and $FF;
  Buf[1] := (Buffer shr 8) and $FF;
  Buf[2] := (Buffer shr 16) and $FF;
  Buf[3] := (Buffer shr 24) and $FF;
  Result := Write(Buf, C);
  if C < Count then
    Inc(Result, Skip(Count - C));
end;

function TStream.Write(const Buffer: Int64): Longint;
begin
  Result := Write(BitConverter.GetBytes(Buffer), SizeOf(Int64));
end;

function TStream.Write(const Buffer: Int64; Count: Integer): Longint;
var
  C: Integer;
begin
  C := Count;
  if C > 8 then
    C := 8;
  Result := Write(BitConverter.GetBytes(Buffer), C);
  if C < Count then
    Inc(Result, Skip(Count - C));
end;

function TStream.Write(const Buffer: UInt64): Longint;
begin
  Result := Write(BitConverter.GetBytes(Buffer), SizeOf(UInt64));
end;

function TStream.Write(const Buffer: UInt64; Count: Integer): Longint;
var
  C: Integer;
begin
  C := Count;
  if C > 8 then
    C := 8;
  Result := Write(BitConverter.GetBytes(Buffer), C);
  if C < Count then
    Inc(Result, Skip(Count - C));
end;

function TStream.Write(const Buffer: Single): Longint;
begin
  Result := Write(BitConverter.GetBytes(Buffer), SizeOf(Single));
end;

function TStream.Write(const Buffer: Single; Count: Integer): Longint;
var
  C: Integer;
begin
  C := Count;
  if C > 4 then
    C := 4;
  Result := Write(BitConverter.GetBytes(Buffer), C);
  if C < Count then
    Inc(Result, Skip(Count - C));
end;

function TStream.Write(const Buffer: Double): Longint;
begin
  Result := Write(BitConverter.GetBytes(Buffer), SizeOf(Double));
end;

function TStream.Write(const Buffer: Double; Count: Integer): Longint;
var
  C: Integer;
begin
  C := Count;
  if C > 8 then
    C := 8;
  Result := Write(BitConverter.GetBytes(Buffer), C);
  if C < Count then
    Inc(Result, Skip(Count - C));
end;

{ TCLRStreamWrapper }

constructor TCLRStreamWrapper.Create(AHandle: System.IO.Stream);
begin
  inherited Create;
  FHandle := AHandle;
end;

destructor TCLRStreamWrapper.Destroy;
begin
  if FHandle <> nil then
    FHandle.Close;
  inherited Destroy;
end;

function TCLRStreamWrapper.Read(var Buffer: array of Byte; Offset, Count: Longint): Longint;
begin
  Result := FHandle.Read(Buffer, Offset, Count);
end;

const
  OriginMap: array[TSeekOrigin] of System.IO.SeekOrigin =
    (System.IO.SeekOrigin.Begin, System.IO.SeekOrigin.Current,
    System.IO.SeekOrigin.End);

function TCLRStreamWrapper.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := FHandle.Seek(Offset, OriginMap[Origin]);
end;

procedure TCLRStreamWrapper.SetSize(NewSize: Int64);
begin
  FHandle.SetLength(NewSize);
end;

function TCLRStreamWrapper.Write(const Buffer: array of Byte; Offset, Count: Longint): Longint;
begin
  try
    FHandle.Write(Buffer, Offset, Count);
    Result := Count;
  except
    Result := 0;
  end;
end;

{ TCustomMemoryStream }

function TCustomMemoryStream.Read(var Buffer: array of Byte; Offset, Count: Longint): Longint;
begin
  if (FPosition >= 0) and (Count >= 0) then
  begin
    Result := FSize - FPosition;
    if Result > 0 then
    begin
      if Result > Count then
        Result := Count;
      System.Array.Copy(System.Array(FMemory), FPosition, System.Array(Buffer),
        Offset, Result);
      Inc(FPosition, Result);
      Exit;
    end;
  end;
  Result := 0;
end;

function TCustomMemoryStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning: FPosition := Offset;
    soCurrent: Inc(FPosition, Offset);
    soEnd: FPosition := FSize + Offset;
  end;
  Result := FPosition;
end;

procedure TCustomMemoryStream.SaveToStream(Stream: TStream);
begin
  if FSize <> 0 then
    Stream.WriteBuffer(FMemory, FSize);
end;

procedure TCustomMemoryStream.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

{ TMemoryStream }

const
  MemoryDelta = $2000; { Must be a power of 2 }

procedure TMemoryStream.Clear;
begin
  SetCapacity(0);
  FSize := 0;
  FPosition := 0;
end;

function TMemoryStream.GetCapacity: Longint;
begin
  if Assigned(FMemory) then
    Result := System.Array(FMemory).Length
  else
    Result := 0;
end;

procedure TMemoryStream.LoadFromStream(Stream: TStream);
var
  Count: Longint;
begin
  Stream.Position := 0;
  Count := Stream.Size;
  SetSize(Count);
  if Count <> 0 then
    Stream.ReadBuffer(FMemory, Count);
end;

procedure TMemoryStream.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TMemoryStream.SetCapacity(NewCapacity: Longint);
begin
  FMemory := Realloc(NewCapacity);
end;

procedure TMemoryStream.SetSize(NewSize: Int64);
var
  OldPosition: Longint;
begin
  OldPosition := FPosition;
  SetCapacity(NewSize);
  FSize := NewSize;
  if OldPosition > NewSize then
    Seek(0, soEnd);
end;

function TMemoryStream.Realloc(var NewCapacity: Longint): TBytes;
begin
  if (NewCapacity > 0) and (NewCapacity <> FSize) then
    NewCapacity := (NewCapacity + (MemoryDelta - 1)) and not (MemoryDelta - 1);
  Result := FMemory;
  if NewCapacity <> Length(Result) then
    SetLength(Result, NewCapacity);
end;

function TMemoryStream.Write(const Buffer: array of Byte; Offset, Count: Longint): Longint;
var
  Pos: Longint;
begin
  if (FPosition >= 0) and (Count >= 0) then
  begin
    Pos := FPosition + Count;
    if Pos > 0 then
    begin
      if Pos > FSize then
      begin
        if Pos > Length(Memory) then
          SetCapacity(Pos);
        FSize := Pos;
      end;
      System.Array.Copy(System.Array(Buffer), Offset, System.Array(Memory),
        FPosition, Count);
      FPosition := Pos;
      Result := Count;
      Exit;
    end;
  end;
  Result := 0;
end;

{ TFileStream }

constructor TFileStream.Create(const AFileName: string; Mode: Word);
begin
  Create(AFileName, Mode, 0);
end;

constructor TFileStream.Create(const AFileName: string; Mode: Word; Rights: Cardinal);
var
  LMode: System.IO.FileMode;
  LAccess: System.IO.FileAccess;
  LShare: System.IO.FileShare;
begin
  inherited Create(nil);
  if Mode = fmCreate then
  begin
    LMode := System.IO.FileMode.Create;
    LAccess := System.IO.FileAccess.ReadWrite;
  end
  else
  begin
    LMode := System.IO.FileMode.Open;
    case Mode and $F of
      fmOpenReadWrite: LAccess := System.IO.FileAccess.ReadWrite;
      fmOpenWrite: LAccess := System.IO.FileAccess.Write;
    else
      LAccess := System.IO.FileAccess.Read;
    end;
  end;
  case Mode and $F0 of
    fmShareDenyWrite: LShare := System.IO.FileShare.Read;
    fmShareDenyRead: LShare := System.IO.FileShare.Write;
    fmShareDenyNone: LShare := System.IO.FileShare.None;
  else
    LShare := System.IO.FileShare.ReadWrite;
  end;
  FHandle := System.IO.FileStream.Create(AFileName, LMode, LAccess, LShare);
  FFileName := AFileName;
end;

{ TResourceStream }

constructor TResourceStream.Create(Instance: THandle; const ResName: string;
  ResType: Integer);
begin
  inherited Create;
  Initialize(Instance, FindResource(Instance, ResName, ResType), ResName);
end;

constructor TResourceStream.Create(Instance: THandle; const ResName, ResType: string);
begin
  inherited Create;
  Initialize(Instance, FindResource(Instance, ResName, ResType), ResName);
end;

constructor TResourceStream.CreateFromID(Instance: THandle; ResID, ResType: Integer);
begin
  inherited Create;
  Initialize(Instance, FindResource(Instance, ResID, ResType), ResID.ToString());
end;

constructor TResourceStream.CreateFromID(Instance: THandle; ResID: Integer;
  ResType: string);
begin
  inherited Create;
  Initialize(Instance, FindResource(Instance, ResID, ResType), ResID.ToString());
end;

procedure TResourceStream.Initialize(Instance, ResInfo: THandle; const Name: string);

  procedure Error;
  begin
    raise EResNotFound.Create(System.&String.Format(SResNotFound, [Name]));
  end;

var
  PResData: IntPtr;
begin
  HResInfo := ResInfo;
  if HResInfo = 0 then Error;
  HGlobal := LoadResource(Instance, HResInfo);
  if HGlobal = 0 then Error;
  try
    SetSize(SizeOfResource(Instance, HResInfo));
    Marshal.Copy(IntPtr.Create(HGlobal), FMemory, 0, FSize);
  finally
                                        
{$IFNDEF CF}
    FreeResource(HGlobal);
{$ENDIF}
  end;
end;

procedure TResourceStream.SetSize(NewSize: Int64);
var
  OldPosition: Longint;
begin
  OldPosition := FPosition;
  FSize := NewSize;
  SetLength(FMemory, FSize);
  if OldPosition > FSize then
    Seek(0, soEnd);
end;

function TResourceStream.Write(const Buffer: array of Byte; Offset, Count: Longint): Longint;
begin
  raise EStreamError.Create(SCantWriteResourceStreamError);
end;

{ TStreamToCLRStream }

constructor TStreamToCLRStream.Create(Stream: TStream);
begin
  inherited Create;
  FStream := Stream;
end;

procedure TStreamToCLRStream.Close;
begin
  FStream.Free;
  FStream := nil;
end;

procedure TStreamToCLRStream.Flush;
begin
  // Nothing applicable
end;

function TStreamToCLRStream.get_CanRead: Boolean;
begin
  Result := True;
end;

function TStreamToCLRStream.get_CanSeek: Boolean;
begin
  Result := True;
end;

function TStreamToCLRStream.get_CanWrite: Boolean;
begin
  Result := True;
end;

function TStreamToCLRStream.get_Length: Int64;
begin
  Result := FStream.Size;
end;

function TStreamToCLRStream.get_Position: Int64;
begin
  Result := FStream.Position;
end;

function TStreamToCLRStream.Read(Buffer: TBytes; Offset: Integer; Count: Integer): Integer;
begin
  Result := FStream.Read(Buffer, Offset, Count);
end;

function TStreamToCLRStream.Seek(Offset: Int64; Origin: System.IO.SeekOrigin): Int64;
var
  LOrigin: TSeekOrigin;
begin
  case Origin of
    SeekOrigin.Current:
      LOrigin := soCurrent;
    SeekOrigin.End:
      LOrigin := soEnd;
  else
    LOrigin := soBeginning;
  end;
  Result := FStream.Seek(Offset, LOrigin);
end;

procedure TStreamToCLRStream.SetLength(Value: Int64);
begin
  FStream.Size := Value;
end;

procedure TStreamToCLRStream.set_Position(Value: Int64);
begin
  FStream.Position := Value;
end;

procedure TStreamToCLRStream.Write(Buffer: TBytes; Offset: Integer; Count: Integer);
begin
  FStream.Write(Buffer, Offset, Count);
end;

destructor TStreamToCLRStream.Destroy;
begin
  FStream.Free;
  inherited;
end;

class function TStreamToCLRStream.GetStream(Stream: TStream): System.IO.Stream;
begin
  if Stream is TCLRStreamWrapper then
    Result := TCLRStreamWrapper(Stream).Handle
  else
    Result := TStreamToCLRStream.Create(Stream);
end;

{==============================================================================}

{ TIniFile }

constructor TIniFile.Create(const FileName: string);
begin
  inherited Create;
  FFileName := FileName;
end;

destructor TIniFile.Destroy;
begin
  UpdateFile;         // flush changes to disk
  inherited Destroy;
end;

function TIniFile.SectionExists(const Section: string): Boolean;
var
  S: TStrings;
begin
  S := TStringList.Create;
  ReadSection(Section, S);
  Result := S.Count > 0;
end;

function TIniFile.ReadInteger(const Section, Ident: string;
  Default: Longint): Longint;
var
  IntStr: string;
begin
  IntStr := ReadString(Section, Ident, '');
  if (Length(IntStr) > 2) and (IntStr[1] = '0') and
     ((IntStr[2] = 'X') or (IntStr[2] = 'x')) then
    IntStr := '$' + Copy(IntStr, 3, Maxint);
  Result := StrToIntDef(IntStr, Default);
end;

procedure TIniFile.WriteInteger(const Section, Ident: string; Value: Longint);
begin
  WriteString(Section, Ident, Value.ToString());
end;

function TIniFile.ReadBool(const Section, Ident: string;
  Default: Boolean): Boolean;
begin
  Result := ReadInteger(Section, Ident, Ord(Default)) <> 0;
end;

function TIniFile.ReadDateTime(const Section, Name: string; Default: DateTime): DateTime;
var
  DateStr: string;
begin
  DateStr := ReadString(Section, Name, '');
  Result := Default;
  if DateStr <> '' then
  try
    Result := DateTime.Parse(DateStr);
  except
    on EConvertError do
      // Ignore EConvertError exceptions
    else
      raise;
  end;
end;

function TIniFile.ReadFloat(const Section, Name: string; Default: Double): Double;
var
  FloatStr: string;
begin
  FloatStr := ReadString(Section, Name, '');
  Result := Default;
  if FloatStr <> '' then
  try
    Result := System.Double.Parse(FloatStr);
  except
    on EConvertError do
      // Ignore EConvertError exceptions
    else
      raise;
  end;
end;

procedure TIniFile.WriteDateTime(const Section, Name: string; Value: DateTime);
begin
  WriteString(Section, Name, Value.ToString());
end;

procedure TIniFile.WriteFloat(const Section, Name: string; Value: Double);
begin
  WriteString(Section, Name, Value.ToString());
end;

const
  Values: array[Boolean] of string = ('0', '1');

procedure TIniFile.WriteBool(const Section, Ident: string; Value: Boolean);
begin
  WriteString(Section, Ident, Values[Value]);
end;

function TIniFile.ValueExists(const Section, Ident: string): Boolean;
var
  S: TStrings;
begin
  S := TStringList.Create;
  ReadSection(Section, S);
  Result := S.IndexOf(Ident) > -1;
end;

function TIniFile.ReadBinaryStream(const Section, Name: string;
  Value: TStream): Integer;
var
  Text: string;
  Stream: TMemoryStream;
  Pos: Integer;
begin
  Text := ReadString(Section, Name, '');
  if Text <> '' then begin
    if Value is TMemoryStream
    then Stream := TMemoryStream(Value)
    else Stream := TMemoryStream.Create;

    try
      Pos := Stream.Position;
      Stream.SetSize(Stream.Size + Length(Text) div 2);
      HexToBin(BytesOf(Text), 0, Stream.Memory, Stream.Position, Length(Text) div 2);
      Stream.Position := Pos;
      if Value <> Stream then
        Value.CopyFrom(Stream, Length(Text) div 2);
      Result := Stream.Size - Pos;
    finally
      if Value <> Stream then Stream.Free;
    end;
  end else Result := 0;
end;

procedure TIniFile.WriteBinaryStream(const Section, Name: string;
  Value: TStream);
var
  Text: string;
  Stream: TMemoryStream;
  Buffer: TBytes;
begin
  SetLength(Text, (Value.Size - Value.Position) * 2);
  if Length(Text) > 0 then begin
    if Value is TMemoryStream
    then Stream := TMemoryStream(Value)
    else Stream := TMemoryStream.Create;

    try
      if Stream <> Value then begin
        Stream.CopyFrom(Value, Value.Size - Value.Position);
        Stream.Position := 0;
      end;
      SetLength(Buffer, Stream.Size * 2);
      BinToHex(Stream.Memory, Stream.Position, Buffer, 0, Stream.Size - Stream.Position);
      Text := StringOf(Buffer);
    finally
      if Value <> Stream then Stream.Free;
    end;
  end;
  WriteString(Section, Name, Text);
end;

procedure TIniFile.ReadSections(const Section: string;
  Strings: TStrings);
var
  I: Integer;
begin
  ReadSections(Strings);
  for I := Strings.Count - 1 downto 0 do
    if not SameText(Section, Copy(Strings[I], 1, Length(Section))) then
      Strings.Delete(I);
end;

function TIniFile.ReadString(const Section, Ident, Default: string): string;
var
  Buffer: StringBuilder;
begin
  Buffer := StringBuilder.Create(2048);
  if GetPrivateProfileString(Section, Ident, Default, Buffer, Buffer.Capacity, Filename) <> 0 then
    Result := Buffer.ToString
  else
    Result := '';
end;

procedure TIniFile.WriteString(const Section, Ident, Value: string);
begin
  if not WritePrivateProfileString(Section, Ident, Value, FileName) then
    raise EIniFileException.Create(System.&String.Format(SIniFileWriteError, [FileName]));
end;

const
  BufSize = 16384;

procedure TIniFile.ReadSections(Strings: TStrings);
var
  S, LB: string;
  Count: Integer;
  Buffer: TBytes;
begin
  SetLength(Buffer, BufSize);
  Count := GetPrivateProfileString(nil, nil, nil, Buffer, Length(Buffer), Filename);
  if Count <> 0 then
  begin
    S := PlatformStringOf(Buffer);
    SetLength(S, Count);
    LB := Strings.LineBreak;
    Strings.LineBreak := #0;
    Strings.Text := S;
    Strings.LineBreak := LB;
  end
  else
    Strings.Clear;
end;

procedure TIniFile.ReadSection(const Section: string; Strings: TStrings);
var
  S, LB: string;
  Count: Integer;
  Buffer: TBytes;
begin
  SetLength(Buffer, BufSize);
  Count := GetPrivateProfileString(Section, nil, nil, Buffer, Length(Buffer), Filename);
  if Count <> 0 then
  begin
    S := PlatformStringOf(Buffer);
    SetLength(S, Count);
    LB := Strings.LineBreak;
    Strings.LineBreak := #0;
    Strings.Text := S;
    Strings.LineBreak := LB;
  end
  else
    Strings.Clear;
end;

procedure TIniFile.ReadSectionValues(const Section: string; Strings: TStrings);
var
  KeyList: TStringList;
  I: Integer;
begin
  KeyList := TStringList.Create;
  ReadSection(Section, KeyList);
  Strings.BeginUpdate;
  try
    Strings.Clear;
    for I := 0 to KeyList.Count - 1 do
      Strings.Add(KeyList[I] + '=' + ReadString(Section, KeyList[I], ''))
  finally
    Strings.EndUpdate;
  end;
end;

procedure TIniFile.EraseSection(const Section: string);
begin
  if not WritePrivateProfileString(Section, nil, nil, FileName) then
    raise EIniFileException.Create(System.&String.Format(SIniFileWriteError, [FileName]));
end;

procedure TIniFile.DeleteKey(const Section, Ident: String);
begin
  WritePrivateProfileString(Section, Ident, nil, FileName);
end;

procedure TIniFile.UpdateFile;
begin
  WritePrivateProfileString(nil, nil, nil, FileName);
end;

end.
