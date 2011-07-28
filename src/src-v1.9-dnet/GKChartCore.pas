unit GKChartCore; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.IO, System.Drawing, System.Windows.Forms, System.Drawing.Imaging,
  VCLStub, GedCom551, GKEngine, GKCommon, GKCtrls, GKLists, GKUtils, GraphCore,
  GKLangs;

type
  TChartFilter = class(TFilter)
  public
    type
      TBranchCut = (bcNone, bcYears, bcPersons);
  private
    Back_SourceMode: TFilter.TGroupMode;
    Back_SourceRef: string;

    Back_BranchCut: TBranchCut;
    Back_BranchYear: Integer;
    Back_BranchPersons: string;
  public
    SourceMode: TFilter.TGroupMode;
    SourceRef: string;

    BranchCut: TBranchCut;
    BranchYear: Integer;
    BranchPersons: string;

    constructor Create();

    procedure Clear();
    procedure Backup();
    procedure Restore();
  end;

  TPerson = class;

  TCustomChartBox = class(System.Windows.Forms.Panel)
  public
    type
      TChartKind = (ckAncestors, ckDescendants, ckBoth);
  private
    // control fields
    FBorderWidth: Integer;
    FImageHeight: Integer;
    FImageWidth: Integer;
    FKind: TChartKind;
    FLeftPos: Integer;
    FTopPos: Integer;
    FRange: TPoint;
    FSPX, FSPY: Integer;

    // chart fields
    FEngine: TGenEngine;
    FFilter: TPersonsFilter;
    FOptions: TChartOptions;
    FRoot: TPerson;
    FSelected: TPerson;
    FShieldState: TGenEngine.TShieldState;
    FTree: TGEDCOMTree;

    function DoScroll(nBar, aOldPos, aMin, aMax: Integer; scrType: ScrollEventType): Integer;
    procedure ScrollRange();
    procedure SetBorderWidth(Value: Integer);
    procedure SetSelected(const Value: TPerson);
    procedure SetLeftPos(Value: Integer);
    procedure SetTopPos(Value: Integer);
  strict protected
    procedure Draw(aCanvas: System.Drawing.Graphics; aPerson: TPerson; aDirKind: TChartKind); virtual; abstract;
    procedure InternalGenChart(aPerson: TGEDCOMIndividualRecord; aKind: TChartKind); virtual; abstract;
    procedure OnMouseDown(e: MouseEventArgs); override;
    procedure OnPaint(pe: PaintEventArgs); override;
    procedure WndProc(var m: System.Windows.Forms.Message); override;
  protected
    FDrawFont: System.Drawing.Font;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure GenChart(aPerson: TGEDCOMIndividualRecord; aKind: TChartKind);
    procedure SelectBy(aX, aY: Integer); virtual; abstract;
    procedure TreeDraw(aCanvas: System.Drawing.Graphics; Default: Boolean); virtual;

    property BorderWidth: Integer read FBorderWidth write SetBorderWidth;
    property Engine: TGenEngine read FEngine write FEngine;
    property Filter: TPersonsFilter read FFilter write FFilter;
    property LeftPos: Integer read FLeftPos write SetLeftPos;
    property Options: TChartOptions read FOptions write FOptions;
    property Root: TPerson read FRoot;
    property Selected: TPerson read FSelected write SetSelected;
    property ShieldState: TGenEngine.TShieldState read FShieldState write FShieldState;
    property TopPos: Integer read FTopPos write SetTopPos;
    property Tree: TGEDCOMTree read FTree write FTree;
  end;

  TPersonList = class;

  TCustomPerson = class(TObject)
  public
    type
      TPersonFlag = (pfDivorced, pfIsDead, pfSelected, pfDescByFather, pfDescByMother);
      TPersonFlags = TEnumSet;
      TPersonKind = (pkDefault, pkSpouse);

  private
    FBirthDate, FBirthYear: string;
    FChart: TCustomChartBox;
    FDeathDate, FDeathYear: string;
    FFamily: string;
    FFlags: TPersonFlags;
    FHeight: Integer;
    FKinship: string;
    FName: string;
    FPathDebug: string;
    FPatronymic: string;
    FPortrait: System.Drawing.Bitmap;
    FPortraitWidth: Longint;
    FPtX, FPtY: Longint;
    FRec: TGEDCOMIndividualRecord;
    FSex: TGEDCOMObject.TGEDCOMSex;
    FSigns: TGenEngine.TChartPersonSigns;
    FWidth: Integer;

    function TextWidth(g: System.Drawing.Graphics; st: string): Integer;
    procedure DrawBorder(aCanvas: System.Drawing.Graphics; xpen: System.Drawing.Pen;
      rt: TRect; dead: Boolean);
    procedure xOffsetRect(var Rect: TRect; DX: Integer; DY: Integer);
    procedure TextOut(aCanvas: System.Drawing.Graphics;
      rt: TRect; s: string; h: Integer; var line: Integer);

    procedure CalcBounds();
    function GetDestRect(rt: TRect; Portrait: System.Drawing.Bitmap): Rectangle;
    function GetDivorced: Boolean;
    function GetIsDead: Boolean;
    function GetPt: TPoint;
    function GetRect(): TRect;
    function GetSelected: Boolean;
    procedure SetDivorced(const Value: Boolean);
    procedure SetIsDead(const Value: Boolean);
    procedure SetKinship(const Value: string);
    procedure SetPt(const Value: TPoint);
    procedure SetSelected(const Value: Boolean);
  protected
    function GetFullName: string;
    function GetLifeYears: string;
  public
    constructor Create(aChart: TCustomChartBox); virtual;
    destructor Destroy; override;

    procedure BuildBy(iRec: TGEDCOMIndividualRecord);
    procedure Draw(aCanvas: System.Drawing.Graphics; SPX, SPY: Integer); virtual;

    property BirthDate: string read FBirthDate;
    property DeathDate: string read FDeathDate;
    property Divorced: Boolean read GetDivorced write SetDivorced;
    property Family: string read FFamily;
    property Height: Integer read FHeight;
    property IsDead: Boolean read GetIsDead write SetIsDead;
    property Kinship: string read FKinship write SetKinship;
    property Name: string read FName;
    property Patronymic: string read FPatronymic;
    property Pt: TPoint read GetPt write SetPt;
    property PtX: Longint read FPtX write FPtX;
    property PtY: Longint read FPtY write FPtY;
    property Rec: TGEDCOMIndividualRecord read FRec;
    property Rect: TRect read GetRect;
    property Selected: Boolean read GetSelected write SetSelected;
    property Sex: TGEDCOMObject.TGEDCOMSex read FSex;
    property Signs: TGenEngine.TChartPersonSigns read FSigns;
    property Width: Integer read FWidth;
  end;

  TPerson = class(TCustomPerson)
  private
    FBaseSpouse: TPerson;
    FChilds: TPersonList;
    FFather: TPerson;
    FGeneration: Integer;
    FKind: TCustomPerson.TPersonKind;
    FMother: TPerson;
    FNode: TGraph.TGraphNode;
    FSpouses: TPersonList;

    function GetChild(Index: Integer): TPerson;
    function GetChildsCount: Integer;
    function GetSpouse(Index: Integer): TPerson;
    function GetSpousesCount: Integer;
  public
    Parent: TPerson;

    constructor Create(aChart: TCustomChartBox); override;
    destructor Destroy; override;

    procedure AddChild(aChild: TPerson);
    procedure AddSpouse(aSpouse: TPerson);

    property BaseSpouse: TPerson read FBaseSpouse write FBaseSpouse;
    property Childs[Index: Integer]: TPerson read GetChild;
    property ChildsCount: Integer read GetChildsCount;
    property Father: TPerson read FFather write FFather;
    property Generation: Integer read FGeneration write FGeneration;
    property Kind: TCustomPerson.TPersonKind read FKind write FKind;
    property Mother: TPerson read FMother write FMother;
    property Spouses[Index: Integer]: TPerson read GetSpouse;
    property SpousesCount: Integer read GetSpousesCount;
  end;

  TPersonList = class(TObjectList)
  private
    function GetItem(Index: Integer): TPerson;
    procedure SetItem(Index: Integer; const Value: TPerson);
  public
    property Items[Index: Integer]: TPerson read GetItem write SetItem; default;
  end;

  TAncestryChartBox = class(TCustomChartBox)
  private
    type
      TEdges = array [Byte] of Longint;

      TRelLink = class(TObject)
      public
        xFrom, xTo: TPerson;
        xRel: TGenEngine.TRelationKind; // кто есть xTo по отношению к xFrom
      end;

    const
      SignsData: array [TGenEngine.TChartPersonSign] of string = (
        'GEORGE_CROSS', 'SOLDIER', 'SOLDIER_FALL', 'VETERAN_REAR');

    const
      ACSpouseDistance = 10;
      ACBranchDistance = 40;
      ACLevelDistance = 46;
      ACMargin = 40;

    var
      FBranchDistance: Integer;
      FDepthLimit: Integer;
      FHMax, FWMax: Integer;
      FFilter: TChartFilter;
      FGraph: TGraph;
      FKinRoot: TPerson;
      FLevelDistance: Integer;
      FMargin: Integer;
      FPathDebug: Boolean;
      FPersons: TPersonList;
      FScale: Integer;
      FSpouseDistance: Integer;

    procedure InitSigns();
    procedure DoneSigns();

    function IsChildless(iRec: TGEDCOMIndividualRecord): Boolean;
    function AddDescPerson(aParent: TPerson; iRec: TGEDCOMIndividualRecord;
      aKind: TCustomPerson.TPersonKind; aGeneration: Integer): TPerson;

    function DoAncestorsStep(aChild: TPerson; aPerson: TGEDCOMIndividualRecord; aGeneration: Integer): TPerson;
    function DoDescendantsStep(aParent: TPerson; aPerson: TGEDCOMIndividualRecord; aLevel: Integer): TPerson;
    function FindRelationship(aTarget: TPerson): string;
    procedure FixLink(path: TObjectList; f, t: TPerson; rel: TGenEngine.TRelationKind);
    function FixRelation(aTarget: TPerson; Rel: TGenEngine.TRelationKind; Great: Integer): string;
    function GetGreat(n: Integer): string;
    procedure InitEdges(var edges: TEdges);
    procedure Line(aCanvas: System.Drawing.Graphics; X1, Y1, X2, Y2: Integer);
    procedure Predef();

    procedure RecalcAncestorsChart();

    procedure RecalcDescendantsChart(aPreDef: Boolean);
    procedure RecalcChart();

    procedure ShiftAnc(edges: TEdges; aPerson: TPerson; aOffset: Integer);
    procedure RecalcAnc(prev: TList;edges: TEdges; aPerson: TPerson; aPt: TPoint);

    procedure ShiftDesc(aPerson: TPerson; aOffset: Integer; aSingle: Boolean);
    procedure RecalcDescChilds(edges: TEdges; aPerson: TPerson);
    procedure RecalcDesc(edges: TEdges; aPerson: TPerson; aPt: TPoint; aPreDef: Boolean = True);

    procedure DrawAncestors(aCanvas: System.Drawing.Graphics; aPerson: TPerson);
    procedure DrawDescendants(aCanvas: System.Drawing.Graphics; aPerson: TPerson);
  strict protected
    procedure Draw(aCanvas: System.Drawing.Graphics; aPerson: TPerson; aDirKind: TCustomChartBox.TChartKind); override;
    procedure InternalGenChart(aPerson: TGEDCOMIndividualRecord; aKind: TCustomChartBox.TChartKind); override;
  public
    var
      SignsPic: array [TGenEngine.TChartPersonSign] of System.Drawing.Bitmap;

    constructor Create; override;
    destructor Destroy; override;

    procedure DoFilter(aRoot: TGEDCOMIndividualRecord);
    function GetPersonSign(iRec: TGEDCOMIndividualRecord): TGenEngine.TChartPersonSigns;
    procedure RebuildKinships();
    procedure SaveSnapshot(const aFileName: string);
    procedure SelectBy(aX, aY: Integer); override;
    procedure SelectByRec(iRec: TGEDCOMIndividualRecord);

    property BranchDistance: Integer read FBranchDistance write FBranchDistance;
    property DepthLimit: Integer read FDepthLimit write FDepthLimit;
    property Filter: TChartFilter read FFilter;
    property Margin: Integer read FMargin write FMargin;
    property PathDebug: Boolean read FPathDebug write FPathDebug;
    property Scale: Integer read FScale write FScale;
  end;

implementation

{$R .\res\gk.res}

{ TCustomChartBox }

constructor TCustomChartBox.Create;
begin
  inherited Create;
  BorderStyle := System.Windows.Forms.BorderStyle.Fixed3D;
  TabStop := True;

  TopPos := 0;
  LeftPos := 0;
end;

destructor TCustomChartBox.Destroy;
begin
  inherited Destroy;
end;

procedure TCustomChartBox.GenChart(aPerson: TGEDCOMIndividualRecord; aKind: TChartKind);
begin
  InternalGenChart(aPerson, aKind);
  ScrollRange();
end;

procedure TCustomChartBox.TreeDraw(aCanvas: System.Drawing.Graphics; Default: Boolean);
begin
  aCanvas.FillRectangle(System.Drawing.SolidBrush.Create(System.Drawing.Color.White), ClientRectangle);

  if (Default) then begin
    FSPX := FBorderWidth - FLeftPos;
    FSPY := FBorderWidth - FTopPos;

    if (FImageWidth < ClientRectangle.Width)
    then FSPX := FSPX + (ClientRectangle.Width - FImageWidth) div 2;

    if (FImageHeight < ClientRectangle.Height)
    then FSPY := FSPY + (ClientRectangle.Height - FImageHeight) div 2;
  end else begin
    FSPX := 0;
    FSPY := 0;
  end;

  Draw(aCanvas, FRoot, FKind);
end;

procedure TCustomChartBox.OnPaint(pe: PaintEventArgs);
begin
  inherited OnPaint(pe);
  TreeDraw(pe.Graphics, True);
end;

procedure TCustomChartBox.SetBorderWidth(Value: Integer);
begin
  if (FBorderWidth <> Value) then begin
    FBorderWidth := Value;
    Invalidate;
  end;
end;

procedure TCustomChartBox.OnMouseDown(e: MouseEventArgs); 
begin
  inherited OnMouseDown(e);
  if not Focused then Focus();
end;

procedure TCustomChartBox.ScrollRange();
begin
  if (FImageWidth < ClientRectangle.Width) then begin
    FRange.X := 0;
    LeftPos := (ClientRectangle.Width - FImageWidth) div 2;
  end else FRange.X := FImageWidth - ClientRectangle.Width;

  if (FImageHeight < ClientRectangle.Height) then begin
    FRange.Y := 0;
    TopPos := (ClientRectangle.Height - FImageHeight) div 2;
  end else FRange.Y := FImageHeight - ClientRectangle.Height;

  SetScrollRange(Handle.ToInt32, SB_HORZ, 0, FRange.X, False);
  SetScrollRange(Handle.ToInt32, SB_VERT, 0, FRange.Y, False);

  Invalidate;
end;

procedure TCustomChartBox.WndProc(var m: System.Windows.Forms.Message);
var
  new_pos: Integer;
  wParam: Cardinal;
  scrType: ScrollEventType;
begin
  inherited WndProc(m);

  if (m.Msg = WM_SIZE) then begin
    ScrollRange();
  end
  else
  if (m.Msg = WM_GETDLGCODE) then begin
    m.Result := IntPtr(m.Result.ToInt32
      or DLGC_WANTARROWS or DLGC_WANTTAB or DLGC_WANTCHARS or DLGC_WANTALLKEYS);
  end
  else
  if (m.Msg = WM_HSCROLL) then begin
    wParam := m.WParam.ToInt32();
    scrType := TGKUtils.GetScrollEventType(wParam and $ffff);

    new_pos := DoScroll(SB_HORZ, LeftPos, 0, FRange.X, scrType);
    SetLeftPos(new_pos);

    {if (HorzScrollValueChanged != null)
    then HorzScrollValueChanged(this, new ScrollEventArgs(scrType, (int)(wParam >> 16)));}
  end
  else
  if (m.Msg = WM_VSCROLL) then begin
    wParam := m.WParam.ToInt32();
    scrType := TGKUtils.GetScrollEventType(wParam and $ffff);

    new_pos := DoScroll(SB_VERT, TopPos, 0, FRange.Y, scrType);
    SetTopPos(new_pos);

    {if (VertScrollValueChanged != null)
    then VertScrollValueChanged(this, new ScrollEventArgs(scrType, (int)(wParam >> 16)));}
  end;
end;

function TCustomChartBox.DoScroll(nBar, aOldPos, aMin, aMax: Integer; scrType: ScrollEventType): Integer;
var
  NewPos: Longint;
  ScrollInfo: TScrollInfo;
begin
  NewPos := aOldPos;

  case scrType of
    ScrollEventType.SmallDecrement:
      Dec(NewPos, 1);

    ScrollEventType.SmallIncrement:
      Inc(NewPos, 1);

    ScrollEventType.LargeDecrement:
      Dec(NewPos, 1);

    ScrollEventType.LargeIncrement:
      Inc(NewPos, 1);

    ScrollEventType.ThumbPosition, ScrollEventType.ThumbTrack:
      begin
        ScrollInfo.cbSize := SizeOf(ScrollInfo);
        ScrollInfo.fMask := SIF_ALL;
        GetScrollInfo(Handle.ToInt32, nBar, ScrollInfo);
        NewPos := ScrollInfo.nTrackPos;
      end;

    ScrollEventType.First:
      NewPos := 0;

    ScrollEventType.Last:
      NewPos := aMax;

    ScrollEventType.EndScroll: ;
  end;

  if (NewPos < aMin) then NewPos := aMin;
  if (NewPos > aMax) then NewPos := aMax;

  Result := NewPos;
end;

procedure TCustomChartBox.SetTopPos(Value: Integer);
var
  dummy, R: TRect;
begin
  if (Value < 0) then Value := 0;
  if (Value > FRange.Y) then Value := FRange.Y;

  if (FTopPos <> Value) then begin
    dummy.Empty();
    ScrollWindowEx(Handle.ToInt32, 0, FTopPos - Value, dummy, dummy, 0, R, 0);
    SetScrollPos(Handle.ToInt32, SB_VERT, FTopPos, True);

    Invalidate;
    FTopPos := Value;
  end;
end;

procedure TCustomChartBox.SetLeftPos(Value: Integer);
var
  dummy, R: TRect;
begin
  if (Value < 0) then Value := 0;
  if (Value > FRange.X) then Value := FRange.X;

  if (FLeftPos <> Value) then begin
    dummy.Empty();
    ScrollWindowEx(Handle.ToInt32, FLeftPos - Value, 0, dummy, dummy, 0, R, 0);
    SetScrollPos(Handle.ToInt32, SB_HORZ, FLeftPos, True);

    Invalidate;
    FLeftPos := Value;
  end;
end;

procedure TCustomChartBox.SetSelected(const Value: TPerson);
begin
  if (FSelected <> nil) then FSelected.Selected := False;
  FSelected := Value;
  if (FSelected <> nil) then FSelected.Selected := True;

  Invalidate;
end;

{==============================================================================}

{ TCustomPerson }

constructor TCustomPerson.Create(aChart: TCustomChartBox);
begin
  inherited Create();
  FChart := aChart;
  FPortrait := nil;
end;

destructor TCustomPerson.Destroy;
begin
  if (FPortrait <> nil) then FPortrait.Free;

  inherited Destroy;
end;

procedure TCustomPerson.BuildBy(iRec: TGEDCOMIndividualRecord);
var
  fam, nam, pat: string;
begin
  FRec := iRec;

  if (iRec <> nil) then begin
    TGenEngine.GetNameParts(iRec, fam, nam, pat);

    FFamily := fam;
    FName := nam;
    FPatronymic := pat;
    FBirthDate := TGenEngine.GetBirthDate(iRec, dfDD_MM_YYYY);
    FDeathDate := TGenEngine.GetDeathDate(iRec, dfDD_MM_YYYY);
    IsDead := not(iRec.IsLive());
    FSex := iRec.Sex;
    FSigns := TAncestryChartBox(FChart).GetPersonSign(iRec);

    FBirthYear := TGenEngine.GetBirthDate(iRec, dfYYYY);
    FDeathYear := TGenEngine.GetDeathDate(iRec, dfYYYY);

    if (FChart.Options.PortraitsVisible)
    then FPortrait := FChart.FEngine.GetPrimaryBitmap(iRec);
  end else begin
    FFamily := '';
    FName := '< ? >';
    FPatronymic := '';
    FBirthDate := '';
    FDeathDate := '';
    IsDead := False;
    FSex := svNone;
    FSigns := [];
  end;

  CalcBounds();
end;

function TCustomPerson.GetLifeYears: string;
begin
  if (FBirthYear = '')
  then Result := '?'
  else Result := FBirthYear;

  if (IsDead) then begin
    if (FDeathYear = '')
    then Result := Result + ' - ?'
    else Result := Result + ' - ' + FDeathYear;
  end;

  Result := '[ ' + Result + ' ]';
end;

function TCustomPerson.GetPt: TPoint;
begin
  Result.X := FPtX;
  Result.Y := FPtY;
end;

procedure TCustomPerson.SetPt(const Value: TPoint);
begin
  FPtX := Value.X;
  FPtY := Value.Y;
end;

function TCustomPerson.GetFullName: string;
begin
  Result := FName + ' ' + FPatronymic;
end;

function TCustomPerson.GetDestRect(rt: TRect; Portrait: System.Drawing.Bitmap): Rectangle;
var
  w, h, cw, ch: Integer;
  xyaspect: Double;
begin
  w := Portrait.Width;
  h := Portrait.Height;
  cw := (rt.Right - rt.Left) + 1;
  ch := (rt.Bottom - rt.Top) + 1;

  // always stretch and proportional
  xyaspect := w / h;
  if (w > h) then begin
    w := cw;
    h := Trunc(cw / xyaspect);
    if (h > ch) then begin // woops, too big
      h := ch;
      w := Trunc(ch * xyaspect);
    end;
  end else begin
    h := ch;
    w := Trunc(ch * xyaspect);
    if (w > cw) then begin // woops, too big
      w := cw;
      h := Trunc(cw / xyaspect);
    end;
  end;

  Result := Rectangle.Create(rt.Left, rt.Top, w, h);
  Result.Offset((cw - w) div 2, (ch - h) div 2);
end;

procedure TCustomPerson.DrawBorder(aCanvas: System.Drawing.Graphics; xpen: System.Drawing.Pen;
  rt: TRect; dead: Boolean);
var
  b_color: System.Drawing.Color;
  rect: System.Drawing.Rectangle;
begin
  rect := rt.ToRectangle();

  case FSex of
    svMale: begin
      if not(dead) then begin
        if Divorced
        then b_color := FChart.Options.UnHusbandColor
        else b_color := FChart.Options.MaleColor;
      end else b_color := System.Drawing.Color.Black;

      aCanvas.FillRectangle(SolidBrush.Create(b_color), rect);
      aCanvas.DrawRectangle(xpen, rect);
    end;

    svFemale: begin
      if not(dead) then begin
        if Divorced
        then b_color := FChart.Options.UnWifeColor
        else b_color := FChart.Options.FemaleColor;
      end else b_color := System.Drawing.Color.Black;

      aCanvas.FillRectangle(SolidBrush.Create(b_color), rect);
      aCanvas.DrawRectangle(xpen, rect);
    end;

    svNone, svUndetermined: begin
      if not(dead) then begin
        b_color := FChart.Options.UnkSexColor;
      end else b_color := System.Drawing.Color.Black;

      aCanvas.FillRectangle(SolidBrush.Create(b_color), rect);
      aCanvas.DrawRectangle(xpen, rect);
    end;
  end;
end;

procedure TCustomPerson.xOffsetRect(var Rect: TRect; DX: Integer; DY: Integer);
begin
  Inc(Rect.Left, DX);
  Dec(Rect.Right, DX);

  Inc(Rect.Top, DY);
  Dec(Rect.Bottom, DY);
end;

procedure TCustomPerson.TextOut(aCanvas: System.Drawing.Graphics;
  rt: TRect; s: string; h: Integer; var line: Integer);
var
  rx, ry, rw, stw: Integer;
begin
  rw := rt.Right - rt.Left + 1;

  stw := aCanvas.MeasureString(s, FChart.FDrawFont).ToSize().Width;
  rx := rt.Left + (rw - stw) div 2;
  ry := rt.Top + 10 + (h * line);

  aCanvas.DrawString(s, FChart.FDrawFont, SolidBrush.Create(Color.Black), rx, ry);

  Inc(line);
end;

procedure TCustomPerson.Draw(aCanvas: System.Drawing.Graphics; SPX, SPY: Integer);
var
  xpen: System.Drawing.Pen;
var
  rt, dt, port_rt: TRect;
  h, line, i: Integer;
  cps: TGenEngine.TChartPersonSign;
  has_port: Boolean;
  pen_color: System.Drawing.Color;
  pen_width: Integer;
  pic: System.Drawing.Bitmap;
begin
  rt := GetRect();
  rt := rt.GetOffset(SPX, SPY);
  h := aCanvas.MeasureString('A', FChart.FDrawFont).ToSize().Height;
  has_port := (FChart.Options.PortraitsVisible) and (FPortrait <> nil);

  xpen := Pen.Create(Color.Black, 1);

  if (IsDead) then begin
    dt := rt;
    dt := dt.GetOffset(-2, -2);
    DrawBorder(aCanvas, xpen, dt, True);
  end;

  if (Selected) then begin
    pen_width := 2;
    case FSex of
      svMale: pen_color := Color.Blue;
      svFemale: pen_color := Color.Red;
      svNone, svUndetermined: pen_color := Color.Black;
    end;
    xpen := Pen.Create(pen_color, pen_width);
  end;

  DrawBorder(aCanvas, xpen, rt, False);

  xpen := Pen.Create(Color.Black, 1);

  if (has_port) then begin
    port_rt := rt;
    port_rt.Right := port_rt.Left + FPortraitWidth;
    xOffsetRect(port_rt, 3, 3);
    aCanvas.DrawImage(FPortrait, GetDestRect(port_rt, FPortrait));
    rt.Left := rt.Left + FPortraitWidth;
  end;

  line := 0;

  TextOut(aCanvas, rt, FFamily, h, line);

  if not(FChart.Options.DiffLines) then begin
    TextOut(aCanvas, rt, GetFullName(), h, line);
  end else begin
    TextOut(aCanvas, rt, FName, h, line);
    TextOut(aCanvas, rt, FPatronymic, h, line);
  end;

  if not(FChart.Options.OnlyYears) then begin
    if (FChart.Options.BirthDateVisible)
    then TextOut(aCanvas, rt, FBirthDate, h, line);

    if (FChart.Options.DeathDateVisible)
    then TextOut(aCanvas, rt, FDeathDate, h, line);
  end else begin
    TextOut(aCanvas, rt, GetLifeYears(), h, line);
  end;

  if (FChart.Options.Kinship)
  then TextOut(aCanvas, rt, FKinship, h, line);

  if (FChart.Options.SignsVisible) and (FSigns <> []) then begin
    i := 0;
    for cps := Low(TGenEngine.TChartPersonSign) to High(TGenEngine.TChartPersonSign) do
      if (cps in FSigns) then begin
        pic := TAncestryChartBox(FChart).SignsPic[cps];
        aCanvas.DrawImage(pic, rt.Right, rt.Top - 21 + (i * (pic.Height)));
        Inc(i);
      end;
  end;

  if TAncestryChartBox(FChart).PathDebug
  then TextOut(aCanvas, rt, FPathDebug, h, line);
end;

procedure TCustomPerson.SetKinship(const Value: string);
begin
  FKinship := Value;
  CalcBounds();
end;

function TCustomPerson.GetRect(): TRect;
begin
  Result.Left := FPtX - (FWidth div 2);
  Result.Right := Result.Left + FWidth - 1;

  Result.Top := FPtY;
  Result.Bottom := Result.Top + FHeight - 1;
end;

function TCustomPerson.TextWidth(g: System.Drawing.Graphics; st: string): Integer;
begin
  Result := g.MeasureString(st, FChart.FDrawFont).ToSize().Width;
end;

procedure TCustomPerson.CalcBounds();
var
  g: System.Drawing.Graphics;
var
  wt, maxwid, lines: Integer;
  prt: Rectangle;
begin
  g := FChart.CreateGraphics();
  try
    lines := 2;
    maxwid := TextWidth(g, FFamily);
    if not(FChart.Options.DiffLines) then begin
      wt := TextWidth(g, GetFullName());
      if (maxwid < wt) then maxwid := wt;
    end else begin
      wt := TextWidth(g, FName);
      if (maxwid < wt) then maxwid := wt;
      wt := TextWidth(g, FPatronymic);
      if (maxwid < wt) then maxwid := wt;
      Inc(lines);
    end;

    if not(FChart.Options.OnlyYears) then begin
      if (FChart.Options.BirthDateVisible) then begin
        wt := TextWidth(g, FBirthDate);
        if (maxwid < wt) then maxwid := wt;
        Inc(lines);
      end;

      if (FChart.Options.DeathDateVisible) then begin
        wt := TextWidth(g, FDeathDate);
        if (maxwid < wt) then maxwid := wt;
        Inc(lines);
      end;
    end else begin
      wt := TextWidth(g, GetLifeYears());
      if (maxwid < wt) then maxwid := wt;
      Inc(lines);
    end;

    if (FChart.Options.Kinship) then begin
      wt := TextWidth(g, FKinship);
      if (maxwid < wt) then maxwid := wt;
      Inc(lines);
    end;

    if TAncestryChartBox(FChart).PathDebug then begin
      wt := TextWidth(g, FPathDebug);
      if (maxwid < wt) then maxwid := wt;
      Inc(lines);
    end;

    FWidth := maxwid + 20;
    FHeight := g.MeasureString('A', FChart.FDrawFont).ToSize().Height * lines + 20;

    if (FChart.Options.PortraitsVisible) and (FPortrait <> nil) then begin
      prt := GetDestRect(TRect.Create(0, 0, FHeight - 1, FHeight - 1), FPortrait);
      FPortraitWidth := (prt.Right - prt.Left) + 1;
      FWidth := FWidth + FPortraitWidth;
    end;
  finally
    g.Dispose;
  end;
end;

function TCustomPerson.GetDivorced: Boolean;
begin
  Result := (FFlags.InSet(pfDivorced));
end;

procedure TCustomPerson.SetDivorced(const Value: Boolean);
begin
  if (Value)
  then FFlags.Include(pfDivorced)
  else FFlags.Exclude(pfDivorced);
end;

function TCustomPerson.GetIsDead: Boolean;
begin
  Result := (FFlags.InSet(pfIsDead));
end;

procedure TCustomPerson.SetIsDead(const Value: Boolean);
begin
  if (Value)
  then FFlags.Include(pfIsDead)
  else FFlags.Exclude(pfIsDead);
end;

function TCustomPerson.GetSelected: Boolean;
begin
  Result := (FFlags.InSet(pfSelected));
end;

procedure TCustomPerson.SetSelected(const Value: Boolean);
begin
  if (Value)
  then FFlags.Include(pfSelected)
  else FFlags.Exclude(pfSelected);
end;

{ TPerson }

constructor TPerson.Create(aChart: TCustomChartBox);
begin
  inherited Create(aChart);
  FSpouses := nil;
  FChilds := nil;
end;

destructor TPerson.Destroy;
begin
  if (FChilds <> nil) then FChilds.Free;
  if (FSpouses <> nil) then FSpouses.Free;
  inherited Destroy;
end;

procedure TPerson.AddChild(aChild: TPerson);
begin
  if (FChilds = nil)
  then FChilds := TPersonList.Create(False);

  if (aChild <> nil)
  then FChilds.Add(aChild);
end;

procedure TPerson.AddSpouse(aSpouse: TPerson);
begin
  if (FSpouses = nil)
  then FSpouses := TPersonList.Create(False);

  FSpouses.Add(aSpouse);
end;

function TPerson.GetChild(Index: Integer): TPerson;
begin
  if (FChilds = nil)
  then Result := nil
  else Result := FChilds.GetItem(Index);
end;

function TPerson.GetChildsCount: Integer;
begin
  if (FChilds = nil)
  then Result := 0
  else Result := FChilds.Count;
end;

function TPerson.GetSpouse(Index: Integer): TPerson;
begin
  if (FSpouses = nil)
  then Result := nil
  else Result := FSpouses.GetItem(Index);
end;

function TPerson.GetSpousesCount: Integer;
begin
  if (FSpouses = nil)
  then Result := 0
  else Result := FSpouses.Count;
end;

{ TPersonList }

function TPersonList.GetItem(Index: Integer): TPerson;
begin
  Result := TPerson(inherited Get(Index));
end;

procedure TPersonList.SetItem(Index: Integer; const Value: TPerson);
begin
  inherited Put(Index, Value);
end;

{ TAncestryChartBox }

constructor TAncestryChartBox.Create;
begin
  inherited Create;

  InitSigns();

  FPersons := TPersonList.Create(True);
  FFilter := TChartFilter.Create;
  FSpouseDistance := 10;
  FBranchDistance := 40;
  FLevelDistance := 46;
  FMargin := 40;
  FDepthLimit := -1;
  FSelected := nil;

  FGraph := TGraph.Create;
end;

destructor TAncestryChartBox.Destroy;
begin
  FGraph.Destroy;

  FFilter.Free;
  FPersons.Free;

  DoneSigns();

  inherited Destroy;
end;

procedure TAncestryChartBox.InitSigns();
var
  ps: TGenEngine.TChartPersonSign;
begin
  for ps := Low(TGenEngine.TChartPersonSign) to High(TGenEngine.TChartPersonSign) do begin
    try
      SignsPic[ps] := System.Drawing.Bitmap.FromResource(IntPtr(HInstance), SignsData[ps]);
      SignsPic[ps].MakeTransparent(SignsPic[ps].GetPixel(0, 0));
    finally
    end;
  end;
end;

procedure TAncestryChartBox.DoneSigns();
var
  ps: TGenEngine.TChartPersonSign;
begin
  for ps := Low(TGenEngine.TChartPersonSign) to High(TGenEngine.TChartPersonSign) do
    SignsData[ps].Free;
end;

function TAncestryChartBox.GetPersonSign(iRec: TGEDCOMIndividualRecord): TGenEngine.TChartPersonSigns;
var
  rs: string;
  cps: TGenEngine.TChartPersonSign;
  i: Integer;
begin
  Result := [];

  for i := 0 to iRec.UserReferencesCount - 1 do begin
    rs := iRec.UserReferences[i].StringValue;

    for cps := Low(TGenEngine.TChartPersonSign) to High(TGenEngine.TChartPersonSign) do
      if (rs = TGenEngine.UserRefs[cps].Name)
      then Include(Result, cps);
  end;
end;

procedure TAncestryChartBox.Predef();
var
  sc: Single;
  fsz: Integer;
  f_name: string;
begin
  sc := (FScale / 100);
  fsz := Round(FOptions.DefFont_Size * sc);
  if (fsz <= 7)
  then f_name := 'Small Fonts'
  else f_name := FOptions.DefFont_Name;

  FDrawFont := System.Drawing.Font.Create(f_name, fsz, FontStyle.Regular, GraphicsUnit.Point);

  FSpouseDistance := Round(ACSpouseDistance * sc);
  FBranchDistance := Round(ACBranchDistance * sc);
  FLevelDistance := Round(ACLevelDistance * sc);
  FMargin := Round(ACMargin * sc);
end;

procedure TAncestryChartBox.Line(aCanvas: System.Drawing.Graphics; X1, Y1, X2, Y2: Integer);
var
  sX1, sY1, sX2, sY2: Integer;
  xpen: System.Drawing.Pen;
begin
  sX1 := FSPX + X1;
  sX2 := FSPX + X2;
  sY1 := FSPY + Y1;
  sY2 := FSPY + Y2;

  aCanvas.DrawLine(Pen.Create(Color.Black, 1), sX1, sY1, sX2, sY2);

  if (FOptions.Decorative) then begin
    xpen := Pen.Create(Color.Silver, 1);
    try
      if (sX1 = sX2) then aCanvas.DrawLine(xpen, sX1 + 1, sY1 + 1, sX2 + 1, sY2 + 0)
      else
      if (sY1 = sY2) then aCanvas.DrawLine(xpen, sX1 + 1, sY1 + 1, sX2 + 0, sY2 + 1);
    finally
      xpen.Free;
    end;
  end;
end;

procedure TAncestryChartBox.DrawAncestors(aCanvas: System.Drawing.Graphics; aPerson: TPerson);
var
  cr_y: Integer;
begin
  Draw(aCanvas, aPerson.Father, ckAncestors);
  Draw(aCanvas, aPerson.Mother, ckAncestors);

  cr_y := aPerson.PtY - FLevelDistance div 2;

  if (aPerson.Father <> nil) or (aPerson.Mother <> nil) then begin
    Line(aCanvas, aPerson.PtX, aPerson.PtY, aPerson.PtX, cr_y);
  end;

  if (aPerson.Father <> nil) then begin
    Line(aCanvas, aPerson.Father.PtX, cr_y, aPerson.PtX, cr_y);
    Line(aCanvas, aPerson.Father.PtX, aPerson.Father.PtY + aPerson.Father.Height - 1, aPerson.Father.PtX, cr_y + 1);
  end;

  if (aPerson.Mother <> nil) then begin
    Line(aCanvas, aPerson.PtX, cr_y, aPerson.Mother.PtX, cr_y);
    Line(aCanvas, aPerson.Mother.PtX, aPerson.Mother.PtY + aPerson.Mother.Height - 1, aPerson.Mother.PtX, cr_y + 1);
  end;
end;

procedure TAncestryChartBox.DrawDescendants(aCanvas: System.Drawing.Graphics; aPerson: TPerson);
var
  cr_y, i, bpx, epx, cx, spb_beg, spb_ofs, spb_v: Integer;
  child_pt: TPoint;
begin
  for i := 0 to aPerson.ChildsCount - 1 do
    Draw(aCanvas, aPerson.Childs[i], ckDescendants);

  spb_ofs := {FLevelDistance div 3} ((aPerson.Height - 10) div (aPerson.SpousesCount + 1));
  spb_beg := aPerson.PtY + (aPerson.Height - spb_ofs * (aPerson.SpousesCount - 1)) div 2;

  case aPerson.Sex of
    svMale: begin
      for i := 0 to aPerson.SpousesCount - 1 do begin
        spb_v := spb_beg + (spb_ofs * i);
        Line(aCanvas, aPerson.Rect.Right+1, spb_v, aPerson.Spouses[i].Rect.Left, spb_v);
      end;
    end;

    svFemale: begin
      for i := 0 to aPerson.SpousesCount - 1 do begin
        spb_v := spb_beg + (spb_ofs * i);
        Line(aCanvas, aPerson.Spouses[i].Rect.Right+1, spb_v, aPerson.Rect.Left, spb_v);
      end;
    end;
  end;

  for i := 0 to aPerson.SpousesCount - 1 do
    Draw(aCanvas, aPerson.Spouses[i], ckDescendants);

  cr_y := aPerson.PtY + aPerson.Height + FLevelDistance div 2;

  if (aPerson.BaseSpouse = nil)
  or ((aPerson.BaseSpouse <> nil) and (aPerson.BaseSpouse.SpousesCount > 1))
  then begin
    cx := aPerson.PtX;
    spb_beg := aPerson.PtY + aPerson.Height - 1;
  end else begin
    case aPerson.Sex of
      svMale: cx := (aPerson.Rect.Right + aPerson.BaseSpouse.Rect.Left) div 2;
      svFemale: cx := (aPerson.BaseSpouse.Rect.Right + aPerson.Rect.Left) div 2;
    end;

    spb_beg := spb_beg - spb_ofs div 2;
  end;

  if (aPerson.ChildsCount <> 0) then begin
    Line(aCanvas, cx, spb_beg, cx, cr_y); // vert, from fam to childs

    if (aPerson.ChildsCount = 1) then begin
      child_pt := aPerson.Childs[0].Pt;
      Line(aCanvas, child_pt.X, cr_y, child_pt.X, child_pt.Y); // vert, connect from ? to child
    end else begin
      bpx := aPerson.Childs[0].PtX;
      epx := aPerson.Childs[aPerson.ChildsCount-1].PtX;

      Line(aCanvas, bpx, cr_y, epx, cr_y); // horiz, merge childs

      for i := 0 to aPerson.ChildsCount - 1 do begin
        child_pt := aPerson.Childs[i].Pt;
        Line(aCanvas, child_pt.X, cr_y, child_pt.X, child_pt.Y); // vert
      end;
    end;
  end;
end;

procedure TAncestryChartBox.Draw(aCanvas: System.Drawing.Graphics; aPerson: TPerson; aDirKind: TCustomChartBox.TChartKind);
begin
  if (aPerson = nil) then Exit;

  case FKind of
    ckAncestors: DrawAncestors(aCanvas, aPerson);
    ckDescendants: DrawDescendants(aCanvas, aPerson);
    ckBoth: begin
      if (aPerson = FRoot) or (aDirKind = ckAncestors) then DrawAncestors(aCanvas, aPerson);
      if (aPerson = FRoot) or (aDirKind = ckDescendants) then DrawDescendants(aCanvas, aPerson);
    end;
  end;

  aPerson.Draw(aCanvas, FSPX, FSPY);
end;

function TAncestryChartBox.DoAncestorsStep(aChild: TPerson; aPerson: TGEDCOMIndividualRecord; aGeneration: Integer): TPerson;
var
  family: TGEDCOMFamilyRecord;
  iFather, iMother: TGEDCOMIndividualRecord;
  divorced: Boolean;
begin
  if (aPerson = nil) then begin
    Result := nil;
    Exit;
  end;

  Result := TPerson.Create(Self);
  Result.BuildBy(aPerson);
  Result.Generation := aGeneration;
  FPersons.Add(Result);

  if (aChild <> nil)
  then Result.AddChild(aChild);

  if (FOptions.Kinship)
  then Result.FNode := FGraph.CreateNode(TGraph.TGraphNode, Result);

  if (FDepthLimit > -1) and (aGeneration = FDepthLimit) then Exit;

  if (aPerson.ChildToFamilyLinksCount > 0) then begin
    family := aPerson.ChildToFamilyLinks[0].Family;

    if (TGenEngine.IsRecordAccess(family.Restriction, FShieldState)) then begin
      iFather := TGEDCOMIndividualRecord(family.Husband.Value);
      iMother := TGEDCOMIndividualRecord(family.Wife.Value);

      divorced := (family.GetTagStringValue('_STAT') = 'NOTMARR');

      if (iFather <> nil) and (TGenEngine.IsRecordAccess(iFather.Restriction, FShieldState)) then begin
        Result.Father := DoAncestorsStep(Result, iFather, aGeneration + 1);
        if (Result.Father <> nil) then begin
          Result.Father.Divorced := divorced;

          if (FOptions.Kinship)
          then FGraph.CreateLink(Result.FNode, Result.Father.FNode, 1, Ord(rkParent), Ord(rkChild));
        end;
      end else Result.Father := nil;

      if (iMother <> nil) and (TGenEngine.IsRecordAccess(iMother.Restriction, FShieldState)) then begin
        Result.Mother := DoAncestorsStep(Result, iMother, aGeneration + 1);
        if (Result.Mother <> nil) then begin
          Result.Mother.Divorced := divorced;

          if (FOptions.Kinship)
          then FGraph.CreateLink(Result.FNode, Result.Mother.FNode, 1, Ord(rkParent), Ord(rkChild));
        end;
      end else Result.Mother := nil;

      if (Result.Father <> nil) and (Result.Mother <> nil) then begin
        if (FOptions.Kinship)
        then FGraph.CreateLink(Result.Father.FNode, Result.Mother.FNode, 1, Ord(rkSpouse), Ord(rkSpouse));
      end;
    end;
  end;
end;

function TAncestryChartBox.AddDescPerson(aParent: TPerson; iRec: TGEDCOMIndividualRecord;
  aKind: TCustomPerson.TPersonKind; aGeneration: Integer): TPerson;
begin
  if (FRoot <> nil) and (FRoot.Rec = iRec) then begin
    Result := FRoot;
    Result.Parent := aParent;
    Result.Kind := aKind;
    Exit;
  end;

  Result := TPerson.Create(Self);
  Result.BuildBy(iRec);
  Result.Generation := aGeneration;
  Result.Parent := aParent;
  Result.Kind := aKind;
  FPersons.Add(Result);

  if (FOptions.Kinship)
  then Result.FNode := FGraph.CreateNode(TGraph.TGraphNode, Result);

  if (aKind <> pkSpouse) and (aParent <> nil)
  then aParent.AddChild(Result);
end;

function TAncestryChartBox.IsChildless(iRec: TGEDCOMIndividualRecord): Boolean;
var
  exp: string;
begin
  exp := TGenEngine.GetLifeExpectancy(iRec);
  if (exp = '') or (exp = '?')
  then Result := False
  else Result := (Int32.Parse(exp) < 15);
end;

function TAncestryChartBox.DoDescendantsStep(aParent: TPerson; aPerson: TGEDCOMIndividualRecord; aLevel: Integer): TPerson;
var
  res, res_parent, ft, mt, child: TPerson;
  family: TGEDCOMFamilyRecord;
  child_rec, sp: TGEDCOMIndividualRecord;
  i, k: Integer;
  filter_source: TGEDCOMSourceRecord;
  desc_flag: TCustomPerson.TPersonFlag;
begin
  Result := nil;
  if (aPerson = nil) then Exit;

  if (FOptions.ChildlessExclude) and (aLevel > 1) then begin
    if (aPerson.SpouseToFamilyLinksCount = 0) and IsChildless(aPerson)
    then Exit;
  end;

  case FFilter.SourceMode of
    gmAll: ;
    gmNone: if (aPerson.SourceCitationsCount <> 0) then Exit;
    gmAny: if (aPerson.SourceCitationsCount = 0) then Exit;
    gmSelected: begin
      if (FFilter.SourceRef = '')
      then filter_source := nil
      else filter_source := FTree.XRefIndex_Find(FFilter.SourceRef) as TGEDCOMSourceRecord;

      if (aPerson.IndexOfSource(filter_source) < 0) then Exit;
    end;
  end;

  case FFilter.BranchCut of
    bcNone: ;
    bcYears, bcPersons: if (Boolean(aPerson.ExtData) = False) then Exit;
  end;

  res := AddDescPerson(aParent, aPerson, pkDefault, aLevel);
  Result := res;

  for k := 0 to aPerson.SpouseToFamilyLinksCount - 1 do begin
    family := aPerson.SpouseToFamilyLinks[k].Family;
    if not(TGenEngine.IsRecordAccess(family.Restriction, FShieldState)) then Continue;

    res_parent := nil;
    case aPerson.Sex of
      svMale: begin
        sp := TGEDCOMIndividualRecord(family.Wife.Value);
        res_parent := AddDescPerson(nil, sp, pkSpouse, aLevel);
        res_parent.FSex := svFemale;

        ft := res;
        mt := res_parent;
        desc_flag := pfDescByMother;
      end;
      svFemale: begin
        sp := TGEDCOMIndividualRecord(family.Husband.Value);
        res_parent := AddDescPerson(nil, sp, pkSpouse, aLevel);
        res_parent.FSex := svMale;

        ft := res_parent;
        mt := res;
        desc_flag := pfDescByFather;
      end;
    end;

    if (FOptions.Kinship)
    then FGraph.CreateLink(res.FNode, res_parent.FNode, 1, Ord(rkSpouse), Ord(rkSpouse));

    if (res_parent <> nil) then begin
      res.AddSpouse(res_parent);
      res_parent.BaseSpouse := res;
    end else res_parent := res;

    if (FDepthLimit > -1) and (aLevel = FDepthLimit) then Continue;

    for i := 0 to family.ChildrenCount - 1 do begin
      child_rec := TGEDCOMIndividualRecord(family.Children[i].Value);
      if not(TGenEngine.IsRecordAccess(child_rec.Restriction, FShieldState)) then Continue;

      child := DoDescendantsStep(res_parent, child_rec, aLevel + 1);
      if (child <> nil) then begin
        child.Father := ft;
        child.Mother := mt;
        child.FFlags.Include(desc_flag);

        if (FOptions.Kinship) then begin
          FGraph.CreateLink(child.FNode, ft.FNode, 1, Ord(rkParent), Ord(rkChild));
          FGraph.CreateLink(child.FNode, mt.FNode, 1, Ord(rkParent), Ord(rkChild));
        end;
      end;
    end;
  end;
end;

procedure TAncestryChartBox.DoFilter(aRoot: TGEDCOMIndividualRecord);

  function DoDescendantsFilter(aPerson: TGEDCOMIndividualRecord): Boolean;
  var
    family: TGEDCOMFamilyRecord;
    child: TGEDCOMIndividualRecord;
    i, k, year: Integer;
    res_child: Boolean;
  begin
    Result := False;
    if (aPerson = nil) then Exit;

    case FFilter.BranchCut of
      bcYears: begin
        year := TGenEngine.GetIndependentYear(aPerson, 'BIRT');
        Result := (year >= FFilter.BranchYear);
      end;
      bcPersons: begin
        Result := (Pos(aPerson.XRef + ';', FFilter.BranchPersons) > 0);
      end;
    end;

    for k := 0 to aPerson.SpouseToFamilyLinksCount - 1 do begin
      family := aPerson.SpouseToFamilyLinks[k].Family;

      for i := 0 to family.ChildrenCount - 1 do begin
        child := TGEDCOMIndividualRecord(family.Children[i].Value);
        res_child := DoDescendantsFilter(child);
        Result := Result or res_child;
      end;
    end;

    aPerson.ExtData := TObject(Result);
  end;

begin
  if (FFilter.BranchCut <> bcNone) then begin
    TGenEngine.InitExtCounts(FTree, Ord(False));
    DoDescendantsFilter(aRoot);
    aRoot.ExtData := TObject(True);
  end;
end;

procedure TAncestryChartBox.InitEdges(var edges: TEdges);
var
  i: Byte;
begin
  for i := Low(Byte) to High(Byte) do edges[i] := 0;
end;

procedure TAncestryChartBox.ShiftAnc(edges: TEdges; aPerson: TPerson; aOffset: Integer);
var
  pp: TPerson;
begin
  pp := aPerson;
  while (pp <> nil) do begin
    pp.PtX := pp.PtX + aOffset;
    edges[pp.Generation] := pp.Rect.Right;

    if (pp.ChildsCount < 1)
    then pp := nil
    else pp := pp.Childs[0];
  end;
end;

procedure TAncestryChartBox.RecalcAnc(prev: TList; edges: TEdges; aPerson: TPerson; aPt: TPoint);
var
  offset, i, gen: Integer;
  pp: TPerson;
  xpt: TPoint;
begin
  if (aPerson = nil) then Exit;

  aPerson.Pt := aPt;
  gen := aPerson.Generation;

  /// prepare intersects

  // horizontal adjustment
  if (edges[gen] > 0) then offset := FBranchDistance else offset := FMargin;
  if (aPerson.Rect.Left <= edges[gen] + offset)
  then ShiftAnc(edges, aPerson, ((edges[gen] + offset) - aPerson.Rect.Left));

  edges[gen] := aPerson.Rect.Right;

  // vertical adjustment
  prev.Add(aPerson);
  if (aPerson.Rect.Top < 0) then begin
    offset := (0 - aPerson.Rect.Top) + Margin;

    for i := 0 to prev.Count - 1 do begin
      pp := TPerson(prev[i]);
      pp.PtY := pp.PtY + offset;
    end;
  end;

  /// adjust parents

  if (aPerson.Father <> nil) and (aPerson.Mother <> nil) then begin
    // calc father
    xpt := TPoint.Create(aPerson.PtX - (FSpouseDistance + (aPerson.Father.Width div 2)),
                         aPerson.PtY - FLevelDistance - aPerson.Height);
    RecalcAnc(prev, edges, aPerson.Father, xpt);

    // calc mother
    xpt := TPoint.Create(aPerson.PtX + (FSpouseDistance + (aPerson.Mother.Width div 2)),
                         aPerson.PtY - FLevelDistance - aPerson.Height);
    RecalcAnc(prev, edges, aPerson.Mother, xpt);

    // align child
    aPerson.PtX := (aPerson.Father.PtX + aPerson.Mother.PtX) div 2;
    edges[aPerson.Generation] := aPerson.Rect.Right;
  end else begin
    xpt := TPoint.Create(aPerson.PtX, aPerson.PtY - FLevelDistance - aPerson.Height);

    if (aPerson.Father <> nil) then RecalcAnc(prev, edges, aPerson.Father, xpt)
    else
    if (aPerson.Mother <> nil) then RecalcAnc(prev, edges, aPerson.Mother, xpt);
  end;

  if (FWMax < aPerson.Rect.Right) then FWMax := aPerson.Rect.Right;
  if (FHMax < aPerson.Rect.Bottom) then FHMax := aPerson.Rect.Bottom;
end;

procedure TAncestryChartBox.RecalcAncestorsChart();
var
  prev: TList;
  edges: TEdges;
begin
  InitEdges(edges);

  prev := TList.Create;
  try
    RecalcAnc(prev, edges, FRoot, TPoint.Create(FMargin, FMargin));
  finally
    prev.Free;
  end;
end;

procedure TAncestryChartBox.ShiftDesc(aPerson: TPerson; aOffset: Integer; aSingle: Boolean);
var
  p: TPerson;
begin
  {p := aPerson;
  while (p <> nil) do begin
    p.PtX := p.PtX + aOffset;

    if (p.BaseSpouse <> nil)
    and ((p.BaseSpouse.Sex = svFemale) or (p.BaseSpouse.SpousesCount = 1))
    then p := p.BaseSpouse
    else p := p.Parent;
  end;}

  p := aPerson;
  if (p = nil) then Exit;
  if (p = FRoot) then aSingle := False;

  p.PtX := p.PtX + aOffset;

  if (p.BaseSpouse <> nil)
  and ((p.BaseSpouse.Sex = svFemale) or (p.BaseSpouse.SpousesCount = 1))
  then begin
    ShiftDesc(p.BaseSpouse, aOffset, aSingle);
  end else begin
    if not(aSingle) then begin
      ShiftDesc(p.Father, aOffset, aSingle);
      ShiftDesc(p.Mother, aOffset, aSingle);
    end else begin
      if (p.FFlags.InSet(pfDescByFather))
      then ShiftDesc(p.Father, aOffset, aSingle)
      else
      if (p.FFlags.InSet(pfDescByMother))
      then ShiftDesc(p.Mother, aOffset, aSingle);
    end;
  end;
end;

procedure TAncestryChartBox.RecalcDescChilds(edges: TEdges; aPerson: TPerson);
var
  childs_width, i, cur_x, cur_y, cent_x: Integer;
  child: TPerson;
  fix_pair: Boolean;
begin
  if (aPerson.ChildsCount = 0) then Exit;

  fix_pair := (aPerson.BaseSpouse <> nil) and (aPerson.BaseSpouse.SpousesCount = 1);

  if (fix_pair) then begin
    case aPerson.Sex of
      svMale: cent_x := (aPerson.Rect.Right + aPerson.BaseSpouse.Rect.Left) div 2;
      svFemale: cent_x := (aPerson.BaseSpouse.Rect.Right + aPerson.Rect.Left) div 2;
    end;
  end else cent_x := aPerson.PtX;

  cur_y := aPerson.PtY + FLevelDistance + aPerson.Height;

  childs_width := (aPerson.ChildsCount - 1) * FBranchDistance;
  for i := 0 to aPerson.ChildsCount - 1 do begin
    childs_width := childs_width + aPerson.Childs[i].Width;
  end;
  cur_x := cent_x - (childs_width div 2);

  for i := 0 to aPerson.ChildsCount - 1 do begin
    child := aPerson.Childs[i];
    RecalcDesc(edges, child, TPoint.Create(cur_x + (child.Width div 2), cur_y));
    cur_x := child.Rect.Right + FBranchDistance;
  end;

  /// adjusting
  cur_x := aPerson.Childs[0].PtX;
  if (aPerson.ChildsCount > 1)
  then cur_x := cur_x + ((aPerson.Childs[aPerson.ChildsCount - 1].PtX - cur_x) div 2);

  if (fix_pair) then begin
    case aPerson.Sex of
      svMale: begin
        //aPerson.PtX := cur_x - ((BranchDistance + aPerson.Width) div 2) + 1;
        //aPerson.BaseSpouse.PtX := cur_x + ((BranchDistance + aPerson.BaseSpouse.Width) div 2);

        ShiftDesc(aPerson, (cur_x - ((BranchDistance + aPerson.Width) div 2) + 1) - aPerson.PtX, True);
        ShiftDesc(aPerson.BaseSpouse, (cur_x + ((BranchDistance + aPerson.BaseSpouse.Width) div 2)) - aPerson.BaseSpouse.PtX, True);
      end;
      svFemale: begin
        //aPerson.PtX := cur_x + ((BranchDistance + aPerson.Width) div 2);
        //aPerson.BaseSpouse.PtX := cur_x - ((BranchDistance + aPerson.BaseSpouse.Width) div 2) + 1;

        ShiftDesc(aPerson, (cur_x + ((BranchDistance + aPerson.Width) div 2)) - aPerson.PtX, True);
        ShiftDesc(aPerson.BaseSpouse, (cur_x - ((BranchDistance + aPerson.BaseSpouse.Width) div 2) + 1) - aPerson.BaseSpouse.PtX, True);
      end;
    end;
  end else begin
    //aPerson.PtX := cur_x;
    ShiftDesc(aPerson, cur_x - aPerson.PtX, True);
  end;
end;

procedure TAncestryChartBox.RecalcDesc(edges: TEdges; aPerson: TPerson; aPt: TPoint; aPreDef: Boolean = True);
var
  i, offset, gen: Integer;
  sp, prev: TPerson;
  sp_pt: TPoint;
begin
  if (aPerson = nil) then Exit;

  gen := aPerson.Generation;

  if (aPreDef)
  then aPerson.Pt := aPt;

  /// prepare intersects
  if (edges[gen] > 0) then offset := FBranchDistance else offset := FMargin;
  if (aPerson.Rect.Left <= edges[gen] + offset)
  then ShiftDesc(aPerson, ((edges[gen] + offset) - aPerson.Rect.Left), True);

  ///
  if (aPerson.Sex = svMale) then begin
    RecalcDescChilds(edges, aPerson);
    edges[gen] := aPerson.Rect.Right;
  end;

  if (aPerson.SpousesCount > 0) then begin
    prev := aPerson;
    for i := 0 to aPerson.SpousesCount - 1 do begin
      sp := aPerson.Spouses[i];

      case aPerson.Sex of
        svMale: sp_pt := TPoint.Create(prev.Rect.Right + (FBranchDistance + (sp.Width div 2)), aPerson.PtY);
        svFemale: sp_pt := TPoint.Create(prev.Rect.Left - (FBranchDistance + (sp.Width div 2)), aPerson.PtY);
      end;

      RecalcDesc(edges, sp, sp_pt);

      if (sp.Sex <> svMale)
      then prev := sp;
    end;
  end;

  if (aPerson.Sex = svFemale) then begin
    RecalcDescChilds(edges, aPerson);
    edges[gen] := aPerson.Rect.Right;
  end;
  ///

  if (FWMax < aPerson.Rect.Right) then FWMax := aPerson.Rect.Right;
  if (FHMax < aPerson.Rect.Bottom) then FHMax := aPerson.Rect.Bottom;
end;

procedure TAncestryChartBox.RecalcDescendantsChart(aPreDef: Boolean);
var
  edges: TEdges;
begin
  InitEdges(edges);
  RecalcDesc(edges, FRoot, TPoint.Create(FMargin, FMargin), aPreDef);
end;

procedure TAncestryChartBox.InternalGenChart(aPerson: TGEDCOMIndividualRecord; aKind: TCustomChartBox.TChartKind);
begin
  FKind := aKind;

  FSelected := nil;
  FPersons.Clear;

  Predef();

  FGraph.Clear;

  DoFilter(aPerson);

  FRoot := nil;
  case FKind of
    ckAncestors: FRoot := DoAncestorsStep(nil, aPerson, 1);
    ckDescendants: FRoot := DoDescendantsStep(nil, aPerson, 1);
    ckBoth: begin
      FRoot := DoAncestorsStep(nil, aPerson, 1);
      DoDescendantsStep(nil, aPerson, 1);
    end;
  end;

  FKinRoot := FRoot;

  RecalcChart();
end;

procedure TAncestryChartBox.RecalcChart();
var
  i: Integer;
  p: TPerson;
begin
  if (FOptions.Kinship) then begin
    FGraph.FindPathTree(FKinRoot.FNode);

    for i := 0 to FPersons.Count - 1 do begin
      p := FPersons[i];
      p.Kinship := FindRelationship(p);
    end;
  end;

  FHMax := 0;
  FWMax := 0;

  case FKind of
    ckAncestors: RecalcAncestorsChart();
    ckDescendants: RecalcDescendantsChart(True);
    ckBoth: begin
      RecalcAncestorsChart();
      RecalcDescendantsChart(False);
    end;
  end;

  FHMax := FHMax + FMargin - 1;
  FWMax := FWMax + FMargin - 1;

  FImageHeight := FHMax;
  FImageWidth := FWMax;
end;

procedure TAncestryChartBox.SaveSnapshot(const aFileName: string);
var
  pic: System.Drawing.Image;
  canv: System.Drawing.Graphics;
  ext: string;
begin
  ext := System.IO.Path.GetExtension(aFileName).ToLower();

  if ((ext = '.bmp') or (ext = '.jpg')) and (FImageWidth >= High(Word)) then begin
    TGKUtils.ShowError(LSList[LSID_TooMuchWidth]);
    Exit;
  end;

  pic := System.Drawing.Bitmap.Create(FImageWidth, FImageHeight, System.Drawing.Imaging.PixelFormat.Format24bppRgb);
  canv := System.Drawing.Graphics.FromImage(pic);
  try
    try
      Predef();
      TreeDraw(canv, False);
    finally
      canv.Dispose;
    end;

    if (ext = '.bmp') then pic.Save(aFileName, ImageFormat.Bmp)
    else
    if (ext = '.emf') then pic.Save(aFileName, ImageFormat.Emf)
    else
    if (ext = '.jpg') then pic.Save(aFileName, ImageFormat.Jpeg);
  finally
    pic.Free;
  end;
end;

procedure TAncestryChartBox.SelectBy(aX, aY: Integer);
var
  i: Integer;
  p: TPerson;
begin
  aX := aX - FSPX;
  aY := aY - FSPY;

  for i := 0 to FPersons.Count - 1 do begin
    p := FPersons[i];

    if p.Rect.Contains(aX, aY) then begin
      SetSelected(p);
      Exit;
    end;
  end;

  SetSelected(nil);
end;

procedure TAncestryChartBox.SelectByRec(iRec: TGEDCOMIndividualRecord);
var
  i: Integer;
  p: TPerson;
begin
  for i := 0 to FPersons.Count - 1 do begin
    p := FPersons[i];

    if (p.Rec = iRec) then begin
      SetSelected(p);
      Exit;
    end;
  end;

  SetSelected(nil);
end;

procedure TAncestryChartBox.RebuildKinships();
var
  p: TPerson;
begin
  if (FOptions.Kinship) then begin
    p := FSelected;

    if (p <> nil) then begin
      FKinRoot := p;
      RecalcChart();
      ScrollRange();
    end;
  end;
end;

procedure TAncestryChartBox.FixLink(path: TObjectList; f, t: TPerson; rel: TGenEngine.TRelationKind);
var
  L: TRelLink;
begin
  L := TRelLink.Create;
  L.xFrom := f;
  L.xTo := t;

  case rel of
    rkParent:
      case L.xTo.Sex of
        svMale: L.xRel := rkFather;
        svFemale: L.xRel := rkMother;
      end;
    rkSpouse:
      case L.xTo.Sex of
        svMale: L.xRel := rkHusband;
        svFemale: L.xRel := rkWife;
      end;
    rkChild:
      case L.xTo.Sex of
        svMale: L.xRel := rkSon;
        svFemale: L.xRel := rkDaughter;
      end;
    else L.xRel := rel;
  end;

  path.Add(L);
end;

function TAncestryChartBox.GetGreat(n: Integer): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to n do Result := Result + 'пра';
end;

function TAncestryChartBox.FixRelation(aTarget: TPerson; Rel: TGenEngine.TRelationKind; Great: Integer): string;
var
  tmp: string;
begin
  if (Great <> 0) then begin
    if (Rel in [rkUncle, rkAunt{, rkCousinM, rkCousinF}]) then begin
      tmp := TGenEngine.Numerals[Great + 1] + TGenEngine.NumKinship[aTarget.Sex] + ' ';

      if (Rel = rkUncle) then Rel := rkGrandfather;
      if (Rel = rkAunt) then Rel := rkGrandmother;

      {if (Rel = rkCousinM) then Rel := rkBrother;
      if (Rel = rkCousinF) then Rel := rkSister;}
    end else begin
      if (Rel <> rkUndefined)
      then tmp := GetGreat(Great);
    end;
  end else tmp := '';

  Result := tmp + LSList[TGenEngine.RelationKinds[Rel]];
end;

function TAncestryChartBox.FindRelationship(aTarget: TPerson): string;
var
  path: TObjectList;
  i, great, g, lev: Integer;
  L: TRelLink;
  prev_rel, cur_rel, fin_rel: TGenEngine.TRelationKind;
  link: TGraph.TGraphLink;
  tmp: string;
begin
  Result := '';

  path := TObjectList.Create(True);
  try
    link := aTarget.FNode.LinkIn;
    while (link <> nil) do begin
      FixLink(path, TPerson(link.Node1.ExtObj), TPerson(link.Node2.ExtObj), TGenEngine.TRelationKind(link.ExtData));
      link := link.Node1.LinkIn;
    end;

    tmp := '';

    prev_rel := rkNone;
    fin_rel := rkNone;
    great := 0;
    for i := path.Count - 1 downto 0 do begin
      L := TRelLink(path[i]);
      cur_rel := L.xRel;

      if (FPathDebug) then begin
        if (tmp <> '') then tmp := tmp + ', ';
        if (L.xFrom.Rec <> nil) then tmp := tmp + L.xFrom.Rec.XRef + '>' + TGenEngine.RelationSigns[cur_rel] + '>';
        if (L.xTo.Rec <> nil) then tmp := tmp + L.xTo.Rec.XRef;
      end;

      if (prev_rel <> rkUndefined) then begin
        fin_rel := FEngine.FindKinship(prev_rel, cur_rel, g, lev);
        great := great + g;

        //if (great <> 0) and (lev <> 0) then Inc(great, lev); // fool

        prev_rel := fin_rel;
      end;
    end;

    if (FPathDebug) then begin
      if (aTarget.Rec <> nil) then aTarget.FPathDebug := aTarget.Rec.XRef + ' ';
      aTarget.FPathDebug := aTarget.FPathDebug + ' [' + tmp + ']';
    end;

    Result := '[' + FixRelation(aTarget, fin_rel, great) + ']';
  finally
    path.Free;
  end;
end;

{==============================================================================}

{ TChartFilter }

constructor TChartFilter.Create;
begin
  inherited Create;
  Clear();
end;

procedure TChartFilter.Backup;
begin
  Back_SourceMode := SourceMode;
  Back_SourceRef := SourceRef;

  Back_BranchCut := BranchCut;
  Back_BranchYear := BranchYear;
  Back_BranchPersons := BranchPersons;
end;

procedure TChartFilter.Clear;
begin
  SourceMode := gmAll;
  BranchCut := bcNone;

  /// remared for saving in session
  //SourceRef := '';
  //BranchYear := 0;
  //BranchPersons := '';
end;

procedure TChartFilter.Restore;
begin
  SourceMode := Back_SourceMode;
  SourceRef := Back_SourceRef;

  BranchCut := Back_BranchCut;
  BranchYear := Back_BranchYear;
  BranchPersons := Back_BranchPersons;
end;

end.
