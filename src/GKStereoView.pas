unit GKStereoView;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Spin, bsCtrls, GedCom551, GKBase;

type
  TfmStereoView = class(TForm)
    Panel1: TPanel;
    ListView1: TListView;
    Label1: TLabel;
    Panel2: TPanel;
    btnInit: TButton;
    btnStep: TButton;
    ScrollBar3: TScrollBar;
    ScrollBar2: TScrollBar;
    ScrollBar1: TScrollBar;
    CheckBox9: TCheckBox;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CheckBox9Click(Sender: TObject);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ScrollBar1Change(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure btnInitClick(Sender: TObject);
    procedure btnStepClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    FPBox: TVGPaintBox;
    FTree: TGEDCOMTree;

    function GetBase(): TfmBase;
  public
    property Base: TfmBase read GetBase;
  end;

var
  fmStereoView: TfmStereoView;

implementation

{$R *.DFM}

uses xygraph, xygraph3d, GKProgress, GKCommon, Contnrs;

const dataA : array[1..9] of single =
         (0,0.1,0.2,0.3,0.45,0.6,0.8,1,1.25);
const dataB : array[0..5] of array[0..5] of single =
           (  ( 0, -2, -1,  0,  1,  2),
              (-2,0.5,0.5,0.5,0.5,0.5),
              (-1,0.5,  1,  1,  1,0.5),
              ( 0,0.5,  1,  0,  1,0.5),
              ( 1,0.5,  1,  1,  1,0.5),
              ( 2,0.5,0.5,0.5,0.5,0.5) );

var data1,data2,data3 : Tdatatype;
    data0 : array[0..4,1..10] of single;
    mdown: Boolean;
    mx, my: Integer;

procedure TfmStereoView.PaintBox1Paint(Sender: TObject);
var
  x, y, sy: Integer;
begin
  xycleargraph(FPBox, clSilver, clBlack, 1.0);
  xystartgraph(0, 100, 0, 100, 20, 20, 20, 20, clipoff);
  xy3dsetframe(-10, 10, -10, 10, 0, 20, 1.5, 0.6, ScrollBar3.Position{zoom} / 100, 0);
  xy3dsetlabels('X-axis', 'Y-axis', 'Z-axis', '', '', '', False, 2, 2, 2, 0);
  xy3dsetview(ScrollBar1.Position, ScrollBar2.Position, 5, 0);
  if CheckBox9.Checked then xy3dsetstereo(3, -0.5, 3, 0);
  xy3dshowframe(4{kind of view}, clBlack, clWhite, clWhite, clWhite, 0);

  xypen.Color := clBlack;
  // 1 - sph, 2=cube
  sy := 1;

  y := 3;
  xylinewidth(2);
  for x := 10 downto 1 do
    if (x = 10)
    then xy3dmove(x, y, data0[2, x])
    else xy3ddraw(x, y, data0[2, x]);

  xy3dmove(0, 0, 0); xy3ddraw(0, 0, 5);
  xy3dmove(2, 2, 0); xy3ddraw(2, 2, 5);
  xy3dmove(0, 0, 5); xy3ddraw(2, 2, 5);

  xy3dmove(1, 1, 5); xy3ddraw(1, 1, 7);

  xylegendentry(0, 'line');
  for x := 10 downto 1 do begin
    xy3dmove(x,y,data0[2,x]);
    xy3dsymbol(0,0,0,sy,5,0,clRed,lightoff,false,1);
  end;

  xysetlinestyle(0, 0, 0);
  xy3dcloseframe();
  xylegendmake(1, FPBox.Width div 2, 0, 0, 1, 0, 0, 0, True);
  xyinitruler(clBlack, FPBox.Width - 4, FPBox.Height - xycharheight - 2, -1);
  xyputbuffer(1);
end;

procedure TfmStereoView.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfmStereoView.FormCreate(Sender: TObject);
var
  i,j : integer;
const
  l2 = 9;
  n2 = l2*2;
begin
  FPBox := TVGPaintBox.Create(Self);
  with FPBox do begin
    Parent := Self;
    Align := alClient;
    OnMouseDown := PaintBox1MouseDown;
    OnMouseMove := PaintBox1MouseMove;
    OnMouseUp := PaintBox1MouseUp;
    OnPaint := PaintBox1Paint;
  end;

  xy3dsetdataarray(data1,n2-1,n2-1);
  for i := 1 to l2 do
    begin
      data1[i,0] := -dataA[l2+1-i];
      data1[i+l2-1,0] := dataA[i];
    end;
  for i := 1 to n2-1 do data1[0,i] := data1[i,0];
  for i := 1 to n2-1 do
    for j := 1 to n2-1 do
      data1[i,j] := exp(-(sqr(data1[i,0])+sqr(data1[0,j])*2));
  data1[0,0] := data1[5,5];

  xy3dsetdataarray(data2,5,5);
  for i := 0 to 5 do
    for j := 0 to 5 do
      data2[i,j] := dataB[i,j];

  randomize;
  for i := 1 to 4 do
    for j := 1 to 10 do
      data0[i,j] := i*1.5 + random * 1.5;
  for j := 1 to 10 do
    data0[0,j] := 1 + random * 1;

  xy3dsetdataarray(data3,10,2);
  for j := 1 to 2 do
    for i := 1 to 10 do
      data3[i,j] := data0[4,i];
end;

procedure TfmStereoView.FormShow(Sender: TObject);
begin
  FTree := Base.Tree;
end;

function TfmStereoView.GetBase(): TfmBase;
begin
  Result := TfmBase(Owner);
end;

procedure TfmStereoView.ScrollBar1Change(Sender: TObject);
begin
  FPBox.Refresh;
end;

procedure TfmStereoView.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  ScrollBar3.Position := ScrollBar3.Position + (WheelDelta div 10);
  Handled := True;
  FPBox.Refresh;
end;

procedure TfmStereoView.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  xymousedown(Button, Shift, X, Y);

  mdown := True;
  mx := X;
  my := Y;
end;

procedure TfmStereoView.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mdown := False;
  xymouseup(Button, Shift, X, Y);
end;

procedure TfmStereoView.CheckBox9Click(Sender: TObject);
begin
  FPBox.Refresh;
end;

procedure TfmStereoView.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  dx, dy: Integer;
begin
  if mdown then begin
    dx := X - mx;
    dy := Y - my;

    FPBox.Refresh;
  end;
end;

procedure TfmStereoView.btnInitClick(Sender: TObject);
var
  lst: TObjectList;

  function GetLinks(pObj: TPatriarchObj): string;
  var
    i: Integer;
  begin
    Result := '';

    for i := 0 to lst.Count - 1 do begin
      if (i in pObj.ILinks) then begin
        if (Result <> '') then Result := Result + ', ';
        Result := Result + GetNameStr(TPatriarchObj(lst[i]).IRec);        
      end;
    end;
  end;

var
  i: Integer;
  p_obj: TPatriarchObj;
  item: TListItem;
begin
  ListView1.Clear;

  lst := TObjectList.Create(True);
  try
    GetPatriarchsList(FTree, True, True, lst);

    Label1.Caption := ' Патриархов: ' + IntToStr(lst.Count);
    for i := 0 to lst.Count - 1 do begin
      p_obj := TPatriarchObj(lst[i]);

      item := ListView1.Items.Add();
      item.Caption := GetNameStr(p_obj.IRec);
      item.SubItems.Add(IntToStr(p_obj.IBirthYear));
      item.SubItems.Add(IntToStr(p_obj.IDescendantsCount));
      item.SubItems.Add(IntToStr(p_obj.IDescGenerations));
      item.SubItems.Add(GetLinks(p_obj));
    end;
  finally
    lst.Destroy;
  end;
end;

procedure TfmStereoView.btnStepClick(Sender: TObject);
begin
  //
end;

end.
