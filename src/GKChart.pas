unit GKChart;

{$I GEDKeeper.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Menus, Jpeg, ToolWin, ComCtrls, GedCom551, GKChartCore,
  StdCtrls;

type
  TfmChart = class(TForm)
    SaveDialog1: TSaveDialog;
    ToolBar1: TToolBar;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    tbImageSave: TToolButton;
    ToolButton1: TToolButton;
    ListDepthLimit: TComboBox;
    Label1: TLabel;
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tbImageSaveClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure ListDepthLimitChange(Sender: TObject);
    procedure ScrollBox1Resize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
  private
    FDown: Boolean;
    FX, FY: Integer;
    FTreeBounds: TRect;
    FTree: TGEDCOMTree;
    FPerson: TGEDCOMIndividualRecord;
    FChartKind: TChartKind;
    FDepthLimit: Integer;
    FChart: TAncestryChart;
    procedure SetTreeBounds(const Value: TRect);
  public
    property ChartKind: TChartKind read FChartKind write FChartKind;
    property DepthLimit: Integer read FDepthLimit write FDepthLimit;
    property Person: TGEDCOMIndividualRecord read FPerson write FPerson;
    property Tree: TGEDCOMTree read FTree write FTree;
    property TreeBounds: TRect read FTreeBounds write SetTreeBounds;

    procedure GenChart();
  end;

implementation

uses GKMain, GKPersonEdit;

{$R *.dfm}

procedure TfmChart.GenChart();
begin
  if (FPerson = nil) then begin
    MessageDlg('Не выбрана персональная запись', mtError, [mbOk], 0);
    Exit;
  end;

  try
    try
      FChart.Bitmap := Image1.Picture.Bitmap;
      FChart.DepthLimit := FDepthLimit;
      FChart.Options := fmGEDKeeper.Options.ChartOptions;
      FChart.Tree := FTree;
      FChart.GenChart(FPerson, FChartKind);

      case FChartKind of
        ckAncestors: Caption := 'Древо предков';
        ckDescendants: Caption := 'Древо потомков';
      end;

      TreeBounds := FChart.TreeBounds;
      Show();
    finally
    end;
  except
    on E: Exception do MessageDlg(E.Message, mtError, [mbOk], 0);
  end;
end;

procedure TfmChart.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FX := X;
  FY := Y;

  if (Button = mbRight) then begin
    Image1.Cursor := crSizeAll;
    FDown := True;
  end;
end;

procedure TfmChart.Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  dx, dy: Integer;
begin
  if (FDown) then begin
    dx := X - FX;
    dy := Y - FY;

    ScrollBox1.HorzScrollBar.Position := ScrollBox1.HorzScrollBar.Position - dx;
    ScrollBox1.VertScrollBar.Position := ScrollBox1.VertScrollBar.Position - dy;
  end;
end;

procedure TfmChart.Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (FDown) then begin
    Image1.Cursor := crDefault;
    FDown := False;
  end;

  FChart.SelectBy(X, Y);
end;

procedure TfmChart.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfmChart.SetTreeBounds(const Value: TRect);
begin
  FTreeBounds := Value;

  Image1.Height := Value.Bottom - Value.Top + 1;
  Image1.Width := Value.Right - Value.Left + 1;

  ScrollBox1Resize(nil);
end;

procedure TfmChart.tbImageSaveClick(Sender: TObject);
var
  bmp: TJPEGImage;
begin
  if SaveDialog1.Execute then begin
    bmp := TJPEGImage.Create;
    try
      bmp.Assign(Image1.Picture.Graphic);
      bmp.CompressionQuality := 100;
      bmp.Compress();

      bmp.SaveToFile(SaveDialog1.FileName);
    finally
      bmp.Destroy;
    end;
  end;
end;

procedure TfmChart.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then Close;
end;

procedure TfmChart.FormCreate(Sender: TObject);
begin
  FDepthLimit := -1;
  FChart := TAncestryChart.Create;
end;

procedure TfmChart.ListDepthLimitChange(Sender: TObject);
begin
  if (ListDepthLimit.ItemIndex = 0)
  then DepthLimit := -1
  else DepthLimit := ListDepthLimit.ItemIndex;

  GenChart();
end;

procedure TfmChart.ScrollBox1Resize(Sender: TObject);
begin
  if (Image1.Height < ScrollBox1.ClientHeight)
  then Image1.Top := (ScrollBox1.ClientHeight - Image1.Height) div 2
  else Image1.Top := 0;

  if (Image1.Width < ScrollBox1.ClientWidth)
  then Image1.Left := (ScrollBox1.ClientWidth - Image1.Width) div 2
  else Image1.Left := 0;
end;

procedure TfmChart.FormDestroy(Sender: TObject);
begin
  FChart.Destroy;
end;

procedure TfmChart.Image1DblClick(Sender: TObject);
var
  p: TPerson;
begin
  p := FChart.Selected;
  if (p <> nil) then begin
    fmPersonEdit := TfmPersonEdit.Create(Application);
    try
      fmPersonEdit.Tree := FTree;
      fmPersonEdit.Person := p.Rec;
      fmPersonEdit.ShowModal;

      fmGEDKeeper.ListsRefresh();
      GenChart();
    finally
      fmPersonEdit.Destroy;
      fmPersonEdit := nil;
    end;
  end;
end;

end.
