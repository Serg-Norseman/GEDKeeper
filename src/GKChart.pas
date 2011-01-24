unit GKChart;

{$I GEDKeeper.inc}

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls,
  Menus, ToolWin, ComCtrls, StdCtrls, GedCom551, GKChartCore, GKBase, GKCommon;

type
  TfmChart = class(TForm)
    SaveDialog1: TSaveDialog;
    ToolBar1: TToolBar;
    ScrollBox1: TScrollBox;
    ImageTree: TImage;
    tbImageSave: TToolButton;
    ToolButton1: TToolButton;
    ListDepthLimit: TComboBox;
    Label1: TLabel;
    ToolButton2: TToolButton;
    tbGotoPerson: TToolButton;
    ToolButton4: TToolButton;
    tbPrev: TToolButton;
    tbNext: TToolButton;
    ToolButton3: TToolButton;
    TrackBar1: TTrackBar;
    PopupMenu1: TPopupMenu;
    miEdit: TMenuItem;
    N1: TMenuItem;
    miSpouseAdd: TMenuItem;
    miSonAdd: TMenuItem;
    miDaughterAdd: TMenuItem;
    miFamilyAdd: TMenuItem;
    ToolButton5: TToolButton;
    chkUseFilter: TCheckBox;
    N2: TMenuItem;
    miDelete: TMenuItem;
    procedure ImageTreeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageTreeMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ImageTreeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tbImageSaveClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListDepthLimitChange(Sender: TObject);
    procedure ScrollBox1Resize(Sender: TObject);
    procedure ImageTreeDblClick(Sender: TObject);
    procedure tbGotoPersonClick(Sender: TObject);
    procedure tbPrevClick(Sender: TObject);
    procedure tbNextClick(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure miEditClick(Sender: TObject);
    procedure miSpouseAddClick(Sender: TObject);
    procedure miSonAddClick(Sender: TObject);
    procedure miDaughterAddClick(Sender: TObject);
    procedure miFamilyAddClick(Sender: TObject);
    procedure chkUseFilterClick(Sender: TObject);
    procedure miDeleteClick(Sender: TObject);
  private
    FDown: Boolean;
    FX, FY: Integer;
    FTreeBounds: TRect;
    FTree: TGEDCOMTree;
    FPerson: TGEDCOMIndividualRecord;
    FChartKind: TChartKind;
    FDepthLimit: Integer;
    FChart: TAncestryChart;
    FFileName: string;
    FBase: TfmBase;

    FBackman: TBackManager;

    procedure InternalChildAdd(aNeedSex: TGEDCOMSex);
    procedure SetTreeBounds(const Value: TRect);
    procedure NavRefresh();
    procedure NavAdd(aRec: TGEDCOMIndividualRecord);
    procedure UpdateChart();
  public
    property Base: TfmBase read FBase write FBase;
    property ChartKind: TChartKind read FChartKind write FChartKind;
    property DepthLimit: Integer read FDepthLimit write FDepthLimit;
    property FileName: string read FFileName write FFileName;
    property Person: TGEDCOMIndividualRecord read FPerson write FPerson;
    property Tree: TGEDCOMTree read FTree write FTree;
    property TreeBounds: TRect read FTreeBounds write SetTreeBounds;

    procedure GenChart(aShow: Boolean = True);
  end;

implementation

uses
  {$IFNDEF DELPHI_NET}Jpeg,{$ENDIF}
  GKEngine, GKMain, GKPersonEdit;

{$R *.dfm}

procedure TfmChart.GenChart(aShow: Boolean = True);
begin
  if (FPerson = nil) then begin
    MessageDlg('Не выбрана персональная запись', mtError, [mbOk], 0);
    Exit;
  end;

  try
    NavAdd(FPerson);

    FChart.Filter := Base.Filter;
    FChart.Bitmap := ImageTree.Picture.Bitmap;
    FChart.DepthLimit := FDepthLimit;
    FChart.Options := fmGEDKeeper.Options.ChartOptions;
    FChart.Tree := FTree;
    FChart.ShieldState := Base.ShieldState;
    FChart.Scale := TrackBar1.Position * 10;
    FChart.UseSourcesFilter := chkUseFilter.Checked;
    FChart.GenChart(FPerson, FChartKind);

    case FChartKind of
      ckAncestors: Caption := 'Древо предков';
      ckDescendants: Caption := 'Древо потомков';
    end;

    Caption := Caption + ' "' + FFileName + '"';

    TreeBounds := FChart.TreeBounds;

    if (aShow) then Show();
  except
    on E: Exception do MessageDlg(E.Message, mtError, [mbOk], 0);
  end;
end;

procedure TfmChart.ImageTreeMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FX := X;
  FY := Y;

  if (Button = mbRight) then begin
    ImageTree.Cursor := crSizeAll;
    FDown := True;
  end;
end;

procedure TfmChart.ImageTreeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
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

procedure TfmChart.ImageTreeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
begin
  if (FDown) then begin
    ImageTree.Cursor := crDefault;
    FDown := False;
  end;

  FChart.SelectBy(X, Y);

  if (Button = mbRight) and (FChart.Selected <> nil) then begin
    pt := ImageTree.ClientToScreen(Point(X, Y));
    PopupMenu1.Popup(pt.X, pt.Y);
  end;
end;

procedure TfmChart.chkUseFilterClick(Sender: TObject);
begin
  GenChart();
end;

procedure TfmChart.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfmChart.SetTreeBounds(const Value: TRect);
begin
  FTreeBounds := Value;

  ImageTree.Height := Value.Bottom - Value.Top + 1;
  ImageTree.Width := Value.Right - Value.Left + 1;

  ScrollBox1Resize(nil);
end;

procedure TfmChart.tbImageSaveClick(Sender: TObject);
{$IFNDEF DELPHI_NET}
var
  bmp: TJPEGImage;
{$ENDIF}
begin
  {$IFNDEF DELPHI_NET}
  if SaveDialog1.Execute then begin
    bmp := TJPEGImage.Create;
    try
      bmp.Assign(ImageTree.Picture.Graphic);
      bmp.CompressionQuality := 100;
      bmp.Compress();

      bmp.SaveToFile(SaveDialog1.FileName);
    finally
      bmp.Destroy;
    end;
  end;
  {$ENDIF}
end;

procedure TfmChart.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then Close;
end;

procedure TfmChart.FormCreate(Sender: TObject);
begin
  FBackman := TBackManager.Create;

  FDepthLimit := -1;
  FChart := TAncestryChart.Create;

  NavRefresh();
end;

procedure TfmChart.FormDestroy(Sender: TObject);
begin
  FChart.Destroy;
  FBackman.Free;
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
  if (ImageTree.Height < ScrollBox1.ClientHeight)
  then ImageTree.Top := (ScrollBox1.ClientHeight - ImageTree.Height) div 2
  else ImageTree.Top := 0;

  if (ImageTree.Width < ScrollBox1.ClientWidth)
  then ImageTree.Left := (ScrollBox1.ClientWidth - ImageTree.Width) div 2
  else ImageTree.Left := 0;
end;

procedure TfmChart.UpdateChart();
var
  hsp, vsp: Integer;
begin
  if (FBase <> nil) then FBase.ListsRefresh();

  hsp := ScrollBox1.HorzScrollBar.Position;
  vsp := ScrollBox1.VertScrollBar.Position;

  ScrollBox1.HorzScrollBar.Position := 0;
  ScrollBox1.VertScrollBar.Position := 0;

  GenChart(True);

  ScrollBox1.HorzScrollBar.Position := hsp;
  ScrollBox1.VertScrollBar.Position := vsp;
end;

procedure TfmChart.ImageTreeDblClick(Sender: TObject);
var
  p: TPerson;
  i_rec: TGEDCOMIndividualRecord;
begin
  p := FChart.Selected;
  if (p <> nil) and (p.Rec <> nil) then begin
    i_rec := p.Rec;
    if FBase.ModifyPerson(i_rec)
    then UpdateChart();
  end;
end;

procedure TfmChart.miEditClick(Sender: TObject);
var
  p: TPerson;
  i_rec: TGEDCOMIndividualRecord;
begin
  p := FChart.Selected;
  if (p <> nil) and (p.Rec <> nil) then begin
    i_rec := p.Rec;
    if FBase.ModifyPerson(i_rec)
    then UpdateChart();
  end;
end;

procedure TfmChart.miFamilyAddClick(Sender: TObject);
var
  p: TPerson;
  fam: TGEDCOMFamilyRecord;
begin
  p := FChart.Selected;
  if (p <> nil) and (p.Rec <> nil) then begin
    if not(p.Rec.Sex in [svMale, svFemale]) then begin
      MessageDlg('У данной персоны не задан пол.', mtError, [mbOk], 0);
      Exit;
    end;

    fam := CreateFamilyEx(FTree);
    AddSpouseToFamily(FTree, fam, p.Rec);

    UpdateChart();
  end;
end;

procedure TfmChart.miSpouseAddClick(Sender: TObject);
var
  p: TPerson;
  sx: TGEDCOMSex;
  i_rec, i_spouse: TGEDCOMIndividualRecord;
  fam: TGEDCOMFamilyRecord;
begin
  p := FChart.Selected;
  if (p <> nil) and (p.Rec <> nil) then begin
    i_rec := p.Rec;
    case i_rec.Sex of
      svMale: sx := svFemale;
      svFemale: sx := svMale;
      else begin
        MessageDlg('У данной персоны не задан пол.', mtError, [mbOk], 0);
        Exit;
      end;
    end;

    i_spouse := Base.SelectPerson(nil, tmNone, sx);
    if (i_spouse <> nil) then begin
      fam := CreateFamilyEx(FTree);
      AddSpouseToFamily(FTree, fam, i_rec);
      AddSpouseToFamily(FTree, fam, i_spouse);

      UpdateChart();
    end;
  end;
end;

procedure TfmChart.InternalChildAdd(aNeedSex: TGEDCOMSex);
var
  p: TPerson;
  i_rec, i_child: TGEDCOMIndividualRecord;
  fam: TGEDCOMFamilyRecord;
begin
  p := FChart.Selected;
  if (p <> nil) and (p.Rec <> nil) then begin
    i_rec := p.Rec;

    if (i_rec.SpouseToFamilyLinksCount = 0) then begin
      MessageDlg('У данной персоны нет семей.', mtWarning, [mbOk], 0);
      Exit;
    end;

    if (i_rec.SpouseToFamilyLinksCount > 1) then begin
      MessageDlg('У данной персоны несколько семей. Выбор еще не реализован.', mtWarning, [mbOk], 0);
      Exit;
    end;

    fam := i_rec.SpouseToFamilyLinks[0].Family;
    i_child := Base.SelectPerson(TGEDCOMIndividualRecord(fam.Husband.Value), tmAncestor, aNeedSex);
    if (i_child <> nil) and FamilyChildAdd(FBase.Tree, fam, i_child)
    then UpdateChart();
  end;
end;

procedure TfmChart.miSonAddClick(Sender: TObject);
begin
  InternalChildAdd(svMale);
end;

procedure TfmChart.miDaughterAddClick(Sender: TObject);
begin
  InternalChildAdd(svFemale);
end;

procedure TfmChart.miDeleteClick(Sender: TObject);
var
  p: TPerson;
begin
  p := FChart.Selected;
  if (p <> nil) and (p.Rec <> nil) and (p <> FChart.Root) then begin
    FBase.DeleteIndividualRecord(p.Rec, True);
    GenChart();
    NavRefresh();
  end;
end;

procedure TfmChart.tbGotoPersonClick(Sender: TObject);
var
  p: TPerson;
begin
  p := FChart.Selected;
  if (p <> nil) and (p.Rec <> nil) then begin
    FPerson := p.Rec;
    GenChart();
    NavRefresh();
  end;
end;

procedure TfmChart.tbPrevClick(Sender: TObject);
begin
  FBackman.BeginNav();
  try
    FPerson := TGEDCOMIndividualRecord(FBackman.Back());
    GenChart();
    NavRefresh();
  finally
    FBackman.EndNav();
  end;
end;

procedure TfmChart.tbNextClick(Sender: TObject);
begin
  FBackman.BeginNav();
  try
    FPerson := TGEDCOMIndividualRecord(FBackman.Next());
    GenChart();
    NavRefresh();
  finally
    FBackman.EndNav();
  end;
end;

procedure TfmChart.NavAdd(aRec: TGEDCOMIndividualRecord);
begin
  if (aRec <> nil) and not(FBackman.Busy) then begin
    FBackman.Current := aRec;
    NavRefresh();
  end;
end;

procedure TfmChart.NavRefresh();
begin
  tbPrev.Enabled := FBackman.CanBackward();
  tbNext.Enabled := FBackman.CanForward();
end;

procedure TfmChart.TrackBar1Change(Sender: TObject);
begin
  GenChart();
end;

end.
