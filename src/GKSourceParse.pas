unit GKSourceParse;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, Grids;

type
  TfmSourceParse = class(TForm)
    Label1: TLabel;
    cbSource: TComboBox;
    Label2: TLabel;
    edPage: TEdit;
    Label3: TLabel;
    edSourceYear: TMaskEdit;
    Label4: TLabel;
    edPlace: TEdit;
    StringGrid1: TStringGrid;
    cbPersonLink: TComboBox;
    btnAdd: TButton;
    procedure StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure cbPersonLinkKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure StringGrid1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure cbPersonLinkExit(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
  private
    procedure InitGrid();
    procedure InitControls();
  public
  end;

var
  fmSourceParse: TfmSourceParse;

implementation

{$R *.dfm}

type
  TPersonLink = (plPerson, plFather, plMother, plGodparent, plSpouse);

const
  PersonLinks: array [TPersonLink] of string = (
    'Лицо', 'Отец', 'Мать', 'Крестный', 'Супруг(-а)'
  );

procedure TfmSourceParse.StringGrid1SelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  R: TRect;
  idx: Integer;
begin
  if (ACol = 0) then begin
    idx := cbPersonLink.Items.IndexOf(StringGrid1.Cells[0, ARow]);
    if (idx < 0) then idx := 0;
    cbPersonLink.ItemIndex := idx;

    R := StringGrid1.CellRect(ACol, ARow);
    R.Left  := R.Left + StringGrid1.Left;
    R.Right := R.Right + StringGrid1.Left;
    R.Top := R.Top + StringGrid1.Top;
    R.Bottom := R.Bottom + StringGrid1.Top;
    cbPersonLink.Top := R.Top + 2;
    cbPersonLink.Left := R.Left + 2;
    cbPersonLink.Width := (R.Right - R.Left);
    cbPersonLink.Height := (R.Bottom - R.Top);
    cbPersonLink.Visible := True;
    cbPersonLink.SetFocus;
  end else begin
    cbPersonLink.Visible := False;
  end;

  CanSelect := True;
end;

procedure TfmSourceParse.cbPersonLinkExit(Sender: TObject);
begin
  StringGrid1.Cells[0, StringGrid1.Row] := cbPersonLink.Text;
end;

procedure TfmSourceParse.cbPersonLinkKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) then begin
    StringGrid1.SetFocus;
    StringGrid1.Col := StringGrid1.Col + 1;
  end;
end;

procedure TfmSourceParse.StringGrid1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) then begin
    if (StringGrid1.Col = 5) then begin
      StringGrid1.Col := 0;
      StringGrid1.Row := StringGrid1.Row + 1;
    end else StringGrid1.Col := StringGrid1.Col + 1;
  end;
end;

procedure TfmSourceParse.InitControls();
var
  col, row: Integer;
begin
  cbSource.Text := '';
  cbSource.ItemIndex := -1;

  edPage.Text := '';
  edSourceYear.Text := '';
  edPlace.Text := '';

  for row := 1 to StringGrid1.RowCount - 1 do
    for col := 0 to StringGrid1.ColCount - 1 do
      StringGrid1.Cells[col, row] := '';    
end;

procedure TfmSourceParse.InitGrid();
begin
  StringGrid1.Cells[0, 0] := 'Связь';
  StringGrid1.Cells[1, 0] := 'Имя';
  StringGrid1.Cells[2, 0] := 'Отчество';
  StringGrid1.Cells[3, 0] := 'Фамилия';
  StringGrid1.Cells[4, 0] := 'Возраст';
  StringGrid1.Cells[5, 0] := 'Примечание';

  StringGrid1.DefaultRowHeight := StringGrid1.Canvas.TextHeight('A') + 7;
  StringGrid1.ColWidths[4] := 60;
  StringGrid1.ColWidths[5] := 150;
end;

procedure TfmSourceParse.FormCreate(Sender: TObject);
var
  pl: TPersonLink;
begin
  InitGrid();
  InitControls();

  for pl := Low(TPersonLink) to High(TPersonLink) do cbPersonLink.Items.Add(PersonLinks[pl]);
end;

procedure TfmSourceParse.btnAddClick(Sender: TObject);
begin
  InitControls();
end;

end.
