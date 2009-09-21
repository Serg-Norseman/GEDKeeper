unit GKNameEdit;

{$I GEDKeeper.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TfmNameEdit = class(TForm)
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    EditKind: TComboBox;
    EditName: TEdit;
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

implementation

uses GedCom551, GKCommon;

{$R *.dfm}

procedure TfmNameEdit.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  EditKind.Clear;
  for i := 0 to NamePiecesSize - 1 do EditKind.Items.Add(NamePieces[i].Name);
end;

end.
