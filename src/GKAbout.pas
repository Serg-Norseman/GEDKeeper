unit GKAbout;

{$I GEDKeeper.inc}

interface

uses
  SysUtils, Classes, Controls, Forms, ExtCtrls, StdCtrls, Buttons;

type
  TfmAbout = class(TForm)
    LabelProduct: TLabel;
    Label3: TLabel;
    btnClose: TBitBtn;
    LabelCopyright: TLabel;
    Label_eMail: TLabel;
    LabelCite: TLabel;
    procedure FormShow(Sender: TObject);
    procedure Label_eMailClick(Sender: TObject);
  private
  public
  end;

procedure AboutDialog(ProductName, Copyright, eMail: string);

implementation

uses
  Windows, GKCommon, bsWinUtils;

{$R *.DFM}

procedure AboutDialog(ProductName, Copyright, eMail: string);
var
  fmAbout: TfmAbout;
begin
  fmAbout := TfmAbout.Create(Application);
  try
    fmAbout.LabelProduct.Caption := ProductName;
    fmAbout.LabelCopyright.Caption := 'Copyright © ' + Copyright;

    if (eMail = '')
    then fmAbout.Label_eMail.Caption := 'mailto:serg.alchemist@gmail.com'
    else fmAbout.Label_eMail.Caption := eMail;

    fmAbout.LabelCite.Caption :=
      '«История рода - это есть история Отечества»'+#13#10+
      '«Неуважение к предкам - есть первый признак дикости и безнравственности»'+#13#10+
      '(Александр Сергеевич Пушкин)';

    fmAbout.ShowModal;
  finally
    fmAbout.Destroy;
  end;
end;

procedure TfmAbout.FormShow(Sender: TObject);
begin
  Label3.Caption := 'Version ' + GetFileVersion();
end;

procedure TfmAbout.Label_eMailClick(Sender: TObject);
begin
  LoadExtFile(Label_eMail.Caption);
end;

end.
