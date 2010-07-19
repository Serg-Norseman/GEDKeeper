unit GKAbout;

{$I GEDKeeper.inc}

interface

uses
  SysUtils, Classes, Controls, Forms, ExtCtrls, StdCtrls, Buttons;

type
  TfmAbout = class(TForm)
    LabelProduct: TLabel;
    LabelVersion: TLabel;
    btnClose: TBitBtn;
    LabelCopyright: TLabel;
    Label_eMail: TLabel;
    LabelCite: TLabel;
    procedure Label_eMailClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

procedure AboutDialog(ProductName, Copyright, eMail: string);

implementation

uses
  Windows, GKCommon, bsWinUtils, GKMain, uVista;

{$R *.DFM}

procedure AboutDialog(ProductName, Copyright, eMail: string);
var
  fmAbout: TfmAbout;
begin
  fmAbout := TfmAbout.Create(fmGEDKeeper);
  try
    fmAbout.LabelProduct.Caption := ProductName;
    fmAbout.LabelVersion.Caption := 'Version ' + GetFileVersion();
    fmAbout.LabelCopyright.Caption := 'Copyright © ' + Copyright;

    if (eMail = '')
    then fmAbout.Label_eMail.Caption := 'http://gedkeeper.ucoz.ru/'
    else fmAbout.Label_eMail.Caption := eMail;

    fmAbout.LabelCite.Caption :=
      '«История рода - это есть история Отечества»'+#13#10+
      '«Неуважение к предкам - есть первый признак дикости и безнравственности»'+#13#10+
      '(Александр Сергеевич Пушкин)';

    ShowModalEx(fmAbout);
  finally
    fmAbout.Destroy;
  end;
end;

procedure TfmAbout.FormCreate(Sender: TObject);
begin
  if IsWindowsVista() then begin
    SetVistaFonts(Self);
    SetVistaContentFonts(LabelProduct.Font);
    SetVistaContentFonts(LabelVersion.Font);
    SetVistaContentFonts(LabelCopyright.Font);
    SetVistaContentFonts(LabelCite.Font);
    SetVistaContentFonts(Label_eMail.Font);
  end;
end;

procedure TfmAbout.Label_eMailClick(Sender: TObject);
begin
  LoadExtFile(Label_eMail.Caption);
end;

end.
