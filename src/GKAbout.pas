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

procedure AboutDialog();

implementation

uses
  GKUtils, GKEngine, bsWinUtils, GKMain, uVista;

{$R *.DFM}

procedure AboutDialog();
var
  fmAbout: TfmAbout;
begin
  fmAbout := TfmAbout.Create(fmGEDKeeper);
  try
    fmAbout.LabelProduct.Caption := AppName;
    fmAbout.LabelVersion.Caption := 'Version ' + GetFileVersion();
    fmAbout.LabelCopyright.Caption := 'Copyright © Serg V. Zhdanovskih';
    fmAbout.Label_eMail.Caption := 'http://gedkeeper.ucoz.ru/';

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
  SetVistaFontsEx(Self, True);
end;

procedure TfmAbout.Label_eMailClick(Sender: TObject);
begin
  LoadExtFile(Label_eMail.Caption);
end;

end.
