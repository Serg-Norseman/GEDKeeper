unit GKAbout; {prepare:fin}

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

procedure ShowAbout(Owner: TForm; AppName, AppVersion: string);

implementation

uses
  GKUtils, uVista;

{$R *.DFM}

procedure ShowAbout(Owner: TForm; AppName, AppVersion: string);
var
  fmAbout: TfmAbout;
begin
  fmAbout := TfmAbout.Create(Owner);
  try
    fmAbout.LabelProduct.Caption := AppName;
    fmAbout.LabelVersion.Caption := 'Version ' + AppVersion;
    fmAbout.ShowModal;
  finally
    fmAbout.Destroy;
  end;
end;

procedure TfmAbout.FormCreate(Sender: TObject);
begin
  SetVistaFontsEx(Self, True);

  LabelCopyright.Caption := 'Copyright © Serg V. Zhdanovskih';
  Label_eMail.Caption := 'http://gedkeeper.ucoz.ru/';

  LabelCite.Caption :=
    '«История рода - это есть история Отечества»'+#13#10+
    '«Неуважение к предкам - есть первый признак дикости и безнравственности»'+#13#10+
    '(Александр Сергеевич Пушкин)';
end;

procedure TfmAbout.Label_eMailClick(Sender: TObject);
begin
  LoadExtFile(Label_eMail.Caption);
end;

end.
