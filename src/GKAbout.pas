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
  Windows, ShellAPI;

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

type
  EVerInfoError = class(Exception);

function GetFileVersion(): string;
var
  Size: Cardinal;
  Handle: DWord;
  RezBuffer, FFileName: string;
  fiBuf: PVSFixedFileInfo;
  Ms, Ls: Longint;
begin
  FFileName := ParamStr(0);
  Size := GetFileVersionInfoSize(PChar(FFileName), Handle);

  if (Size <= 0)
  then raise EVerInfoError.Create('Information of version inaccessible.');

  SetLength(RezBuffer, Size);

  if not GetFileVersionInfo(PChar(FFileName), Handle, Size, PChar(RezBuffer))
  then raise EVerInfoError.Create('Impossible define version of file.');

  if VerQueryValue(PChar(RezBuffer), '\', Pointer(fiBuf), Size) then begin
    if (Size < SizeOf(TVSFixedFileInfo))
    then raise EVerInfoError.Create('No fixed file info');
  end else raise EVerInfoError.Create('No fixed file info');

  Ms := fiBuf^.dwFileVersionMS;
  Ls := fiBuf^.dwFileVersionLS;

  Result := Format('%d.%d.%d.%d', [HIWORD(Ms), LOWORD(Ms), HIWORD(Ls), LOWORD(Ls)]);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TfmAbout.FormShow(Sender: TObject);
begin
  Label3.Caption := 'Version ' + GetFileVersion();
end;

procedure TfmAbout.Label_eMailClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PChar(Label_eMail.Caption), '', '', SW_SHOW);
end;

end.
