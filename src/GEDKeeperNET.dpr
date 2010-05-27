program GEDKeeperNET;

{$I GEDKeeper.inc}

{%DelphiDotNetAssemblyCompiler '$(SystemRoot)\microsoft.net\framework\v1.1.4322\System.Drawing.dll'}

uses
  Windows,
  Messages,
  Forms,
  GedCom551 in 'GedCom551.pas',
  GKCommon in 'GKCommon.pas',
  GKMain in 'GKMain.pas' {fmGEDKeeper},
  GKBase in 'GKBase.pas' {fmBase},
  GKPersonNew in 'GKPersonNew.pas' {fmPersonNew},
  GKRecordSelect in 'GKRecordSelect.pas' {fmRecordSelect},
  GKEventEdit in 'GKEventEdit.pas' {fmEventEdit},
  GKNoteEdit in 'GKNoteEdit.pas' {fmNoteEdit},
  GKSourceEdit in 'GKSourceEdit.pas' {fmSourceEdit},
  GKChart in 'GKChart.pas' {fmChart},
  GKAbout in 'GKAbout.pas' {fmAbout},
  GKChartCore in 'GKChartCore.pas',
  GKFileProperties in 'GKFileProperties.pas' {fmFileProperties},
  GKTreeTools in 'GKTreeTools.pas' {fmTreeTools},
  GKStats in 'GKStats.pas' {fmStats},
  GKPersonEdit in 'GKPersonEdit.pas' {fmPersonEdit},
  GKExport in 'GKExport.pas',
  GKOptions in 'GKOptions.pas' {fmOptions},
  GKFamilyEdit in 'GKFamilyEdit.pas' {fmFamilyEdit},
  GKAssociationEdit in 'GKAssociationEdit.pas' {fmAssociationEdit},
  GKFilter in 'GKFilter.pas' {fmFilter},
  GKGroupEdit in 'GKGroupEdit.pas' {fmGroupEdit},
  GKPersonScan in 'GKPersonScan.pas' {fmPersonScan},
  GKProgress in 'GKProgress.pas' {fmProgress},
  GKAddressEdit in 'GKAddressEdit.pas' {fmAddressEdit},
  GKImport in 'GKImport.pas',
  GKSourceCitEdit in 'GKSourceCitEdit.pas' {fmSourceCitEdit},
  GKRepositoryEdit in 'GKRepositoryEdit.pas' {fmRepositoryEdit},
  GKMediaEdit in 'GKMediaEdit.pas' {fmMediaEdit}
  {$IFNDEF DELPHI_NET}
  , GKMaps in 'GKMaps.pas' {fmMaps}
  , GKMediaView in 'GKMediaView.pas' {fmMediaView}
  {$ENDIF};

{$R *.res}

procedure RunInstance();
var
  i: integer;
  hMainForm: hwnd;
  copyDataStruct: TCopyDataStruct;
  ParamString: string;
  WParam, LParam: integer;
begin
  // ищем главное окно приложения, вместо Caption - nil,
  // поскольку к заголовку главного окна может добавиться заголовок MDIChild
  // (нужно позаботиться об уникальности имени класса главной формы)

  hMainForm := FindWindow('TfmGEDKeeper', nil);
  if (hMainForm = 0) then begin
    Application.Initialize;
    Application.Title := 'GEDKeeper';
    Application.CreateForm(TfmGEDKeeper, fmGEDKeeper);
  for i := 1 to ParamCount do fmGEDKeeper.CreateBase(ParamStr(i));
    Application.Run;
  end else begin
    {$IFNDEF DELPHI_NET}
    ParamString := '';
    for i := 1 to ParamCount do begin
      // запихиваем все параметры в одну строку с разделителями ?13
      ParamString := ParamString + ParamStr(i) + #13;
    end;
    // создаем запись типа TCopyDataStruct

    CopyDataStruct.lpData := PChar(ParamString);
    CopyDataStruct.cbData := Length(ParamString);
    CopyDataStruct.dwData := 0;
    WParam := Application.Handle;
    LParam := Integer(@CopyDataStruct);
    // отсылаем сообщение WM_COPYDATA главному окну открытого приложения

    SendMessage(hMainForm, WM_CopyData, WParam, LParam);
    Application.Terminate;
    {$ENDIF}
  end;
end;

[STAThread]
begin
  RunInstance();
end.
