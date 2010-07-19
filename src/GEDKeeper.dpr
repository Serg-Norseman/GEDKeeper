program GEDKeeper;

{$I GEDKeeper.inc}

uses
  Windows,
  Messages,
  Forms,
  GedCom551 in 'GedCom551.pas',
  GKCommon in 'GKCommon.pas',
  GKImport in 'GKImport.pas',
  GKExport in 'GKExport.pas',
  GKChartCore in 'GKChartCore.pas',
  GKMain in 'GKMain.pas' {fmGEDKeeper},
  GKBase in 'GKBase.pas' {fmBase},
  GKPersonNew in 'GKPersonNew.pas' {fmPersonNew},
  GKRecordSelect in 'GKRecordSelect.pas' {fmRecordSelect},
  GKEventEdit in 'GKEventEdit.pas' {fmEventEdit},
  GKNoteEdit in 'GKNoteEdit.pas' {fmNoteEdit},
  GKSourceEdit in 'GKSourceEdit.pas' {fmSourceEdit},
  GKChart in 'GKChart.pas' {fmChart},
  GKAbout in 'GKAbout.pas' {fmAbout},
  GKFileProperties in 'GKFileProperties.pas' {fmFileProperties},
  GKStats in 'GKStats.pas' {fmStats},
  GKPersonEdit in 'GKPersonEdit.pas' {fmPersonEdit},
  GKOptions in 'GKOptions.pas' {fmOptions},
  GKFamilyEdit in 'GKFamilyEdit.pas' {fmFamilyEdit},
  GKAssociationEdit in 'GKAssociationEdit.pas' {fmAssociationEdit},
  GKFilter in 'GKFilter.pas' {fmFilter},
  GKGroupEdit in 'GKGroupEdit.pas' {fmGroupEdit},
  GKPersonScan in 'GKPersonScan.pas' {fmPersonScan},
  GKProgress in 'GKProgress.pas' {fmProgress},
  GKAddressEdit in 'GKAddressEdit.pas' {fmAddressEdit},
  GKSourceCitEdit in 'GKSourceCitEdit.pas' {fmSourceCitEdit},
  GKRepositoryEdit in 'GKRepositoryEdit.pas' {fmRepositoryEdit},
  GKMediaEdit in 'GKMediaEdit.pas' {fmMediaEdit},
  GKMediaView in 'GKMediaView.pas' {fmMediaView},
  GKMaps in 'GKMaps.pas' {fmMaps},
  GKResearchEdit in 'GKResearchEdit.pas' {fmResearchEdit},
  GKTaskEdit in 'GKTaskEdit.pas' {fmTaskEdit},
  GKCommunicationEdit in 'GKCommunicationEdit.pas' {fmCommunicationEdit},
  GKLocationEdit in 'GKLocationEdit.pas' {fmLocationEdit},
  GKSheetList in 'GKSheetList.pas',
  GKLangs in 'GKLangs.pas',
  GKUIToolkit in 'GKUIToolkit.pas',
  GKCommands in 'GKCommands.pas',
  GKLists in 'GKLists.pas',
  GKTreeTools in 'GKTreeTools.pas' {fmTreeTools},
  GKTipsDlg in 'GKTipsDlg.pas' {fmTipsDialog},
  GKUserRefEdit in 'GKUserRefEdit.pas' {fmUserRefEdit},
  GKExpCalc in 'GKExpCalc.pas' {fmCalcWidget},
  GKNamesBook in 'GKNamesBook.pas' {fmNamesBook},
  GKGenBase in 'GKGenBase.pas',
  GKCalendar in 'GKCalendar.pas' {fmCalendar};

{$R *.res}

function GetCurrentFile(): PChar; far;
begin
  Result := PChar(fmGEDKeeper.GetCurrentFileName());
end;

exports
  GetCurrentFile;

procedure RunInstance();
var
  i, WParam, LParam: Integer;
  hMainForm: hwnd;
  copyDataStruct: TCopyDataStruct;
  ParamString: string;
begin
  // ищем главное окно приложения, вместо Caption - nil,
  // поскольку к заголовку главного окна может добавиться заголовок MDIChild
  // (нужно позаботиться об уникальности имени класса главной формы)

  hMainForm := 0;//FindWindow('TfmGEDKeeper', nil);
  if (hMainForm = 0) then begin
    Application.Initialize;
    Application.Title := 'GEDKeeper';
    Application.CreateForm(TfmGEDKeeper, fmGEDKeeper);
  for i := 1 to ParamCount do fmGEDKeeper.CreateBase(ParamStr(i));
    Application.Run;
  end else begin
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
  end;
end;

begin
  RunInstance();
end.
