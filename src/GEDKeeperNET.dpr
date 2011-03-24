program GEDKeeperNET;

{%DelphiDotNetAssemblyCompiler '$(SystemRoot)\microsoft.net\framework\v1.1.4322\System.Drawing.dll'}

uses
  Windows,
  Messages,
  Forms,
  GedCom551 in 'GedCom551.pas',
  GKUtils in 'GKUtils.pas',
  GKEngine in 'GKEngine.pas',
  GKCommon in 'GKCommon.pas',
  GKImport in 'GKImport.pas',
  GKExport in 'GKExport.pas',
  GKChartCore in 'GKChartCore.pas',
  GKLangs in 'GKLangs.pas',
  GKCommands in 'GKCommands.pas',
  GKLists in 'GKLists.pas',
  GKMapBrowser in 'GKMapBrowser.pas',
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
  GKResearchEdit in 'GKResearchEdit.pas' {fmResearchEdit},
  GKTaskEdit in 'GKTaskEdit.pas' {fmTaskEdit},
  GKCommunicationEdit in 'GKCommunicationEdit.pas' {fmCommunicationEdit},
  GKLocationEdit in 'GKLocationEdit.pas' {fmLocationEdit},
  GKTreeTools in 'GKTreeTools.pas' {fmTreeTools},
  GKTipsDlg in 'GKTipsDlg.pas' {fmTipsDialog},
  GKUserRefEdit in 'GKUserRefEdit.pas' {fmUserRefEdit},
  GKExpCalc in 'GKExpCalc.pas' {fmCalcWidget},
  GKNamesBook in 'GKNamesBook.pas' {fmNamesBook},
  GKTimeLine in 'GKTimeLine.pas' {fmTimeLine},
  GKOrganizer in 'GKOrganizer.pas' {fmOrganizer},
  GKDBImport in 'GKDBImport.pas' {fmDBImport},
  GKSexCheck in 'GKSexCheck.pas' {fmSexCheck},
  GKScriptDaemon in 'GKScriptDaemon.pas' {fmScriptDaemon},
  GKCalendar in 'GKCalendar.pas' {fmCalendar},
  GKNameEdit in 'GKNameEdit.pas' {fmNameEdit},
  GKMediaView in 'GKMediaView.pas' {fmMediaView},
  GKMaps in 'GKMaps.pas' {fmMaps};

{$R *.res}

[STAThread]
begin
  RunInstance();
end.
