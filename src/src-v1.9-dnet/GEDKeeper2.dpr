program GEDKeeper2;

{%DelphiDotNetAssemblyCompiler '$(SystemRoot)\microsoft.net\framework\v1.1.4322\System.dll'}
{%DelphiDotNetAssemblyCompiler '$(SystemRoot)\microsoft.net\framework\v1.1.4322\System.Data.dll'}
{%DelphiDotNetAssemblyCompiler '$(SystemRoot)\microsoft.net\framework\v1.1.4322\System.Drawing.dll'}
{%DelphiDotNetAssemblyCompiler '$(SystemRoot)\microsoft.net\framework\v1.1.4322\System.Windows.Forms.dll'}
{%DelphiDotNetAssemblyCompiler '$(SystemRoot)\microsoft.net\framework\v1.1.4322\System.XML.dll'}
{%DelphiDotNetAssemblyCompiler '.\ext\ZedGraph.dll'}
{$R 'GKAbout.TfmAbout.resources' 'GKAbout.resx'}
{$R 'GKAddressEdit.TfmAddressEdit.resources' 'GKAddressEdit.resx'}
{$R 'GKAssociationEdit.TfmAssociationEdit.resources' 'GKAssociationEdit.resx'}
{$R 'GKBase.TfmBase.resources' 'GKBase.resx'}
{$R 'GKCalendar.TfmCalendar.resources' 'GKCalendar.resx'}
{$R 'GKChart.TfmChart.resources' 'GKChart.resx'}
{$R 'GKCommunicationEdit.TfmCommunicationEdit.resources' 'GKCommunicationEdit.resx'}
{$R 'GKEventEdit.TfmEventEdit.resources' 'GKEventEdit.resx'}
{$R 'GKExpCalc.TfmCalcWidget.resources' 'GKExpCalc.resx'}
{$R 'GKFamilyEdit.TfmFamilyEdit.resources' 'GKFamilyEdit.resx'}
{$R 'GKFileProperties.TfmFileProperties.resources' 'GKFileProperties.resx'}
{$R 'GKFilter.TfmFilter.resources' 'GKFilter.resx'}
{$R 'GKGroupEdit.TfmGroupEdit.resources' 'GKGroupEdit.resx'}
{$R 'GKLocationEdit.TfmLocationEdit.resources' 'GKLocationEdit.resx'}
{$R 'GKMain.TfmGEDKeeper.resources' 'GKMain.resx'}
{$R 'GKMaps.TfmMaps.resources' 'GKMaps.resx'}
{$R 'GKMediaEdit.TfmMediaEdit.resources' 'GKMediaEdit.resx'}
{$R 'GKMediaView.TfmMediaView.resources' 'GKMediaView.resx'}
{$R 'GKNameEdit.TfmNameEdit.resources' 'GKNameEdit.resx'}
{$R 'GKNamesBook.TfmNamesBook.resources' 'GKNamesBook.resx'}
{$R 'GKNoteEdit.TfmNoteEdit.resources' 'GKNoteEdit.resx'}
{$R 'GKOptions.TfmOptions.resources' 'GKOptions.resx'}
{$R 'GKOrganizer.TfmOrganizer.resources' 'GKOrganizer.resx'}
{$R 'GKPersonEdit.TfmPersonEdit.resources' 'GKPersonEdit.resx'}
{$R 'GKPersonNew.TfmPersonNew.resources' 'GKPersonNew.resx'}
{$R 'GKPersonScan.TfmPersonScan.resources' 'GKPersonScan.resx'}
{$R 'GKProgress.TfmProgress.resources' 'GKProgress.resx'}
{$R 'GKRecordSelect.TfmRecordSelect.resources' 'GKRecordSelect.resx'}
{$R 'GKRepositoryEdit.TfmRepositoryEdit.resources' 'GKRepositoryEdit.resx'}
{$R 'GKResearchEdit.TfmResearchEdit.resources' 'GKResearchEdit.resx'}
{$R 'GKScriptDaemon.TfmScriptDaemon.resources' 'GKScriptDaemon.resx'}
{$R 'GKSexCheck.TfmSexCheck.resources' 'GKSexCheck.resx'}
{$R 'GKSourceCitEdit.TfmSourceCitEdit.resources' 'GKSourceCitEdit.resx'}
{$R 'GKSourceEdit.TfmSourceEdit.resources' 'GKSourceEdit.resx'}
{$R 'GKStats.TfmStats.resources' 'GKStats.resx'}
{$R 'GKTaskEdit.TfmTaskEdit.resources' 'GKTaskEdit.resx'}
{$R 'GKTimeLine.TfmTimeLine.resources' 'GKTimeLine.resx'}
{$R 'GKTipsDlg.TfmTipsDialog.resources' 'GKTipsDlg.resx'}
{$R 'GKTreeFilter.TfmTreeFilter.resources' 'GKTreeFilter.resx'}
{$R 'GKTreeTools.TfmTreeTools.resources' 'GKTreeTools.resx'}
{$R 'GKUserRefEdit.TfmUserRefEdit.resources' 'GKUserRefEdit.resx'}

uses
  System.Reflection,
  System.Runtime.CompilerServices,
  System.Runtime.InteropServices,
  System.Windows.Forms,
  GedCom551 in 'GedCom551.pas',
  GKAbout in 'GKAbout.pas' {GKAbout.TfmAbout: System.Windows.Forms.Form},
  GKAddressEdit in 'GKAddressEdit.pas' {GKAddressEdit.TfmAddressEdit: System.Windows.Forms.Form},
  GKAssociationEdit in 'GKAssociationEdit.pas' {GKAssociationEdit.TfmAssociationEdit: System.Windows.Forms.Form},
  GKBase in 'GKBase.pas' {GKBase.TfmBase: System.Windows.Forms.Form},
  GKCalendar in 'GKCalendar.pas' {GKCalendar.TfmCalendar: System.Windows.Forms.Form},
  GKCalendarCore in 'GKCalendarCore.pas',
  GKChart in 'GKChart.pas' {GKChart.TfmChart: System.Windows.Forms.Form},
  GKChartCore in 'GKChartCore.pas' {GKChartCore.TCustomChartBox: System.Windows.Forms.Panel},
  GKCommands in 'GKCommands.pas',
  GKCommon in 'GKCommon.pas',
  GKCommunicationEdit in 'GKCommunicationEdit.pas' {GKCommunicationEdit.TfmCommunicationEdit: System.Windows.Forms.Form},
  GKCtrls in 'GKCtrls.pas' {GKCtrls.TGKMenuItem: System.Windows.Forms.MenuItem},
  GKEngine in 'GKEngine.pas',
  GKEngineAPI in 'GKEngineAPI.pas',
  GKEventEdit in 'GKEventEdit.pas' {GKEventEdit.TfmEventEdit: System.Windows.Forms.Form},
  GKExpCalc in 'GKExpCalc.pas' {GKExpCalc.TfmCalcWidget: System.Windows.Forms.Form},
  GKExport in 'GKExport.pas',
  GKFamilyEdit in 'GKFamilyEdit.pas' {GKFamilyEdit.TfmFamilyEdit: System.Windows.Forms.Form},
  GKFileProperties in 'GKFileProperties.pas' {GKFileProperties.TfmFileProperties: System.Windows.Forms.Form},
  GKFilter in 'GKFilter.pas' {GKFilter.TfmFilter: System.Windows.Forms.Form},
  GKGroupEdit in 'GKGroupEdit.pas' {GKGroupEdit.TfmGroupEdit: System.Windows.Forms.Form},
  GKImport in 'GKImport.pas',
  GKLangs in 'GKLangs.pas',
  GKLists in 'GKLists.pas' {GKLists.TSheetList: System.Windows.Forms.ContainerControl},
  GKLocationEdit in 'GKLocationEdit.pas' {GKLocationEdit.TfmLocationEdit: System.Windows.Forms.Form},
  GKMain in 'GKMain.pas' {GKMain.TfmGEDKeeper: System.Windows.Forms.Form},
  GKMapBrowser in 'GKMapBrowser.pas' {GKMapBrowser.TMapBrowser: System.Windows.Forms.Control},
  GKMaps in 'GKMaps.pas' {GKMaps.TfmMaps: System.Windows.Forms.Form},
  GKMediaEdit in 'GKMediaEdit.pas' {GKMediaEdit.TfmMediaEdit: System.Windows.Forms.Form},
  GKMediaView in 'GKMediaView.pas' {GKMediaView.TfmMediaView: System.Windows.Forms.Form},
  GKNameEdit in 'GKNameEdit.pas' {GKNameEdit.TfmNameEdit: System.Windows.Forms.Form},
  GKNamesBook in 'GKNamesBook.pas' {GKNamesBook.TfmNamesBook: System.Windows.Forms.Form},
  GKNoteEdit in 'GKNoteEdit.pas' {GKNoteEdit.TfmNoteEdit: System.Windows.Forms.Form},
  GKOptions in 'GKOptions.pas' {GKOptions.TfmOptions: System.Windows.Forms.Form},
  GKOrganizer in 'GKOrganizer.pas' {GKOrganizer.TfmOrganizer: System.Windows.Forms.Form},
  GKPersonEdit in 'GKPersonEdit.pas' {GKPersonEdit.TfmPersonEdit: System.Windows.Forms.Form},
  GKPersonNew in 'GKPersonNew.pas' {GKPersonNew.TfmPersonNew: System.Windows.Forms.Form},
  GKPersonScan in 'GKPersonScan.pas' {GKPersonScan.TfmPersonScan: System.Windows.Forms.Form},
  GKProgress in 'GKProgress.pas' {GKProgress.TfmProgress: System.Windows.Forms.Form},
  GKRecordSelect in 'GKRecordSelect.pas' {GKRecordSelect.TfmRecordSelect: System.Windows.Forms.Form},
  GKRepositoryEdit in 'GKRepositoryEdit.pas' {GKRepositoryEdit.TfmRepositoryEdit: System.Windows.Forms.Form},
  GKResearchEdit in 'GKResearchEdit.pas' {GKResearchEdit.TfmResearchEdit: System.Windows.Forms.Form},
  GKScriptDaemon in 'GKScriptDaemon.pas' {GKScriptDaemon.TfmScriptDaemon: System.Windows.Forms.Form},
  GKSexCheck in 'GKSexCheck.pas' {GKSexCheck.TfmSexCheck: System.Windows.Forms.Form},
  GKSourceCitEdit in 'GKSourceCitEdit.pas' {GKSourceCitEdit.TfmSourceCitEdit: System.Windows.Forms.Form},
  GKSourceEdit in 'GKSourceEdit.pas' {GKSourceEdit.TfmSourceEdit: System.Windows.Forms.Form},
  GKStats in 'GKStats.pas' {GKStats.TfmStats: System.Windows.Forms.Form},
  GKTaskEdit in 'GKTaskEdit.pas' {GKTaskEdit.TfmTaskEdit: System.Windows.Forms.Form},
  GKTimeLine in 'GKTimeLine.pas' {GKTimeLine.TfmTimeLine: System.Windows.Forms.Form},
  GKTipsDlg in 'GKTipsDlg.pas' {GKTipsDlg.TfmTipsDialog: System.Windows.Forms.Form},
  GKTreeFilter in 'GKTreeFilter.pas' {GKTreeFilter.TfmTreeFilter: System.Windows.Forms.Form},
  GKTreeTools in 'GKTreeTools.pas' {GKTreeTools.TfmTreeTools: System.Windows.Forms.Form},
  GKUI.Common.InputBox in 'GKUI.Common.InputBox.pas' {GKUI.Common.InputBox.InputBox: System.Windows.Forms.Form},
  GKUserRefEdit in 'GKUserRefEdit.pas' {GKUserRefEdit.TfmUserRefEdit: System.Windows.Forms.Form},
  GKUtils in 'GKUtils.pas',
  VCLStub in 'VCLStub.pas',
  ExpCalc in 'ExpCalc.pas';

{$R *.res}

{$REGION 'Program/Assembly Information'}
//
// General Information about an assembly is controlled through the following
// set of attributes. Change these attribute values to modify the information
// associated with an assembly.
//
[assembly: AssemblyDescription('')]
[assembly: AssemblyConfiguration('')]
[assembly: AssemblyCompany('')]
[assembly: AssemblyProduct('GEDKeeper')]
[assembly: AssemblyCopyright('Copyright © Serg V. Zhdanovskih')]
[assembly: AssemblyTrademark('')]
[assembly: AssemblyCulture('')]

// The Delphi compiler controls the AssemblyTitleAttribute via the ExeDescription.
// You can set this in the IDE via the Project Options.
// Manually setting the AssemblyTitle attribute below will override the IDE
// setting.
 [assembly: AssemblyTitle('GEDKeeper')]


//
// Version information for an assembly consists of the following four values:
//
//      Major Version
//      Minor Version 
//      Build Number
//      Revision
//
// You can specify all the values or you can default the Revision and Build Numbers 
// by using the '*' as shown below:

[assembly: AssemblyVersion('2.0.437.1')]
//[assembly: AssemblyVersion('1.0.*')]


//
// In order to sign your assembly you must specify a key to use. Refer to the 
// Microsoft .NET Framework documentation for more information on assembly signing.
//
// Use the attributes below to control which key is used for signing. 
//
// Notes: 
//   (*) If no key is specified, the assembly is not signed.
//   (*) KeyName refers to a key that has been installed in the Crypto Service
//       Provider (CSP) on your machine. KeyFile refers to a file which contains
//       a key.
//   (*) If the KeyFile and the KeyName values are both specified, the 
//       following processing occurs:
//       (1) If the KeyName can be found in the CSP, that key is used.
//       (2) If the KeyName does not exist and the KeyFile does exist, the key 
//           in the KeyFile is installed into the CSP and used.
//   (*) In order to create a KeyFile, you can use the sn.exe (Strong Name) utility.
//       When specifying the KeyFile, the location of the KeyFile should be
//       relative to the project output directory. For example, if your KeyFile is
//       located in the project directory, you would specify the AssemblyKeyFile 
//       attribute as [assembly: AssemblyKeyFile('mykey.snk')], provided your output
//       directory is the project directory (the default).
//   (*) Delay Signing is an advanced option - see the Microsoft .NET Framework
//       documentation for more information on this.
//
[assembly: AssemblyDelaySign(false)]
[assembly: AssemblyKeyFile('')]
[assembly: AssemblyKeyName('')]


//
// Use the attributes below to control the COM visibility of your assembly. By
// default the entire assembly is visible to COM. Setting ComVisible to false
// is the recommended default for your assembly. To then expose a class and interface
// to COM set ComVisible to true on each one. It is also recommended to add a
// Guid attribute.
//

[assembly: ComVisible(False)]
//[assembly: Guid('')]
//[assembly: TypeLibVersion(1, 0)]
{$ENDREGION}

[STAThread]
begin
  Application.Run(TfmGEDKeeper.Create);
end.
