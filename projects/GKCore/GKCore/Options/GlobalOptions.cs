/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using BSLib;
using GDModel;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Types;

namespace GKCore.Options
{
    public enum OptionsPage
    {
        opCommon, opTreeChart, opCircleChart, opInterface, opPedigree, opMultimedia
    }

    /// <summary>
    ///
    /// </summary>
    public sealed class GlobalOptions : BaseObject, IOptions
    {
        public const int OPTS_VERSION = 3;

        private static GlobalOptions fInstance = null;

        private readonly CircleChartOptions fCircleChartOptions;
        private readonly StringList fEventFilters;
        private readonly StringList fFARPatterns;
        private readonly StringList fFARReplacements;
        private readonly List<LangRecord> fLanguages;
        private readonly StringList fLastBases;
        private readonly ListOptionsCollection fListOptions;
        private readonly List<MRUFile> fMRUFiles;
        private readonly StringList fNameFilters;
        private readonly PedigreeOptions fPedigreeOptions;
        private readonly ProxyOptions fProxy;
        private readonly StringList fRelations;
        private readonly StringList fResidenceFilters;
        private readonly Dictionary<GDMRecordType, StringList> fRSFilters;
        private readonly TreeChartOptions fTreeChartOptions;

        private ExtRect fMWinRect;


        public static GlobalOptions Instance
        {
            get {
                if (fInstance == null) fInstance = new GlobalOptions();
                return fInstance;
            }
        }


        public bool AllowDeleteMediaFileFromStgArc { get; set; }

        public bool AllowDeleteMediaFileFromRefs { get; set; }

        public bool AllowMediaStoreReferences { get; set; }

        public bool AllowMediaStoreRelativeReferences { get; set; }

        public bool AutoCheckUpdates { get; set; }

        public bool Autosave { get; set; }

        public int AutosaveInterval { get; set; }

        public bool AutoSortChildren { get; set; }

        public bool AutoSortSpouses { get; set; }

        public CertaintyAlgorithm CertaintyAlgorithm { get; set; }

        public bool CharsetDetection { get; set; }

        public bool CheckTreeSize { get; set; }

        public CircleChartOptions CircleChartOptions
        {
            get { return fCircleChartOptions; }
        }

        public GEDCOMCharacterSet DefCharacterSet
        {
            get { return GEDCOMCharacterSet.csUTF8; }
        }

        public DateFormat DefDateFormat { get; set; }

        public NameFormat DefNameFormat { get; set; }

        public bool DeleteMediaFileWithoutConfirm { get; set; }

        public bool DialogClosingWarn { get; set; }

        public bool DisplayFullFileName { get; set; }

        public bool EmbeddedMediaPlayer { get; set; }

        public StringList EventFilters
        {
            get { return fEventFilters; }
        }

        public bool ExtendedKinships { get; set; }

        public bool ExtendedLocations { get; set; }

        public bool EL_AbbreviatedNames { get; set; }

        public StringList FARPatterns
        {
            get { return fFARPatterns; }
        }

        public StringList FARReplacements
        {
            get { return fFARReplacements; }
        }

        public FileBackup FileBackup { get; set; }

        public int FileBackupEachRevisionMaxCount { get; set; }

        public bool FilesOverwriteWarn { get; set; }

        public bool FirstCapitalLetterInNames { get; set; }

        public string Geocoder { get; set; }

        public string GeoSearchCountry { get; set; }

        public int InterfaceLang { get; set; }

        public int InfoPansOverallSize { get; set; }

        public bool KeepInfoPansOverallSize { get; set; }

        public bool KeepRichNames { get; set; }

        public IList<LangRecord> Languages
        {
            get { return fLanguages; }
        }

        public StringList LastBases
        {
            get { return fLastBases; }
        }

        public string LastDir { get; set; }

        public bool HighlightInaccessibleFiles { get; set; }

        public string ImageExportLastDir { get; set; }

        public bool ListHighlightUnmarriedPersons { get; set; }

        public bool ListHighlightUnparentedPersons { get; set; }

        public ListOptionsCollection ListOptions
        {
            get { return fListOptions; }
        }

        public bool LocalizedCalendarSignatures { get; set; }

        public bool LoadRecentFiles { get; set; }

        public ChartWindowsShowMode ChartWindowsShowMode { get; set; }

        public MatchPatternMethod MatchPatternMethod { get; set; }

        public MediaStoreType MediaStoreDefault { get; set; }

        public List<MRUFile> MRUFiles
        {
            get { return fMRUFiles; }
        }

        public ExtRect MWinRect
        {
            get { return fMWinRect; }
            set { fMWinRect = value; }
        }

        public WindowState MWinState { get; set; }

        public bool MultipagePrint { get; set; }

        public StringList NameFilters
        {
            get { return fNameFilters; }
        }

        public PedigreeOptions PedigreeOptions
        {
            get { return fPedigreeOptions; }
        }

        public bool PlacesWithAddress { get; set; }

        public ProxyOptions Proxy
        {
            get { return fProxy; }
        }

        public bool ReadabilityHighlightRows { get; set; }

        public StringList Relations
        {
            get { return fRelations; }
        }

        public bool RemovableMediaWarning { get; set; }

        public string ReportExportLastDir { get; set; }

        public StringList ResidenceFilters
        {
            get { return fResidenceFilters; }
        }

        /// <summary>
        /// Initial implementation (now just default) for enumerating ATDs: from largest to smallest.
        /// Reverse order of places: from smallest to largest.
        ///
        /// This option affects the display of places in trees and in event editing dialogs.
        /// </summary>
        public bool ReversePlaceEntitiesOrder { get; set; }

        public string ScriptsLastDir { get; set; }

        public bool SearchAndFilterByAllNames { get; set; }

        /// <summary>
        /// Maps.
        /// </summary>
        public bool SearchPlacesWithoutCoords { get; set; }

        public bool ShortKinshipForm { get; set; }

        public bool ShowDatesCalendar { get; set; }

        public bool ShowDatesSign { get; set; }

        /// <summary>
        /// Hidden option.
        /// </summary>
        public bool ShowIndiAssociations { get; set; }

        /// <summary>
        /// Hidden option.
        /// </summary>
        public bool ShowIndiNamesakes { get; set; }

        public bool ShowNumberOfSubstructures { get; set; }

        public bool ShowTips { get; set; }

        public bool SurnameFirstInOrder { get; set; }

        public bool SurnameInCapitals { get; set; }

        public string Theme { get; set; }

        public TreeChartOptions TreeChartOptions
        {
            get { return fTreeChartOptions; }
        }

        public bool UnrestrictedExtendedSurnames { get; set; }

        public bool UseExtendedNotes { get; set; }

        public bool UseSurnamesInPersonSelectionFilter { get; set; }

        public bool UseBirthDatesInPersonSelectionFilter { get; set; }

        public WomanSurnameFormat WomanSurnameFormat { get; set; }

        public bool SimpleSingleSurnames { get; set; }


        private GlobalOptions()
        {
            fTreeChartOptions = new TreeChartOptions();
            fEventFilters = new StringList();
            fMRUFiles = new List<MRUFile>();
            fNameFilters = new StringList();
            fResidenceFilters = new StringList();
            fPedigreeOptions = new PedigreeOptions();
            fProxy = new ProxyOptions();
            fRelations = new StringList();
            fCircleChartOptions = new CircleChartOptions();
            fLanguages = new List<LangRecord>();
            fLastBases = new StringList();
            fListOptions = new ListOptionsCollection();
            fFARPatterns = new StringList();
            fFARReplacements = new StringList();
            fRSFilters = new Dictionary<GDMRecordType, StringList>();

            ResetDefaults();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fFARPatterns.Dispose();
                fFARReplacements.Dispose();

                fLastBases.Dispose();
                fRelations.Dispose();

                fResidenceFilters.Dispose();
                fNameFilters.Dispose();
                fEventFilters.Dispose();
            }
            base.Dispose(disposing);
        }

        public void ResetDefaults()
        {
            ResetDefaults_Common();
            ResetDefaults_Media();
            ResetDefaults_Interface();
            ResetDefaults_Specials();
        }

        public void ResetDefaults_Common()
        {
            fProxy.ResetDefaults();

            Autosave = false;
            AutosaveInterval = 10;
            FileBackupEachRevisionMaxCount = 0;

            ShowTips = true;
            LoadRecentFiles = false;
            AutoCheckUpdates = true;
            CharsetDetection = false;
            DialogClosingWarn = false;
            Geocoder = "Google";
            GeoSearchCountry = string.Empty;
            CertaintyAlgorithm = CertaintyAlgorithm.WeightedAverage;
            DisplayFullFileName = false;

            Theme = string.Empty;

            // hidden
            ReadabilityHighlightRows = true;
            ReversePlaceEntitiesOrder = false;
            MultipagePrint = false;

            // other section
            CheckTreeSize = true;
        }

        public void ResetDefaults_Media()
        {
            RemovableMediaWarning = true;
            EmbeddedMediaPlayer = true;
            AllowMediaStoreReferences = false;
            AllowMediaStoreRelativeReferences = true;
            MediaStoreDefault = 0;
            AllowDeleteMediaFileFromStgArc = true;
            AllowDeleteMediaFileFromRefs = false;
            DeleteMediaFileWithoutConfirm = false;
            HighlightInaccessibleFiles = false;
        }

        public void ResetDefaults_Interface()
        {
            DefNameFormat = NameFormat.nfFNP;
            DefDateFormat = DateFormat.dfDD_MM_YYYY;
            ShowDatesCalendar = false;
            ShowDatesSign = false;
            LocalizedCalendarSignatures = false;
            PlacesWithAddress = false;
            ListHighlightUnparentedPersons = false;
            ListHighlightUnmarriedPersons = false;
            AutoSortChildren = true;
            AutoSortSpouses = false;
            FirstCapitalLetterInNames = false;
            ShortKinshipForm = false;
            SurnameFirstInOrder = true;
            SurnameInCapitals = false;
            UseSurnamesInPersonSelectionFilter = false;
            UseBirthDatesInPersonSelectionFilter = false;
            ShowIndiAssociations = false;
            ShowIndiNamesakes = true;
            MatchPatternMethod = MatchPatternMethod.RegEx;

            WomanSurnameFormat = WomanSurnameFormat.wsfNotExtend;
            SimpleSingleSurnames = false;
            UnrestrictedExtendedSurnames = false;
        }

        public void ResetDefaults_Specials()
        {
            UseExtendedNotes = false;
            KeepRichNames = true;
            ChartWindowsShowMode = ChartWindowsShowMode.Default;
            SearchAndFilterByAllNames = false;
            FilesOverwriteWarn = true;

            InfoPansOverallSize = -1;
            KeepInfoPansOverallSize = false;
            ExtendedKinships = false;

            ExtendedLocations = false;
            EL_AbbreviatedNames = true;

            ShowNumberOfSubstructures = false;

            // maps?
            SearchPlacesWithoutCoords = false;
        }

        public void Assign(IOptions source)
        {
            // dummy
        }

        #region Language

        private void LngPrepareProc(string fileName, Assembly resAssembly)
        {
            try {
                using (var inputStream = (resAssembly == null) ? new FileStream(fileName, FileMode.Open, FileAccess.Read) : resAssembly.GetManifestResourceStream(fileName)) {
                    using (var reader = new StreamReader(inputStream, Encoding.UTF8)) {
                        string st = reader.ReadLine(); // header

                        if (!string.IsNullOrEmpty(st) && st[0] == ';') {
                            st = st.Remove(0, 1);
                            string[] lngParams = st.Split(',');
                            if (lngParams.Length < 3)
                                throw new GKException("Header is incorrect");

                            string lngCode = lngParams[0];
                            string lngSign = lngParams[1];
                            string lngName = lngParams[2];

                            LangRecord lngRec = new LangRecord((ushort)int.Parse(lngCode), lngSign, lngName, fileName, resAssembly);
                            fLanguages.Add(lngRec);
                        }
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GlobalOptions.LngPrepareProc(" + fileName + ")", ex);
            }
        }

        public void FindLanguages()
        {
            try {
                var appInstance = AppHost.Instance;

                if (!appInstance.HasFeatureSupport(Feature.EmbeddedLocales)) {
                    string path = GKUtils.GetLangsPath();
                    string[] langFiles = Directory.GetFiles(path, "*.lng", SearchOption.TopDirectoryOnly);
                    for (int i = 0; i < langFiles.Length; i++) {
                        LngPrepareProc(langFiles[i], null);
                    }
                } else {
                    var appAssembly = appInstance.GetType().Assembly;
                    string path = "Resources.locales";
                    string[] langFiles = appAssembly.GetManifestResourceNames().Where(r => r.StartsWith(path) && r.EndsWith(".lng")).ToArray();
                    for (int i = 0; i < langFiles.Length; i++) {
                        LngPrepareProc(langFiles[i], appAssembly);
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GlobalOptions.FindLanguages()", ex);
            }
        }

        public void LoadLanguage(int langCode)
        {
            if (langCode != LangMan.LS_DEF_CODE) {
                bool loaded = false;

                foreach (LangRecord langRec in fLanguages) {
                    if (langRec.Code == langCode) {
                        loaded = LangMan.LoadFromFile(langRec.FileName, langRec.ResAssembly);
                        break;
                    }
                }

                if (!loaded) langCode = LangMan.LS_DEF_CODE;
            }

            if (langCode == LangMan.LS_DEF_CODE) {
                LangMan.DefInit();
            }

            InterfaceLang = (ushort)langCode;

            LocaleOptions.Instance.SetLocale((ushort)langCode);
        }

        public LangRecord GetLangByCode(int code)
        {
            foreach (LangRecord lngRec in fLanguages) {
                if (lngRec.Code == code) {
                    return lngRec;
                }
            }

            return null;
        }

        public string GetLanguageSign()
        {
            LangRecord lngrec = GetLangByCode(InterfaceLang);
            string lngSign = (lngrec == null) ? LangMan.LS_DEF_SIGN : lngrec.Sign;
            return lngSign;
        }

        public GDMLanguageID GetCurrentItfLang()
        {
            if (InterfaceLang == LangMan.LS_DEF_CODE) {
                return GDMLanguageID.English;
            } else {
                LangRecord langRec = GetLangByCode(InterfaceLang);
                return (langRec == null) ? GDMLanguageID.English : langRec.LangID;
            }
        }

        #endregion

        #region MRU

        private static bool EqualsFileNames(string mruFileName, string fileName)
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                return mruFileName == fileName;
            } else {
                return Path.GetFileName(mruFileName) == Path.GetFileName(fileName);
            }
        }

        public int MRUFiles_IndexOf(string fileName)
        {
            int num = fMRUFiles.Count;
            for (int i = 0; i < num; i++) {
                if (EqualsFileNames(fMRUFiles[i].FileName, fileName)) {
                    return i;
                }
            }

            return -1;
        }

        public static void LoadMRUFromFile(IniFile ini, List<MRUFile> mruFiles)
        {
            if (ini == null)
                throw new ArgumentNullException("ini");

            try {
                int cnt = ini.ReadInteger("Common", "MRUFiles_Count", 0);
                for (int i = 0; i < cnt; i++) {
                    string sect = "MRUFile_" + i.ToString();
                    string fn = ini.ReadString(sect, "FileName", "");
                    if (File.Exists(fn)) {
                        MRUFile mf = new MRUFile();
                        mf.LoadFromFile(ini, sect);
                        mruFiles.Add(mf);
                    } else {
                        MRUFile.DeleteKeys(ini, sect);
                    }
                }

                // remove duplicates (only for Mobile)
                if (AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                    for (int i = 0; i < cnt; i++) {
                        for (int k = cnt - 1; k > i; k--) {
                            if (EqualsFileNames(mruFiles[i].FileName, mruFiles[k].FileName)) {
                                mruFiles.RemoveAt(k);
                                cnt--;
                            }
                        }
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GlobalOptions.LoadMRUFromFile()", ex);
            }
        }

        #endregion

        #region Plugins

        public void LoadPluginsFromFile(IniFile ini)
        {
            if (ini == null)
                throw new ArgumentNullException("ini");

            try {
                int num = AppHost.Plugins.Count;
                for (int i = 0; i < num; i++) {
                    var plugin = AppHost.Plugins[i];

                    var dlgPlugin = plugin as IDialogReplacement;
                    if (dlgPlugin != null) {
                        var plgName = plugin.GetType().Name;
                        dlgPlugin.Enabled = ini.ReadBool("Plugins", plgName + ".Enabled", false);
                    }

                    plugin.LoadOptions(ini);
                }
            } catch (Exception ex) {
                Logger.WriteError("GlobalOptions.LoadPluginsFromFile()", ex);
            }
        }

        public void SavePluginsToFile(IniFile ini)
        {
            if (ini == null)
                throw new ArgumentNullException("ini");

            try {
                int num = AppHost.Plugins.Count;
                for (int i = 0; i < num; i++) {
                    var plugin = AppHost.Plugins[i];

                    var dlgPlugin = plugin as IDialogReplacement;
                    if (dlgPlugin != null) {
                        var plgName = plugin.GetType().Name;
                        ini.WriteBool("Plugins", plgName + ".Enabled", dlgPlugin.Enabled);
                    }

                    plugin.SaveOptions(ini);
                }
            } catch (Exception ex) {
                Logger.WriteError("GlobalOptions.SavePluginsToFile()", ex);
            }
        }

        #endregion

        #region Any strings

        private void LoadStringList(IniFile ini, StringList list, string section, string itemPrefix = "Item_")
        {
            if (ini == null)
                throw new ArgumentNullException("ini");

            try {
                int cnt = ini.ReadInteger(section, "Count", 0);
                for (int i = 0; i < cnt; i++) {
                    string st = ini.ReadString(section, itemPrefix + i.ToString(), "");
                    if (st != "") list.Add(st);
                }
            } catch (Exception ex) {
                Logger.WriteError("GlobalOptions.LoadStringList()", ex);
            }
        }

        private void SaveStringList(IniFile ini, StringList list, string section, string itemPrefix = "Item_")
        {
            if (ini == null)
                throw new ArgumentNullException("ini");

            try {
                int cnt = list.Count;
                ini.WriteInteger(section, "Count", cnt);
                for (int i = 0; i < cnt; i++) {
                    ini.WriteString(section, itemPrefix + i.ToString(), list[i]);
                }
            } catch (Exception ex) {
                Logger.WriteError("GlobalOptions.SaveStringList()", ex);
            }
        }

        #endregion

        #region Features

        public bool CanExtendedSurname(GDMSex selectedSex)
        {
            bool result = (WomanSurnameFormat != WomanSurnameFormat.wsfNotExtend) && (UnrestrictedExtendedSurnames || (selectedSex == GDMSex.svFemale));
            return result;
        }

        #region Record Select Dialog Filters

        public StringList GetRSFilters(GDMRecordType rt)
        {
            StringList result;
            if (!fRSFilters.TryGetValue(rt, out result)) {
                result = new StringList();
                fRSFilters[rt] = result;
            }
            return result;
        }

        private void LoadRSFilters(IniFile ini)
        {
            if (ini == null)
                throw new ArgumentNullException("ini");

            try {
                for (var rt = GDMRecordType.rtIndividual; rt <= GDMRecordType.rtLocation; rt++) {
                    var sectName = GKData.RecordTypes[(int)rt].Sign + "_RSFilters";
                    var list = GetRSFilters(rt);
                    LoadStringList(ini, list, sectName, "Flt_");
                }
            } catch (Exception ex) {
                Logger.WriteError("GlobalOptions.LoadRSFilters()", ex);
            }
        }

        private void SaveRSFilters(IniFile ini)
        {
            if (ini == null)
                throw new ArgumentNullException("ini");

            try {
                for (var rt = GDMRecordType.rtIndividual; rt <= GDMRecordType.rtLocation; rt++) {
                    var sectName = GKData.RecordTypes[(int)rt].Sign + "_RSFilters";
                    var list = GetRSFilters(rt);
                    SaveStringList(ini, list, sectName, "Flt_");
                }
            } catch (Exception ex) {
                Logger.WriteError("GlobalOptions.SaveRSFilters()", ex);
            }
        }

        #endregion

        #endregion

        public void LoadFromFile(IniFile ini)
        {
            if (ini == null)
                throw new ArgumentNullException("ini");

            int optsVersion = ini.ReadInteger("Common", "OptsVersion", 0);

            DefNameFormat = (NameFormat)ini.ReadInteger("Common", "DefNameFormat", 0);
            DefDateFormat = (DateFormat)ini.ReadInteger("Common", "DefDateFormat", 0);
            LastDir = ini.ReadString("Common", "LastDir", "");
            PlacesWithAddress = ini.ReadBool("Common", "PlacesWithAddress", false);
            ShowTips = ini.ReadBool("Common", "ShowTips", true);
            InterfaceLang = (ushort)ini.ReadInteger("Common", "InterfaceLang", 0);
            FileBackup = (FileBackup)ini.ReadInteger("Common", "FileBackup", 0);
            FileBackupEachRevisionMaxCount = ini.ReadInteger("Common", "FileBackupEachRevisionMaxCount", 0);
            ShowDatesCalendar = ini.ReadBool("Common", "ShowDatesCalendar", false);
            ShowDatesSign = ini.ReadBool("Common", "ShowDatesSigns", false);
            RemovableMediaWarning = ini.ReadBool("Common", "RemovableMediaWarning", true);
            LoadRecentFiles = ini.ReadBool("Common", "LoadRecentFiles", false);
            EmbeddedMediaPlayer = ini.ReadBool("Common", "EmbeddedMediaPlayer", true);
            AllowMediaStoreReferences = ini.ReadBool("Common", "AllowMediaStoreReferences", false);
            AllowMediaStoreRelativeReferences = ini.ReadBool("Common", "AllowMediaStoreRelativeReferences", true); // only when AllowMediaStoreReferences is true
            MediaStoreDefault = (MediaStoreType)ini.ReadInteger("Common", "MediaStoreDefault", 0); // (int)MediaStoreType.mstReference
            AllowDeleteMediaFileFromStgArc = ini.ReadBool("Common", "AllowDeleteMediaFileFromStgArc", true);
            AllowDeleteMediaFileFromRefs = ini.ReadBool("Common", "AllowDeleteMediaFileFromRefs", false);
            DeleteMediaFileWithoutConfirm = ini.ReadBool("Common", "DeleteMediaFileWithoutConfirm", false);
            AutoCheckUpdates = ini.ReadBool("Common", "AutoCheckUpdates", true);
            AutoSortChildren = ini.ReadBool("Common", "AutoSortChildren", true);
            AutoSortSpouses = ini.ReadBool("Common", "AutoSortSpouses", false);
            CharsetDetection = ini.ReadBool("Common", "CharsetDetection", false);
            FirstCapitalLetterInNames = ini.ReadBool("Common", "FirstCapitalLetterInNames", false);
            DialogClosingWarn = ini.ReadBool("Common", "DialogClosingWarn", false);
            ShortKinshipForm = ini.ReadBool("Common", "ShortKinshipForm", false);
            SurnameFirstInOrder = ini.ReadBool("Common", "SurnameFirstInOrder", true);
            SurnameInCapitals = ini.ReadBool("Common", "SurnameInCapitals", false);
            UseExtendedNotes = ini.ReadBool("Common", "UseExtendedNotes", false);
            DisplayFullFileName = ini.ReadBool("Common", "DisplayFullFileName", false);

            Autosave = ini.ReadBool("Common", "Autosave", false);
            AutosaveInterval = ini.ReadInteger("Common", "AutosaveInterval", 10);

            WomanSurnameFormat = (WomanSurnameFormat)ini.ReadInteger("Common", "WomanSurnameFormat", 0);
            SimpleSingleSurnames = ini.ReadBool("Common", "SimpleSingleSurnames", false);
            UnrestrictedExtendedSurnames = ini.ReadBool("Common", "UnrestrictedExtendedSurnames", false);

            Geocoder = ini.ReadString("Common", "Geocoder", "Google");
            GeoSearchCountry = ini.ReadString("Common", "GeoSearchCountry", "");

            KeepRichNames = ini.ReadBool("Common", "KeepRichNames", true);

            int kl = ini.ReadInteger("Common", "KeyLayout", AppHost.Instance.GetKeyLayout());
            AppHost.Instance.SetKeyLayout(kl);

            fTreeChartOptions.LoadFromFile(ini);
            fPedigreeOptions.LoadFromFile(ini);
            fProxy.LoadFromFile(ini);

            LoadStringList(ini, fNameFilters, "NameFilters", "Filter_");

            LoadStringList(ini, fResidenceFilters, "ResidenceFilters", "Filter_");

            LoadStringList(ini, fEventFilters, "EventFilters", "EventVal_");

            LoadMRUFromFile(ini, fMRUFiles);

            LoadStringList(ini, fRelations, "Relations", "Relation_");

            if (optsVersion <= 2) {
                fListOptions[GDMRecordType.rtIndividual].Columns.LoadFromFile(ini, "PersonsColumns", optsVersion);
            }

            ListHighlightUnmarriedPersons = ini.ReadBool("ListPersons", "HighlightUnmarried", false);
            ListHighlightUnparentedPersons = ini.ReadBool("ListPersons", "HighlightUnparented", false);

            fMWinRect.Left = ini.ReadInteger("Common", "MWinL", -1);
            fMWinRect.Top = ini.ReadInteger("Common", "MWinT", -1);
            fMWinRect.Right = ini.ReadInteger("Common", "MWinW", -1);
            fMWinRect.Bottom = ini.ReadInteger("Common", "MWinH", -1);
            MWinState = (WindowState)((uint)ini.ReadInteger("Common", "MWinState", 0));

            LoadStringList(ini, fLastBases, "LastBases", "LB");

            fCircleChartOptions.LoadFromFile(ini);

            fListOptions.LoadFromFile(ini, optsVersion);

            ReversePlaceEntitiesOrder = ini.ReadBool("Common", "ReversePlaceEntitiesOrder", false);
            CertaintyAlgorithm = (CertaintyAlgorithm)ini.ReadInteger("Common", "CertaintyAlgorithm", 0);
            LocalizedCalendarSignatures = ini.ReadBool("Common", "LocalizedCalendarSignatures", false);
            ChartWindowsShowMode = (ChartWindowsShowMode)ini.ReadInteger("Common", "ChartWindowsShowMode", 0);
            HighlightInaccessibleFiles = ini.ReadBool("Common", "HighlightInaccessibleFiles", false);

            LoadStringList(ini, fFARPatterns, "FARPatterns");
            LoadStringList(ini, fFARReplacements, "FARReplacements");

            SearchAndFilterByAllNames = ini.ReadBool("Common", "SearchAndFilterByAllNames", false);

            ScriptsLastDir = ini.ReadString("Common", "ScriptsLastDir", "");
            ImageExportLastDir = ini.ReadString("Common", "ImageExportLastDir", "");
            ReportExportLastDir = ini.ReadString("Common", "ReportExportLastDir", "");

            LoadRSFilters(ini);

            MultipagePrint = ini.ReadBool("Common", "MultipagePrint", false);
            InfoPansOverallSize = ini.ReadInteger("Common", "InfoPansOverallSize", 0);
            KeepInfoPansOverallSize = ini.ReadBool("Common", "KeepInfoPansOverallSize", false);
            FilesOverwriteWarn = ini.ReadBool("Common", "FilesOverwriteWarn", true);
            ExtendedKinships = ini.ReadBool("Common", "ExtendedKinships", false);
            ExtendedLocations = ini.ReadBool("Common", "ExtendedLocations", false);
            EL_AbbreviatedNames = ini.ReadBool("Common", "EL_AbbreviatedNames", true);

            UseSurnamesInPersonSelectionFilter = ini.ReadBool("Common", "UseSurnamesInPersonSelectionFilter", false);
            UseBirthDatesInPersonSelectionFilter = ini.ReadBool("Common", "UseBirthDatesInPersonSelectionFilter", false);
            ShowIndiAssociations = ini.ReadBool("Common", "ShowIndiAssociations", false);
            ShowIndiNamesakes = ini.ReadBool("Common", "ShowIndiNamesakes", false);
            ShowNumberOfSubstructures = ini.ReadBool("Common", "ShowNumberOfSubstructures", false);
            MatchPatternMethod = (MatchPatternMethod)ini.ReadInteger("Common", "MatchPatternMethod", 0);
            SearchPlacesWithoutCoords = ini.ReadBool("Common", "SearchPlacesWithoutCoords", false);

            Theme = ini.ReadString("Common", "Theme", "");

            LoadPluginsFromFile(ini);
        }

        public void LoadFromFile(string fileName)
        {
            if (string.IsNullOrEmpty(fileName))
                throw new ArgumentNullException("fileName");

            Logger.WriteInfo("Options load path: " + fileName);

            try {
                using (var ini = new IniFile(fileName)) {
                    LoadFromFile(ini);
                }
            } catch (Exception ex) {
                Logger.WriteError("GlobalOptions.LoadFromFile()", ex);
            }
        }


        public void SaveToFile(IniFile ini)
        {
            if (ini == null)
                throw new ArgumentNullException("ini");

            ini.WriteInteger("Common", "OptsVersion", OPTS_VERSION);

            ini.WriteInteger("Common", "DefNameFormat", (int)DefNameFormat);
            ini.WriteInteger("Common", "DefDateFormat", (int)DefDateFormat);
            ini.WriteString("Common", "LastDir", LastDir);
            ini.WriteBool("Common", "PlacesWithAddress", PlacesWithAddress);
            ini.WriteBool("Common", "ShowTips", ShowTips);
            ini.WriteInteger("Common", "InterfaceLang", InterfaceLang);
            ini.WriteInteger("Common", "FileBackup", (int)FileBackup);
            ini.WriteInteger("Common", "FileBackupEachRevisionMaxCount", FileBackupEachRevisionMaxCount);
            ini.WriteBool("Common", "ShowDatesCalendar", ShowDatesCalendar);
            ini.WriteBool("Common", "ShowDatesSigns", ShowDatesSign);
            ini.WriteBool("Common", "RemovableMediaWarning", RemovableMediaWarning);
            ini.WriteBool("Common", "LoadRecentFiles", LoadRecentFiles);
            ini.WriteBool("Common", "EmbeddedMediaPlayer", EmbeddedMediaPlayer);
            ini.WriteBool("Common", "AllowMediaStoreReferences", AllowMediaStoreReferences);
            ini.WriteBool("Common", "AllowMediaStoreRelativeReferences", AllowMediaStoreRelativeReferences);
            ini.WriteInteger("Common", "MediaStoreDefault", (int)MediaStoreDefault);
            ini.WriteBool("Common", "AllowDeleteMediaFileFromStgArc", AllowDeleteMediaFileFromStgArc);
            ini.WriteBool("Common", "AllowDeleteMediaFileFromRefs", AllowDeleteMediaFileFromRefs);
            ini.WriteBool("Common", "DeleteMediaFileWithoutConfirm", DeleteMediaFileWithoutConfirm);
            ini.WriteBool("Common", "AutoCheckUpdates", AutoCheckUpdates);
            ini.WriteBool("Common", "AutoSortChildren", AutoSortChildren);
            ini.WriteBool("Common", "AutoSortSpouses", AutoSortSpouses);
            ini.WriteBool("Common", "CharsetDetection", CharsetDetection);
            ini.WriteBool("Common", "FirstCapitalLetterInNames", FirstCapitalLetterInNames);
            ini.WriteBool("Common", "DialogClosingWarn", DialogClosingWarn);
            ini.WriteBool("Common", "ShortKinshipForm", ShortKinshipForm);
            ini.WriteBool("Common", "SurnameFirstInOrder", SurnameFirstInOrder);
            ini.WriteBool("Common", "SurnameInCapitals", SurnameInCapitals);
            ini.WriteBool("Common", "UseExtendedNotes", UseExtendedNotes);
            ini.WriteBool("Common", "DisplayFullFileName", DisplayFullFileName);

            ini.WriteInteger("Common", "KeyLayout", AppHost.Instance.GetKeyLayout());

            ini.WriteBool("Common", "Autosave", Autosave);
            ini.WriteInteger("Common", "AutosaveInterval", AutosaveInterval);

            ini.WriteInteger("Common", "WomanSurnameFormat", (int)WomanSurnameFormat);
            ini.WriteBool("Common", "SimpleSingleSurnames", SimpleSingleSurnames);
            ini.WriteBool("Common", "UnrestrictedExtendedSurnames", UnrestrictedExtendedSurnames);

            ini.WriteString("Common", "Geocoder", Geocoder);
            ini.WriteString("Common", "GeoSearchCountry", GeoSearchCountry);

            ini.WriteBool("Common", "KeepRichNames", KeepRichNames);

            fTreeChartOptions.SaveToFile(ini);
            fPedigreeOptions.SaveToFile(ini);
            fProxy.SaveToFile(ini);

            fNameFilters.Sort();

            SaveStringList(ini, fNameFilters, "NameFilters", "Filter_");

            SaveStringList(ini, fResidenceFilters, "ResidenceFilters", "Filter_");

            SaveStringList(ini, fEventFilters, "EventFilters", "EventVal_");

            int cnt = fMRUFiles.Count;
            ini.WriteInteger("Common", "MRUFiles_Count", cnt);
            for (int i = 0; i < cnt; i++) {
                fMRUFiles[i].SaveToFile(ini, "MRUFile_" + i.ToString());
            }

            SaveStringList(ini, fRelations, "Relations", "Relation_");

            // obsolete at OPTS_VERSION = 3
            //fIndividualListColumns.SaveToFile(ini, "PersonsColumns", OPTS_VERSION);

            ini.WriteBool("ListPersons", "HighlightUnmarried", ListHighlightUnmarriedPersons);
            ini.WriteBool("ListPersons", "HighlightUnparented", ListHighlightUnparentedPersons);

            //------------------------------------------------------------------
            // 2016-09-30 Ruslan Garipov <brigadir15@gmail.com>
            // FIXME: If `Control::Left`, `Control::Top`, `Control::Width` and
            // `Control::Height` return physical values (device depended), code
            // here must convert members of `fMWinRect` to logical values
            // (device independed) before storing it as the application
            // settings. Had GK been a native Windows application, it had to do
            // that. But since it's a .NET application I don't know is it a
            // true. See also implementation of `GKCore::GKUtils::GetFormRect`
            // member.
            //------------------------------------------------------------------
            ini.WriteInteger("Common", "MWinL", fMWinRect.Left);
            ini.WriteInteger("Common", "MWinT", fMWinRect.Top);
            ini.WriteInteger("Common", "MWinW", fMWinRect.Right);
            ini.WriteInteger("Common", "MWinH", fMWinRect.Bottom);
            ini.WriteInteger("Common", "MWinState", (int)MWinState);

            SaveStringList(ini, fLastBases, "LastBases", "LB");

            fCircleChartOptions.SaveToFile(ini);

            fListOptions.SaveToFile(ini, OPTS_VERSION);

            ini.WriteBool("Common", "ReversePlaceEntitiesOrder", ReversePlaceEntitiesOrder);
            ini.WriteInteger("Common", "CertaintyAlgorithm", (int)CertaintyAlgorithm);
            ini.WriteBool("Common", "LocalizedCalendarSignatures", LocalizedCalendarSignatures);
            ini.WriteInteger("Common", "ChartWindowsShowMode", (int)ChartWindowsShowMode);
            ini.WriteBool("Common", "HighlightInaccessibleFiles", HighlightInaccessibleFiles);

            SaveStringList(ini, fFARPatterns, "FARPatterns");
            SaveStringList(ini, fFARReplacements, "FARReplacements");

            ini.WriteBool("Common", "SearchAndFilterByAllNames", SearchAndFilterByAllNames);

            ini.WriteString("Common", "ScriptsLastDir", ScriptsLastDir);
            ini.WriteString("Common", "ImageExportLastDir", ImageExportLastDir);
            ini.WriteString("Common", "ReportExportLastDir", ReportExportLastDir);

            SaveRSFilters(ini);

            ini.WriteBool("Common", "MultipagePrint", MultipagePrint);
            ini.WriteInteger("Common", "InfoPansOverallSize", InfoPansOverallSize);
            ini.WriteBool("Common", "KeepInfoPansOverallSize", KeepInfoPansOverallSize);
            ini.WriteBool("Common", "FilesOverwriteWarn", FilesOverwriteWarn);
            ini.WriteBool("Common", "ExtendedKinships", ExtendedKinships);
            ini.WriteBool("Common", "ExtendedLocations", ExtendedLocations);
            ini.WriteBool("Common", "EL_AbbreviatedNames", EL_AbbreviatedNames);

            ini.WriteBool("Common", "UseSurnamesInPersonSelectionFilter", UseSurnamesInPersonSelectionFilter);
            ini.WriteBool("Common", "UseBirthDatesInPersonSelectionFilter", UseBirthDatesInPersonSelectionFilter);
            ini.WriteBool("Common", "ShowIndiAssociations", ShowIndiAssociations);
            ini.WriteBool("Common", "ShowIndiNamesakes", ShowIndiNamesakes);
            ini.WriteBool("Common", "ShowNumberOfSubstructures", ShowNumberOfSubstructures);
            ini.WriteInteger("Common", "MatchPatternMethod", (int)MatchPatternMethod);
            ini.WriteBool("Common", "SearchPlacesWithoutCoords", SearchPlacesWithoutCoords);

            ini.WriteString("Common", "Theme", Theme);

            SavePluginsToFile(ini);
        }

        public void SaveToFile(string fileName)
        {
            if (string.IsNullOrEmpty(fileName))
                throw new ArgumentNullException("fileName");

            try {
                using (var ini = new IniFile(fileName)) {
                    SaveToFile(ini);
                }
            } catch (Exception ex) {
                Logger.WriteError("GlobalOptions.SaveToFile()", ex);
            }
        }
    }
}
