/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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
    public sealed class GlobalOptions : BaseObject
    {
        public const int OPTS_VERSION = 1;

        private static GlobalOptions fInstance = null;

        private readonly TreeChartOptions fTreeChartOptions;
        private DateFormat fDefDateFormat;
        private bool fShowDatesSign;
        private NameFormat fDefNameFormat;
        private readonly StringList fEventFilters;
        private int fInterfaceLang;
        private readonly List<LangRecord> fLanguages;
        private string fLastDir;
        private readonly List<MRUFile> fMRUFiles;
        private readonly StringList fNameFilters;
        private readonly PedigreeOptions fPedigreeOptions;
        private bool fPlacesWithAddress;
        private readonly ProxyOptions fProxy;
        private readonly StringList fRelations;
        private readonly StringList fResidenceFilters;
        private bool fShowTips;
        private readonly ListColumns fIndividualListColumns;
        private bool fListHighlightUnmarriedPersons;
        private bool fListHighlightUnparentedPersons;
        private ExtRect fMWinRect;
        private WindowState fMWinState;
        private readonly StringList fLastBases;
        private bool fShowDatesCalendar;
        private FileBackup fFileBackup;
        private int fFileBackupEachRevisionMaxCount;
        private bool fAutosave;
        private int fAutosaveInterval;
        private bool fExtendedNames;
        private WomanSurnameFormat fWomanSurnameFormat;
        private readonly CircleChartOptions fCircleChartOptions;
        private string fGeocoder;
        private bool fRemovableMediaWarning;
        private bool fLoadRecentFiles;
        private bool fEmbeddedMediaPlayer;
        private bool fAllowMediaStoreReferences;
        private bool fAllowMediaStoreRelativeReferences;
        private MediaStoreType fMediaStoreDefault;
        private bool fAllowDeleteMediaFileFromStgArc;
        private bool fAllowDeleteMediaFileFromRefs;
        private bool fDeleteMediaFileWithoutConfirm;
        private bool fUseExtendedNotes;

        private bool fAutoCheckUpdates;
        private bool fAutoSortChildren;
        private bool fAutoSortSpouses;
        private bool fCharsetDetection;
        private bool fCheckTreeSize;
        private bool fDialogClosingWarn;
        private bool fFirstCapitalLetterInNames;
        private string fGeoSearchCountry;
        private readonly ListOptionsCollection fListOptions;
        private bool fReadabilityHighlightRows;
        private bool fReversePlaceEntitiesOrder;
        private bool fShortKinshipForm;
        private bool fSurnameFirstInOrder;


        public static GlobalOptions Instance
        {
            get {
                if (fInstance == null) fInstance = new GlobalOptions();
                return fInstance;
            }
        }


        public bool AllowDeleteMediaFileFromStgArc
        {
            get { return fAllowDeleteMediaFileFromStgArc; }
            set { fAllowDeleteMediaFileFromStgArc = value; }
        }

        public bool AllowDeleteMediaFileFromRefs
        {
            get { return fAllowDeleteMediaFileFromRefs; }
            set { fAllowDeleteMediaFileFromRefs = value; }
        }

        public bool AllowMediaStoreReferences
        {
            get { return fAllowMediaStoreReferences; }
            set { fAllowMediaStoreReferences = value; }
        }

        public bool AllowMediaStoreRelativeReferences
        {
            get { return fAllowMediaStoreRelativeReferences; }
            set { fAllowMediaStoreRelativeReferences = value; }
        }

        public bool AutoCheckUpdates
        {
            get { return fAutoCheckUpdates; }
            set { fAutoCheckUpdates = value; }
        }

        public bool Autosave
        {
            get { return fAutosave; }
            set { fAutosave = value; }
        }

        public int AutosaveInterval
        {
            get { return fAutosaveInterval; }
            set { fAutosaveInterval = value; }
        }

        public bool AutoSortChildren
        {
            get { return fAutoSortChildren; }
            set { fAutoSortChildren = value; }
        }

        public bool AutoSortSpouses
        {
            get { return fAutoSortSpouses; }
            set { fAutoSortSpouses = value; }
        }

        public bool CharsetDetection
        {
            get { return fCharsetDetection; }
            set { fCharsetDetection = value; }
        }

        public bool CheckTreeSize
        {
            get { return fCheckTreeSize; }
            set { fCheckTreeSize = value; }
        }

        public CircleChartOptions CircleChartOptions
        {
            get { return fCircleChartOptions; }
        }

        public GEDCOMCharacterSet DefCharacterSet
        {
            get { return GEDCOMCharacterSet.csUTF8; }
        }

        public DateFormat DefDateFormat
        {
            get { return fDefDateFormat; }
            set { fDefDateFormat = value; }
        }

        public NameFormat DefNameFormat
        {
            get { return fDefNameFormat; }
            set { fDefNameFormat = value; }
        }

        public bool DeleteMediaFileWithoutConfirm
        {
            get { return fDeleteMediaFileWithoutConfirm; }
            set { fDeleteMediaFileWithoutConfirm = value; }
        }

        public bool DialogClosingWarn
        {
            get { return fDialogClosingWarn; }
            set { fDialogClosingWarn = value; }
        }

        public bool EmbeddedMediaPlayer
        {
            get { return fEmbeddedMediaPlayer; }
            set { fEmbeddedMediaPlayer = value; }
        }

        public StringList EventFilters
        {
            get { return fEventFilters; }
        }

        // TODO: Need to make a decision on additional types of names:
        // religious and according to the census (see GDMPersonalNamePieces)
        public bool ExtendedNames
        {
            get { return fExtendedNames; }
            set { fExtendedNames = value; }
        }

        public FileBackup FileBackup
        {
            get { return fFileBackup; }
            set { fFileBackup = value; }
        }

        public int FileBackupEachRevisionMaxCount
        {
            get { return fFileBackupEachRevisionMaxCount; }
            set { fFileBackupEachRevisionMaxCount = value; }
        }

        public bool FirstCapitalLetterInNames
        {
            get { return fFirstCapitalLetterInNames; }
            set { fFirstCapitalLetterInNames = value; }
        }

        public string Geocoder
        {
            get { return fGeocoder; }
            set { fGeocoder = value; }
        }

        public string GeoSearchCountry
        {
            get { return fGeoSearchCountry; }
            set { fGeoSearchCountry = value; }
        }

        public ListColumns IndividualListColumns
        {
            get { return fIndividualListColumns; }
        }

        public int InterfaceLang
        {
            get { return fInterfaceLang; }
            set { fInterfaceLang = value; }
        }

        public bool KeepRichNames { get; set; }

        public IList<LangRecord> Languages
        {
            get { return fLanguages; }
        }

        public string LastDir
        {
            get { return fLastDir; }
            set { fLastDir = value; }
        }

        public bool ListHighlightUnmarriedPersons
        {
            get { return fListHighlightUnmarriedPersons; }
            set { fListHighlightUnmarriedPersons = value; }
        }

        public bool ListHighlightUnparentedPersons
        {
            get { return fListHighlightUnparentedPersons; }
            set { fListHighlightUnparentedPersons = value; }
        }

        public ListOptionsCollection ListOptions
        {
            get { return fListOptions; }
        }

        public bool LoadRecentFiles
        {
            get { return fLoadRecentFiles; }
            set { fLoadRecentFiles = value; }
        }

        public MediaStoreType MediaStoreDefault
        {
            get { return fMediaStoreDefault; }
            set { fMediaStoreDefault = value; }
        }

        public List<MRUFile> MRUFiles
        {
            get { return fMRUFiles; }
        }

        public ExtRect MWinRect
        {
            get { return fMWinRect; }
            set { fMWinRect = value; }
        }

        public WindowState MWinState
        {
            get { return fMWinState; }
            set { fMWinState = value; }
        }

        public StringList NameFilters
        {
            get { return fNameFilters; }
        }

        public PedigreeOptions PedigreeOptions
        {
            get { return fPedigreeOptions; }
        }

        public bool PlacesWithAddress
        {
            get { return fPlacesWithAddress; }
            set { fPlacesWithAddress = value; }
        }

        public ProxyOptions Proxy
        {
            get { return fProxy; }
        }

        public bool ReadabilityHighlightRows
        {
            get { return fReadabilityHighlightRows; }
            set { fReadabilityHighlightRows = value; }
        }

        public StringList Relations
        {
            get { return fRelations; }
        }

        public bool RemovableMediaWarning
        {
            get { return fRemovableMediaWarning; }
            set { fRemovableMediaWarning = value; }
        }

        public StringList ResidenceFilters
        {
            get { return fResidenceFilters; }
        }

        /// <summary>
        /// Hidden option for non-standard order.
        /// </summary>
        public bool ReversePlaceEntitiesOrder
        {
            get { return fReversePlaceEntitiesOrder; }
            set { fReversePlaceEntitiesOrder = value; }
        }

        public bool ShortKinshipForm
        {
            get { return fShortKinshipForm; }
            set { fShortKinshipForm = value; }
        }

        public bool ShowDatesCalendar
        {
            get { return fShowDatesCalendar; }
            set { fShowDatesCalendar = value; }
        }

        public bool ShowDatesSign
        {
            get { return fShowDatesSign; }
            set { fShowDatesSign = value; }
        }

        public bool ShowTips
        {
            get { return fShowTips; }
            set { fShowTips = value; }
        }

        public bool SurnameFirstInOrder
        {
            get { return fSurnameFirstInOrder; }
            set { fSurnameFirstInOrder = value; }
        }

        public TreeChartOptions TreeChartOptions
        {
            get { return fTreeChartOptions; }
        }

        public bool UseExtendedNotes
        {
            get { return fUseExtendedNotes; }
            set { fUseExtendedNotes = value; }
        }

        public WomanSurnameFormat WomanSurnameFormat
        {
            get { return fWomanSurnameFormat; }
            set { fWomanSurnameFormat = value; }
        }


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
            fGeocoder = "Google";
            fRemovableMediaWarning = true;
            fLoadRecentFiles = true;
            fEmbeddedMediaPlayer = true;
            fAllowMediaStoreReferences = false;
            fAllowMediaStoreRelativeReferences = true;
            fMediaStoreDefault = 0;
            fAllowDeleteMediaFileFromStgArc = true;
            fAllowDeleteMediaFileFromRefs = false;
            fDeleteMediaFileWithoutConfirm = false;
            fUseExtendedNotes = false;

            fAutoCheckUpdates = true;
            fAutoSortChildren = true;
            fAutoSortSpouses = false;
            fCheckTreeSize = true;

            fIndividualListColumns = IndividualListMan.CreateIndividualListColumns();
            fIndividualListColumns.ResetDefaults();

            fLanguages = new List<LangRecord>();
            fLastBases = new StringList();

            fAutosave = false;
            fAutosaveInterval = 10;

            fListOptions = new ListOptionsCollection();
            fReadabilityHighlightRows = true;
            fShortKinshipForm = false;
            fSurnameFirstInOrder = true;

            fCharsetDetection = false;
            fFirstCapitalLetterInNames = false;
            fGeoSearchCountry = string.Empty;

            KeepRichNames = true;
            fReversePlaceEntitiesOrder = false;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fLastBases.Dispose();
                fRelations.Dispose();

                fResidenceFilters.Dispose();
                fNameFilters.Dispose();
                fEventFilters.Dispose();
            }
            base.Dispose(disposing);
        }

        private void LngPrepareProc(string fileName)
        {
            try {
                using (StreamReader lngFile = new StreamReader(fileName, Encoding.UTF8)) {
                    string st = lngFile.ReadLine(); // header

                    if (!string.IsNullOrEmpty(st) && st[0] == ';') {
                        st = st.Remove(0, 1);
                        string[] lngParams = st.Split(',');
                        if (lngParams.Length < 3)
                            throw new GKException("Header is incorrect");

                        string lngCode = lngParams[0];
                        string lngSign = lngParams[1];
                        string lngName = lngParams[2];

                        bool xt = (lngParams.Length == 4 && lngParams[3] == "xt");

                        LangRecord lngRec = new LangRecord((ushort)int.Parse(lngCode), lngSign, lngName, fileName);
                        fLanguages.Add(lngRec);
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GlobalOptions.LngPrepareProc(" + fileName + ")", ex);
            }
        }

        public void FindLanguages()
        {
            try {
                string path = GKUtils.GetLangsPath();
                string[] langFiles = Directory.GetFiles(path, "*.lng", SearchOption.TopDirectoryOnly);
                for (int i = 0; i < langFiles.Length; i++) LngPrepareProc(langFiles[i]);
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
                        loaded = LangMan.LoadFromFile(langRec.FileName);
                        break;
                    }
                }

                if (!loaded) langCode = LangMan.LS_DEF_CODE;
            }

            if (langCode == LangMan.LS_DEF_CODE) {
                LangMan.DefInit();
            }

            fInterfaceLang = (ushort)langCode;
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

        public string GetLastBase(int index)
        {
            return fLastBases[index];
        }

        public int GetLastBasesCount()
        {
            return fLastBases.Count;
        }

        public int MRUFiles_IndexOf(string fileName)
        {
            int num = fMRUFiles.Count;
            for (int i = 0; i < num; i++) {
                if (fMRUFiles[i].FileName == fileName) {
                    return i;
                }
            }

            return -1;
        }

        public void AddLastBase(string fileName)
        {
            fLastBases.Add(fileName);
        }

        public void ClearLastBases()
        {
            fLastBases.Clear();
        }


        public static void LoadMRUFromFile(IniFile ini, List<MRUFile> mruFiles)
        {
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
        }

        public void LoadPluginsFromFile(IniFile ini)
        {
            if (ini == null)
                throw new ArgumentNullException("ini");

            int num = AppHost.Plugins.Count;
            for (int i = 0; i < num; i++) {
                var plugin = AppHost.Plugins[i];

                var dlgPlugin = plugin as IDialogReplacement;
                if (dlgPlugin != null) {
                    var plgName = plugin.GetType().Name;
                    dlgPlugin.Enabled = ini.ReadBool("Plugins", plgName + ".Enabled", false);
                }
            }
        }

        public void SavePluginsToFile(IniFile ini)
        {
            if (ini == null)
                throw new ArgumentNullException("ini");

            int num = AppHost.Plugins.Count;
            for (int i = 0; i < num; i++) {
                var plugin = AppHost.Plugins[i];

                var dlgPlugin = plugin as IDialogReplacement;
                if (dlgPlugin != null) {
                    var plgName = plugin.GetType().Name;
                    ini.WriteBool("Plugins", plgName + ".Enabled", dlgPlugin.Enabled);
                }
            }
        }

        public void LoadFromFile(IniFile ini)
        {
            if (ini == null)
                throw new ArgumentNullException("ini");

            int optsVersion = ini.ReadInteger("Common", "OptsVersion", 0);

            fDefNameFormat = (NameFormat)ini.ReadInteger("Common", "DefNameFormat", 0);
            fDefDateFormat = (DateFormat)ini.ReadInteger("Common", "DefDateFormat", 0);
            fLastDir = ini.ReadString("Common", "LastDir", "");
            fPlacesWithAddress = ini.ReadBool("Common", "PlacesWithAddress", false);
            fShowTips = ini.ReadBool("Common", "ShowTips", true);
            fInterfaceLang = (ushort)ini.ReadInteger("Common", "InterfaceLang", 0);
            fFileBackup = (FileBackup)ini.ReadInteger("Common", "FileBackup", 0);
            fFileBackupEachRevisionMaxCount = ini.ReadInteger("Common", "FileBackupEachRevisionMaxCount", 0);
            fShowDatesCalendar = ini.ReadBool("Common", "ShowDatesCalendar", false);
            fShowDatesSign = ini.ReadBool("Common", "ShowDatesSigns", false);
            fRemovableMediaWarning = ini.ReadBool("Common", "RemovableMediaWarning", true);
            fLoadRecentFiles = ini.ReadBool("Common", "LoadRecentFiles", true);
            fEmbeddedMediaPlayer = ini.ReadBool("Common", "EmbeddedMediaPlayer", true);
            fAllowMediaStoreReferences = ini.ReadBool("Common", "AllowMediaStoreReferences", false);
            fAllowMediaStoreRelativeReferences = ini.ReadBool("Common", "AllowMediaStoreRelativeReferences", true); // only when AllowMediaStoreReferences is true
            fMediaStoreDefault = (MediaStoreType)ini.ReadInteger("Common", "MediaStoreDefault", 0); // (int)MediaStoreType.mstReference
            fAllowDeleteMediaFileFromStgArc = ini.ReadBool("Common", "AllowDeleteMediaFileFromStgArc", true);
            fAllowDeleteMediaFileFromRefs = ini.ReadBool("Common", "AllowDeleteMediaFileFromRefs", false);
            fDeleteMediaFileWithoutConfirm = ini.ReadBool("Common", "DeleteMediaFileWithoutConfirm", false);
            fAutoCheckUpdates = ini.ReadBool("Common", "AutoCheckUpdates", true);
            fAutoSortChildren = ini.ReadBool("Common", "AutoSortChildren", true);
            fAutoSortSpouses = ini.ReadBool("Common", "AutoSortSpouses", false);
            fCharsetDetection = ini.ReadBool("Common", "CharsetDetection", false);
            fFirstCapitalLetterInNames = ini.ReadBool("Common", "FirstCapitalLetterInNames", false);
            fDialogClosingWarn = ini.ReadBool("Common", "DialogClosingWarn", false);
            fShortKinshipForm = ini.ReadBool("Common", "ShortKinshipForm", false);
            fSurnameFirstInOrder = ini.ReadBool("Common", "SurnameFirstInOrder", true);

            fAutosave = ini.ReadBool("Common", "Autosave", false);
            fAutosaveInterval = ini.ReadInteger("Common", "AutosaveInterval", 10);

            fExtendedNames = ini.ReadBool("Common", "ExtendedNames", false);
            fWomanSurnameFormat = (WomanSurnameFormat)ini.ReadInteger("Common", "WomanSurnameFormat", 0);

            fGeocoder = ini.ReadString("Common", "Geocoder", "Google");
            fGeoSearchCountry = ini.ReadString("Common", "GeoSearchCountry", "");

            KeepRichNames = ini.ReadBool("Common", "KeepRichNames", true);

            int kl = ini.ReadInteger("Common", "KeyLayout", AppHost.Instance.GetKeyLayout());
            AppHost.Instance.SetKeyLayout(kl);

            fTreeChartOptions.LoadFromFile(ini);
            fPedigreeOptions.LoadFromFile(ini);
            fProxy.LoadFromFile(ini);

            int cnt = ini.ReadInteger("NameFilters", "Count", 0);
            for (int i = 0; i < cnt; i++)
            {
                string st = ini.ReadString("NameFilters", "Filter_" + i.ToString(), "");
                if (st != "") fNameFilters.Add(st);
            }

            cnt = ini.ReadInteger("ResidenceFilters", "Count", 0);
            for (int i = 0; i < cnt; i++)
            {
                fResidenceFilters.Add(ini.ReadString("ResidenceFilters", "Filter_" + i.ToString(), ""));
            }

            cnt = ini.ReadInteger("EventFilters", "Count", 0);
            for (int i = 0; i < cnt; i++)
            {
                fEventFilters.Add(ini.ReadString("EventFilters", "EventVal_" + i.ToString(), ""));
            }

            LoadMRUFromFile(ini, fMRUFiles);

            cnt = ini.ReadInteger("Relations", "Count", 0);
            for (int i = 0; i < cnt; i++)
            {
                fRelations.Add(ini.ReadString("Relations", "Relation_" + i.ToString(), ""));
            }

            fIndividualListColumns.LoadFromFile(ini, "PersonsColumns");

            fListHighlightUnmarriedPersons = ini.ReadBool("ListPersons", "HighlightUnmarried", false);
            fListHighlightUnparentedPersons = ini.ReadBool("ListPersons", "HighlightUnparented", false);

            fMWinRect.Left = ini.ReadInteger("Common", "MWinL", -1);
            fMWinRect.Top = ini.ReadInteger("Common", "MWinT", -1);
            fMWinRect.Right = ini.ReadInteger("Common", "MWinW", -1);
            fMWinRect.Bottom = ini.ReadInteger("Common", "MWinH", -1);
            fMWinState = (WindowState)((uint)ini.ReadInteger("Common", "MWinState", 0));

            cnt = ini.ReadInteger("LastBases", "Count", 0);
            for (int i = 0; i < cnt; i++)
            {
                string st = ini.ReadString("LastBases", "LB" + i.ToString(), "");
                AddLastBase(st);
            }

            fCircleChartOptions.LoadFromFile(ini);

            fListOptions.LoadFromFile(ini);

            LoadPluginsFromFile(ini);

            fReversePlaceEntitiesOrder = ini.ReadBool("Common", "ReversePlaceEntitiesOrder", false);
        }

        public void LoadFromFile(string fileName)
        {
            if (string.IsNullOrEmpty(fileName))
                throw new ArgumentNullException("fileName");

            try {
                IniFile ini = new IniFile(fileName);
                try {
                    LoadFromFile(ini);
                } finally {
                    ini.Dispose();
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

            ini.WriteInteger("Common", "DefNameFormat", (int)fDefNameFormat);
            ini.WriteInteger("Common", "DefDateFormat", (int)fDefDateFormat);
            ini.WriteString("Common", "LastDir", fLastDir);
            ini.WriteBool("Common", "PlacesWithAddress", fPlacesWithAddress);
            ini.WriteBool("Common", "ShowTips", fShowTips);
            ini.WriteInteger("Common", "InterfaceLang", fInterfaceLang);
            ini.WriteInteger("Common", "FileBackup", (int)fFileBackup);
            ini.WriteInteger("Common", "FileBackupEachRevisionMaxCount", fFileBackupEachRevisionMaxCount);
            ini.WriteBool("Common", "ShowDatesCalendar", fShowDatesCalendar);
            ini.WriteBool("Common", "ShowDatesSigns", fShowDatesSign);
            ini.WriteBool("Common", "RemovableMediaWarning", fRemovableMediaWarning);
            ini.WriteBool("Common", "LoadRecentFiles", fLoadRecentFiles);
            ini.WriteBool("Common", "EmbeddedMediaPlayer", fEmbeddedMediaPlayer);
            ini.WriteBool("Common", "AllowMediaStoreReferences", fAllowMediaStoreReferences);
            ini.WriteBool("Common", "AllowMediaStoreRelativeReferences", fAllowMediaStoreRelativeReferences);
            ini.WriteInteger("Common", "MediaStoreDefault", (int)fMediaStoreDefault);
            ini.WriteBool("Common", "AllowDeleteMediaFileFromStgArc", fAllowDeleteMediaFileFromStgArc);
            ini.WriteBool("Common", "AllowDeleteMediaFileFromRefs", fAllowDeleteMediaFileFromRefs);
            ini.WriteBool("Common", "DeleteMediaFileWithoutConfirm", fDeleteMediaFileWithoutConfirm);
            ini.WriteBool("Common", "AutoCheckUpdates", fAutoCheckUpdates);
            ini.WriteBool("Common", "AutoSortChildren", fAutoSortChildren);
            ini.WriteBool("Common", "AutoSortSpouses", fAutoSortSpouses);
            ini.WriteBool("Common", "CharsetDetection", fCharsetDetection);
            ini.WriteBool("Common", "FirstCapitalLetterInNames", fFirstCapitalLetterInNames);
            ini.WriteBool("Common", "DialogClosingWarn", fDialogClosingWarn);
            ini.WriteBool("Common", "ShortKinshipForm", fShortKinshipForm);
            ini.WriteBool("Common", "SurnameFirstInOrder", fSurnameFirstInOrder);

            ini.WriteInteger("Common", "KeyLayout", AppHost.Instance.GetKeyLayout());

            ini.WriteBool("Common", "Autosave", fAutosave);
            ini.WriteInteger("Common", "AutosaveInterval", fAutosaveInterval);

            ini.WriteBool("Common", "ExtendedNames", fExtendedNames);
            ini.WriteInteger("Common", "WomanSurnameFormat", (int)fWomanSurnameFormat);

            ini.WriteString("Common", "Geocoder", fGeocoder);
            ini.WriteString("Common", "GeoSearchCountry", fGeoSearchCountry);

            ini.WriteBool("Common", "KeepRichNames", KeepRichNames);

            fTreeChartOptions.SaveToFile(ini);
            fPedigreeOptions.SaveToFile(ini);
            fProxy.SaveToFile(ini);

            fNameFilters.Sort();

            int cnt = fNameFilters.Count;
            ini.WriteInteger("NameFilters", "Count", cnt);
            for (int i = 0; i < cnt; i++)
            {
                ini.WriteString("NameFilters", "Filter_" + i.ToString(), fNameFilters[i]);
            }

            cnt = fResidenceFilters.Count;
            ini.WriteInteger("ResidenceFilters", "Count", cnt);
            for (int i = 0; i < cnt; i++)
            {
                ini.WriteString("ResidenceFilters", "Filter_" + i.ToString(), fResidenceFilters[i]);
            }

            cnt = fEventFilters.Count;
            ini.WriteInteger("EventFilters", "Count", cnt);
            for (int i = 0; i < cnt; i++)
            {
                ini.WriteString("EventFilters", "EventVal_" + i.ToString(), fEventFilters[i]);
            }

            cnt = fMRUFiles.Count;
            ini.WriteInteger("Common", "MRUFiles_Count", cnt);
            for (int i = 0; i < cnt; i++)
            {
                fMRUFiles[i].SaveToFile(ini, "MRUFile_" + i.ToString());
            }

            cnt = fRelations.Count;
            ini.WriteInteger("Relations", "Count", cnt);
            for (int i = 0; i < cnt; i++)
            {
                ini.WriteString("Relations", "Relation_" + i.ToString(), fRelations[i]);
            }

            fIndividualListColumns.SaveToFile(ini, "PersonsColumns");

            ini.WriteBool("ListPersons", "HighlightUnmarried", fListHighlightUnmarriedPersons);
            ini.WriteBool("ListPersons", "HighlightUnparented", fListHighlightUnparentedPersons);

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
            ini.WriteInteger("Common", "MWinState", (int)fMWinState);

            cnt = fLastBases.Count;
            ini.WriteInteger("LastBases", "Count", cnt);
            for (int i = 0; i < cnt; i++) {
                ini.WriteString("LastBases", "LB" + i.ToString(), fLastBases[i]);
            }

            fCircleChartOptions.SaveToFile(ini);

            fListOptions.SaveToFile(ini);

            SavePluginsToFile(ini);

            ini.WriteBool("Common", "ReversePlaceEntitiesOrder", fReversePlaceEntitiesOrder);
        }

        public void SaveToFile(string fileName)
        {
            if (string.IsNullOrEmpty(fileName))
                throw new ArgumentNullException("fileName");

            try {
                IniFile ini = new IniFile(fileName);

                try {
                    SaveToFile(ini);
                } finally {
                    ini.Dispose();
                }
            } catch (Exception ex) {
                Logger.WriteError("GlobalOptions.SaveToFile()", ex);
            }
        }
    }
}
