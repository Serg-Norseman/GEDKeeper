/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCore.Lists;
using GKCore.Types;

namespace GKCore.Options
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class GlobalOptions : BaseObject
    {
        private static GlobalOptions fInstance = null;

        private readonly TreeChartOptions fChartOptions;
        /*private GEDCOMCharacterSet fDefCharacterSet;*/
        private DateFormat fDefDateFormat;
        private bool fShowDatesSign;
        private NameFormat fDefNameFormat;
        private readonly StringList fEventFilters;
        private ushort fInterfaceLang;
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
        private readonly IndividualListColumns fIndividualListColumns;
        private bool fListHighlightUnmarriedPersons;
        private bool fListHighlightUnparentedPersons;
        private ExtRect fMWinRect;
        private FormWindowState fMWinState;
        private readonly StringList fLastBases;
        private bool fShowDatesCalendar;
        private FileBackup fFileBackup;
        private bool fAutosave;
        private int fAutosaveInterval;
        private bool fExtendedNames;
        private WomanSurnameFormat fWomanSurnameFormat;
        private readonly AncestorsCircleOptions fAncestorsCircleOptions;
        private string fGeocoder;


        public static GlobalOptions Instance
        {
            get {
                if (fInstance == null) {
                    fInstance = new GlobalOptions();
                }

                return fInstance;
            }
        }


        public TreeChartOptions ChartOptions
        {
            get { return this.fChartOptions; }
        }

        public AncestorsCircleOptions AncestorsCircleOptions
        {
            get { return this.fAncestorsCircleOptions; }
        }

        /*public GEDCOMCharacterSet DefCharacterSet
        {
            get { return this.fDefCharacterSet; }
            set { this.fDefCharacterSet = value; }
        }*/

        public GEDCOMCharacterSet DefCharacterSet
        {
            get { return GEDCOMCharacterSet.csUTF8; }
        }

        public DateFormat DefDateFormat
        {
            get { return this.fDefDateFormat; }
            set { this.fDefDateFormat = value; }
        }

        public bool ShowDatesSign
        {
            get { return this.fShowDatesSign; }
            set { this.fShowDatesSign = value; }
        }

        public NameFormat DefNameFormat
        {
            get { return this.fDefNameFormat; }
            set { this.fDefNameFormat = value; }
        }

        public StringList EventFilters
        {
            get { return this.fEventFilters; }
        }

        public ushort InterfaceLang
        {
            get { return this.fInterfaceLang; }
            set { this.fInterfaceLang = value; }
        }

        public string LastDir
        {
            get { return this.fLastDir; }
            set { this.fLastDir = value; }
        }

        public List<MRUFile> MRUFiles
        {
            get { return this.fMRUFiles; }
        }

        public ExtRect MWinRect
        {
            get { return this.fMWinRect; }
            set { this.fMWinRect = value; }
        }

        public FormWindowState MWinState
        {
            get { return this.fMWinState; }
            set { this.fMWinState = value; }
        }

        public StringList NameFilters
        {
            get { return this.fNameFilters; }
        }

        public PedigreeOptions PedigreeOptions
        {
            get { return this.fPedigreeOptions; }
        }

        public bool PlacesWithAddress
        {
            get { return this.fPlacesWithAddress; }
            set { this.fPlacesWithAddress = value; }
        }

        public ProxyOptions Proxy
        {
            get { return this.fProxy; }
        }

        public StringList Relations
        {
            get { return this.fRelations; }
        }

        public StringList ResidenceFilters
        {
            get { return this.fResidenceFilters; }
        }

        public FileBackup FileBackup
        {
            get { return this.fFileBackup; }
            set { this.fFileBackup = value; }
        }

        public bool ShowTips
        {
            get { return this.fShowTips; }
            set { this.fShowTips = value; }
        }

        public bool ListHighlightUnmarriedPersons
        {
            get { return this.fListHighlightUnmarriedPersons; }
            set { this.fListHighlightUnmarriedPersons = value; }
        }

        public bool ListHighlightUnparentedPersons
        {
            get { return this.fListHighlightUnparentedPersons; }
            set { this.fListHighlightUnparentedPersons = value; }
        }

        public IndividualListColumns IndividualListColumns
        {
            get { return this.fIndividualListColumns; }
        }

        public bool ShowDatesCalendar
        {
            get { return this.fShowDatesCalendar; }
            set { this.fShowDatesCalendar = value; }
        }



        public bool Autosave
        {
            get { return this.fAutosave; }
            set { this.fAutosave = value; }
        }

        public int AutosaveInterval
        {
            get { return this.fAutosaveInterval; }
            set { this.fAutosaveInterval = value; }
        }


        // TODO: Need to make a decision on additional types of names:
        // religious and according to the census (see GEDCOMPersonalNamePieces)
        public bool ExtendedNames
        {
            get { return this.fExtendedNames; }
            set { this.fExtendedNames = value; }
        }

        public WomanSurnameFormat WomanSurnameFormat
        {
            get { return this.fWomanSurnameFormat; }
            set { this.fWomanSurnameFormat = value; }
        }

        public string Geocoder
        {
            get { return this.fGeocoder; }
            set { this.fGeocoder = value; }
        }


        public int GetLangsCount()
        {
            return this.fLanguages.Count;
        }

        public LangRecord GetLang(int index)
        {
            return this.fLanguages[index];
        }

        public LangRecord GetLangByCode(int code)
        {
            foreach (LangRecord lngRec in this.fLanguages) {
                if (lngRec.Code == code) {
                    return lngRec;
                }
            }

            return null;
        }

        // TODO: rework it
        public GEDCOMLanguageID GetCurrentItfLang()
        {
            if (this.InterfaceLang == LangMan.LS_DEF_CODE) {
                return GEDCOMLanguageID.English;
            } else {
                LangRecord langRec = this.GetLangByCode(this.InterfaceLang);
                return (langRec == null) ? GEDCOMLanguageID.English : langRec.LangID;
            }
        }

        public string GetLastBase(int index)
        {
            return this.fLastBases[index];
        }

        public int GetLastBasesCount()
        {
            return this.fLastBases.Count;
        }

        public int MRUFiles_IndexOf(string fileName)
        {
            int num = this.fMRUFiles.Count;
            for (int i = 0; i < num; i++) {
                if (this.fMRUFiles[i].FileName == fileName) {
                    return i;
                }
            }

            return -1;
        }

        private GlobalOptions()
        {
            this.fChartOptions = new TreeChartOptions();
            this.fEventFilters = new StringList();
            this.fMRUFiles = new List<MRUFile>();
            this.fNameFilters = new StringList();
            this.fResidenceFilters = new StringList();
            this.fPedigreeOptions = new PedigreeOptions();
            this.fProxy = new ProxyOptions();
            this.fRelations = new StringList();
            this.fAncestorsCircleOptions = new AncestorsCircleOptions();
            this.fGeocoder = "Google";

            this.fIndividualListColumns = new IndividualListColumns();
            this.fIndividualListColumns.ResetDefaults();

            this.fLanguages = new List<LangRecord>();
            this.fLastBases = new StringList();

            this.fAutosave = false;
            this.fAutosaveInterval = 10;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                this.fLastBases.Dispose();
                //this.fLanguages.Dispose();
                this.fRelations.Dispose();

                this.fResidenceFilters.Dispose();
                this.fNameFilters.Dispose();
                //this.FMRUFiles.Dispose();
                this.fEventFilters.Dispose();

                this.fProxy.Dispose();
                this.fPedigreeOptions.Dispose();
                this.fChartOptions.Dispose();
            }
            base.Dispose(disposing);
        }

        private void LngPrepareProc(string fileName)
        {
            try
            {
                using (StreamReader lngFile = new StreamReader(fileName, Encoding.UTF8))
                {
                    string st = lngFile.ReadLine();

                    if (!string.IsNullOrEmpty(st) && st[0] == ';')
                    {
                        st = st.Remove(0, 1);
                        string[] lngParams = st.Split(',');
                        string lngCode = lngParams[0];
                        string lngSign = lngParams[1];
                        string lngName = lngParams[2];

                        LangRecord lngRec = new LangRecord((ushort)int.Parse(lngCode), lngSign, lngName, fileName);
                        this.fLanguages.Add(lngRec);
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GlobalOptions.LngPrepareProc(): " + ex.Message);
                throw;
            }
        }

        public void FindLanguages()
        {
            try {
                string path = GKUtils.GetLangsPath();
                string[] langFiles = Directory.GetFiles(path, "*.lng", SearchOption.TopDirectoryOnly);
                for (int i = 0; i < langFiles.Length; i++) this.LngPrepareProc(langFiles[i]);
            } catch (Exception ex) {
                Logger.LogWrite("GlobalOptions.FindLanguages(): " + ex.Message);
            }
        }

        public void AddLastBase(string fileName)
        {
            this.fLastBases.Add(fileName);
        }

        public void ClearLastBases()
        {
            this.fLastBases.Clear();
        }


        public void LoadFromFile(IniFile ini)
        {
            if (ini == null)
                throw new ArgumentNullException("ini");

            /*this.fDefCharacterSet = (GEDCOMCharacterSet)ini.ReadInteger("Common", "DefCharacterSet", 3);*/
            this.fDefNameFormat = (NameFormat)ini.ReadInteger("Common", "DefNameFormat", 0);
            this.fDefDateFormat = (DateFormat)ini.ReadInteger("Common", "DefDateFormat", 0);
            this.fLastDir = ini.ReadString("Common", "LastDir", "");
            this.fPlacesWithAddress = ini.ReadBool("Common", "PlacesWithAddress", false);
            this.fShowTips = ini.ReadBool("Common", "ShowTips", true);
            this.fInterfaceLang = (ushort)ini.ReadInteger("Common", "InterfaceLang", 0);
            this.fFileBackup = (FileBackup)ini.ReadInteger("Common", "FileBackup", 0);
            this.fShowDatesCalendar = ini.ReadBool("Common", "ShowDatesCalendar", false);
            this.fShowDatesSign = ini.ReadBool("Common", "ShowDatesSigns", false);

            this.fAutosave = ini.ReadBool("Common", "Autosave", false);
            this.fAutosaveInterval = ini.ReadInteger("Common", "AutosaveInterval", 10);

            this.fExtendedNames = ini.ReadBool("Common", "ExtendedNames", false);
            this.fWomanSurnameFormat = (WomanSurnameFormat)ini.ReadInteger("Common", "WomanSurnameFormat", 0);

            this.fGeocoder = ini.ReadString("Common", "Geocoder", "Google");

            int kl = ini.ReadInteger("Common", "KeyLayout", SysUtils.GetKeyLayout());
            SysUtils.SetKeyLayout(kl);

            this.fChartOptions.LoadFromFile(ini);
            this.fPedigreeOptions.LoadFromFile(ini);
            this.fProxy.LoadFromFile(ini);

            int cnt = ini.ReadInteger("NameFilters", "Count", 0);
            for (int i = 0; i < cnt; i++)
            {
                string st = ini.ReadString("NameFilters", "Filter_" + i.ToString(), "");
                if (st != "") this.fNameFilters.Add(st);
            }

            cnt = ini.ReadInteger("ResidenceFilters", "Count", 0);
            for (int i = 0; i < cnt; i++)
            {
                this.fResidenceFilters.Add(ini.ReadString("ResidenceFilters", "Filter_" + i.ToString(), ""));
            }

            cnt = ini.ReadInteger("EventFilters", "Count", 0);
            for (int i = 0; i < cnt; i++)
            {
                this.fEventFilters.Add(ini.ReadString("EventFilters", "EventVal_" + i.ToString(), ""));
            }

            cnt = ini.ReadInteger("Common", "MRUFiles_Count", 0);
            for (int i = 0; i < cnt; i++)
            {
                string sect = "MRUFile_" + i.ToString();
                string fn = ini.ReadString(sect, "FileName", "");
                if (File.Exists(fn)) {
                    MRUFile mf = new MRUFile();
                    mf.LoadFromFile(ini, sect);
                    this.fMRUFiles.Add(mf);
                } else {
                    MRUFile.DeleteKeys(ini, sect);
                }
            }

            cnt = ini.ReadInteger("Relations", "Count", 0);
            for (int i = 0; i < cnt; i++)
            {
                this.fRelations.Add(ini.ReadString("Relations", "Relation_" + i.ToString(), ""));
            }

            this.fIndividualListColumns.LoadFromFile(ini, "PersonsColumns");

            this.fListHighlightUnmarriedPersons = ini.ReadBool("ListPersons", "HighlightUnmarried", false);
            this.fListHighlightUnparentedPersons = ini.ReadBool("ListPersons", "HighlightUnparented", false);

            this.fMWinRect.Left = ini.ReadInteger("Common", "MWinL", -1);
            this.fMWinRect.Top = ini.ReadInteger("Common", "MWinT", -1);
            this.fMWinRect.Right = ini.ReadInteger("Common", "MWinW", -1);
            this.fMWinRect.Bottom = ini.ReadInteger("Common", "MWinH", -1);
            this.fMWinState = (FormWindowState)((uint)ini.ReadInteger("Common", "MWinState", 0));

            UIHelper.NormalizeFormRect(ref this.fMWinRect);

            cnt = ini.ReadInteger("LastBases", "Count", 0);
            for (int i = 0; i < cnt; i++)
            {
                string st = ini.ReadString("LastBases", "LB" + i.ToString(), "");
                this.AddLastBase(st);
            }

            this.fAncestorsCircleOptions.LoadFromFile(ini);
        }

        public void LoadFromFile(string fileName)
        {
            try
            {
                IniFile ini = new IniFile(fileName);
                try
                {
                    this.LoadFromFile(ini);
                } finally {
                    ini.Dispose();
                }
            } catch (Exception ex) {
                Logger.LogWrite("GlobalOptions.LoadFromFile(): " + ex.Message);
            }
        }


        public void SaveToFile(IniFile ini)
        {
            if (ini == null)
                throw new ArgumentNullException("ini");

            /*ini.WriteInteger("Common", "DefCharacterSet", (int)this.fDefCharacterSet);*/
            ini.WriteInteger("Common", "DefNameFormat", (int)this.fDefNameFormat);
            ini.WriteInteger("Common", "DefDateFormat", (int)this.fDefDateFormat);
            ini.WriteString("Common", "LastDir", this.fLastDir);
            ini.WriteBool("Common", "PlacesWithAddress", this.fPlacesWithAddress);
            ini.WriteBool("Common", "ShowTips", this.fShowTips);
            ini.WriteInteger("Common", "InterfaceLang", this.fInterfaceLang);
            ini.WriteInteger("Common", "FileBackup", (int)this.fFileBackup);
            ini.WriteBool("Common", "ShowDatesCalendar", this.fShowDatesCalendar);
            ini.WriteBool("Common", "ShowDatesSigns", this.fShowDatesSign);

            ini.WriteInteger("Common", "KeyLayout", SysUtils.GetKeyLayout());

            ini.WriteBool("Common", "Autosave", this.fAutosave);
            ini.WriteInteger("Common", "AutosaveInterval", this.fAutosaveInterval);

            ini.WriteBool("Common", "ExtendedNames", this.fExtendedNames);
            ini.WriteInteger("Common", "WomanSurnameFormat", (int)this.fWomanSurnameFormat);

            ini.WriteString("Common", "Geocoder", this.fGeocoder);

            this.fChartOptions.SaveToFile(ini);
            this.fPedigreeOptions.SaveToFile(ini);
            this.fProxy.SaveToFile(ini);

            this.fNameFilters.Sort();

            int cnt = this.fNameFilters.Count;
            ini.WriteInteger("NameFilters", "Count", cnt);
            for (int i = 0; i < cnt; i++)
            {
                ini.WriteString("NameFilters", "Filter_" + i.ToString(), this.fNameFilters[i]);
            }

            cnt = this.fResidenceFilters.Count;
            ini.WriteInteger("ResidenceFilters", "Count", cnt);
            for (int i = 0; i < cnt; i++)
            {
                ini.WriteString("ResidenceFilters", "Filter_" + i.ToString(), this.fResidenceFilters[i]);
            }

            cnt = this.fEventFilters.Count;
            ini.WriteInteger("EventFilters", "Count", cnt);
            for (int i = 0; i < cnt; i++)
            {
                ini.WriteString("EventFilters", "EventVal_" + i.ToString(), this.fEventFilters[i]);
            }

            cnt = this.fMRUFiles.Count;
            ini.WriteInteger("Common", "MRUFiles_Count", cnt);
            for (int i = 0; i < cnt; i++)
            {
                this.fMRUFiles[i].SaveToFile(ini, "MRUFile_" + i.ToString());
            }
            //this.FMRUFiles.Sort();

            cnt = this.fRelations.Count;
            ini.WriteInteger("Relations", "Count", cnt);
            for (int i = 0; i < cnt; i++)
            {
                ini.WriteString("Relations", "Relation_" + i.ToString(), this.fRelations[i]);
            }

            this.fIndividualListColumns.SaveToFile(ini, "PersonsColumns");

            ini.WriteBool("ListPersons", "HighlightUnmarried", this.fListHighlightUnmarriedPersons);
            ini.WriteBool("ListPersons", "HighlightUnparented", this.fListHighlightUnparentedPersons);

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
            ini.WriteInteger("Common", "MWinL", this.fMWinRect.Left);
            ini.WriteInteger("Common", "MWinT", this.fMWinRect.Top);
            ini.WriteInteger("Common", "MWinW", this.fMWinRect.Right);
            ini.WriteInteger("Common", "MWinH", this.fMWinRect.Bottom);
            ini.WriteInteger("Common", "MWinState", (int)this.fMWinState);

            cnt = this.fLastBases.Count;
            ini.WriteInteger("LastBases", "Count", cnt);
            for (int i = 0; i < cnt; i++)
            {
                ini.WriteString("LastBases", "LB" + i.ToString(), this.GetLastBase(i));
            }

            this.fAncestorsCircleOptions.SaveToFile(ini);
        }

        public void SaveToFile(string fileName)
        {
            try {
                IniFile ini = new IniFile(fileName);

                try
                {
                    this.SaveToFile(ini);
                }
                finally
                {
                    ini.Dispose();
                }
            } catch (Exception ex) {
                Logger.LogWrite("GlobalOptions.SaveToFile(): " + ex.Message);
            }
        }
    }
}
