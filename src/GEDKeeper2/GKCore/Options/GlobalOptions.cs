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
		private GEDCOMCharacterSet fDefCharacterSet;
		private DateFormat fDefDateFormat;
		private bool fDefDateSigns;
		private NameFormat fDefNameFormat;
		private readonly StringList fEventFilters;
		private ushort fInterfaceLang;
		private readonly ExtList<LangRecord> fLanguages;
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
		private bool fRevisionsBackup;
		private bool fShowDatesCalendar;


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

		public GEDCOMCharacterSet DefCharacterSet
		{
			get { return this.fDefCharacterSet; }
			set { this.fDefCharacterSet = value; }
		}

		public DateFormat DefDateFormat
		{
			get { return this.fDefDateFormat; }
			set { this.fDefDateFormat = value; }
		}

		public bool DefDateSigns
		{
			get { return this.fDefDateSigns; }
			set { this.fDefDateSigns = value; }
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

		public bool RevisionsBackup
		{
			get { return this.fRevisionsBackup; }
			set { this.fRevisionsBackup = value; }
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
                        string lngName = lngParams[1];

                        LangRecord lngRec = new LangRecord((ushort)int.Parse(lngCode), lngName, fileName);
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

		public int GetLangsCount()
		{
			return this.fLanguages.Count;
		}

		public LangRecord GetLang(int index)
		{
			return this.fLanguages[index];
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

			this.fIndividualListColumns = new IndividualListColumns();
			this.fIndividualListColumns.ResetDefaults();

			this.fLanguages = new ExtList<LangRecord>(true);
			this.fLastBases = new StringList();
		}

        protected override void Dispose(bool disposing)
		{
            if (disposing)
			{
                this.fLastBases.Dispose();
				this.fLanguages.Dispose();
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

		public void FindLanguages()
		{
			string path = GKUtils.GetLangsPath();
			string[] langFiles = Directory.GetFiles(path, "*.lng", SearchOption.TopDirectoryOnly);
			for (int i = 0; i < langFiles.Length; i++) this.LngPrepareProc(langFiles[i]);
		}

		public void LoadFromFile(string fileName)
		{
			try
			{
				IniFile ini = new IniFile(fileName);
				try
				{
					this.fDefCharacterSet = (GEDCOMCharacterSet)ini.ReadInteger("Common", "DefCharacterSet", 3);
					this.fDefNameFormat = (NameFormat)ini.ReadInteger("Common", "DefNameFormat", 0);
					this.fDefDateFormat = (DateFormat)ini.ReadInteger("Common", "DefDateFormat", 0);
					this.fLastDir = ini.ReadString("Common", "LastDir", "");
					this.fPlacesWithAddress = ini.ReadBool("Common", "PlacesWithAddress", false);
					this.fShowTips = ini.ReadBool("Common", "ShowTips", true);
					this.fInterfaceLang = (ushort)ini.ReadInteger("Common", "InterfaceLang", 1049);
					this.fRevisionsBackup = ini.ReadBool("Common", "RevisionsBackup", false);
					this.fShowDatesCalendar = ini.ReadBool("Common", "ShowDatesCalendar", false);

					int kl = ini.ReadInteger("Common", "KeyLayout", GKUtils.GetKeyLayout());
					GKUtils.SetKeyLayout(kl);

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
							mf.Load(ini, sect);
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

					cnt = ini.ReadInteger("LastBases", "Count", 0);
					for (int i = 0; i < cnt; i++)
					{
						string st = ini.ReadString("LastBases", "LB" + i.ToString(), "");
						this.AddLastBase(st);
					}
				} finally {
					ini.Dispose();
				}
			} catch (Exception ex) {
				Logger.LogWrite("GlobalOptions.LoadFromFile(): " + ex.Message);
			}
		}

		public void SaveToFile(string fileName)
		{
			try {
				IniFile ini = new IniFile(fileName);
				try
				{
					ini.WriteInteger("Common", "DefCharacterSet", (int)this.fDefCharacterSet);
					ini.WriteInteger("Common", "DefNameFormat", (int)this.fDefNameFormat);
					ini.WriteInteger("Common", "DefDateFormat", (int)this.fDefDateFormat);
					ini.WriteString("Common", "LastDir", this.fLastDir);
					ini.WriteBool("Common", "PlacesWithAddress", this.fPlacesWithAddress);
					ini.WriteBool("Common", "ShowTips", this.fShowTips);
					ini.WriteInteger("Common", "InterfaceLang", this.fInterfaceLang);
					ini.WriteBool("Common", "RevisionsBackup", this.fRevisionsBackup);
					ini.WriteBool("Common", "ShowDatesCalendar", this.fShowDatesCalendar);
					
					ini.WriteInteger("Common", "KeyLayout", GKUtils.GetKeyLayout());

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
						this.fMRUFiles[i].Save(ini, "MRUFile_" + i.ToString());
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
				}
				finally
				{
					ini.Dispose();
				}
			} catch (Exception ex) {
				Logger.LogWrite("GlobalOptions.SaveToFile(): " + ex.Message);
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
	}
}
