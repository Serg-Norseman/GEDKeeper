using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM.Enums;
using GKCore.Types;
using GKUI.Lists;

namespace GKCore.Options
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class GlobalOptions : BaseObject
	{
		public struct ColumnRec
		{
			public LSID Name;
			public int DefWidth;
			public bool OnlyMain;

			public ColumnRec(LSID name, int defWidth, bool onlyMain) {
				this.Name = name;
				this.DefWidth = defWidth;
				this.OnlyMain = onlyMain;
			}
		}


		public static readonly GlobalOptions.ColumnRec[] PersonColumnsName;

		private readonly TreeChartOptions fChartOptions;
		private GEDCOMCharacterSet fDefCharacterSet;
		private DateFormat fDefDateFormat;
		private NameFormat fDefNameFormat;
		private readonly StringList fEventFilters;
		private ushort fInterfaceLang;
		private readonly ExtList<LangRecord> fLanguages;
		private string fLastDir;
		private readonly List<MRUFile> FMRUFiles;
		private readonly StringList fNameFilters;
		private readonly PedigreeOptions fPedigreeOptions;
		private bool fPlacesWithAddress;
		private readonly ProxyOptions fProxy;
		private readonly StringList fRelations;
		private readonly StringList fResidenceFilters;
		private bool fShowTips;
		private readonly IndividualListColumns fIndividualListColumns;
		private bool FListPersons_HighlightUnmarried;
		private bool FListPersons_HighlightUnparented;
		private ExtRect FMWinRect;
		private FormWindowState FMWinState;
		private readonly StringList fLastBases;
		private bool fRevisionsBackup;


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
			get { return this.FMRUFiles; }
		}

		public ExtRect MWinRect
		{
			get { return this.FMWinRect; }
			set { this.FMWinRect = value; }
		}

		public FormWindowState MWinState
		{
			get { return this.FMWinState; }
			set { this.FMWinState = value; }
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

		public bool ListPersons_HighlightUnmarried
		{
			get { return this.FListPersons_HighlightUnmarried; }
			set { this.FListPersons_HighlightUnmarried = value; }
		}

		public bool ListPersons_HighlightUnparented
		{
			get { return this.FListPersons_HighlightUnparented; }
			set { this.FListPersons_HighlightUnparented = value; }
		}

		public IndividualListColumns IndividualListColumns
		{
			get { return this.fIndividualListColumns; }
		}

		private void LngPrepareProc(string fileName)
		{
			StreamReader lngFile = new StreamReader(fileName, Encoding.UTF8);
			try
			{
				string st = lngFile.ReadLine();

				if (st[0] == ';')
				{
					try
					{
						st = st.Remove(0, 1);
						string[] lng_params = st.Split(',');
						string lng_code = lng_params[0];
						string lng_name = lng_params[1];
						LangRecord lng_rec = new LangRecord();
						lng_rec.Code = (ushort)int.Parse(lng_code);
						lng_rec.Name = lng_name;
						lng_rec.FileName = fileName;
						this.fLanguages.Add(lng_rec);
					}
					catch (Exception ex)
					{
						SysUtils.LogWrite("TGlobalOptions.LngPrepareProc(): " + ex.Message);
						throw;
					}
				}
			}
			finally
			{
				lngFile.Close();
			}
		}

		public int GetLangsCount()
		{
			return this.fLanguages.Count;
		}

		public LangRecord GetLang(int index)
		{
			return this.fLanguages[index] as LangRecord;
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
			int result = -1;

			for (int i = 0; i <= FMRUFiles.Count - 1; i++)
			{
				if (this.FMRUFiles[i].FileName == fileName)
				{
					result = i;
					break;
				}
			}

			return result;
		}

		public GlobalOptions()
		{
			this.fChartOptions = new TreeChartOptions();
			this.fEventFilters = new StringList();
			this.FMRUFiles = new List<MRUFile>();
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
		}

		public void FindLanguages()
		{
			string path = GKUtils.GetAppPath() + "langs\\";
			string[] langFiles = Directory.GetFiles(path, "*.lng", SearchOption.TopDirectoryOnly);
			for (int i = 0; i < langFiles.Length; i++) this.LngPrepareProc(langFiles[i]);
		}

		public void LoadFromFile(string fileName)
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

				ushort kl = (ushort)ini.ReadInteger("Common", "KeyLayout", SysUtils.GetKeyLayout());
                SysUtils.SetKeyLayout(kl);

				this.fChartOptions.LoadFromFile(ini);
				this.fPedigreeOptions.LoadFromFile(ini);
				this.fProxy.LoadFromFile(ini);

				int cnt = ini.ReadInteger("NameFilters", "Count", 0);
				for (int i = 0; i <= cnt - 1; i++)
				{
					string st = ini.ReadString("NameFilters", "Filter_" + i.ToString(), "");
					if (st != "") this.fNameFilters.Add(st);
				}

				cnt = ini.ReadInteger("ResidenceFilters", "Count", 0);
				for (int i = 0; i <= cnt - 1; i++)
				{
					this.fResidenceFilters.Add(ini.ReadString("ResidenceFilters", "Filter_" + i.ToString(), ""));
				}

				cnt = ini.ReadInteger("EventFilters", "Count", 0);
				for (int i = 0; i <= cnt - 1; i++)
				{
					this.fEventFilters.Add(ini.ReadString("EventFilters", "EventVal_" + i.ToString(), ""));
				}

				cnt = ini.ReadInteger("Common", "MRUFiles_Count", 0);
				for (int i = 0; i <= cnt - 1; i++)
				{
					string sect = "MRUFile_" + i.ToString();
					string fn = ini.ReadString(sect, "FileName", "");
					if (File.Exists(fn)) {
						MRUFile mf = new MRUFile();
						mf.Load(ini, sect);
						this.FMRUFiles.Add(mf);
					} else {
						MRUFile.DeleteKeys(ini, sect);
					}
				}

				cnt = ini.ReadInteger("Relations", "Count", 0);
				for (int i = 0; i <= cnt - 1; i++)
				{
					this.fRelations.Add(ini.ReadString("Relations", "Relation_" + i.ToString(), ""));
				}

				this.fIndividualListColumns.LoadFromFile(ini, "PersonsColumns");

				this.FListPersons_HighlightUnmarried = ini.ReadBool("ListPersons", "HighlightUnmarried", false);
				this.FListPersons_HighlightUnparented = ini.ReadBool("ListPersons", "HighlightUnparented", false);
				
				this.FMWinRect.Left = ini.ReadInteger("Common", "MWinL", -1);
				this.FMWinRect.Top = ini.ReadInteger("Common", "MWinT", -1);
				this.FMWinRect.Right = ini.ReadInteger("Common", "MWinW", -1);
				this.FMWinRect.Bottom = ini.ReadInteger("Common", "MWinH", -1);
				this.FMWinState = (FormWindowState)((uint)ini.ReadInteger("Common", "MWinState", 0));

				cnt = ini.ReadInteger("LastBases", "Count", 0);
				for (int i = 0; i <= cnt - 1; i++)
				{
					string st = ini.ReadString("LastBases", "LB" + i.ToString(), "");
					this.AddLastBase(st);
				}
			}
			finally
			{
				ini.Dispose();
			}
		}

		public void SaveToFile(string fileName)
		{
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
				ini.WriteInteger("Common", "KeyLayout", SysUtils.GetKeyLayout());

				this.fChartOptions.SaveToFile(ini);
				this.fPedigreeOptions.SaveToFile(ini);
				this.fProxy.SaveToFile(ini);

				this.fNameFilters.Sort();
				ini.WriteInteger("NameFilters", "Count", this.fNameFilters.Count);
				int num = this.fNameFilters.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					ini.WriteString("NameFilters", "Filter_" + i.ToString(), this.fNameFilters[i]);
				}

				ini.WriteInteger("ResidenceFilters", "Count", this.fResidenceFilters.Count);
				int num2 = this.fResidenceFilters.Count - 1;
				for (int i = 0; i <= num2; i++)
				{
					ini.WriteString("ResidenceFilters", "Filter_" + i.ToString(), this.fResidenceFilters[i]);
				}

				ini.WriteInteger("EventFilters", "Count", this.fEventFilters.Count);
				int num3 = this.fEventFilters.Count - 1;
				for (int i = 0; i <= num3; i++)
				{
					ini.WriteString("EventFilters", "EventVal_" + i.ToString(), this.fEventFilters[i]);
				}

				ini.WriteInteger("Common", "MRUFiles_Count", this.FMRUFiles.Count);
				int num4 = this.FMRUFiles.Count - 1;
				for (int i = 0; i <= num4; i++)
				{
					this.FMRUFiles[i].Save(ini, "MRUFile_" + i.ToString());
				}
				//this.FMRUFiles.Sort();

				ini.WriteInteger("Relations", "Count", this.fRelations.Count);
				int num5 = this.fRelations.Count - 1;
				for (int i = 0; i <= num5; i++)
				{
					ini.WriteString("Relations", "Relation_" + i.ToString(), this.fRelations[i]);
				}

				this.fIndividualListColumns.SaveToFile(ini, "PersonsColumns");

				ini.WriteBool("ListPersons", "HighlightUnmarried", this.FListPersons_HighlightUnmarried);
				ini.WriteBool("ListPersons", "HighlightUnparented", this.FListPersons_HighlightUnparented);
				ini.WriteInteger("Common", "MWinL", this.FMWinRect.Left);
				ini.WriteInteger("Common", "MWinT", this.FMWinRect.Top);
				ini.WriteInteger("Common", "MWinW", this.FMWinRect.Right);
				ini.WriteInteger("Common", "MWinH", this.FMWinRect.Bottom);
				ini.WriteInteger("Common", "MWinState", (int)this.FMWinState);

				ini.WriteInteger("LastBases", "Count", this.fLastBases.Count);
				int num6 = this.fLastBases.Count - 1;
				for (int i = 0; i <= num6; i++)
				{
					ini.WriteString("LastBases", "LB" + i.ToString(), this.GetLastBase(i));
				}
			}
			finally
			{
				ini.Dispose();
			}
		}

		public void AddLastBase(string aFileName)
		{
			this.fLastBases.Add(aFileName);
		}

		public void ClearLastBases()
		{
			this.fLastBases.Clear();
		}

		static GlobalOptions()
		{
			GlobalOptions.ColumnRec[] array2 = new GlobalOptions.ColumnRec[25];
			array2[0] = new ColumnRec(LSID.LSID_Patriarch, 25, false);
			array2[1] = new ColumnRec(LSID.LSID_FullName, 25, false);
			array2[2] = new ColumnRec(LSID.LSID_Nickname, 75, false);
			array2[3] = new ColumnRec(LSID.LSID_Sex, 45, false);
			array2[4] = new ColumnRec(LSID.LSID_BirthDate, 100, false);
			array2[5] = new ColumnRec(LSID.LSID_DeathDate, 100, false);
			array2[6] = new ColumnRec(LSID.LSID_BirthPlace, 100, false);
			array2[7] = new ColumnRec(LSID.LSID_DeathPlace, 100, false);
			array2[8] = new ColumnRec(LSID.LSID_Residence, 100, false);
			array2[9] = new ColumnRec(LSID.LSID_Age, 100, true);
			array2[10] = new ColumnRec(LSID.LSID_LifeExpectancy, 100, true);
			array2[11] = new ColumnRec(LSID.LSID_DaysForBirth, 100, true);
			array2[12] = new ColumnRec(LSID.LSID_RPGroups, 200, true);
			array2[13] = new ColumnRec(LSID.LSID_Religion, 200, true);
			array2[14] = new ColumnRec(LSID.LSID_Nationality, 200, true);
			array2[15] = new ColumnRec(LSID.LSID_Education, 200, true);
			array2[16] = new ColumnRec(LSID.LSID_Occupation, 200, true);
			array2[17] = new ColumnRec(LSID.LSID_Caste, 200, true);
			array2[18] = new ColumnRec(LSID.LSID_Mili, 200, true);
			array2[19] = new ColumnRec(LSID.LSID_MiliInd, 200, true);
			array2[20] = new ColumnRec(LSID.LSID_MiliDis, 200, true);
			array2[21] = new ColumnRec(LSID.LSID_MiliRank, 200, true);
			array2[22] = new ColumnRec(LSID.LSID_Changed, 150, true);
			array2[23] = new ColumnRec(LSID.LSID_Bookmark, 25, true);
			array2[24] = new ColumnRec(LSID.LSID_NobilityTitle, 200, true);
			GlobalOptions.PersonColumnsName = array2;
		}
	}
}
