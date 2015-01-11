using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Windows.Forms;

using ExtUtils;
using GedCom551;
using GKUI.Lists;

/// <summary>
/// 
/// </summary>

namespace GKCore.Options
{
	public sealed class GlobalOptions : IDisposable
	{
		public class TLangRecord
		{
			public ushort Code;
			public string Name;
			public string FileName;

			public void Free()
			{
				SysUtils.Free(this);
			}
		}

		public class TMRUFile
		{
			public string FileName;
			public ExtRect WinRect;
			public FormWindowState WinState;

			public void Load(IniFile iniFile, string section)
			{
			    if (iniFile == null) return;

				this.FileName		= iniFile.ReadString(section, "FileName", "");
				this.WinRect.Left	= iniFile.ReadInteger(section, "WinL", 10);
				this.WinRect.Top	= iniFile.ReadInteger(section, "WinT", 10);
				this.WinRect.Right	= iniFile.ReadInteger(section, "WinR", 778);
				this.WinRect.Bottom = iniFile.ReadInteger(section, "WinB", 312);
				this.WinState		= (FormWindowState)((uint)iniFile.ReadInteger(section, "WinState", 0));
			}

			public void Save(IniFile iniFile, string section)
			{
                if (iniFile == null) return;

				iniFile.WriteString(section, "FileName", this.FileName);
				iniFile.WriteInteger(section, "WinL", this.WinRect.Left);
				iniFile.WriteInteger(section, "WinT", this.WinRect.Top);
				iniFile.WriteInteger(section, "WinR", this.WinRect.Right);
				iniFile.WriteInteger(section, "WinB", this.WinRect.Bottom);
				iniFile.WriteInteger(section, "WinState", (int)this.WinState);
			}

			public static void DeleteKeys(IniFile iniFile, string section)
			{
                if (iniFile == null) return;

                iniFile.DeleteKey(section, "FileName");
				iniFile.DeleteKey(section, "WinL");
				iniFile.DeleteKey(section, "WinT");
				iniFile.DeleteKey(section, "WinR");
				iniFile.DeleteKey(section, "WinB");
				iniFile.DeleteKey(section, "WinState");
			}

			public void Free()
			{
				SysUtils.Free(this);
			}
		}

		public struct TColumnRec
		{
			public LSID Name;
			public int DefWidth;
			public bool colOnlyMain;

			public TColumnRec(LSID aName, int aDefWidth, bool aOnlyMain) {
				this.Name = aName;
				this.DefWidth = aDefWidth;
				this.colOnlyMain = aOnlyMain;
			}
		}


		public static readonly GlobalOptions.TColumnRec[] PersonColumnsName;

		private readonly TreeChartOptions FChartOptions;
		private TGEDCOMCharacterSet FDefCharacterSet;
		private DateFormat FDefDateFormat;
		private NameFormat FDefNameFormat;
		private readonly StringList FEventFilters;
		private ushort FInterfaceLang;
		private readonly ExtList<TLangRecord> FLanguages;
		private string FLastDir;
		private readonly List<TMRUFile> FMRUFiles;
		private readonly StringList FNameFilters;
		private readonly PedigreeOptions FPedigreeOptions;
		private bool FPlacesWithAddress;
		private readonly ProxyOptions FProxy;
		private readonly StringList FRelations;
		private readonly StringList FResidenceFilters;
		private bool FShowTips;
		private readonly TIndividualListColumns FIndividualListColumns;
		private bool FListPersons_HighlightUnmarried;
		private bool FListPersons_HighlightUnparented;
		private ExtRect FMWinRect;
		private FormWindowState FMWinState;
		private readonly StringList FLastBases;
		private bool FRevisionsBackup;

	    private bool fDisposed;


		public TreeChartOptions ChartOptions
		{
			get { return this.FChartOptions; }
		}

		public TGEDCOMCharacterSet DefCharacterSet
		{
			get { return this.FDefCharacterSet; }
			set { this.FDefCharacterSet = value; }
		}

		public DateFormat DefDateFormat
		{
			get { return this.FDefDateFormat; }
			set { this.FDefDateFormat = value; }
		}

		public NameFormat DefNameFormat
		{
			get { return this.FDefNameFormat; }
			set { this.FDefNameFormat = value; }
		}

		public StringList EventFilters
		{
			get { return this.FEventFilters; }
		}

		public ushort InterfaceLang
		{
			get { return this.FInterfaceLang; }
			set { this.FInterfaceLang = value; }
		}

		public string LastDir
		{
			get { return this.FLastDir; }
			set { this.FLastDir = value; }
		}

		public List<TMRUFile> MRUFiles
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
			get { return this.FNameFilters; }
		}

		public PedigreeOptions PedigreeOptions
		{
			get { return this.FPedigreeOptions; }
		}

		public bool PlacesWithAddress
		{
			get { return this.FPlacesWithAddress; }
			set { this.FPlacesWithAddress = value; }
		}

		public ProxyOptions Proxy
		{
			get { return this.FProxy; }
		}

		public StringList Relations
		{
			get { return this.FRelations; }
		}

		public StringList ResidenceFilters
		{
			get { return this.FResidenceFilters; }
		}

		public bool RevisionsBackup
		{
			get { return this.FRevisionsBackup; }
			set { this.FRevisionsBackup = value; }
		}

		public bool ShowTips
		{
			get { return this.FShowTips; }
			set { this.FShowTips = value; }
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

		public TIndividualListColumns IndividualListColumns
		{
			get { return this.FIndividualListColumns; }
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
						TLangRecord lng_rec = new TLangRecord();
						lng_rec.Code = (ushort)int.Parse(lng_code);
						lng_rec.Name = lng_name;
						lng_rec.FileName = fileName;
						this.FLanguages.Add(lng_rec);
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
			return this.FLanguages.Count;
		}

		public TLangRecord GetLang(int Index)
		{
			return this.FLanguages[Index] as TLangRecord;
		}

		public string GetLastBase(int Index)
		{
			return this.FLastBases[Index];
		}

		public int GetLastBasesCount()
		{
			return this.FLastBases.Count;
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
			this.FChartOptions = new TreeChartOptions();
			this.FEventFilters = new StringList();
			this.FMRUFiles = new List<TMRUFile>();
			this.FNameFilters = new StringList();
			this.FResidenceFilters = new StringList();
			this.FPedigreeOptions = new PedigreeOptions();
			this.FProxy = new ProxyOptions();
			this.FRelations = new StringList();

			this.FIndividualListColumns = new TIndividualListColumns();
			this.FIndividualListColumns.ResetDefaults();

			this.FLanguages = new ExtList<TLangRecord>(true);
			this.FLastBases = new StringList();
		}

		public void Dispose()
		{
			if (!this.fDisposed)
			{
                this.FLastBases.Dispose();
				this.FLanguages.Dispose();
                this.FRelations.Dispose();

                this.FResidenceFilters.Dispose();
                this.FNameFilters.Dispose();
				//this.FMRUFiles.Dispose();
                this.FEventFilters.Dispose();

                this.FProxy.Dispose();
                this.FPedigreeOptions.Dispose();
                this.FChartOptions.Dispose();

                this.fDisposed = true;
			}
		}

		public void FindLanguages()
		{
			string path = GKUtils.GetAppPath() + "langs\\";
			string[] lang_files = Directory.GetFiles(path, "*.lng", SearchOption.TopDirectoryOnly);
			for (int i = 0; i < lang_files.Length; i++) this.LngPrepareProc(lang_files[i]);
		}

		public void LoadFromFile(string fileName)
		{
			IniFile ini = new IniFile(fileName);
			try
			{
				this.FDefCharacterSet = (TGEDCOMCharacterSet)ini.ReadInteger("Common", "DefCharacterSet", 3);
				this.FDefNameFormat = (NameFormat)ini.ReadInteger("Common", "DefNameFormat", 0);
				this.FDefDateFormat = (DateFormat)ini.ReadInteger("Common", "DefDateFormat", 0);
				this.FLastDir = ini.ReadString("Common", "LastDir", "");
				this.FPlacesWithAddress = ini.ReadBool("Common", "PlacesWithAddress", false);
				this.FShowTips = ini.ReadBool("Common", "ShowTips", true);
				this.FInterfaceLang = (ushort)ini.ReadInteger("Common", "InterfaceLang", 1049);
				this.FRevisionsBackup = ini.ReadBool("Common", "RevisionsBackup", false);

				ushort kl = (ushort)ini.ReadInteger("Common", "KeyLayout", SysUtils.GetKeyLayout());
                SysUtils.SetKeyLayout(kl);

				this.FChartOptions.LoadFromFile(ini);
				this.FPedigreeOptions.LoadFromFile(ini);
				this.FProxy.LoadFromFile(ini);

				int cnt = ini.ReadInteger("NameFilters", "Count", 0);
				for (int i = 0; i <= cnt - 1; i++)
				{
					string st = ini.ReadString("NameFilters", "Filter_" + i.ToString(), "");
					if (st != "") this.FNameFilters.Add(st);
				}

				cnt = ini.ReadInteger("ResidenceFilters", "Count", 0);
				for (int i = 0; i <= cnt - 1; i++)
				{
					this.FResidenceFilters.Add(ini.ReadString("ResidenceFilters", "Filter_" + i.ToString(), ""));
				}

				cnt = ini.ReadInteger("EventFilters", "Count", 0);
				for (int i = 0; i <= cnt - 1; i++)
				{
					this.FEventFilters.Add(ini.ReadString("EventFilters", "EventVal_" + i.ToString(), ""));
				}

				cnt = ini.ReadInteger("Common", "MRUFiles_Count", 0);
				for (int i = 0; i <= cnt - 1; i++)
				{
					string sect = "MRUFile_" + i.ToString();
					string fn = ini.ReadString(sect, "FileName", "");
					if (File.Exists(fn)) {
						TMRUFile mf = new TMRUFile();
						mf.Load(ini, sect);
						this.FMRUFiles.Add(mf);
					} else {
						TMRUFile.DeleteKeys(ini, sect);
					}
				}

				cnt = ini.ReadInteger("Relations", "Count", 0);
				for (int i = 0; i <= cnt - 1; i++)
				{
					this.FRelations.Add(ini.ReadString("Relations", "Relation_" + i.ToString(), ""));
				}

				this.FIndividualListColumns.LoadFromFile(ini, "PersonsColumns");

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
				ini.WriteInteger("Common", "DefCharacterSet", (int)this.FDefCharacterSet);
				ini.WriteInteger("Common", "DefNameFormat", (int)this.FDefNameFormat);
				ini.WriteInteger("Common", "DefDateFormat", (int)this.FDefDateFormat);
				ini.WriteString("Common", "LastDir", this.FLastDir);
				ini.WriteBool("Common", "PlacesWithAddress", this.FPlacesWithAddress);
				ini.WriteBool("Common", "ShowTips", this.FShowTips);
				ini.WriteInteger("Common", "InterfaceLang", this.FInterfaceLang);
				ini.WriteBool("Common", "RevisionsBackup", this.FRevisionsBackup);
				ini.WriteInteger("Common", "KeyLayout", SysUtils.GetKeyLayout());

				this.FChartOptions.SaveToFile(ini);
				this.FPedigreeOptions.SaveToFile(ini);
				this.FProxy.SaveToFile(ini);

				this.FNameFilters.Sort();
				ini.WriteInteger("NameFilters", "Count", this.FNameFilters.Count);
				int num = this.FNameFilters.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					ini.WriteString("NameFilters", "Filter_" + i.ToString(), this.FNameFilters[i]);
				}

				ini.WriteInteger("ResidenceFilters", "Count", this.FResidenceFilters.Count);
				int num2 = this.FResidenceFilters.Count - 1;
				for (int i = 0; i <= num2; i++)
				{
					ini.WriteString("ResidenceFilters", "Filter_" + i.ToString(), this.FResidenceFilters[i]);
				}

				ini.WriteInteger("EventFilters", "Count", this.FEventFilters.Count);
				int num3 = this.FEventFilters.Count - 1;
				for (int i = 0; i <= num3; i++)
				{
					ini.WriteString("EventFilters", "EventVal_" + i.ToString(), this.FEventFilters[i]);
				}

				ini.WriteInteger("Common", "MRUFiles_Count", this.FMRUFiles.Count);
				int num4 = this.FMRUFiles.Count - 1;
				for (int i = 0; i <= num4; i++)
				{
					this.FMRUFiles[i].Save(ini, "MRUFile_" + i.ToString());
				}
				//this.FMRUFiles.Sort();

				ini.WriteInteger("Relations", "Count", this.FRelations.Count);
				int num5 = this.FRelations.Count - 1;
				for (int i = 0; i <= num5; i++)
				{
					ini.WriteString("Relations", "Relation_" + i.ToString(), this.FRelations[i]);
				}

				this.FIndividualListColumns.SaveToFile(ini, "PersonsColumns");

				ini.WriteBool("ListPersons", "HighlightUnmarried", this.FListPersons_HighlightUnmarried);
				ini.WriteBool("ListPersons", "HighlightUnparented", this.FListPersons_HighlightUnparented);
				ini.WriteInteger("Common", "MWinL", this.FMWinRect.Left);
				ini.WriteInteger("Common", "MWinT", this.FMWinRect.Top);
				ini.WriteInteger("Common", "MWinW", this.FMWinRect.Right);
				ini.WriteInteger("Common", "MWinH", this.FMWinRect.Bottom);
				ini.WriteInteger("Common", "MWinState", (int)this.FMWinState);

				ini.WriteInteger("LastBases", "Count", this.FLastBases.Count);
				int num6 = this.FLastBases.Count - 1;
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
			this.FLastBases.Add(aFileName);
		}

		public void ClearLastBases()
		{
			this.FLastBases.Clear();
		}

		static GlobalOptions()
		{
			GlobalOptions.TColumnRec[] array2 = new GlobalOptions.TColumnRec[25];
			array2[0] = new TColumnRec(LSID.LSID_Patriarch, 25, false);
			array2[1] = new TColumnRec(LSID.LSID_FullName, 25, false);
			array2[2] = new TColumnRec(LSID.LSID_Nickname, 75, false);
			array2[3] = new TColumnRec(LSID.LSID_Sex, 45, false);
			array2[4] = new TColumnRec(LSID.LSID_BirthDate, 100, false);
			array2[5] = new TColumnRec(LSID.LSID_DeathDate, 100, false);
			array2[6] = new TColumnRec(LSID.LSID_BirthPlace, 100, false);
			array2[7] = new TColumnRec(LSID.LSID_DeathPlace, 100, false);
			array2[8] = new TColumnRec(LSID.LSID_Residence, 100, false);
			array2[9] = new TColumnRec(LSID.LSID_Age, 100, true);
			array2[10] = new TColumnRec(LSID.LSID_LifeExpectancy, 100, true);
			array2[11] = new TColumnRec(LSID.LSID_DaysForBirth, 100, true);
			array2[12] = new TColumnRec(LSID.LSID_RPGroups, 200, true);
			array2[13] = new TColumnRec(LSID.LSID_Religion, 200, true);
			array2[14] = new TColumnRec(LSID.LSID_Nationality, 200, true);
			array2[15] = new TColumnRec(LSID.LSID_Education, 200, true);
			array2[16] = new TColumnRec(LSID.LSID_Occupation, 200, true);
			array2[17] = new TColumnRec(LSID.LSID_Caste, 200, true);
			array2[18] = new TColumnRec(LSID.LSID_Mili, 200, true);
			array2[19] = new TColumnRec(LSID.LSID_MiliInd, 200, true);
			array2[20] = new TColumnRec(LSID.LSID_MiliDis, 200, true);
			array2[21] = new TColumnRec(LSID.LSID_MiliRank, 200, true);
			array2[22] = new TColumnRec(LSID.LSID_Changed, 150, true);
			array2[23] = new TColumnRec(LSID.LSID_Bookmark, 25, true);
			array2[24] = new TColumnRec(LSID.LSID_NobilityTitle, 200, true);
			GlobalOptions.PersonColumnsName = array2;
		}
	}
}
