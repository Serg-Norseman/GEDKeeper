using System;
using System.Collections.Generic;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;
using System.Windows.Forms;

using Ext.Utils;
using GedCom551;
using GKUI.Lists;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKCore
{
	public class GlobalOptions : IDisposable
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
			public TRect WinRect;
			public FormWindowState WinState;

			public void Load([In] IniFile ini, string aSection)
			{
				this.FileName		= ini.ReadString(aSection, "FileName", "");
				this.WinRect.Left	= ini.ReadInteger(aSection, "WinL", 10);
				this.WinRect.Top	= ini.ReadInteger(aSection, "WinT", 10);
				this.WinRect.Right	= ini.ReadInteger(aSection, "WinR", 778);
				this.WinRect.Bottom = ini.ReadInteger(aSection, "WinB", 312);
				this.WinState		= (FormWindowState)((uint)ini.ReadInteger(aSection, "WinState", 0));
			}

			public void Save([In] IniFile ini, string aSection)
			{
				ini.WriteString(aSection, "FileName", this.FileName);
				ini.WriteInteger(aSection, "WinL", this.WinRect.Left);
				ini.WriteInteger(aSection, "WinT", this.WinRect.Top);
				ini.WriteInteger(aSection, "WinR", this.WinRect.Right);
				ini.WriteInteger(aSection, "WinB", this.WinRect.Bottom);
				ini.WriteInteger(aSection, "WinState", (int)this.WinState);
			}

			public static void DeleteKeys([In] IniFile ini, string aSection)
			{
				ini.DeleteKey(aSection, "FileName");
				ini.DeleteKey(aSection, "WinL");
				ini.DeleteKey(aSection, "WinT");
				ini.DeleteKey(aSection, "WinR");
				ini.DeleteKey(aSection, "WinB");
				ini.DeleteKey(aSection, "WinState");
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

		private AncestorsCircleOptions FAncCircleOptions;
		private TreeChartOptions FChartOptions;
		private TGEDCOMCharacterSet FDefCharacterSet;
		private TGenEngine.TDateFormat FDefDateFormat;
		private TGenEngine.TNameFormat FDefNameFormat;
		private StringList FEventFilters;
		private ushort FInterfaceLang;
		private TList FLanguages;
		private string FLastDir;
		private List<TMRUFile> FMRUFiles;
		private StringList FNameFilters;
		private PedigreeOptions FPedigreeOptions;
		private bool FPlacesWithAddress;
		private ProxyOptions FProxy;
		private StringList FRelations;
		private StringList FResidenceFilters;
		private bool FShowTips;
		private TIndividualListColumns FIndividualListColumns;
		private bool FListPersons_HighlightUnmarried;
		private bool FListPersons_HighlightUnparented;
		private TRect FMWinRect;
		private FormWindowState FMWinState;
		private StringList FLastBases;
		private bool FRevisionsBackup;

		protected bool Disposed_;


		public AncestorsCircleOptions AncCircleOptions
		{
			get { return this.FAncCircleOptions; }
		}

		public TreeChartOptions ChartOptions
		{
			get { return this.FChartOptions; }
		}

		public TGEDCOMCharacterSet DefCharacterSet
		{
			get { return this.FDefCharacterSet; }
			set { this.FDefCharacterSet = value; }
		}

		public TGenEngine.TDateFormat DefDateFormat
		{
			get { return this.FDefDateFormat; }
			set { this.FDefDateFormat = value; }
		}

		public TGenEngine.TNameFormat DefNameFormat
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

		/*public TGlobalOptions.TLangRecord Langs
		{
			get { return this.GetLang(Index); }
		}*/

		public int LangsCount
		{
			get { return this.GetLangsCount(); }
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

		public TRect MWinRect
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

		/*
		public TGlobalOptions.TBaseWin LastBases
		{
			get
			{
				return this.GetLastBase(Index);
			}
		}*/

		public int LastBasesCount
		{
			get { return this.GetLastBasesCount(); }
		}

		private ushort GetKeyLayout()
		{
			return unchecked((ushort)SysUtils.GetKeyboardLayout(0u));
		}

		private void SetKeyLayout(ushort aLayout)
		{
			SysUtils.ActivateKeyboardLayout((uint)aLayout, 0u);
		}

		private void LngPrepareProc([In] string FileName)
		{
			StreamReader lng_file = new StreamReader(FileName, Encoding.UTF8);
			try
			{
				string st = lng_file.ReadLine();
				if (st[0] == ';')
				{
					try
					{
						st = st.Remove(0, 1);
						string[] lng_params = st.Split(',');
						string lng_code = lng_params[0];
						string lng_name = lng_params[1];
						GlobalOptions.TLangRecord lng_rec = new GlobalOptions.TLangRecord();
						lng_rec.Code = (ushort)int.Parse(lng_code);
						lng_rec.Name = lng_name;
						lng_rec.FileName = FileName;
						this.FLanguages.Add(lng_rec);
					}
					catch (Exception E)
					{
						SysUtils.LogWrite("TGlobalOptions.LngPrepareProc(): " + E.Message);
						throw;
					}
				}
			}
			finally
			{
				lng_file.Close();
			}
		}

		public int GetLangsCount()
		{
			return this.FLanguages.Count;
		}

		public GlobalOptions.TLangRecord GetLang(int Index)
		{
			return this.FLanguages[Index] as GlobalOptions.TLangRecord;
		}

		public string GetLastBase(int Index)
		{
			return this.FLastBases[Index];
		}

		public int GetLastBasesCount()
		{
			return this.FLastBases.Count;
		}

		public int MRUFiles_IndexOf(string aFileName)
		{
			int result = -1;

			for (int i = 0; i <= FMRUFiles.Count - 1; i++)
			{
				if (this.FMRUFiles[i].FileName == aFileName)
				{
					result = i;
					break;
				}
			}

			return result;
		}

		public GlobalOptions()
		{
			this.FAncCircleOptions = new AncestorsCircleOptions();
			this.FChartOptions = new TreeChartOptions();
			this.FEventFilters = new StringList();
			this.FMRUFiles = new List<TMRUFile>();
			this.FNameFilters = new StringList();
			this.FResidenceFilters = new StringList();
			this.FPedigreeOptions = new PedigreeOptions();
			this.FProxy = new ProxyOptions();
			this.FRelations = new StringList();

			this.FIndividualListColumns = new TIndividualListColumns();
			this.FIndividualListColumns.SetDefaults();

			this.FLanguages = new TList(true);
			this.FLastBases = new StringList();
		}

		public void Dispose()
		{
			if (!this.Disposed_)
			{
				this.FLastBases.Free();
				this.FLanguages.Dispose();
				this.FRelations.Free();
				this.FProxy.Free();
				this.FPedigreeOptions.Free();
				this.FResidenceFilters.Free();
				this.FNameFilters.Free();
				//this.FMRUFiles.Dispose();
				this.FEventFilters.Free();
				this.FChartOptions.Dispose();
				this.Disposed_ = true;
			}
		}

		public void FindLanguages()
		{
			string path = TGenEngine.GetAppPath() + "langs\\";
			string[] lang_files = Directory.GetFiles(path, "*.lng", SearchOption.TopDirectoryOnly);
			for (int i = 0; i < lang_files.Length; i++) this.LngPrepareProc(lang_files[i]);
		}

		public void LoadFromFile([In] string FileName)
		{
			IniFile ini = new IniFile(FileName);
			try
			{
				this.FDefCharacterSet = (TGEDCOMCharacterSet)ini.ReadInteger("Common", "DefCharacterSet", 3);
				this.FDefNameFormat = (TGenEngine.TNameFormat)ini.ReadInteger("Common", "DefNameFormat", 0);
				this.FDefDateFormat = (TGenEngine.TDateFormat)ini.ReadInteger("Common", "DefDateFormat", 0);
				this.FLastDir = ini.ReadString("Common", "LastDir", "");
				this.FPlacesWithAddress = ini.ReadBool("Common", "PlacesWithAddress", false);
				this.FShowTips = ini.ReadBool("Common", "ShowTips", true);
				this.FInterfaceLang = (ushort)ini.ReadInteger("Common", "InterfaceLang", 1049);
				this.FRevisionsBackup = ini.ReadBool("Common", "RevisionsBackup", false);

				ushort kl = (ushort)ini.ReadInteger("Common", "KeyLayout", (int)this.GetKeyLayout());
				this.SetKeyLayout(kl);

				this.FChartOptions.LoadFromFile(ini);
				this.FPedigreeOptions.LoadFromFile(ini);
				this.FProxy.LoadFromFile(ini);

				int cnt = ini.ReadInteger("NameFilters", "Count", 0);
				for (int i = 0; i <= cnt - 1; i++)
				{
					this.FNameFilters.Add(ini.ReadString("NameFilters", "Filter_" + i.ToString(), ""));
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

		public void SaveToFile([In] string FileName)
		{
			IniFile ini = new IniFile(FileName);
			try
			{
				ini.WriteInteger("Common", "DefCharacterSet", (int)this.FDefCharacterSet);
				ini.WriteInteger("Common", "DefNameFormat", (int)this.FDefNameFormat);
				ini.WriteInteger("Common", "DefDateFormat", (int)this.FDefDateFormat);
				ini.WriteString("Common", "LastDir", this.FLastDir);
				ini.WriteBool("Common", "PlacesWithAddress", this.FPlacesWithAddress);
				ini.WriteBool("Common", "ShowTips", this.FShowTips);
				ini.WriteInteger("Common", "InterfaceLang", (int)this.FInterfaceLang);
				ini.WriteBool("Common", "RevisionsBackup", this.FRevisionsBackup);
				ini.WriteInteger("Common", "KeyLayout", (int)this.GetKeyLayout());

				this.FChartOptions.SaveToFile(ini);
				this.FPedigreeOptions.SaveToFile(ini);
				this.FProxy.SaveToFile(ini);

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

		public void Free()
		{
			SysUtils.Free(this);
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
