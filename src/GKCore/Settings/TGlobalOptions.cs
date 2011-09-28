using System;
using System.ComponentModel;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;
using System.Windows.Forms;

using GedCom551;
using GKCore.Sys;

namespace GKCore
{
	public class TGlobalOptions : IDisposable
	{
		public class TLangRecord
		{
			public ushort Code;
			public string Name;
			public string FileName;

			public void Free()
			{
				TObjectHelper.Free(this);
			}
		}

		public class TBaseWin
		{
			public string FileName;
			public TRect WinRect;
			public FormWindowState WinState;

			public void Free()
			{
				TObjectHelper.Free(this);
			}
		}

		public struct TPersonColumnProps
		{
			public TPersonColumnType colType;
			public byte colSubType;
			public bool colActive;

			public TPersonColumnProps(TPersonColumnType colType, bool colActive) {
				this.colType = colType;
				this.colSubType = 0;
				this.colActive = colActive;
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

		public enum TWorkMode : byte
		{
			wmSimple,
			wmExpert
		}

		public enum TPersonColumnType : byte
		{
			pctPatriarch,
			pctName,
			pctNick,
			pctSex,
			pctBirthDate,
			pctDeathDate,
			pctBirthPlace,
			pctDeathPlace,
			pctResidence,
			pctAge,
			pctLifeExpectancy,
			pctDaysForBirth,
			pctGroups,
			pctReligion,
			pctNationality,
			pctEducation,
			pctOccupation,
			pctCaste,
			pctMili,
			pctMiliInd,
			pctMiliDis,
			pctMiliRank,
			pctChangeDate,
			pctBookmark
		}

		public static readonly TGlobalOptions.TColumnRec[] PersonColumnsName;
		public static readonly TGlobalOptions.TPersonColumnProps[] DefPersonColumns;

		private TChartOptions FChartOptions;
		private TGEDCOMCharacterSet FDefCharacterSet;
		private TGenEngine.TDateFormat FDefDateFormat;
		private TGenEngine.TNameFormat FDefNameFormat;
		private TStringList FEventFilters;
		private ushort FInterfaceLang;
		private TObjectList FLanguages;
		private string FLastDir;
		private TStringList FMRUFiles;
		private TStringList FNameFilters;
		private TPedigreeOptions FPedigreeOptions;
		private bool FPlacesWithAddress;
		private TProxy FProxy;
		private TStringList FRelations;
		private TStringList FResidenceFilters;
		private bool FShowTips;
		private TGlobalOptions.TWorkMode FWorkMode;
		private TGlobalOptions.TPersonColumnProps[] FListPersonsColumns = new TGlobalOptions.TPersonColumnProps[24];
		private bool FListPersons_HighlightUnmarried;
		private bool FListPersons_HighlightUnparented;
		private TRect FMWinRect;
		private FormWindowState FMWinState;
		private TObjectList FLastBases;
		protected bool Disposed_;


		public TChartOptions ChartOptions
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

		public TStringList EventFilters
		{
			get { return this.FEventFilters; }
		}

		public ushort InterfaceLang
		{
			get { return this.FInterfaceLang; }
			set { this.FInterfaceLang = value; }
		}

		/*
		public TGlobalOptions.TLangRecord Langs
		{
			get
			{
				return this.GetLang(Index);
			}
		}*/

		public int LangsCount
		{
			get
			{
				return this.GetLangsCount();
			}
		}

		public string LastDir
		{
			get
			{
				return this.FLastDir;
			}
			set
			{
				this.FLastDir = value;
			}
		}

		public TStringList MRUFiles
		{
			get
			{
				return this.FMRUFiles;
			}
		}

		public TRect MWinRect
		{
			get
			{
				return this.FMWinRect;
			}
			set
			{
				this.FMWinRect = value;
			}
		}

		public FormWindowState MWinState
		{
			get
			{
				return this.FMWinState;
			}
			set
			{
				this.FMWinState = value;
			}
		}

		public TStringList NameFilters
		{
			get
			{
				return this.FNameFilters;
			}
		}

		public TPedigreeOptions PedigreeOptions
		{
			get
			{
				return this.FPedigreeOptions;
			}
		}

		public bool PlacesWithAddress
		{
			get { return this.FPlacesWithAddress; }
			set { this.FPlacesWithAddress = value; }
		}

		public TProxy Proxy
		{
			get
			{
				return this.FProxy;
			}
		}

		public TStringList Relations
		{
			get
			{
				return this.FRelations;
			}
		}

		public TStringList ResidenceFilters
		{
			get
			{
				return this.FResidenceFilters;
			}
		}

		public bool ShowTips
		{
			get
			{
				return this.FShowTips;
			}
			set
			{
				this.FShowTips = value;
			}
		}

		public TGlobalOptions.TWorkMode WorkMode
		{
			get
			{
				return this.FWorkMode;
			}
			set
			{
				this.FWorkMode = value;
			}
		}

		public bool ListPersons_HighlightUnmarried
		{
			get
			{
				return this.FListPersons_HighlightUnmarried;
			}
			set
			{
				this.FListPersons_HighlightUnmarried = value;
			}
		}

		public bool ListPersons_HighlightUnparented
		{
			get
			{
				return this.FListPersons_HighlightUnparented;
			}
			set
			{
				this.FListPersons_HighlightUnparented = value;
			}
		}

		public TGlobalOptions.TPersonColumnProps[] ListPersonsColumns
		{
			get
			{
				TGlobalOptions.TPersonColumnProps[] Result = new TGlobalOptions.TPersonColumnProps[24];
				Array.Copy(this.FListPersonsColumns, Result, 24);
				return Result;
			}
			set
			{
				Array.Copy(value, this.FListPersonsColumns, 24);
			}
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
			get
			{
				return this.GetLastBasesCount();
			}
		}

		private ushort GetKeyLayout()
		{
			return (ushort)SysUtils.GetKeyboardLayout(0u);
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
						TGlobalOptions.TLangRecord lng_rec = new TGlobalOptions.TLangRecord();
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

		public TGlobalOptions.TLangRecord GetLang(int Index)
		{
			return this.FLanguages[Index] as TGlobalOptions.TLangRecord;
		}

		public TGlobalOptions.TBaseWin GetLastBase(int Index)
		{
			return this.FLastBases[Index] as TGlobalOptions.TBaseWin;
		}

		public int GetLastBasesCount()
		{
			return this.FLastBases.Count;
		}

		public TGlobalOptions()
		{
			this.FChartOptions = new TChartOptions();
			this.FEventFilters = new TStringList();
			this.FMRUFiles = new TStringList();
			this.FNameFilters = new TStringList();
			this.FResidenceFilters = new TStringList();
			this.FPedigreeOptions = new TPedigreeOptions();
			this.FProxy = new TProxy();
			this.FRelations = new TStringList();
			Array.Copy(TGlobalOptions.DefPersonColumns, this.FListPersonsColumns, 24);
			this.FLanguages = new TObjectList(true);
			this.FLastBases = new TObjectList(true);
		}

		public void Dispose()
		{
			if (!this.Disposed_)
			{
				this.FLastBases.Free();
				this.FLanguages.Free();
				this.FRelations.Free();
				this.FProxy.Free();
				this.FPedigreeOptions.Free();
				this.FResidenceFilters.Free();
				this.FNameFilters.Free();
				this.FMRUFiles.Free();
				this.FEventFilters.Free();
				this.FChartOptions.Dispose();
				this.Disposed_ = true;
			}
		}

		public void FindLanguages()
		{
			string path = SysUtils.GetAppPath() + "langs\\";
			SysUtils.ScanDir(path, new SysUtils.TFilePrepareProc(this.LngPrepareProc), false, 63, "*.lng");
		}

		public void LoadFromFile([In] string FileName)
		{
			TIniFile ini = new TIniFile(FileName);
			try
			{
				this.FDefCharacterSet = (TGEDCOMCharacterSet)ini.ReadInteger("Common", "DefCharacterSet", 3);
				this.FDefNameFormat = (TGenEngine.TNameFormat)ini.ReadInteger("Common", "DefNameFormat", 0);
				this.FDefDateFormat = (TGenEngine.TDateFormat)ini.ReadInteger("Common", "DefDateFormat", 0);
				this.FLastDir = ini.ReadString("Common", "LastDir", "");
				this.FPlacesWithAddress = ini.ReadBool("Common", "PlacesWithAddress", false);
				this.FShowTips = ini.ReadBool("Common", "ShowTips", true);
				this.FWorkMode = (TGlobalOptions.TWorkMode)ini.ReadInteger("Common", "WorkMode", 0);
				this.FInterfaceLang = (ushort)ini.ReadInteger("Common", "InterfaceLang", 1049);
				ushort kl = (ushort)ini.ReadInteger("Common", "KeyLayout", (int)this.GetKeyLayout());
				this.SetKeyLayout(kl);
				this.FChartOptions.LoadFromFile(ini);
				this.FPedigreeOptions.LoadFromFile(ini);
				this.FProxy.LoadFromFile(ini);

				int cnt = ini.ReadInteger("NameFilters", "Count", 0);
				int num = cnt - 1;
				for (int i = 0; i <= num; i++)
				{
					this.FNameFilters.Add(ini.ReadString("NameFilters", "Filter_" + i.ToString(), ""));
				}

				cnt = ini.ReadInteger("ResidenceFilters", "Count", 0);
				int num2 = cnt - 1;
				for (int i = 0; i <= num2; i++)
				{
					this.FResidenceFilters.Add(ini.ReadString("ResidenceFilters", "Filter_" + i.ToString(), ""));
				}

				cnt = ini.ReadInteger("EventFilters", "Count", 0);
				int num3 = cnt - 1;
				for (int i = 0; i <= num3; i++)
				{
					this.FEventFilters.Add(ini.ReadString("EventFilters", "EventVal_" + i.ToString(), ""));
				}

				cnt = ini.ReadInteger("MRUFiles", "Count", 0);
				int num4 = cnt - 1;
				for (int i = 0; i <= num4; i++)
				{
					string fn = ini.ReadString("MRUFiles", "File_" + i.ToString(), "");
					if (File.Exists(fn))
					{
						this.FMRUFiles.Add(fn);
					}
					else
					{
						ini.DeleteKey("MRUFiles", "File_" + i.ToString());
					}
				}

				cnt = ini.ReadInteger("Relations", "Count", 0);
				int num5 = cnt - 1;
				for (int i = 0; i <= num5; i++)
				{
					this.FRelations.Add(ini.ReadString("Relations", "Relation_" + i.ToString(), ""));
				}

				for (int i = 0; i < 24; i++)
				{
					this.FListPersonsColumns[i].colType = (TGlobalOptions.TPersonColumnType)ini.ReadInteger("PersonsColumns", "ColType_" + i.ToString(), (int)((sbyte)TGlobalOptions.DefPersonColumns[i].colType));
					this.FListPersonsColumns[i].colActive = ini.ReadBool("PersonsColumns", "ColActive_" + i.ToString(), TGlobalOptions.DefPersonColumns[i].colActive);
				}

				this.FListPersons_HighlightUnmarried = ini.ReadBool("ListPersons", "HighlightUnmarried", false);
				this.FListPersons_HighlightUnparented = ini.ReadBool("ListPersons", "HighlightUnparented", false);
				this.FMWinRect.Left = ini.ReadInteger("Common", "MWinL", -1);
				this.FMWinRect.Top = ini.ReadInteger("Common", "MWinT", -1);
				this.FMWinRect.Right = ini.ReadInteger("Common", "MWinW", -1);
				this.FMWinRect.Bottom = ini.ReadInteger("Common", "MWinH", -1);
				this.FMWinState = (FormWindowState)((uint)ini.ReadInteger("Common", "MWinState", 0));

				cnt = ini.ReadInteger("LastBases", "Count", 0);
				int num6 = cnt - 1;
				for (int i = 0; i <= num6; i++)
				{
					TGlobalOptions.TBaseWin lb = this.AddLastBase();
					string st = "B" + i.ToString() + "_";
					lb.FileName = ini.ReadString("LastBases", st + "FileName", "");
					lb.WinRect.Left = ini.ReadInteger("LastBases", st + "WinL", 10);
					lb.WinRect.Top = ini.ReadInteger("LastBases", st + "WinT", 10);
					lb.WinRect.Right = ini.ReadInteger("LastBases", st + "WinW", 778);
					lb.WinRect.Bottom = ini.ReadInteger("LastBases", st + "WinH", 312);
					lb.WinState = (FormWindowState)((uint)ini.ReadInteger("LastBases", st + "WinState", 0));
				}
			}
			finally
			{
				ini.Dispose();
			}
		}

		public void SaveToFile([In] string FileName)
		{
			TIniFile ini = new TIniFile(FileName);
			try
			{
				ini.WriteInteger("Common", "DefCharacterSet", (int)this.FDefCharacterSet);
				ini.WriteInteger("Common", "DefNameFormat", (int)this.FDefNameFormat);
				ini.WriteInteger("Common", "DefDateFormat", (int)this.FDefDateFormat);
				ini.WriteString("Common", "LastDir", this.FLastDir);
				ini.WriteBool("Common", "PlacesWithAddress", this.FPlacesWithAddress);
				ini.WriteBool("Common", "ShowTips", this.FShowTips);
				ini.WriteInteger("Common", "WorkMode", (int)this.FWorkMode);
				ini.WriteInteger("Common", "InterfaceLang", (int)this.FInterfaceLang);
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

				ini.WriteInteger("MRUFiles", "Count", this.FMRUFiles.Count);
				int num4 = this.FMRUFiles.Count - 1;
				for (int i = 0; i <= num4; i++)
				{
					ini.WriteString("MRUFiles", "File_" + i.ToString(), this.FMRUFiles[i]);
				}
				this.FMRUFiles.Sort();

				ini.WriteInteger("Relations", "Count", this.FRelations.Count);
				int num5 = this.FRelations.Count - 1;
				for (int i = 0; i <= num5; i++)
				{
					ini.WriteString("Relations", "Relation_" + i.ToString(), this.FRelations[i]);
				}

				{ // temp scope
					int i = 0;
					do
					{
						ini.WriteInteger("PersonsColumns", "ColType_" + i.ToString(), (int)((sbyte)this.FListPersonsColumns[i].colType));
						ini.WriteBool("PersonsColumns", "ColActive_" + i.ToString(), this.FListPersonsColumns[i].colActive);
						i++;
					}
					while (i != 24);
				}

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
					TGlobalOptions.TBaseWin lb = this.GetLastBase(i);
					string st = "B" + i.ToString() + "_";
					ini.WriteString("LastBases", st + "FileName", lb.FileName);
					ini.WriteInteger("LastBases", st + "WinL", lb.WinRect.Left);
					ini.WriteInteger("LastBases", st + "WinT", lb.WinRect.Top);
					ini.WriteInteger("LastBases", st + "WinW", lb.WinRect.Right);
					ini.WriteInteger("LastBases", st + "WinH", lb.WinRect.Bottom);
					ini.WriteInteger("LastBases", st + "WinState", (int)lb.WinState);
				}
			}
			finally
			{
				ini.Dispose();
			}
		}

		public TGlobalOptions.TBaseWin AddLastBase()
		{
			TGlobalOptions.TBaseWin Result = new TGlobalOptions.TBaseWin();
			this.FLastBases.Add(Result);
			return Result;
		}

		public void ClearLastBases()
		{
			this.FLastBases.Clear();
		}

		public void Free()
		{
			TObjectHelper.Free(this);
		}

		static TGlobalOptions()
		{
			TGlobalOptions.TPersonColumnProps[] array1 = new TGlobalOptions.TPersonColumnProps[24];
			array1[0] = new TPersonColumnProps(TPersonColumnType.pctPatriarch, true);
			array1[1] = new TPersonColumnProps(TPersonColumnType.pctName, true);
			array1[2] = new TPersonColumnProps(TPersonColumnType.pctNick, false);
			array1[3] = new TPersonColumnProps(TPersonColumnType.pctSex, true);
			array1[4] = new TPersonColumnProps(TPersonColumnType.pctBirthDate, true);
			array1[5] = new TPersonColumnProps(TPersonColumnType.pctDeathDate, true);
			array1[6] = new TPersonColumnProps(TPersonColumnType.pctBirthPlace, true);
			array1[7] = new TPersonColumnProps(TPersonColumnType.pctDeathPlace, true);
			array1[8] = new TPersonColumnProps(TPersonColumnType.pctResidence, true);
			array1[9] = new TPersonColumnProps(TPersonColumnType.pctAge, true);
			array1[10] = new TPersonColumnProps(TPersonColumnType.pctLifeExpectancy, true);
			array1[11] = new TPersonColumnProps(TPersonColumnType.pctDaysForBirth, true);
			array1[12] = new TPersonColumnProps(TPersonColumnType.pctGroups, true);
			array1[13] = new TPersonColumnProps(TPersonColumnType.pctReligion, false);
			array1[14] = new TPersonColumnProps(TPersonColumnType.pctNationality, false);
			array1[15] = new TPersonColumnProps(TPersonColumnType.pctEducation, false);
			array1[16] = new TPersonColumnProps(TPersonColumnType.pctOccupation, false);
			array1[17] = new TPersonColumnProps(TPersonColumnType.pctCaste, false);
			array1[18] = new TPersonColumnProps(TPersonColumnType.pctMili, false);
			array1[19] = new TPersonColumnProps(TPersonColumnType.pctMiliInd, false);
			array1[20] = new TPersonColumnProps(TPersonColumnType.pctMiliDis, false);
			array1[21] = new TPersonColumnProps(TPersonColumnType.pctMiliRank, false);
			array1[22] = new TPersonColumnProps(TPersonColumnType.pctChangeDate, true);
			array1[23] = new TPersonColumnProps(TPersonColumnType.pctBookmark, true);
			TGlobalOptions.DefPersonColumns = array1;

			TGlobalOptions.TColumnRec[] array2 = new TGlobalOptions.TColumnRec[24];
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
			TGlobalOptions.PersonColumnsName = array2;
		}
	}
}
