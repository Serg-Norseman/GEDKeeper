using GedCom551;
using GKCore;
using GKSys;
using System;
using System.ComponentModel;
using System.IO;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Text;
using System.Windows.Forms;

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

		[StructLayout(LayoutKind.Auto)]
		public struct TPersonColumnProps
		{
			public TGlobalOptions.TPersonColumnType colType;
			public byte colSubType;
			public bool colActive;
		}

		[StructLayout(LayoutKind.Auto)]
		public struct TColumnRec
		{
			public LSID Name;
			public int DefWidth;
			public bool colOnlyMain;
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
		internal TChartOptions FChartOptions;
		internal TGEDCOMObject.TGEDCOMCharacterSet FDefCharacterSet;
		internal TGenEngine.TDateFormat FDefDateFormat;
		internal TGenEngine.TNameFormat FDefNameFormat;
		internal TStringList FEventFilters;
		internal ushort FInterfaceLang;
		internal TObjectList FLanguages;
		internal string FLastDir;
		internal TStringList FMRUFiles;
		internal TStringList FNameFilters;
		internal TPedigreeOptions FPedigreeOptions;
		internal bool FPlacesWithAddress;
		internal TProxy FProxy;
		internal TStringList FRelations;
		internal TStringList FResidenceFilters;
		internal bool FShowTips;
		internal TGlobalOptions.TWorkMode FWorkMode;
		internal TGlobalOptions.TPersonColumnProps[] FListPersonsColumns = new TGlobalOptions.TPersonColumnProps[24];
		internal bool FListPersons_HighlightUnmarried;
		internal bool FListPersons_HighlightUnparented;
		internal TRect FMWinRect;
		internal FormWindowState FMWinState;
		internal TObjectList FLastBases;
		protected internal bool Disposed_;

		[Browsable(false)]
		public TChartOptions ChartOptions
		{
			get
			{
				return this.FChartOptions;
			}
		}
		[Browsable(false)]
		public TGEDCOMObject.TGEDCOMCharacterSet DefCharacterSet
		{
			get
			{
				return this.FDefCharacterSet;
			}
			set
			{
				this.FDefCharacterSet = value;
			}
		}
		[Browsable(false)]
		public TGenEngine.TDateFormat DefDateFormat
		{
			get
			{
				return this.FDefDateFormat;
			}
			set
			{
				this.FDefDateFormat = value;
			}
		}
		[Browsable(false)]
		public TGenEngine.TNameFormat DefNameFormat
		{
			get
			{
				return this.FDefNameFormat;
			}
			set
			{
				this.FDefNameFormat = value;
			}
		}
		[Browsable(false)]
		public TStringList EventFilters
		{
			get
			{
				return this.FEventFilters;
			}
		}
		[Browsable(false)]
		public ushort InterfaceLang
		{
			get
			{
				return this.FInterfaceLang;
			}
			set
			{
				this.FInterfaceLang = value;
			}
		}
		/*[Browsable(false)]
		public TGlobalOptions.TLangRecord Langs
		{
			get
			{
				return this.GetLang(Index);
			}
		}*/
		[Browsable(false)]
		public int LangsCount
		{
			get
			{
				return this.GetLangsCount();
			}
		}
		[Browsable(false)]
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
		[Browsable(false)]
		public TStringList MRUFiles
		{
			get
			{
				return this.FMRUFiles;
			}
		}
		[Browsable(false)]
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
		[Browsable(false)]
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
		[Browsable(false)]
		public TStringList NameFilters
		{
			get
			{
				return this.FNameFilters;
			}
		}
		[Browsable(false)]
		public TPedigreeOptions PedigreeOptions
		{
			get
			{
				return this.FPedigreeOptions;
			}
		}
		[Browsable(false)]
		public bool PlacesWithAddress
		{
			get
			{
				return this.FPlacesWithAddress;
			}
			set
			{
				this.FPlacesWithAddress = value;
			}
		}
		[Browsable(false)]
		public TProxy Proxy
		{
			get
			{
				return this.FProxy;
			}
		}
		[Browsable(false)]
		public TStringList Relations
		{
			get
			{
				return this.FRelations;
			}
		}
		[Browsable(false)]
		public TStringList ResidenceFilters
		{
			get
			{
				return this.FResidenceFilters;
			}
		}
		[Browsable(false)]
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
		[Browsable(false)]
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
		[Browsable(false)]
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
		[Browsable(false)]
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
		[Browsable(false)]
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
		/*[Browsable(false)]
		public TGlobalOptions.TBaseWin LastBases
		{
			get
			{
				return this.GetLastBase(Index);
			}
		}*/
		[Browsable(false)]
		public int LastBasesCount
		{
			get
			{
				return this.GetLastBasesCount();
			}
		}
		internal ushort GetKeyLayout()
		{
			return (ushort)VCLUtils.GetKeyboardLayout(0u);
		}
		internal void SetKeyLayout(ushort aLayout)
		{
			VCLUtils.ActivateKeyboardLayout((uint)aLayout, 0u);
		}
		internal void LngPrepareProc([In] string FileName)
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
						string lng_code = TGKSys.GetToken(st, ',', 1);
						string lng_name = TGKSys.GetToken(st, ',', 2);
						TGlobalOptions.TLangRecord lng_rec = new TGlobalOptions.TLangRecord();
						lng_rec.Code = (ushort)int.Parse(lng_code);
						lng_rec.Name = lng_name;
						lng_rec.FileName = FileName;
						this.FLanguages.Add(lng_rec);
					}
					catch (Exception E)
					{
					}
				}
			}
			finally
			{
				lng_file.Close();
			}
		}
		internal int GetLangsCount()
		{
			return this.FLanguages.Count;
		}

		public TGlobalOptions.TLangRecord GetLang(int Index)
		{
			return this.FLanguages[Index] as TGlobalOptions.TLangRecord;
		}

		internal TGlobalOptions.TBaseWin GetLastBase(int Index)
		{
			return this.FLastBases[Index] as TGlobalOptions.TBaseWin;
		}
		internal int GetLastBasesCount()
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
			string path = TGKSys.GetAppPath() + "langs\\";
			TGKSys.ScanDir(path, new TGKSys.TFilePrepareProc(this.LngPrepareProc), false, 63, "*.lng");
		}
		public void LoadFromFile([In] string FileName)
		{
			TIniFile ini = new TIniFile(FileName);
			try
			{
				this.FDefCharacterSet = (TGEDCOMObject.TGEDCOMCharacterSet)ini.ReadInteger("Common", "DefCharacterSet", 3);
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
				int arg_12A_0 = 0;
				int num = cnt - 1;
				int i = arg_12A_0;
				if (num >= i)
				{
					num++;
					do
					{
						this.FNameFilters.Add(ini.ReadString("NameFilters", "Filter_" + i.ToString(), ""));
						i++;
					}
					while (i != num);
				}
				cnt = ini.ReadInteger("ResidenceFilters", "Count", 0);
				int arg_184_0 = 0;
				int num2 = cnt - 1;
				i = arg_184_0;
				if (num2 >= i)
				{
					num2++;
					do
					{
						this.FResidenceFilters.Add(ini.ReadString("ResidenceFilters", "Filter_" + i.ToString(), ""));
						i++;
					}
					while (i != num2);
				}
				cnt = ini.ReadInteger("EventFilters", "Count", 0);
				int arg_1DE_0 = 0;
				int num3 = cnt - 1;
				i = arg_1DE_0;
				if (num3 >= i)
				{
					num3++;
					do
					{
						this.FEventFilters.Add(ini.ReadString("EventFilters", "EventVal_" + i.ToString(), ""));
						i++;
					}
					while (i != num3);
				}
				cnt = ini.ReadInteger("MRUFiles", "Count", 0);
				int arg_238_0 = 0;
				int num4 = cnt - 1;
				i = arg_238_0;
				if (num4 >= i)
				{
					num4++;
					do
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
						i++;
					}
					while (i != num4);
				}
				cnt = ini.ReadInteger("Relations", "Count", 0);
				int arg_2BD_0 = 0;
				int num5 = cnt - 1;
				i = arg_2BD_0;
				if (num5 >= i)
				{
					num5++;
					do
					{
						this.FRelations.Add(ini.ReadString("Relations", "Relation_" + i.ToString(), ""));
						i++;
					}
					while (i != num5);
				}
				i = 0;
				do
				{
					this.FListPersonsColumns[i].colType = (TGlobalOptions.TPersonColumnType)ini.ReadInteger("PersonsColumns", "ColType_" + i.ToString(), (int)((sbyte)TGlobalOptions.DefPersonColumns[i].colType));
					this.FListPersonsColumns[i].colActive = ini.ReadBool("PersonsColumns", "ColActive_" + i.ToString(), TGlobalOptions.DefPersonColumns[i].colActive);
					i++;
				}
				while (i != 24);
				this.FListPersons_HighlightUnmarried = ini.ReadBool("ListPersons", "HighlightUnmarried", false);
				this.FListPersons_HighlightUnparented = ini.ReadBool("ListPersons", "HighlightUnparented", false);
				this.FMWinRect.Left = ini.ReadInteger("Common", "MWinL", -1);
				this.FMWinRect.Top = ini.ReadInteger("Common", "MWinT", -1);
				this.FMWinRect.Right = ini.ReadInteger("Common", "MWinW", -1);
				this.FMWinRect.Bottom = ini.ReadInteger("Common", "MWinH", -1);
				this.FMWinState = (FormWindowState)((uint)ini.ReadInteger("Common", "MWinState", 0));
				cnt = ini.ReadInteger("LastBases", "Count", 0);
				int arg_458_0 = 0;
				int num6 = cnt - 1;
				i = arg_458_0;
				if (num6 >= i)
				{
					num6++;
					do
					{
						TGlobalOptions.TBaseWin lb = this.AddLastBase();
						string st = "B" + i.ToString() + "_";
						lb.FileName = ini.ReadString("LastBases", st + "FileName", "");
						lb.WinRect.Left = ini.ReadInteger("LastBases", st + "WinL", 10);
						lb.WinRect.Top = ini.ReadInteger("LastBases", st + "WinT", 10);
						lb.WinRect.Right = ini.ReadInteger("LastBases", st + "WinW", 778);
						lb.WinRect.Bottom = ini.ReadInteger("LastBases", st + "WinH", 312);
						lb.WinState = (FormWindowState)((uint)ini.ReadInteger("LastBases", st + "WinState", 0));
						i++;
					}
					while (i != num6);
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
				ini.WriteInteger("Common", "DefCharacterSet", (int)((sbyte)this.FDefCharacterSet));
				ini.WriteInteger("Common", "DefNameFormat", (int)((sbyte)this.FDefNameFormat));
				ini.WriteInteger("Common", "DefDateFormat", (int)((sbyte)this.FDefDateFormat));
				ini.WriteString("Common", "LastDir", this.FLastDir);
				ini.WriteBool("Common", "PlacesWithAddress", this.FPlacesWithAddress);
				ini.WriteBool("Common", "ShowTips", this.FShowTips);
				ini.WriteInteger("Common", "WorkMode", (int)((sbyte)this.FWorkMode));
				ini.WriteInteger("Common", "InterfaceLang", (int)this.FInterfaceLang);
				ini.WriteInteger("Common", "KeyLayout", (int)this.GetKeyLayout());
				this.FChartOptions.SaveToFile(ini);
				this.FPedigreeOptions.SaveToFile(ini);
				this.FProxy.SaveToFile(ini);
				ini.WriteInteger("NameFilters", "Count", this.FNameFilters.Count);
				int arg_125_0 = 0;
				int num = this.FNameFilters.Count - 1;
				int i = arg_125_0;
				if (num >= i)
				{
					num++;
					do
					{
						ini.WriteString("NameFilters", "Filter_" + i.ToString(), this.FNameFilters[i]);
						i++;
					}
					while (i != num);
				}
				ini.WriteInteger("ResidenceFilters", "Count", this.FResidenceFilters.Count);
				int arg_189_0 = 0;
				int num2 = this.FResidenceFilters.Count - 1;
				i = arg_189_0;
				if (num2 >= i)
				{
					num2++;
					do
					{
						ini.WriteString("ResidenceFilters", "Filter_" + i.ToString(), this.FResidenceFilters[i]);
						i++;
					}
					while (i != num2);
				}
				ini.WriteInteger("EventFilters", "Count", this.FEventFilters.Count);
				int arg_1F1_0 = 0;
				int num3 = this.FEventFilters.Count - 1;
				i = arg_1F1_0;
				if (num3 >= i)
				{
					num3++;
					do
					{
						ini.WriteString("EventFilters", "EventVal_" + i.ToString(), this.FEventFilters[i]);
						i++;
					}
					while (i != num3);
				}
				ini.WriteInteger("MRUFiles", "Count", this.FMRUFiles.Count);
				int arg_259_0 = 0;
				int num4 = this.FMRUFiles.Count - 1;
				i = arg_259_0;
				if (num4 >= i)
				{
					num4++;
					do
					{
						ini.WriteString("MRUFiles", "File_" + i.ToString(), this.FMRUFiles[i]);
						i++;
					}
					while (i != num4);
				}
				this.FMRUFiles.Sort();
				ini.WriteInteger("Relations", "Count", this.FRelations.Count);
				int arg_2CC_0 = 0;
				int num5 = this.FRelations.Count - 1;
				i = arg_2CC_0;
				if (num5 >= i)
				{
					num5++;
					do
					{
						ini.WriteString("Relations", "Relation_" + i.ToString(), this.FRelations[i]);
						i++;
					}
					while (i != num5);
				}
				i = 0;
				do
				{
					ini.WriteInteger("PersonsColumns", "ColType_" + i.ToString(), (int)((sbyte)this.FListPersonsColumns[i].colType));
					ini.WriteBool("PersonsColumns", "ColActive_" + i.ToString(), this.FListPersonsColumns[i].colActive);
					i++;
				}
				while (i != 24);
				ini.WriteBool("ListPersons", "HighlightUnmarried", this.FListPersons_HighlightUnmarried);
				ini.WriteBool("ListPersons", "HighlightUnparented", this.FListPersons_HighlightUnparented);
				ini.WriteInteger("Common", "MWinL", this.FMWinRect.Left);
				ini.WriteInteger("Common", "MWinT", this.FMWinRect.Top);
				ini.WriteInteger("Common", "MWinW", this.FMWinRect.Right);
				ini.WriteInteger("Common", "MWinH", this.FMWinRect.Bottom);
				ini.WriteInteger("Common", "MWinState", (int)this.FMWinState);
				ini.WriteInteger("LastBases", "Count", this.FLastBases.Count);
				int arg_44A_0 = 0;
				int num6 = this.FLastBases.Count - 1;
				i = arg_44A_0;
				if (num6 >= i)
				{
					num6++;
					do
					{
						TGlobalOptions.TBaseWin lb = this.GetLastBase(i);
						string st = "B" + i.ToString() + "_";
						ini.WriteString("LastBases", st + "FileName", lb.FileName);
						ini.WriteInteger("LastBases", st + "WinL", lb.WinRect.Left);
						ini.WriteInteger("LastBases", st + "WinT", lb.WinRect.Top);
						ini.WriteInteger("LastBases", st + "WinW", lb.WinRect.Right);
						ini.WriteInteger("LastBases", st + "WinH", lb.WinRect.Bottom);
						ini.WriteInteger("LastBases", st + "WinState", (int)lb.WinState);
						i++;
					}
					while (i != num6);
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
			TGlobalOptions.TPersonColumnProps[] dpc = new TGlobalOptions.TPersonColumnProps[24];
			TGlobalOptions.TPersonColumnProps pcp = new TGlobalOptions.TPersonColumnProps();

			pcp.colType = TGlobalOptions.TPersonColumnType.pctPatriarch;
			pcp.colActive = true;
			dpc[0] = pcp;

			pcp.colType = TGlobalOptions.TPersonColumnType.pctName;
			pcp.colActive = true;
			dpc[1] = pcp;

			pcp.colType = TGlobalOptions.TPersonColumnType.pctNick;
			pcp.colActive = false;
			dpc[2] = pcp;

			pcp.colType = TGlobalOptions.TPersonColumnType.pctSex;
			pcp.colActive = true;
			dpc[3] = pcp;

			pcp.colType = TGlobalOptions.TPersonColumnType.pctBirthDate;
			pcp.colActive = true;
			dpc[4] = pcp;

			pcp.colType = TGlobalOptions.TPersonColumnType.pctDeathDate;
			pcp.colActive = true;
			dpc[5] = pcp;

			pcp.colType = TGlobalOptions.TPersonColumnType.pctBirthPlace;
			pcp.colActive = true;
			dpc[6] = pcp;

			pcp.colType = TGlobalOptions.TPersonColumnType.pctDeathPlace;
			pcp.colActive = true;
			dpc[7] = pcp;

			pcp.colType = TGlobalOptions.TPersonColumnType.pctResidence;
			pcp.colActive = true;
			dpc[8] = pcp;

			pcp.colType = TGlobalOptions.TPersonColumnType.pctAge;
			pcp.colActive = true;
			dpc[9] = pcp;

			pcp.colType = TGlobalOptions.TPersonColumnType.pctLifeExpectancy;
			pcp.colActive = true;
			dpc[10] = pcp;

			pcp.colType = TGlobalOptions.TPersonColumnType.pctDaysForBirth;
			pcp.colActive = true;
			dpc[11] = pcp;

			pcp.colType = TGlobalOptions.TPersonColumnType.pctGroups;
			pcp.colActive = true;
			dpc[12] = pcp;

			pcp.colType = TGlobalOptions.TPersonColumnType.pctReligion;
			pcp.colActive = false;
			dpc[13] = pcp;

			pcp.colType = TGlobalOptions.TPersonColumnType.pctNationality;
			pcp.colActive = false;
			dpc[14] = pcp;

			pcp.colType = TGlobalOptions.TPersonColumnType.pctEducation;
			pcp.colActive = false;
			dpc[15] = pcp;

			pcp.colType = TGlobalOptions.TPersonColumnType.pctOccupation;
			pcp.colActive = false;
			dpc[16] = pcp;

			pcp.colType = TGlobalOptions.TPersonColumnType.pctCaste;
			pcp.colActive = false;
			dpc[17] = pcp;

			pcp.colType = TGlobalOptions.TPersonColumnType.pctMili;
			pcp.colActive = false;
			dpc[18] = pcp;

			pcp.colType = TGlobalOptions.TPersonColumnType.pctMiliInd;
			pcp.colActive = false;
			dpc[19] = pcp;

			pcp.colType = TGlobalOptions.TPersonColumnType.pctMiliDis;
			pcp.colActive = false;
			dpc[20] = pcp;

			pcp.colType = TGlobalOptions.TPersonColumnType.pctMiliRank;
			pcp.colActive = false;
			dpc[21] = pcp;

			pcp.colType = TGlobalOptions.TPersonColumnType.pctChangeDate;
			pcp.colActive = true;
			dpc[22] = pcp;

			pcp.colType = TGlobalOptions.TPersonColumnType.pctBookmark;
			pcp.colActive = true;
			dpc[23] = pcp;
			TGlobalOptions.DefPersonColumns = dpc;

			TGlobalOptions.TColumnRec[] array2 = new TGlobalOptions.TColumnRec[24];
			TGlobalOptions.TColumnRec[] arg_349_0_cp_0 = array2;
			int arg_349_0_cp_1 = 0;
			TGlobalOptions.TColumnRec tColumnRec;
			tColumnRec.Name = LSID.LSID_Patriarch;
			tColumnRec.DefWidth = 25;
			tColumnRec.colOnlyMain = false;
			arg_349_0_cp_0[arg_349_0_cp_1] = tColumnRec;
			TGlobalOptions.TColumnRec[] arg_375_0_cp_0 = array2;
			int arg_375_0_cp_1 = 1;
			TGlobalOptions.TColumnRec tColumnRec2;
			tColumnRec2.Name = LSID.LSID_FullName;
			tColumnRec2.DefWidth = 25;
			tColumnRec2.colOnlyMain = false;
			arg_375_0_cp_0[arg_375_0_cp_1] = tColumnRec2;
			TGlobalOptions.TColumnRec[] arg_39E_0_cp_0 = array2;
			int arg_39E_0_cp_1 = 2;
			TGlobalOptions.TColumnRec tColumnRec3;
			tColumnRec3.Name = LSID.LSID_Nickname;
			tColumnRec3.DefWidth = 75;
			tColumnRec3.colOnlyMain = false;
			arg_39E_0_cp_0[arg_39E_0_cp_1] = tColumnRec3;
			TGlobalOptions.TColumnRec[] arg_3C7_0_cp_0 = array2;
			int arg_3C7_0_cp_1 = 3;
			TGlobalOptions.TColumnRec tColumnRec4;
			tColumnRec4.Name = LSID.LSID_Sex;
			tColumnRec4.DefWidth = 45;
			tColumnRec4.colOnlyMain = false;
			arg_3C7_0_cp_0[arg_3C7_0_cp_1] = tColumnRec4;
			TGlobalOptions.TColumnRec[] arg_3F0_0_cp_0 = array2;
			int arg_3F0_0_cp_1 = 4;
			TGlobalOptions.TColumnRec tColumnRec5;
			tColumnRec5.Name = LSID.LSID_BirthDate;
			tColumnRec5.DefWidth = 100;
			tColumnRec5.colOnlyMain = false;
			arg_3F0_0_cp_0[arg_3F0_0_cp_1] = tColumnRec5;
			TGlobalOptions.TColumnRec[] arg_419_0_cp_0 = array2;
			int arg_419_0_cp_1 = 5;
			TGlobalOptions.TColumnRec tColumnRec6;
			tColumnRec6.Name = LSID.LSID_DeathDate;
			tColumnRec6.DefWidth = 100;
			tColumnRec6.colOnlyMain = false;
			arg_419_0_cp_0[arg_419_0_cp_1] = tColumnRec6;
			TGlobalOptions.TColumnRec[] arg_445_0_cp_0 = array2;
			int arg_445_0_cp_1 = 6;
			TGlobalOptions.TColumnRec tColumnRec7;
			tColumnRec7.Name = LSID.LSID_BirthPlace;
			tColumnRec7.DefWidth = 100;
			tColumnRec7.colOnlyMain = false;
			arg_445_0_cp_0[arg_445_0_cp_1] = tColumnRec7;
			TGlobalOptions.TColumnRec[] arg_471_0_cp_0 = array2;
			int arg_471_0_cp_1 = 7;
			TGlobalOptions.TColumnRec tColumnRec8;
			tColumnRec8.Name = LSID.LSID_DeathPlace;
			tColumnRec8.DefWidth = 100;
			tColumnRec8.colOnlyMain = false;
			arg_471_0_cp_0[arg_471_0_cp_1] = tColumnRec8;
			TGlobalOptions.TColumnRec[] arg_49D_0_cp_0 = array2;
			int arg_49D_0_cp_1 = 8;
			TGlobalOptions.TColumnRec tColumnRec9;
			tColumnRec9.Name = LSID.LSID_Residence;
			tColumnRec9.DefWidth = 100;
			tColumnRec9.colOnlyMain = false;
			arg_49D_0_cp_0[arg_49D_0_cp_1] = tColumnRec9;
			TGlobalOptions.TColumnRec[] arg_4CA_0_cp_0 = array2;
			int arg_4CA_0_cp_1 = 9;
			TGlobalOptions.TColumnRec tColumnRec10;
			tColumnRec10.Name = LSID.LSID_Age;
			tColumnRec10.DefWidth = 100;
			tColumnRec10.colOnlyMain = true;
			arg_4CA_0_cp_0[arg_4CA_0_cp_1] = tColumnRec10;
			TGlobalOptions.TColumnRec[] arg_4F7_0_cp_0 = array2;
			int arg_4F7_0_cp_1 = 10;
			TGlobalOptions.TColumnRec tColumnRec11;
			tColumnRec11.Name = LSID.LSID_LifeExpectancy;
			tColumnRec11.DefWidth = 100;
			tColumnRec11.colOnlyMain = true;
			arg_4F7_0_cp_0[arg_4F7_0_cp_1] = tColumnRec11;
			TGlobalOptions.TColumnRec[] arg_524_0_cp_0 = array2;
			int arg_524_0_cp_1 = 11;
			TGlobalOptions.TColumnRec tColumnRec12;
			tColumnRec12.Name = LSID.LSID_DaysForBirth;
			tColumnRec12.DefWidth = 100;
			tColumnRec12.colOnlyMain = true;
			arg_524_0_cp_0[arg_524_0_cp_1] = tColumnRec12;
			TGlobalOptions.TColumnRec[] arg_551_0_cp_0 = array2;
			int arg_551_0_cp_1 = 12;
			TGlobalOptions.TColumnRec tColumnRec13;
			tColumnRec13.Name = LSID.LSID_RPGroups;
			tColumnRec13.DefWidth = 200;
			tColumnRec13.colOnlyMain = true;
			arg_551_0_cp_0[arg_551_0_cp_1] = tColumnRec13;
			TGlobalOptions.TColumnRec[] arg_581_0_cp_0 = array2;
			int arg_581_0_cp_1 = 13;
			TGlobalOptions.TColumnRec tColumnRec14;
			tColumnRec14.Name = LSID.LSID_Religion;
			tColumnRec14.DefWidth = 200;
			tColumnRec14.colOnlyMain = true;
			arg_581_0_cp_0[arg_581_0_cp_1] = tColumnRec14;
			TGlobalOptions.TColumnRec[] arg_5B1_0_cp_0 = array2;
			int arg_5B1_0_cp_1 = 14;
			TGlobalOptions.TColumnRec tColumnRec15;
			tColumnRec15.Name = LSID.LSID_Nationality;
			tColumnRec15.DefWidth = 200;
			tColumnRec15.colOnlyMain = true;
			arg_5B1_0_cp_0[arg_5B1_0_cp_1] = tColumnRec15;
			TGlobalOptions.TColumnRec[] arg_5E1_0_cp_0 = array2;
			int arg_5E1_0_cp_1 = 15;
			TGlobalOptions.TColumnRec tColumnRec16;
			tColumnRec16.Name = LSID.LSID_Education;
			tColumnRec16.DefWidth = 200;
			tColumnRec16.colOnlyMain = true;
			arg_5E1_0_cp_0[arg_5E1_0_cp_1] = tColumnRec16;
			TGlobalOptions.TColumnRec[] arg_611_0_cp_0 = array2;
			int arg_611_0_cp_1 = 16;
			TGlobalOptions.TColumnRec tColumnRec17;
			tColumnRec17.Name = LSID.LSID_Occupation;
			tColumnRec17.DefWidth = 200;
			tColumnRec17.colOnlyMain = true;
			arg_611_0_cp_0[arg_611_0_cp_1] = tColumnRec17;
			TGlobalOptions.TColumnRec[] arg_641_0_cp_0 = array2;
			int arg_641_0_cp_1 = 17;
			TGlobalOptions.TColumnRec tColumnRec18;
			tColumnRec18.Name = LSID.LSID_Caste;
			tColumnRec18.DefWidth = 200;
			tColumnRec18.colOnlyMain = true;
			arg_641_0_cp_0[arg_641_0_cp_1] = tColumnRec18;
			TGlobalOptions.TColumnRec[] arg_671_0_cp_0 = array2;
			int arg_671_0_cp_1 = 18;
			TGlobalOptions.TColumnRec tColumnRec19;
			tColumnRec19.Name = LSID.LSID_Mili;
			tColumnRec19.DefWidth = 200;
			tColumnRec19.colOnlyMain = true;
			arg_671_0_cp_0[arg_671_0_cp_1] = tColumnRec19;
			TGlobalOptions.TColumnRec[] arg_6A1_0_cp_0 = array2;
			int arg_6A1_0_cp_1 = 19;
			TGlobalOptions.TColumnRec tColumnRec20;
			tColumnRec20.Name = LSID.LSID_MiliInd;
			tColumnRec20.DefWidth = 200;
			tColumnRec20.colOnlyMain = true;
			arg_6A1_0_cp_0[arg_6A1_0_cp_1] = tColumnRec20;
			TGlobalOptions.TColumnRec[] arg_6D1_0_cp_0 = array2;
			int arg_6D1_0_cp_1 = 20;
			TGlobalOptions.TColumnRec tColumnRec21;
			tColumnRec21.Name = LSID.LSID_MiliDis;
			tColumnRec21.DefWidth = 200;
			tColumnRec21.colOnlyMain = true;
			arg_6D1_0_cp_0[arg_6D1_0_cp_1] = tColumnRec21;
			TGlobalOptions.TColumnRec[] arg_701_0_cp_0 = array2;
			int arg_701_0_cp_1 = 21;
			TGlobalOptions.TColumnRec tColumnRec22;
			tColumnRec22.Name = LSID.LSID_MiliRank;
			tColumnRec22.DefWidth = 200;
			tColumnRec22.colOnlyMain = true;
			arg_701_0_cp_0[arg_701_0_cp_1] = tColumnRec22;
			TGlobalOptions.TColumnRec[] arg_731_0_cp_0 = array2;
			int arg_731_0_cp_1 = 22;
			TGlobalOptions.TColumnRec tColumnRec23;
			tColumnRec23.Name = LSID.LSID_Changed;
			tColumnRec23.DefWidth = 150;
			tColumnRec23.colOnlyMain = true;
			arg_731_0_cp_0[arg_731_0_cp_1] = tColumnRec23;
			TGlobalOptions.TColumnRec[] arg_75B_0_cp_0 = array2;
			int arg_75B_0_cp_1 = 23;
			TGlobalOptions.TColumnRec tColumnRec24;
			tColumnRec24.Name = LSID.LSID_Bookmark;
			tColumnRec24.DefWidth = 25;
			tColumnRec24.colOnlyMain = true;
			arg_75B_0_cp_0[arg_75B_0_cp_1] = tColumnRec24;
			TGlobalOptions.PersonColumnsName = array2;
		}
	}
}
