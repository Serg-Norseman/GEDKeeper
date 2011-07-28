using GKSys;
using System;
using System.ComponentModel;
using System.Globalization;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Threading;

namespace GedCom551
{
	public class TGEDCOMDate : TGEDCOMCustomDate
	{

		public enum TGEDCOMCalendar : byte
		{
			dcGregorian,
			dcJulian,
			dcHebrew,
			dcFrench,
			dcRoman,
			dcUnknown
		}
		public enum TGEDCOMDateFormat : byte
		{
			dfGEDCOMStd,
			dfSystem
		}
		public static readonly string[] GEDCOMDateEscapeArray;
		public static readonly string[] GEDCOMMonthRusArray;
		public static readonly string[] GEDCOMMonthSysArray;
		public static readonly string[] GEDCOMMonthArray;
		public static readonly string[] GEDCOMMonthFrenchArray;
		public static readonly string[] GEDCOMMonthHebrewArray;
		internal TGEDCOMDate.TGEDCOMCalendar FDateCalendar;
		internal TGEDCOMDate.TGEDCOMDateFormat FDateFormat;
		internal ushort FDay;
		internal string FMonth;
		internal int FYear;
		internal bool FYearBC;
		internal string FYearModifier;
		[Browsable(false)]
		public new DateTime Date
		{
			get
			{
				return this.GetDateTime();
			}
			set
			{
				this.SetDateTime(value);
			}
		}
		[Browsable(false)]
		public TGEDCOMDate.TGEDCOMCalendar DateCalendar
		{
			get
			{
				return this.FDateCalendar;
			}
		}
		[Browsable(false)]
		public int Year
		{
			get
			{
				return this.FYear;
			}
			set
			{
				this.FYear = value;
			}
		}
		[Browsable(false)]
		public bool YearBC
		{
			get
			{
				return this.FYearBC;
			}
			set
			{
				this.FYearBC = value;
			}
		}
		[Browsable(false)]
		public string YearModifier
		{
			get
			{
				return this.FYearModifier;
			}
			set
			{
				this.FYearModifier = value;
			}
		}
		[Browsable(false)]
		public string Month
		{
			get
			{
				return this.GetMonth();
			}
			set
			{
				this.SetMonth(value);
			}
		}
		[Browsable(false)]
		public ushort Day
		{
			get
			{
				return this.FDay;
			}
			set
			{
				this.FDay = value;
			}
		}

		internal string GetMonth()
		{
			return this.FMonth;
		}

		internal void SetMonth([In] string Value)
		{
			this.FMonth = Value;
		}

		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FDateCalendar = TGEDCOMDate.TGEDCOMCalendar.dcGregorian;
			this.FYear = -1;
			this.FYearBC = false;
			this.FYearModifier = "";
			this.FMonth = "";
			this.FDay = 0;
			base.Name = "DATE";
			this.FDateFormat = TGEDCOMDate.TGEDCOMDateFormat.dfGEDCOMStd;
		}

		protected internal string DayString(bool NoDelimiter)
		{
			string Result;
			if (this.FDay <= 0)
			{
				Result = "";
			}
			else
			{
				Result = this.FDay.ToString();
				if (((Result != null) ? Result.Length : 0) == 1)
				{
					Result = "0" + Result;
				}
				if (!NoDelimiter)
				{
					Result += " ";
				}
			}
			return Result;
		}
		protected internal string EscapeString(bool NoDelimiter, bool AllwaysShowEscape)
		{
			string Result;
			if (AllwaysShowEscape || this.FDateCalendar != TGEDCOMDate.TGEDCOMCalendar.dcGregorian)
			{
				Result = TGEDCOMDate.GEDCOMDateEscapeArray[(int)this.FDateCalendar];
				if (!NoDelimiter)
				{
					Result += " ";
				}
			}
			else
			{
				Result = "";
			}
			return Result;
		}
		protected internal string MonthString(bool NoDelimiter)
		{
			string Result;
			if (BDSSystem.WStrCmp(this.FMonth, "") == 0)
			{
				Result = "";
			}
			else
			{
				Result = this.FMonth;
				if (!NoDelimiter)
				{
					Result += " ";
				}
			}
			return Result;
		}
		protected internal string YearGregString(bool NoDelimiter)
		{
			string Result;
			if (this.FYear == -1)
			{
				Result = "";
			}
			else
			{
				Result = this.FYear.ToString();
				if (BDSSystem.WStrCmp(this.FYearModifier, "") != 0)
				{
					Result = Result + "/" + this.FYearModifier;
				}
				if (this.FYearBC)
				{
					Result += "B.C.";
				}
				if (!NoDelimiter)
				{
					Result += " ";
				}
			}
			return Result;
		}
		protected internal string YearString(bool NoDelimiter)
		{
			string Result;
			if (this.FYear == -1)
			{
				Result = "";
			}
			else
			{
				Result = this.FYear.ToString();
				if (this.FYearBC)
				{
					Result += "B.C.";
				}
				if (!NoDelimiter)
				{
					Result += " ";
				}
			}
			return Result;
		}
		protected internal string ExtractEscape([In] string S)
		{
			string Result = S;
			if (BDSSystem.WStrCmp(BDSSystem.WStrCopy(Result, 1, 2), "@#") == 0)
			{
				int P = BDSSystem.Pos("@", BDSSystem.WStrCopy(Result, 3, 2147483647));
				if (P > 0)
				{
					string SU = BDSSystem.WStrCopy(Result, 1, P + 2);
					TGEDCOMDate.TGEDCOMCalendar I = TGEDCOMDate.TGEDCOMCalendar.dcGregorian;
					while (BDSSystem.WStrCmp(TGEDCOMDate.GEDCOMDateEscapeArray[(int)I], SU) != 0)
					{
						I++;
						if (I == (TGEDCOMDate.TGEDCOMCalendar)6)
						{
							return Result;
						}
					}
					this.FDateCalendar = I;
					int num = (SU != null) ? SU.Length : 0;
					Result = Result.Remove(0, num);
				}
			}
			return Result;
		}
		protected internal string ExtractDay([In] string S)
		{
			string Result = S;
			int I = 0;
			while (I < ((Result != null) ? Result.Length : 0) && TGKSys.IsDigit(Result[I + 1 - 1]))
			{
				I++;
			}
			if (I >= 1 && I <= 2)
			{
				this.FDay = (ushort)int.Parse(BDSSystem.WStrCopy(Result, 1, I));
				Result = Result.Remove(0, I);
			}
			return Result;
		}
		protected internal string ExtractDelimiterEx([In] string S)
		{
			string Result;
			if (this.FDateFormat == TGEDCOMDate.TGEDCOMDateFormat.dfSystem)
			{
				Result = base.ExtractDotDelimiter(S, 0);
			}
			else
			{
				Result = base.ExtractDelimiter(S, 0);
			}
			return Result;
		}
		protected internal string ExtractMonth([In] string S)
		{
			DateTimeFormatInfo DateTimeInfo = Thread.CurrentThread.CurrentCulture.DateTimeFormat;
			string Result = S;
			if (BDSSystem.WStrCmp(Result, "") != 0)
			{
				TGEDCOMDate.TGEDCOMCalendar fDateCalendar = this.FDateCalendar;
				if (fDateCalendar != TGEDCOMDate.TGEDCOMCalendar.dcHebrew)
				{
					if (fDateCalendar == TGEDCOMDate.TGEDCOMCalendar.dcFrench)
					{
						string SU = BDSSystem.WStrCopy(Result, 1, 4).ToUpper();
						int I = 1;
						while (BDSSystem.WStrCmp(TGEDCOMDate.GEDCOMMonthFrenchArray[I - 1], SU) != 0)
						{
							I++;
							if (I == 14)
							{
								return Result;
							}
						}
						this.FMonth = SU;
						Result = Result.Remove(0, 4);
					}
					else
					{
						if (!TGKSys.IsDigit(Result[0]))
						{
							string SU = BDSSystem.WStrCopy(Result, 1, 3).ToUpper();
							int I = 1;
							while (BDSSystem.WStrCmp(TGEDCOMDate.GEDCOMMonthArray[I - 1], SU) != 0 && BDSSystem.WStrCmp(DateTimeInfo.AbbreviatedMonthNames[I - 1].ToUpper(), SU) != 0)
							{
								I++;
								if (I == 13)
								{
									return Result;
								}
							}
							this.FMonth = TGEDCOMDate.GEDCOMMonthArray[I - 1];
							Result = Result.Remove(0, 3);
						}
						else
						{
							string SU = BDSSystem.WStrCopy(Result, 1, 3).ToUpper();
							int I = 1;
							while (BDSSystem.WStrCmp(TGEDCOMDate.GEDCOMMonthSysArray[I - 1], SU) != 0)
							{
								I++;
								if (I == 13)
								{
									return Result;
								}
							}
							this.FMonth = TGEDCOMDate.GEDCOMMonthArray[I - 1];
							Result = Result.Remove(0, 2);
						}
					}
				}
				else
				{
					string SU = BDSSystem.WStrCopy(Result, 1, 3).ToUpper();
					int I = 1;
					while (BDSSystem.WStrCmp(TGEDCOMDate.GEDCOMMonthHebrewArray[I - 1], SU) != 0)
					{
						I++;
						if (I == 14)
						{
							return Result;
						}
					}
					this.FMonth = SU;
					Result = Result.Remove(0, 3);
				}
			}
			return Result;
		}
		protected internal string ExtractYear([In] string S)
		{
			string Result = S;
			int I = 0;
			while (I < ((Result != null) ? Result.Length : 0) && TGKSys.IsDigit(Result[I + 1 - 1]))
			{
				I++;
			}
			if (I > 0)
			{
				this.FYear = int.Parse(BDSSystem.WStrCopy(Result, 1, I));
				Result = Result.Remove(0, I);
				if (BDSSystem.WStrCmp(BDSSystem.WStrCopy(Result, 1, 1), "/") == 0 && TGKSys.IsDigits(BDSSystem.WStrCopy(Result, 2, 2)))
				{
					this.FYearModifier = BDSSystem.WStrCopy(Result, 2, 2);
					Result = Result.Remove(0, 3);
				}
				if (BDSSystem.WStrCmp(BDSSystem.WStrCopy(Result, 1, 4).ToUpper(), "B.C.") == 0)
				{
					this.FYearBC = true;
					Result = Result.Remove(0, 4);
				}
			}
			return Result;
		}
		protected internal override string GetStringValue()
		{
			string Result;
			if (this.FDateCalendar == TGEDCOMDate.TGEDCOMCalendar.dcGregorian)
			{
				Result = this.EscapeString(false, false) + this.DayString(false) + this.MonthString(false) + this.YearGregString(true);
			}
			else
			{
				Result = this.EscapeString(false, false) + this.DayString(false) + this.MonthString(false) + this.YearString(true);
			}
			return Result;
		}
		protected internal override DateTime GetDateTime()
		{
			ushort M = this.GEDCOMMonthToInt(this.FMonth);
			DateTime Result;
			if (this.FYear >= 0 && M >= 1 && M < 13)
			{
				ushort fDay = this.FDay;
				if (fDay >= 1 && fDay < 32)
				{
					Result = new DateTime(this.FYear, (int)M, (int)this.FDay);
					return Result;
				}
			}
			Result = new DateTime((long)((ulong)0));
			return Result;
		}
		protected internal override void SetDateTime(DateTime ADateTime)
		{
			this.SetGregorian((ushort)ADateTime.Day, TGEDCOMDate.GEDCOMMonthArray[ADateTime.Month - 1], ADateTime.Year, "", false);
		}
		protected internal string StrToGEDCOMMonth([In] string S)
		{
			if (((S != null) ? S.Length : 0) != 3)
			{
				throw new EGEDCOMException(string.Format("The string {0} is not a valid month identifier", new object[]
				{
					S
				}));
			}
			string SU = S.ToUpper();
			int Month = 1;
			while (BDSSystem.WStrCmp(TGEDCOMDate.GEDCOMMonthArray[Month - 1], SU) != 0)
			{
				Month++;
				if (Month == 13)
				{
					throw new EGEDCOMException(string.Format("The string {0} is not a valid month identifier", new object[]
					{
						S
					}));
				}
			}
			return TGEDCOMDate.GEDCOMMonthArray[Month - 1];
		}
		protected internal string StrToGEDCOMMonthFrench([In] string S)
		{
			if (((S != null) ? S.Length : 0) != 4)
			{
				throw new EGEDCOMException(string.Format("The string {0} is not a valid French month identifier", new object[]
				{
					S
				}));
			}
			string SU = S.ToUpper();
			int Month = 1;
			while (BDSSystem.WStrCmp(TGEDCOMDate.GEDCOMMonthFrenchArray[Month - 1], SU) != 0)
			{
				Month++;
				if (Month == 14)
				{
					throw new EGEDCOMException(string.Format("The string {0} is not a valid French month identifier", new object[]
					{
						S
					}));
				}
			}
			return TGEDCOMDate.GEDCOMMonthFrenchArray[Month - 1];
		}
		protected internal string StrToGEDCOMMonthHebrew([In] string S)
		{
			if (((S != null) ? S.Length : 0) != 3)
			{
				throw new EGEDCOMException(string.Format("The string {0} is not a valid Hebrew month identifier", new object[]
				{
					S
				}));
			}
			string SU = S.ToUpper();
			int Month = 1;
			while (BDSSystem.WStrCmp(TGEDCOMDate.GEDCOMMonthHebrewArray[Month - 1], SU) != 0)
			{
				Month++;
				if (Month == 14)
				{
					throw new EGEDCOMException(string.Format("The string {0} is not a valid Hebrew month identifier", new object[]
					{
						S
					}));
				}
			}
			return TGEDCOMDate.GEDCOMMonthHebrewArray[Month - 1];
		}
		protected internal string IntToGEDCOMMonth(ushort M)
		{
			return TGEDCOMDate.GEDCOMMonthArray[(int)M - 1];
		}
		protected internal string IntToGEDCOMMonthFrench(ushort M)
		{
			return TGEDCOMDate.GEDCOMMonthFrenchArray[(int)M - 1];
		}
		protected internal string IntToGEDCOMMonthHebrew(ushort M)
		{
			return TGEDCOMDate.GEDCOMMonthHebrewArray[(int)M - 1];
		}
		protected internal ushort GEDCOMMonthToInt([In] string S)
		{
			ushort Result = 0;
			if (BDSSystem.WStrCmp(S, null) != 0)
			{
				string SU = S.ToUpper();
				ushort M = 1;
				while (BDSSystem.WStrCmp(TGEDCOMDate.GEDCOMMonthArray[(int)M - 1], SU) != 0)
				{
					M += 1;
					if (M == 13)
					{
						return Result;
					}
				}
				Result = M;
			}
			return Result;
		}
		protected internal ushort GEDCOMMonthFrenchToInt([In] string S)
		{
			ushort Result = 0;
			if (BDSSystem.WStrCmp(S, null) != 0)
			{
				string SU = S.ToUpper();
				ushort M = 1;
				while (BDSSystem.WStrCmp(TGEDCOMDate.GEDCOMMonthFrenchArray[(int)M - 1], SU) != 0)
				{
					M += 1;
					if (M == 14)
					{
						return Result;
					}
				}
				Result = M;
			}
			return Result;
		}
		protected internal ushort GEDCOMMonthHebrewToInt([In] string S)
		{
			ushort Result = 0;
			if (BDSSystem.WStrCmp(S, null) != 0)
			{
				string SU = S.ToUpper();
				ushort M = 1;
				while (BDSSystem.WStrCmp(TGEDCOMDate.GEDCOMMonthHebrewArray[(int)M - 1], SU) != 0)
				{
					M += 1;
					if (M == 14)
					{
						return Result;
					}
				}
				Result = M;
			}
			return Result;
		}
		public override void Assign(TGEDCOMCustomTag Source)
		{
			if (Source != null && Source is TGEDCOMDate)
			{
				this.FDateCalendar = (Source as TGEDCOMDate).FDateCalendar;
				this.FYear = (Source as TGEDCOMDate).FYear;
				this.FYearBC = (Source as TGEDCOMDate).FYearBC;
				this.FYearModifier = (Source as TGEDCOMDate).FYearModifier;
				this.FMonth = (Source as TGEDCOMDate).FMonth;
				this.FDay = (Source as TGEDCOMDate).FDay;
			}
			else
			{
				base.Assign(Source);
			}
		}
		public void SetDate(ushort ADay, ushort AMonth, ushort AYear)
		{
			this.SetGregorian(ADay, this.IntToGEDCOMMonth(AMonth), (int)AYear, "", false);
		}
		public void GetDate(ref int AYear, ref ushort AMonth, ref ushort ADay)
		{
			if (this.FYearBC)
			{
				AYear = -this.FYear;
			}
			else
			{
				AYear = this.FYear;
			}
			AMonth = this.GEDCOMMonthToInt(this.FMonth);
			ADay = this.FDay;
		}
		public void SetGregorian([In] ushort ADay, [In] string AMonth, int AYear, [In] string AYearModifier, bool BC)
		{
			this.FDateCalendar = TGEDCOMDate.TGEDCOMCalendar.dcGregorian;
			this.FDay = ADay;
			this.FMonth = this.StrToGEDCOMMonth(AMonth);
			this.FYear = AYear;
			this.FYearModifier = AYearModifier;
			this.FYearBC = BC;
		}
		public void SetJulian([In] ushort ADay, [In] string AMonth, ushort AYear, bool BC)
		{
			this.FDateCalendar = TGEDCOMDate.TGEDCOMCalendar.dcJulian;
			this.FYear = (int)AYear;
			this.FYearBC = BC;
			this.FYearModifier = "";
			this.FDay = ADay;
			this.FMonth = this.StrToGEDCOMMonth(AMonth);
		}
		public void SetHebrew([In] ushort ADay, [In] string AMonth, int AYear, bool BC)
		{
			this.FDateCalendar = TGEDCOMDate.TGEDCOMCalendar.dcHebrew;
			this.FYear = AYear;
			this.FYearBC = BC;
			this.FYearModifier = "";
			this.FMonth = this.StrToGEDCOMMonth(AMonth);
			this.FDay = ADay;
		}
		public void SetFrench([In] ushort ADay, [In] string AMonth, ushort AYear, bool BC)
		{
			this.FDateCalendar = TGEDCOMDate.TGEDCOMCalendar.dcFrench;
			this.FYear = (int)AYear;
			this.FYearBC = BC;
			this.FYearModifier = "";
			this.FMonth = this.StrToGEDCOMMonthFrench(AMonth);
			this.FDay = ADay;
		}
		public void SetRoman([In] ushort ADay, [In] string AMonth, ushort AYear, bool BC)
		{
			this.FDateCalendar = TGEDCOMDate.TGEDCOMCalendar.dcRoman;
			this.FYear = (int)AYear;
			this.FYearBC = BC;
			this.FYearModifier = "";
			this.FDay = ADay;
			this.FMonth = this.StrToGEDCOMMonth(AMonth);
		}
		public void SetUnknown([In] ushort ADay, [In] string AMonth, ushort AYear, bool BC)
		{
			this.FDateCalendar = TGEDCOMDate.TGEDCOMCalendar.dcUnknown;
			this.FYear = (int)AYear;
			this.FYearBC = BC;
			this.FYearModifier = "";
			this.FDay = ADay;
			this.FMonth = this.StrToGEDCOMMonth(AMonth);
		}
		public override void Clear()
		{
			this.FDateCalendar = TGEDCOMDate.TGEDCOMCalendar.dcGregorian;
			this.FYear = -1;
			this.FYearBC = false;
			this.FYearModifier = "";
			this.FMonth = "";
			this.FDay = 0;
		}
		public override bool IsEmpty()
		{
			return base.IsEmpty() && this.FYear <= -1 && BDSSystem.WStrCmp(this.FMonth, "") == 0 && this.FDay <= 0;
		}
		public override string ParseString([In] string AString)
		{
			this.FDateCalendar = TGEDCOMDate.TGEDCOMCalendar.dcGregorian;
			this.FYear = -1;
			this.FYearBC = false;
			this.FYearModifier = "";
			this.FMonth = "";
			this.FDay = 0;
			string Result = AString;
			if (BDSSystem.WStrCmp(Result, "") != 0)
			{
				Result = base.ExtractDelimiter(Result, 0);
				Result = this.ExtractEscape(Result);
				Result = base.ExtractDelimiter(Result, 0);
				Result = this.ExtractDay(Result);
				if (((Result != null) ? Result.Length : 0) > 0)
				{
					if (Result[0] == ' ')
					{
						this.FDateFormat = TGEDCOMDate.TGEDCOMDateFormat.dfGEDCOMStd;
					}
					else
					{
						if (Result[0] == '.')
						{
							this.FDateFormat = TGEDCOMDate.TGEDCOMDateFormat.dfSystem;
						}
					}
				}
				Result = this.ExtractDelimiterEx(Result);
				Result = this.ExtractMonth(Result);
				Result = this.ExtractDelimiterEx(Result);
				Result = this.ExtractYear(Result);
			}
			return Result;
		}

		static TGEDCOMDate()
		{
			TGEDCOMDate.GEDCOMMonthHebrewArray = new string[]
			{
				"TSH", 
				"CSH", 
				"KSL", 
				"TVT", 
				"SHV", 
				"ADR", 
				"ADS", 
				"NSN", 
				"IYR", 
				"SVN", 
				"TMZ", 
				"AAV", 
				"ELL"
			};
			TGEDCOMDate.GEDCOMMonthFrenchArray = new string[]
			{
				"VEND", 
				"BRUM", 
				"FRIM", 
				"NIVO", 
				"PLUV", 
				"VENT", 
				"GERM", 
				"FLOR", 
				"PRAI", 
				"MESS", 
				"THER", 
				"FRUC", 
				"COMP"
			};
			TGEDCOMDate.GEDCOMMonthArray = new string[]
			{
				"JAN", 
				"FEB", 
				"MAR", 
				"APR", 
				"MAY", 
				"JUN", 
				"JUL", 
				"AUG", 
				"SEP", 
				"OCT", 
				"NOV", 
				"DEC"
			};
			TGEDCOMDate.GEDCOMMonthSysArray = new string[]
			{
				"01.", 
				"02.", 
				"03.", 
				"04.", 
				"05.", 
				"06.", 
				"07.", 
				"08.", 
				"09.", 
				"10.", 
				"11.", 
				"12."
			};
			TGEDCOMDate.GEDCOMMonthRusArray = new string[]
			{
				"ЯНВ", 
				"ФЕВ", 
				"МАР", 
				"АПР", 
				"МАЙ", 
				"ИЮН", 
				"ИЮЛ", 
				"АВГ", 
				"СЕН", 
				"ОКТ", 
				"НОЯ", 
				"ДЕК"
			};
			TGEDCOMDate.GEDCOMDateEscapeArray = new string[]
			{
				"@#DGREGORIAN@", 
				"@#DJULIAN@", 
				"@#DHEBREW@", 
				"@#DFRENCH R@", 
				"@#DROMAN@", 
				"@#DUNKNOWN@"
			};
		}
		public TGEDCOMDate(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
