using System;
using System.Globalization;
using System.Threading;

namespace GedCom551
{
	public class TGEDCOMDate : TGEDCOMCustomDate
	{
		private TGEDCOMCalendar FDateCalendar;
		private TGEDCOMDateFormat FDateFormat;
		private ushort FDay;
		private string FMonth;
		private int FYear;
		private bool FYearBC;
		private string FYearModifier;

		public TGEDCOMCalendar DateCalendar
		{
			get { return this.FDateCalendar; }
		}

		public int Year
		{
			get { return this.FYear; }
			set { this.FYear = value; }
		}

		public bool YearBC
		{
			get { return this.FYearBC; }
			set { this.FYearBC = value; }
		}

		public string YearModifier
		{
			get { return this.FYearModifier; }
			set { this.FYearModifier = value; }
		}

		public string Month
		{
			get { return this.FMonth; }
			set { this.FMonth = value; }
		}

		public ushort Day
		{
			get { return this.FDay; }
			set { this.FDay = value; }
		}

		protected override void CreateObj(TGEDCOMTree owner, TGEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.FDateCalendar = TGEDCOMCalendar.dcGregorian;
			this.FYear = -1;
			this.FYearBC = false;
			this.FYearModifier = "";
			this.FMonth = "";
			this.FDay = 0;
			base.FName = "DATE";
			this.FDateFormat = TGEDCOMDateFormat.dfGEDCOMStd;
		}

		private string DayString(bool NoDelimiter)
		{
			string result;
			if (this.FDay <= 0)
			{
				result = "";
			}
			else
			{
				result = this.FDay.ToString();
				if (((result != null) ? result.Length : 0) == 1)
				{
					result = "0" + result;
				}
				if (!NoDelimiter)
				{
					result += " ";
				}
			}
			return result;
		}

		private string EscapeString(bool NoDelimiter, bool AlwaysShowEscape)
		{
			string Result;
			if (AlwaysShowEscape || this.FDateCalendar != TGEDCOMCalendar.dcGregorian)
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

		private string MonthString(bool NoDelimiter)
		{
			string Result;
			if (this.FMonth == "")
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

		private string YearGregString(bool NoDelimiter)
		{
			string Result;
			if (this.FYear == -1)
			{
				Result = "";
			}
			else
			{
				Result = this.FYear.ToString();
				if (this.FYearModifier != "")
				{
					Result = Result + "/" + this.FYearModifier;
				}
				if (this.FYearBC)
				{
					Result += GEDCOMYearBC;
				}
				if (!NoDelimiter)
				{
					Result += " ";
				}
			}
			return Result;
		}

		private string YearString(bool NoDelimiter)
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
					Result += GEDCOMYearBC;
				}
				if (!NoDelimiter)
				{
					Result += " ";
				}
			}
			return Result;
		}

		private string ExtractEscape(string S)
		{
			string Result = S;
			if (Result.StartsWith("@#"))
			{
				int P = Result.IndexOf("@", 2);
				if (P >= 0)
				{
					string SU = Result.Substring(0, P + 1);

					for (TGEDCOMCalendar I = TGEDCOMCalendar.dcGregorian; I <= TGEDCOMCalendar.dcLast; I++)
					{
						if (TGEDCOMDate.GEDCOMDateEscapeArray[(int)I] == SU)
						{
							this.FDateCalendar = I;
							Result = Result.Remove(0, SU.Length);
							break;
						}
					}
				}
			}
			return Result;
		}

		private string ExtractDay(string S)
		{
			string Result = S;

			int I = 0;
			int num = ((Result != null) ? Result.Length : 0);
			while (I < num && IsDigit(Result[I]))
			{
				I++;
			}

			if (I >= 1 && I <= 2)
			{
				this.FDay = (ushort)int.Parse(Result.Substring(0, I));
				Result = Result.Remove(0, I);
			}

			return Result;
		}

		private string ExtractDelimiterEx(string S)
		{
			string Result;
			if (this.FDateFormat == TGEDCOMDateFormat.dfSystem)
			{
				Result = base.ExtractDotDelimiter(S, 0);
			}
			else
			{
				Result = base.ExtractDelimiter(S, 0);
			}
			return Result;
		}

		private string ExtractMonth(string S)
		{
			string result = S;
			if (!string.IsNullOrEmpty(result))
			{
				switch (this.FDateCalendar)
				{
					case TGEDCOMCalendar.dcHebrew:
						{
							string SU = result.Substring(0, 3).ToUpper();
							for (int I = 1; I <= GEDCOMMonthHebrewArray.Length; I++)
							{
								if (GEDCOMMonthHebrewArray[I - 1] == SU)
								{
									this.FMonth = SU;
									result = result.Remove(0, 3);
									break;
								}
							}
							break;
						}

					case TGEDCOMCalendar.dcFrench:
						{
							string SU = result.Substring(0, 4).ToUpper();
							for (int I = 1; I <= TGEDCOMDate.GEDCOMMonthFrenchArray.Length; I++)
							{
								if (TGEDCOMDate.GEDCOMMonthFrenchArray[I - 1] == SU)
								{
									this.FMonth = SU;
									result = result.Remove(0, 4);
									break;
								}
							}
							break;
						}

					default:
						{
							if (!IsDigit(result[0]))
							{
								DateTimeFormatInfo DateTimeInfo = Thread.CurrentThread.CurrentCulture.DateTimeFormat;
								string SU = result.Substring(0, 3).ToUpper();
								for (int I = 1; I <= GEDCOMMonthArray.Length; I++)
								{
									if (GEDCOMMonthArray[I - 1] == SU || DateTimeInfo.AbbreviatedMonthNames[I - 1].ToUpper() == SU)
									{
										this.FMonth = TGEDCOMDate.GEDCOMMonthArray[I - 1];
										result = result.Remove(0, 3);
										break;
									}
								}
							}
							else
							{
								string SU = result.Substring(0, 3).ToUpper();
								for (int I = 1; I <= GEDCOMMonthSysArray.Length; I++)
								{
									if (GEDCOMMonthSysArray[I - 1] == SU)
									{
										this.FMonth = GEDCOMMonthArray[I - 1];
										result = result.Remove(0, 2);
										break;
									}
								}
							}
							break;
						}
				}
			}
			return result;
		}

		private string ExtractYear(string S)
		{
			string Result = S;

			int I = 0;
			int num = ((Result != null) ? Result.Length : 0);
			while (I < num && IsDigit(Result[I]))
			{
				I++;
			}

			if (I > 0)
			{
				this.FYear = int.Parse(Result.Substring(0, I));
				Result = Result.Remove(0, I);

				if (Result != "" && Result[0] == '/' && IsDigits(Result.Substring(1, 2)))
				{
					this.FYearModifier = Result.Substring(1, 2);
					Result = Result.Remove(0, 3);
				}

				if (Result != "" && Result.Substring(0, 4).ToUpper() == GEDCOMYearBC)
				{
					this.FYearBC = true;
					Result = Result.Remove(0, 4);
				}
			}

			return Result;
		}

		protected override string GetStringValue()
		{
			string Result;
			if (this.FDateCalendar == TGEDCOMCalendar.dcGregorian)
			{
				Result = this.EscapeString(false, false) + this.DayString(false) + this.MonthString(false) + this.YearGregString(true);
			}
			else
			{
				Result = this.EscapeString(false, false) + this.DayString(false) + this.MonthString(false) + this.YearString(true);
			}
			return Result;
		}

		public override DateTime GetDateTime()
		{
			ushort M = this.GEDCOMMonthToInt(this.FMonth);
			DateTime Result;
			if (this.FYear >= 0 && M >= 1 && M <= 12)
			{
				ushort fDay = this.FDay;
				if (fDay >= 1 && fDay < 32)
				{
					Result = new DateTime(this.FYear, (int)M, (int)this.FDay);
					return Result;
				}
			}
			Result = new DateTime(0);
			return Result;
		}

		public override void SetDateTime(DateTime ADateTime)
		{
			this.SetGregorian((ushort)ADateTime.Day, TGEDCOMDate.GEDCOMMonthArray[ADateTime.Month - 1], ADateTime.Year, "", false);
		}

		private string StrToGEDCOMMonth(string S)
		{
			if (((S != null) ? S.Length : 0) != 3)
			{
				throw new EGEDCOMException(string.Format("The string {0} is not a valid month identifier", S));
			}
			string SU = S.ToUpper();
			int Month = 1;
			while (TGEDCOMDate.GEDCOMMonthArray[Month - 1] != SU)
			{
				Month++;
				if (Month == 13)
				{
					throw new EGEDCOMException(string.Format("The string {0} is not a valid month identifier", S));
				}
			}
			return TGEDCOMDate.GEDCOMMonthArray[Month - 1];
		}

		private string StrToGEDCOMMonthFrench(string S)
		{
			if (((S != null) ? S.Length : 0) != 4)
			{
				throw new EGEDCOMException(string.Format("The string {0} is not a valid French month identifier", S));
			}
			string SU = S.ToUpper();
			int Month = 1;
			while (TGEDCOMDate.GEDCOMMonthFrenchArray[Month - 1] != SU)
			{
				Month++;
				if (Month == 14)
				{
					throw new EGEDCOMException(string.Format("The string {0} is not a valid French month identifier", S));
				}
			}
			return TGEDCOMDate.GEDCOMMonthFrenchArray[Month - 1];
		}

		private string StrToGEDCOMMonthHebrew(string S)
		{
			if (((S != null) ? S.Length : 0) != 3)
			{
				throw new EGEDCOMException(string.Format("The string {0} is not a valid Hebrew month identifier", S));
			}
			string SU = S.ToUpper();
			int Month = 1;
			while (TGEDCOMDate.GEDCOMMonthHebrewArray[Month - 1] != SU)
			{
				Month++;
				if (Month == 14)
				{
					throw new EGEDCOMException(string.Format("The string {0} is not a valid Hebrew month identifier", S));
				}
			}
			return TGEDCOMDate.GEDCOMMonthHebrewArray[Month - 1];
		}

		private string IntToGEDCOMMonth(ushort M)
		{
			return TGEDCOMDate.GEDCOMMonthArray[(int)M - 1];
		}

		private string IntToGEDCOMMonthFrench(ushort M)
		{
			return TGEDCOMDate.GEDCOMMonthFrenchArray[(int)M - 1];
		}

		private string IntToGEDCOMMonthHebrew(ushort M)
		{
			return TGEDCOMDate.GEDCOMMonthHebrewArray[(int)M - 1];
		}

		private ushort GEDCOMMonthToInt(string S)
		{
			ushort result = 0;

			if (S != null && S.Length != 0)
			{
				string SU = S.ToUpper();

				for (int M = 1; M <= 12; M++)
				{
					if (GEDCOMMonthArray[M - 1] == SU)
					{
						result = (ushort)M;
						break;
					}
				}
			}

			return result;
		}

		private ushort GEDCOMMonthFrenchToInt(string S)
		{
			ushort Result = 0;
			if (S != null)
			{
				string SU = S.ToUpper();
				ushort M = 1;
				while (TGEDCOMDate.GEDCOMMonthFrenchArray[(int)M - 1] != SU)
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

		private ushort GEDCOMMonthHebrewToInt(string S)
		{
			ushort Result = 0;
			if (S != null)
			{
				string SU = S.ToUpper();
				ushort M = 1;
				while (TGEDCOMDate.GEDCOMMonthHebrewArray[(int)M - 1] != SU)
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

        public override void Assign(TGEDCOMTag Source)
		{
			if (Source != null && Source is TGEDCOMDate)
			{
				TGEDCOMDate src_date = (Source as TGEDCOMDate);

				this.FDateCalendar = src_date.FDateCalendar;
				this.FYear = src_date.FYear;
				this.FYearBC = src_date.FYearBC;
				this.FYearModifier = src_date.FYearModifier;
				this.FMonth = src_date.FMonth;
				this.FDay = src_date.FDay;
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

		public void GetDate(out int AYear, out ushort AMonth, out ushort ADay)
		{
			AYear = this.FYear;
			AMonth = this.GEDCOMMonthToInt(this.FMonth);
			ADay = this.FDay;
		}

		public void SetGregorian(ushort ADay, string AMonth, int AYear, string AYearModifier, bool BC)
		{
			this.FDateCalendar = TGEDCOMCalendar.dcGregorian;
			this.FDay = ADay;
			this.FMonth = this.StrToGEDCOMMonth(AMonth);
			this.FYear = AYear;
			this.FYearModifier = AYearModifier;
			this.FYearBC = BC;
		}

		public void SetJulian(ushort ADay, string AMonth, ushort AYear, bool BC)
		{
			this.FDateCalendar = TGEDCOMCalendar.dcJulian;
			this.FYear = (int)AYear;
			this.FYearBC = BC;
			this.FYearModifier = "";
			this.FDay = ADay;
			this.FMonth = this.StrToGEDCOMMonth(AMonth);
		}

		public void SetHebrew(ushort ADay, string AMonth, int AYear, bool BC)
		{
			this.FDateCalendar = TGEDCOMCalendar.dcHebrew;
			this.FYear = AYear;
			this.FYearBC = BC;
			this.FYearModifier = "";
			this.FMonth = this.StrToGEDCOMMonth(AMonth);
			this.FDay = ADay;
		}

		public void SetFrench(ushort ADay, string AMonth, ushort AYear, bool BC)
		{
			this.FDateCalendar = TGEDCOMCalendar.dcFrench;
			this.FYear = (int)AYear;
			this.FYearBC = BC;
			this.FYearModifier = "";
			this.FMonth = this.StrToGEDCOMMonthFrench(AMonth);
			this.FDay = ADay;
		}

		public void SetRoman(ushort ADay, string AMonth, ushort AYear, bool BC)
		{
			this.FDateCalendar = TGEDCOMCalendar.dcRoman;
			this.FYear = (int)AYear;
			this.FYearBC = BC;
			this.FYearModifier = "";
			this.FDay = ADay;
			this.FMonth = this.StrToGEDCOMMonth(AMonth);
		}

		public void SetUnknown(ushort ADay, string AMonth, ushort AYear, bool BC)
		{
			this.FDateCalendar = TGEDCOMCalendar.dcUnknown;
			this.FYear = (int)AYear;
			this.FYearBC = BC;
			this.FYearModifier = "";
			this.FDay = ADay;
			this.FMonth = this.StrToGEDCOMMonth(AMonth);
		}

		public override void Clear()
		{
			this.FDateCalendar = TGEDCOMCalendar.dcGregorian;
			this.FYear = -1;
			this.FYearBC = false;
			this.FYearModifier = "";
			this.FMonth = "";
			this.FDay = 0;
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this.FYear <= -1 && this.FMonth == "" && this.FDay <= 0;
		}

		public override string ParseString(string AString)
		{
			this.FDateCalendar = TGEDCOMCalendar.dcGregorian;
			this.FYear = -1;
			this.FYearBC = false;
			this.FYearModifier = "";
			this.FMonth = "";
			this.FDay = 0;
			string Result = AString;

			if (!string.IsNullOrEmpty(Result))
			{
				Result = base.ExtractDelimiter(Result, 0);
				Result = this.ExtractEscape(Result);
				Result = base.ExtractDelimiter(Result, 0);
				Result = this.ExtractDay(Result);
				if (Result.Length > 0)
				{
					if (Result[0] == ' ')
					{
						this.FDateFormat = TGEDCOMDateFormat.dfGEDCOMStd;
					}
					else
					{
						if (Result[0] == '.')
						{
							this.FDateFormat = TGEDCOMDateFormat.dfSystem;
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

		public TGEDCOMDate(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}
	}
}
