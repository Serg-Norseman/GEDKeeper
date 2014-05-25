using System;
using System.Globalization;
using System.Threading;

namespace GedCom551
{
    [Serializable]
    public class GEDCOMDateException : EGEDCOMException
    {
        public GEDCOMDateException()
        {
        }

        public GEDCOMDateException(string message) : base(message)
        {
        }
    }

    public class TGEDCOMDate : TGEDCOMCustomDate
	{
		private TGEDCOMCalendar fDateCalendar;
		private TGEDCOMDateFormat fDateFormat;
		private ushort fDay;
		private string fMonth;
		private int fYear;
		private bool fYearBC;
		private string fYearModifier;

		public TGEDCOMCalendar DateCalendar
		{
			get { return this.fDateCalendar; }
		}

		public int Year
		{
			get { return this.fYear; }
			set { this.fYear = value; }
		}

		public bool YearBC
		{
			get { return this.fYearBC; }
			set { this.fYearBC = value; }
		}

		public string YearModifier
		{
			get { return this.fYearModifier; }
			set { this.fYearModifier = value; }
		}

		public string Month
		{
			get { return this.fMonth; }
			set { this.fMonth = value; }
		}

		public ushort Day
		{
			get { return this.fDay; }
			set { this.fDay = value; }
		}

		protected override void CreateObj(TGEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);

            this.fDateCalendar = TGEDCOMCalendar.dcGregorian;
			this.fYear = -1;
			this.fYearBC = false;
			this.fYearModifier = "";
			this.fMonth = "";
			this.fDay = 0;
			base.fName = "DATE";
			this.fDateFormat = TGEDCOMDateFormat.dfGEDCOMStd;
		}

		private string DayString(bool noDelimiter)
		{
			string result;

            if (this.fDay <= 0)
			{
				result = "";
			}
			else
			{
				result = this.fDay.ToString();
				if (result.Length == 1)
				{
					result = "0" + result;
				}
				if (!noDelimiter)
				{
					result += " ";
				}
			}

            return result;
		}

		private string EscapeString(bool noDelimiter, bool alwaysShowEscape)
		{
			string result;
			if (alwaysShowEscape || this.fDateCalendar != TGEDCOMCalendar.dcGregorian)
			{
				result = TGEDCOMCustomDate.GEDCOMDateEscapeArray[(int)this.fDateCalendar];
				if (!noDelimiter)
				{
					result += " ";
				}
			}
			else
			{
				result = "";
			}
			return result;
		}

		private string MonthString(bool noDelimiter)
		{
			string result;
			if (this.fMonth == "")
			{
				result = "";
			}
			else
			{
				result = this.fMonth;
				if (!noDelimiter)
				{
					result += " ";
				}
			}
			return result;
		}

		private string YearGregString(bool noDelimiter)
		{
			string result;
			if (this.fYear == -1)
			{
				result = "";
			}
			else
			{
				result = this.fYear.ToString();
				if (this.fYearModifier != "")
				{
					result = result + "/" + this.fYearModifier;
				}
				if (this.fYearBC)
				{
					result += GEDCOMYearBC;
				}
				if (!noDelimiter)
				{
					result += " ";
				}
			}
			return result;
		}

		private string YearString(bool noDelimiter)
		{
			string result;
			if (this.fYear == -1)
			{
				result = "";
			}
			else
			{
				result = this.fYear.ToString();
				if (this.fYearBC)
				{
					result += GEDCOMYearBC;
				}
				if (!noDelimiter)
				{
					result += " ";
				}
			}
			return result;
		}

		private string ExtractEscape(string S)
		{
			string result = S;
			if (result.StartsWith("@#"))
			{
				int P = result.IndexOf("@", 2);
				if (P >= 0)
				{
					string SU = result.Substring(0, P + 1);

					for (TGEDCOMCalendar I = TGEDCOMCalendar.dcGregorian; I <= TGEDCOMCalendar.dcLast; I++)
					{
						if (TGEDCOMCustomDate.GEDCOMDateEscapeArray[(int)I] == SU)
						{
							this.fDateCalendar = I;
							result = result.Remove(0, SU.Length);
							break;
						}
					}
				}
			}
			return result;
		}

		private string ExtractDay(string S)
		{
			string result = S;

			int I = 0;
			int num = ((result != null) ? result.Length : 0);
			while (I < num && GEDCOMUtils.IsDigit(result[I]))
			{
				I++;
			}

			if (I >= 1 && I <= 2)
			{
				this.fDay = (ushort)int.Parse(result.Substring(0, I));
				result = result.Remove(0, I);
			}

			return result;
		}

		private string ExtractDelimiterEx(string S)
		{
			string result;
			if (this.fDateFormat == TGEDCOMDateFormat.dfSystem)
			{
				result = GEDCOMUtils.ExtractDotDelimiter(S, 0);
			}
			else
			{
				result = GEDCOMUtils.ExtractDelimiter(S, 0);
			}
			return result;
		}

		private string ExtractMonth(string S)
		{
			string result = S;
			if (!string.IsNullOrEmpty(result))
			{
				switch (this.fDateCalendar)
				{
					case TGEDCOMCalendar.dcHebrew:
						{
							string SU = result.Substring(0, 3).ToUpperInvariant();
							for (int I = 1; I <= GEDCOMMonthHebrewArray.Length; I++)
							{
								if (GEDCOMMonthHebrewArray[I - 1] == SU)
								{
									this.fMonth = SU;
									result = result.Remove(0, 3);
									break;
								}
							}
							break;
						}

					case TGEDCOMCalendar.dcFrench:
						{
							string SU = result.Substring(0, 4).ToUpperInvariant();
							for (int I = 1; I <= TGEDCOMCustomDate.GEDCOMMonthFrenchArray.Length; I++)
							{
                                if (TGEDCOMCustomDate.GEDCOMMonthFrenchArray[I - 1] == SU)
								{
									this.fMonth = SU;
									result = result.Remove(0, 4);
									break;
								}
							}
							break;
						}

					default:
						{
							if (!GEDCOMUtils.IsDigit(result[0]))
							{
								DateTimeFormatInfo DateTimeInfo = Thread.CurrentThread.CurrentCulture.DateTimeFormat;
								string SU = result.Substring(0, 3).ToUpper();
								for (int I = 1; I <= GEDCOMMonthArray.Length; I++)
								{
									if (GEDCOMMonthArray[I - 1] == SU || DateTimeInfo.AbbreviatedMonthNames[I - 1].ToUpper() == SU)
									{
                                        this.fMonth = TGEDCOMCustomDate.GEDCOMMonthArray[I - 1];
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
										this.fMonth = GEDCOMMonthArray[I - 1];
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
			string result = S;

			int I = 0;
			int num = ((result != null) ? result.Length : 0);
			while (I < num && GEDCOMUtils.IsDigit(result[I]))
			{
				I++;
			}

			if (I > 0)
			{
				this.fYear = int.Parse(result.Substring(0, I));
				result = result.Remove(0, I);

				if (result != "" && result[0] == '/' && GEDCOMUtils.IsDigits(result.Substring(1, 2)))
				{
					this.fYearModifier = result.Substring(1, 2);
					result = result.Remove(0, 3);
				}

				if (result != "" && result.Substring(0, 4).ToUpper() == GEDCOMYearBC)
				{
					this.fYearBC = true;
					result = result.Remove(0, 4);
				}
			}

			return result;
		}

		protected override string GetStringValue()
		{
			string result;
			if (this.fDateCalendar == TGEDCOMCalendar.dcGregorian)
			{
				result = this.EscapeString(false, false) + this.DayString(false) + this.MonthString(false) + this.YearGregString(true);
			}
			else
			{
				result = this.EscapeString(false, false) + this.DayString(false) + this.MonthString(false) + this.YearString(true);
			}
			return result;
		}

		public override DateTime GetDateTime()
		{
            DateTime result;
            
            ushort month = GEDCOMMonthToInt(this.fMonth);
			if (this.fYear >= 0 && month >= 1 && month <= 12)
			{
				ushort day = this.fDay;
				if (day >= 1 && day < 32)
				{
					result = new DateTime(this.fYear, (int)month, (int)this.fDay);
					return result;
				}
			}
			
            result = new DateTime(0);
			return result;
		}

		public override void SetDateTime(DateTime value)
		{
            this.SetGregorian((ushort)value.Day, TGEDCOMCustomDate.GEDCOMMonthArray[value.Month - 1], value.Year, "", false);
		}

		private static string CheckGEDCOMMonth(string S)
		{
			if (((S != null) ? S.Length : 0) != 3)
			{
                throw new GEDCOMDateException(string.Format("The string {0} is not a valid month identifier", S));
			}
			string SU = S.ToUpper();

			for (int month = 1; month <= 12; month++)
			{
				if (TGEDCOMCustomDate.GEDCOMMonthArray[month - 1] == SU)
				{
					return TGEDCOMCustomDate.GEDCOMMonthArray[month - 1];
				}
			}

            throw new GEDCOMDateException(string.Format("The string {0} is not a valid month identifier", S));
		}

		private static string CheckGEDCOMMonthFrench(string S)
		{
			if (((S != null) ? S.Length : 0) != 4)
			{
                throw new GEDCOMDateException(string.Format("The string {0} is not a valid French month identifier", S));
			}
			string SU = S.ToUpper();

			for (int month = 1; month <= 13; month++)
			{
				if (TGEDCOMCustomDate.GEDCOMMonthFrenchArray[month - 1] == SU)
				{
					return TGEDCOMCustomDate.GEDCOMMonthFrenchArray[month - 1];
				}
			}

            throw new GEDCOMDateException(string.Format("The string {0} is not a valid French month identifier", S));
		}

		private static string CheckGEDCOMMonthHebrew(string S)
		{
			if (((S != null) ? S.Length : 0) != 3)
			{
                throw new GEDCOMDateException(string.Format("The string {0} is not a valid Hebrew month identifier", S));
			}
			string SU = S.ToUpperInvariant();

			for (int month = 1; month <= 13; month++)
			{
				if (TGEDCOMCustomDate.GEDCOMMonthHebrewArray[month - 1] == SU)
				{
					return TGEDCOMCustomDate.GEDCOMMonthHebrewArray[month - 1];
				}
			}

            throw new GEDCOMDateException(string.Format("The string {0} is not a valid Hebrew month identifier", S));
		}

		private static string IntToGEDCOMMonth(ushort m)
		{
            return TGEDCOMCustomDate.GEDCOMMonthArray[(int)m - 1];
		}

		private static string IntToGEDCOMMonthFrench(ushort m)
		{
            return TGEDCOMCustomDate.GEDCOMMonthFrenchArray[(int)m - 1];
		}

		private static string IntToGEDCOMMonthHebrew(ushort m)
		{
            return TGEDCOMCustomDate.GEDCOMMonthHebrewArray[(int)m - 1];
		}

		private static ushort GEDCOMMonthToInt(string S)
		{
			ushort result = 0;

			if (!string.IsNullOrEmpty(S))
			{
				string SU = S.ToUpperInvariant();

				for (int m = 1; m <= 12; m++)
				{
					if (GEDCOMMonthArray[m - 1] == SU)
					{
						result = (ushort)m;
						break;
					}
				}
			}

			return result;
		}

		private static ushort GEDCOMMonthFrenchToInt(string S)
		{
			ushort result = 0;

			if (S != null)
			{
				string SU = S.ToUpperInvariant();

				for (ushort M = 1; M <= 13; M++)
				{
					if (TGEDCOMCustomDate.GEDCOMMonthFrenchArray[(int)M - 1] == SU)
					{
						result = M;
						break;
					}
				}
			}

			return result;
		}

		private static ushort GEDCOMMonthHebrewToInt(string S)
		{
			ushort result = 0;

			if (S != null)
			{
				string SU = S.ToUpperInvariant();

				for (ushort M = 1; M <= 13; M++)
				{
					if (TGEDCOMCustomDate.GEDCOMMonthHebrewArray[(int)M - 1] == SU)
					{
						result = M;
						break;
					}
				}
			}

			return result;
		}

        public override void Assign(TGEDCOMTag source)
		{
			if (source != null && source is TGEDCOMDate)
			{
				TGEDCOMDate srcDate = (source as TGEDCOMDate);

				this.fDateCalendar = srcDate.fDateCalendar;
				this.fYear = srcDate.fYear;
				this.fYearBC = srcDate.fYearBC;
				this.fYearModifier = srcDate.fYearModifier;
				this.fMonth = srcDate.fMonth;
				this.fDay = srcDate.fDay;
			}
			else
			{
				base.Assign(source);
			}
		}

		public void GetDate(out int AYear, out ushort AMonth, out ushort ADay)
		{
			AYear = this.fYear;

			switch (this.fDateCalendar) {
				case TGEDCOMCalendar.dcHebrew:
					AMonth = GEDCOMMonthHebrewToInt(this.fMonth);
					break;
					
				case TGEDCOMCalendar.dcFrench:
					AMonth = GEDCOMMonthFrenchToInt(this.fMonth);
					break;
				
				case TGEDCOMCalendar.dcGregorian:
				case TGEDCOMCalendar.dcJulian:
				case TGEDCOMCalendar.dcRoman:
				case TGEDCOMCalendar.dcUnknown:
				default:
					AMonth = GEDCOMMonthToInt(this.fMonth);
					break;
			}

			ADay = this.fDay;
		}

		public void SetGregorian(ushort ADay, ushort AMonth, ushort AYear)
		{
			this.SetGregorian(ADay, IntToGEDCOMMonth(AMonth), (int)AYear, "", false);
		}

		public void SetGregorian(ushort ADay, string AMonth, int AYear, string AYearModifier, bool BC)
		{
			this.fDateCalendar = TGEDCOMCalendar.dcGregorian;
			this.fDay = ADay;
			this.fMonth = CheckGEDCOMMonth(AMonth);
			this.fYear = AYear;
			this.fYearModifier = AYearModifier;
			this.fYearBC = BC;
		}

		public void SetJulian(ushort ADay, string AMonth, ushort AYear, bool BC)
		{
			this.fDateCalendar = TGEDCOMCalendar.dcJulian;
			this.fYear = (int)AYear;
			this.fYearBC = BC;
			this.fYearModifier = "";
			this.fDay = ADay;
			this.fMonth = CheckGEDCOMMonth(AMonth);
		}

		public void SetHebrew(ushort ADay, ushort AMonth, int AYear)
		{
			this.SetHebrew(ADay, IntToGEDCOMMonthHebrew(AMonth), AYear, false);
		}

		public void SetHebrew(ushort ADay, string AMonth, int AYear, bool BC)
		{
			this.fDateCalendar = TGEDCOMCalendar.dcHebrew;
			this.fYear = AYear;
			this.fYearBC = BC;
			this.fYearModifier = "";
			this.fMonth = CheckGEDCOMMonthHebrew(AMonth);
			this.fDay = ADay;
		}

		public void SetFrench(ushort ADay, ushort AMonth, ushort AYear)
		{
			this.SetFrench(ADay, IntToGEDCOMMonthFrench(AMonth), AYear, false);
		}

		public void SetFrench(ushort ADay, string AMonth, ushort AYear, bool BC)
		{
			this.fDateCalendar = TGEDCOMCalendar.dcFrench;
			this.fYear = (int)AYear;
			this.fYearBC = BC;
			this.fYearModifier = "";
			this.fMonth = CheckGEDCOMMonthFrench(AMonth);
			this.fDay = ADay;
		}

		public void SetRoman(ushort ADay, string AMonth, ushort AYear, bool BC)
		{
			this.fDateCalendar = TGEDCOMCalendar.dcRoman;
			this.fYear = (int)AYear;
			this.fYearBC = BC;
			this.fYearModifier = "";
			this.fDay = ADay;
			this.fMonth = CheckGEDCOMMonth(AMonth);
		}

		public void SetUnknown(ushort ADay, string AMonth, ushort AYear, bool BC)
		{
			this.fDateCalendar = TGEDCOMCalendar.dcUnknown;
			this.fYear = (int)AYear;
			this.fYearBC = BC;
			this.fYearModifier = "";
			this.fDay = ADay;
			this.fMonth = CheckGEDCOMMonth(AMonth);
		}

		public override void Clear()
		{
			this.fDateCalendar = TGEDCOMCalendar.dcGregorian;
			this.fYear = -1;
			this.fYearBC = false;
			this.fYearModifier = "";
			this.fMonth = "";
			this.fDay = 0;
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this.fYear <= -1 && this.fMonth == "" && this.fDay <= 0;
		}

		public override string ParseString(string strValue)
		{
			this.fDateCalendar = TGEDCOMCalendar.dcGregorian;
			this.fYear = -1;
			this.fYearBC = false;
			this.fYearModifier = "";
			this.fMonth = "";
			this.fDay = 0;

			string result = strValue;

			if (!string.IsNullOrEmpty(result))
			{
				result = GEDCOMUtils.ExtractDelimiter(result, 0);
				result = this.ExtractEscape(result);
				result = GEDCOMUtils.ExtractDelimiter(result, 0);
				result = this.ExtractDay(result);
				if (result.Length > 0)
				{
					if (result[0] == ' ')
					{
						this.fDateFormat = TGEDCOMDateFormat.dfGEDCOMStd;
					}
					else
					{
						if (result[0] == '.')
						{
							this.fDateFormat = TGEDCOMDateFormat.dfSystem;
						}
					}
				}
				result = this.ExtractDelimiterEx(result);
				result = this.ExtractMonth(result);
				result = this.ExtractDelimiterEx(result);
				result = this.ExtractYear(result);
			}
			return result;
		}

		public TGEDCOMDate(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}
	}
}
