using System;
using System.Globalization;
using System.Threading;

namespace GKCommon.GEDCOM
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

    public class GEDCOMDate : GEDCOMCustomDate
	{
		private GEDCOMCalendar fDateCalendar;
		private GEDCOMDateFormat fDateFormat;
		private ushort fDay;
		private string fMonth;
		private int fYear;
		private bool fYearBC;
		private string fYearModifier;

		public GEDCOMCalendar DateCalendar
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

		protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
            base.SetName("DATE");

            this.fDateCalendar = GEDCOMCalendar.dcGregorian;
			this.fYear = -1;
			this.fYearBC = false;
			this.fYearModifier = "";
			this.fMonth = "";
			this.fDay = 0;
			this.fDateFormat = GEDCOMDateFormat.dfGEDCOMStd;
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
			if (alwaysShowEscape || this.fDateCalendar != GEDCOMCalendar.dcGregorian)
			{
				result = GEDCOMDateEscapeArray[(int)this.fDateCalendar];
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
					result += GEDCOM_YEAR_BC;
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
					result += GEDCOM_YEAR_BC;
				}
				if (!noDelimiter)
				{
					result += " ";
				}
			}

            return result;
		}

		private string ExtractEscape(string str)
		{
			string result = str;

            if (result.StartsWith("@#"))
			{
				int p = result.IndexOf("@", 2);
				if (p >= 0)
				{
					string su = result.Substring(0, p + 1);

					for (GEDCOMCalendar I = GEDCOMCalendar.dcGregorian; I <= GEDCOMCalendar.dcLast; I++)
					{
						if (GEDCOMDateEscapeArray[(int)I] == su)
						{
							this.fDateCalendar = I;
							result = result.Remove(0, su.Length);
							break;
						}
					}
				}
			}

            return result;
		}

		private string ExtractDay(string str)
		{
		    if (string.IsNullOrEmpty(str)) return str;

			string result = str;

			int I = 0;
			int num = result.Length;
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

		private string ExtractDelimiterEx(string str)
		{
		    string result = (this.fDateFormat == GEDCOMDateFormat.dfSystem) ? GEDCOMUtils.ExtractDotDelimiter(str, 0) : GEDCOMUtils.ExtractDelimiter(str, 0);
		    return result;
		}

        private string ExtractMonth(string str)
		{
			string result = str;
			if (!string.IsNullOrEmpty(result))
			{
				switch (this.fDateCalendar)
				{
					case GEDCOMCalendar.dcHebrew:
						{
							string su = result.Substring(0, 3).ToUpperInvariant();

							for (int I = 1; I <= GEDCOMMonthHebrewArray.Length; I++)
							{
								if (GEDCOMMonthHebrewArray[I - 1] == su)
								{
									this.fMonth = su;
									result = result.Remove(0, 3);
									break;
								}
							}
							break;
						}

					case GEDCOMCalendar.dcFrench:
						{
							string su = result.Substring(0, 4).ToUpperInvariant();

							for (int I = 1; I <= GEDCOMMonthFrenchArray.Length; I++)
							{
                                if (GEDCOMMonthFrenchArray[I - 1] == su)
								{
									this.fMonth = su;
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
								DateTimeFormatInfo dtInfo = Thread.CurrentThread.CurrentCulture.DateTimeFormat;

                                string su = result.Substring(0, 3).ToUpper();

								for (int I = 1; I <= GEDCOMMonthArray.Length; I++)
								{
									if (GEDCOMMonthArray[I - 1] == su || dtInfo.AbbreviatedMonthNames[I - 1].ToUpper() == su)
									{
                                        this.fMonth = GEDCOMMonthArray[I - 1];
										result = result.Remove(0, 3);
										break;
									}
								}
							}
							else
							{
								string su = result.Substring(0, 3).ToUpper();

								for (int I = 1; I <= GEDCOMMonthSysArray.Length; I++)
								{
									if (GEDCOMMonthSysArray[I - 1] == su)
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

		private string ExtractYear(string str)
		{
            if (string.IsNullOrEmpty(str)) return str;

			string result = str;

			int I = 0;
			int num = result.Length;
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

				if (result != "" && result.Substring(0, 4).ToUpper() == GEDCOM_YEAR_BC)
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
			if (this.fDateCalendar == GEDCOMCalendar.dcGregorian)
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
					result = new DateTime(this.fYear, month, this.fDay);
					return result;
				}
			}
			
            result = new DateTime(0);
			return result;
		}

		public override void SetDateTime(DateTime value)
		{
            this.SetGregorian((ushort)value.Day, GEDCOMMonthArray[value.Month - 1], value.Year, "", false);
		}

		private static string CheckGEDCOMMonth(string str)
		{
            if (str != null && str.Length == 3)
            {
                str = str.ToUpperInvariant();

                for (int m = 1; m <= 12; m++)
                {
                    if (GEDCOMMonthArray[m - 1] == str)
                    {
                        return GEDCOMMonthArray[m - 1];
                    }
                }
            }

            throw new GEDCOMDateException(string.Format("The string {0} is not a valid month identifier", str));
		}

		private static string CheckGEDCOMMonthFrench(string str)
		{
            if (str != null && str.Length == 4)
            {
                str = str.ToUpperInvariant();

                for (int m = 1; m <= 13; m++)
                {
                    if (GEDCOMMonthFrenchArray[m - 1] == str)
                    {
                        return GEDCOMMonthFrenchArray[m - 1];
                    }
                }
            }

            throw new GEDCOMDateException(string.Format("The string {0} is not a valid French month identifier", str));
		}

		private static string CheckGEDCOMMonthHebrew(string str)
		{
            if (str != null && str.Length == 3)
            {
                str = str.ToUpperInvariant();

                for (int m = 1; m <= 13; m++)
                {
                    if (GEDCOMMonthHebrewArray[m - 1] == str)
                    {
                        return GEDCOMMonthHebrewArray[m - 1];
                    }
                }
            }

            throw new GEDCOMDateException(string.Format("The string {0} is not a valid Hebrew month identifier", str));
		}

		private static string IntToGEDCOMMonth(ushort m)
		{
            return GEDCOMMonthArray[m - 1];
		}

		private static string IntToGEDCOMMonthFrench(ushort m)
		{
            return GEDCOMMonthFrenchArray[m - 1];
		}

		private static string IntToGEDCOMMonthHebrew(ushort m)
		{
            return GEDCOMMonthHebrewArray[m - 1];
		}

		private static ushort GEDCOMMonthToInt(string st)
		{
			ushort result = 0;

			if (!string.IsNullOrEmpty(st))
			{
				st = st.ToUpperInvariant();

				for (int m = 1; m <= 12; m++)
				{
					if (GEDCOMMonthArray[m - 1] == st)
					{
						result = (ushort)m;
						break;
					}
				}
			}

			return result;
		}

		private static ushort GEDCOMMonthFrenchToInt(string str)
		{
			ushort result = 0;

			if (str != null)
			{
				str = str.ToUpperInvariant();

				for (ushort m = 1; m <= 13; m++)
				{
					if (GEDCOMMonthFrenchArray[m - 1] == str)
					{
						result = m;
						break;
					}
				}
			}

			return result;
		}

		private static ushort GEDCOMMonthHebrewToInt(string str)
		{
			ushort result = 0;

			if (str != null)
			{
				str = str.ToUpperInvariant();

				for (ushort m = 1; m <= 13; m++)
				{
					if (GEDCOMMonthHebrewArray[m - 1] == str)
					{
						result = m;
						break;
					}
				}
			}

			return result;
		}

        public override void Assign(GEDCOMTag source)
		{
			if (source is GEDCOMDate)
			{
				GEDCOMDate srcDate = (source as GEDCOMDate);

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

		public override void GetDateParts(out int year, out ushort month, out ushort day, out bool yearBC)
		{
			year = this.fYear;

			switch (this.fDateCalendar) {
				case GEDCOMCalendar.dcHebrew:
					month = GEDCOMMonthHebrewToInt(this.fMonth);
					break;
					
				case GEDCOMCalendar.dcFrench:
					month = GEDCOMMonthFrenchToInt(this.fMonth);
					break;

			    default:
					month = GEDCOMMonthToInt(this.fMonth);
					break;
			}

			day = this.fDay;
			yearBC = this.fYearBC;
		}

        public void SetGregorian(ushort day, ushort month, int year)
		{
			this.SetGregorian(day, IntToGEDCOMMonth(month), year, "", false);
		}

		public void SetGregorian(ushort day, string month, int year, string yearModifier, bool yearBC)
		{
			this.fDateCalendar = GEDCOMCalendar.dcGregorian;
			this.fYear = year;
			this.fYearBC = yearBC;
			this.fYearModifier = yearModifier;
			this.fDay = day;
			this.fMonth = CheckGEDCOMMonth(month);
		}

        public void SetJulian(ushort day, string month, int year, bool yearBC)
		{
			this.fDateCalendar = GEDCOMCalendar.dcJulian;
			this.fYear = year;
			this.fYearBC = yearBC;
			this.fYearModifier = "";
			this.fDay = day;
			this.fMonth = CheckGEDCOMMonth(month);
		}

		public void SetHebrew(ushort day, ushort month, int year)
		{
			this.SetHebrew(day, IntToGEDCOMMonthHebrew(month), year, false);
		}

		public void SetHebrew(ushort day, string month, int year, bool yearBC)
		{
			this.fDateCalendar = GEDCOMCalendar.dcHebrew;
			this.fYear = year;
			this.fYearBC = yearBC;
			this.fYearModifier = "";
			this.fDay = day;
			this.fMonth = CheckGEDCOMMonthHebrew(month);
		}

        public void SetFrench(ushort day, ushort month, int year)
        {
            this.SetFrench(day, IntToGEDCOMMonthFrench(month), year, false);
        }

        public void SetFrench(ushort day, string month, int year, bool yearBC)
		{
			this.fDateCalendar = GEDCOMCalendar.dcFrench;
			this.fYear = year;
			this.fYearBC = yearBC;
			this.fYearModifier = "";
			this.fDay = day;
			this.fMonth = CheckGEDCOMMonthFrench(month);
		}

        public void SetRoman(ushort day, string month, int year, bool yearBC)
		{
			this.fDateCalendar = GEDCOMCalendar.dcRoman;
			this.fYear = year;
			this.fYearBC = yearBC;
			this.fYearModifier = "";
			this.fDay = day;
			this.fMonth = CheckGEDCOMMonth(month);
		}

        public void SetUnknown(ushort day, string month, int year, bool yearBC)
		{
			this.fDateCalendar = GEDCOMCalendar.dcUnknown;
			this.fYear = year;
			this.fYearBC = yearBC;
			this.fYearModifier = "";
			this.fDay = day;
			this.fMonth = CheckGEDCOMMonth(month);
		}

		public override void Clear()
		{
			this.fDateCalendar = GEDCOMCalendar.dcGregorian;
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
			this.fDateCalendar = GEDCOMCalendar.dcGregorian;
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
						this.fDateFormat = GEDCOMDateFormat.dfGEDCOMStd;
					}
					else
					{
						if (result[0] == '.')
						{
							this.fDateFormat = GEDCOMDateFormat.dfSystem;
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

		public GEDCOMDate(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

		// TODO: temp implementation
		public bool IsValid()
		{
			return (this.fYear > 0 && this.fMonth != "" && this.fDay > 0);
		}

		public override double GetUDN()
		{
			return double.NaN;
		}

		public override AbsDate GetAbstractDate()
		{
			int year;
			ushort month, day;
			bool yearBC;
			this.GetDateParts(out year, out month, out day, out yearBC);

			AbsDate result;

			if (year == -1) {
				result = AbsDate.Empty(); // it's empty date, as negative dates has yearBC-attribute
			} else {
				result = new AbsDate(year, month, day, yearBC);
			}

			return result;
		}
	}
}
