using System;

namespace GKCommon.GEDCOM
{
	public abstract class GEDCOMCustomDate : GEDCOMTag, IComparable
	{
		public static readonly string[] GEDCOMDateApproximatedArray;
		public static readonly string[] GEDCOMDateRangeArray;
		public static readonly string[] GEDCOMDateEscapeArray;
		public static readonly string[] GEDCOMMonthRusArray;
		public static readonly string[] GEDCOMMonthSysArray;
		public static readonly string[] GEDCOMMonthArray;
		public static readonly string[] GEDCOMMonthFrenchArray;
		public static readonly string[] GEDCOMMonthHebrewArray;

		public DateTime Date
		{
			get { return this.GetDateTime(); }
			set { this.SetDateTime(value); }
		}

		static GEDCOMCustomDate()
		{
			GEDCOMDateApproximatedArray = new string[] { "", "ABT", "CAL", "EST" };
			GEDCOMDateRangeArray = new string[] { "AFT", "BEF", "BET", "AND" };

			GEDCOMMonthHebrewArray = new string[]
			{
				"TSH", "CSH", "KSL", "TVT", "SHV", "ADR", 
				"ADS", "NSN", "IYR", "SVN", "TMZ", "AAV", "ELL"
			};

			GEDCOMMonthFrenchArray = new string[]
			{
				"VEND", "BRUM", "FRIM", "NIVO", "PLUV", "VENT", 
				"GERM", "FLOR", "PRAI", "MESS", "THER", "FRUC", "COMP"
			};

			GEDCOMMonthArray = new string[]
			{
				"JAN", "FEB", "MAR", "APR", "MAY", "JUN", 
				"JUL", "AUG", "SEP", "OCT", "NOV", "DEC"
			};

			GEDCOMMonthSysArray = new string[]
			{
				"01.", "02.", "03.", "04.", "05.", "06.", 
				"07.", "08.", "09.", "10.", "11.", "12."
			};

			GEDCOMMonthRusArray = new string[]
			{
				"ﬂÕ¬", "‘≈¬", "Ã¿–", "¿œ–", "Ã¿…", "»ﬁÕ", 
				"»ﬁÀ", "¿¬√", "—≈Õ", "Œ “", "ÕŒﬂ", "ƒ≈ "
			};

			GEDCOMDateEscapeArray = new string[]
			{
				"@#DGREGORIAN@", "@#DJULIAN@", "@#DHEBREW@", "@#DFRENCH R@", "@#DROMAN@", "@#DUNKNOWN@"
			};
		}

		protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fName = "DATE";
		}

	    protected GEDCOMCustomDate(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

		public abstract DateTime GetDateTime();
		public abstract void SetDateTime(DateTime value);

		public abstract AbsDate GetAbstractDate();
		public abstract void GetDateParts(out int year, out ushort month, out ushort day, out bool yearBC);

		public int CompareTo(object obj)
		{
			GEDCOMCustomDate otherDate = obj as GEDCOMCustomDate;

			if (otherDate != null) {
				AbsDate abs1 = this.GetAbstractDate();
				AbsDate abs2 = otherDate.GetAbstractDate();
				return abs1.CompareTo(abs2);
			} else {
				return -1;
			}
		}
	}
}
