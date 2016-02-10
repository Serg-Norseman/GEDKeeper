using System;
using System.Globalization;
using System.Text;
using System.Threading;

namespace BSLib
{
	/// <summary>
	/// Description of ConvHelper.
	/// </summary>
	public static class ConvHelper
	{
		public static int ParseInt(string S, int Default)
		{
			int res;
			if (!int.TryParse(S, out res)) res = Default;
			return res;
		}

		public static double ParseFloat(string S, double Default, bool checkSeparator = false)
		{
			if (string.IsNullOrEmpty(S)) return Default;

			string decSep;
			if (checkSeparator) {
				decSep = (S.Contains(",") ? "," : ".");
			} else {
				decSep = ".";
			}

            NumberFormatInfo formatInfo = (NumberFormatInfo)Thread.CurrentThread.CurrentCulture.NumberFormat.Clone();
			formatInfo.NumberDecimalSeparator = decSep;
			formatInfo.NumberGroupSeparator = " ";

			double value;
			double result;
			if (double.TryParse(S, NumberStyles.Float, formatInfo, out value)) {
				result = value;
			} else {
				result = Default;
			}
			return result;
		}

		public static string AdjustNum(int val, int up)
		{
			string result = val.ToString();
			if (result.Length < up)
			{
				StringBuilder sb = new StringBuilder(result);
				while (sb.Length < up) sb.Insert(0, '0');
				result = sb.ToString();
			}
			return result;
		}
	}
}
