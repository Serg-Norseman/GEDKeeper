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
		public static int ParseInt(string str, int Default)
		{
			int res;
			if (!int.TryParse(str, out res)) res = Default;
			return res;
		}

		public static double ParseFloat(string str, double Default, bool checkSeparator = false)
		{
			if (string.IsNullOrEmpty(str)) return Default;

			string decSep;
			if (checkSeparator) {
				decSep = (str.Contains(",") ? "," : ".");
			} else {
				decSep = ".";
			}

            NumberFormatInfo formatInfo = (NumberFormatInfo)Thread.CurrentThread.CurrentCulture.NumberFormat.Clone();
			formatInfo.NumberDecimalSeparator = decSep;
			formatInfo.NumberGroupSeparator = " ";

			double value;
			double result;
			if (double.TryParse(str, NumberStyles.Float, formatInfo, out value)) {
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
