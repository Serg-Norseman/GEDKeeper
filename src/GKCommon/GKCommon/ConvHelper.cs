using System.Globalization;
using System.Text;
using System.Threading;

namespace GKCommon
{
	/// <summary>
	/// Description of ConvHelper.
	/// </summary>
	public static class ConvHelper
	{
		private static readonly int[] RN_N;
		private static readonly string[] RN_S;

		static ConvHelper()
		{
			RN_N = new int[]	{ 1, 4, 5, 9, 10, 40, 50, 90, 100, 400, 500, 900, 1000 };
			RN_S = new string[]	{ "I", "IV", "V", "IX", "X", "XL", "L", "XC", "C", "CD", "D", "CM", "M" };
		}

		public static string GetRome(int num)
		{
			string rome = "";
			int T = 12;

			if (num > 0)
			{
				while (true)
				{
					int rn = RN_N[T];
					
					if (num >= rn) {
						while (num >= rn) {
							num -= rn;
							rome += RN_S[T];
						}

						if (num <= 0) break;
					} else {
						T -= 1;
					}
				}
			}
			return rome;
		}

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
