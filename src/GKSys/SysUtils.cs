using System;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Security;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;

/// <summary>
/// Localization: unknown
/// </summary>

namespace GKSys
{
	public class EMaskException : Exception
	{
		public EMaskException()
		{
		}
		public EMaskException(string message) : base(message)
		{
		}
	}

	public class EConvertError : Exception
	{
		public EConvertError()
		{
		}
		public EConvertError(string message) : base(message)
		{
		}
	}

	[StructLayout(LayoutKind.Sequential, Pack = 1, Size = 1)]
	public struct RomeData
	{
		public static readonly int[] Rn_N;
		public static readonly string[] Rn_S;

		static RomeData()
		{
			RomeData.Rn_N = new int[]
			{
				1, 
				4, 
				5, 
				9, 
				10, 
				40, 
				50, 
				90, 
				100, 
				400, 
				500, 
				900, 
				1000
			};

			RomeData.Rn_S = new string[]
			{
				"I", 
				"IV", 
				"V", 
				"IX", 
				"X", 
				"XL", 
				"L", 
				"XC", 
				"C", 
				"CD", 
				"D", 
				"CM", 
				"M"
			};
		}
	}

	public sealed class SysUtils
	{
		public static readonly ushort[][] MonthDays = new ushort[][]
		{
			new ushort[] { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 }, 
			new ushort[] { 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 }
		};

		public static void Free(object Self)
		{
			if (Self != null && Self is IDisposable)
			{
				((IDisposable)Self).Dispose();
			}
		}

		public static long Trunc([In] double AValue)
		{
			return Convert.ToInt64(Int(AValue));
		}

		public static double Int([In] double AValue)
		{
			double result;
			if (AValue > (double)0f)
			{
				result = Math.Floor(AValue);
			}
			else
			{
				result = Math.Ceiling(AValue);
			}
			return result;
		}

		public static double Frac([In] double AValue)
		{
			return (AValue - Int(AValue));
		}

		public static int Floor([In] double X)
		{
			return Convert.ToInt32(Math.Floor(X));
		}

		public static int Ceil([In] double X)
		{
			return Convert.ToInt32(Math.Ceiling(X));
		}

		public static int ValLong([In] string s, out int code)
		{
			int res;
			if (int.TryParse(s, out res)) { code = 0; } else { code = -1; }
			return res;
		}

		public static double ValExt([In] string s, out int code)
		{
			double res;
			if (double.TryParse(s, out res)) { code = 0; } else { code = -1; }
			return res;
		}

		public static int StrToIntDef([In] string S, int Default)
		{
			int E;
			int Result = ValLong(S, out E);
			if (E != 0)
			{
				Result = Default;
			}
			return Result;
		}

		public static string ParamStr(int Index)
		{
			string result;
			if (Index == 0)
			{
				Assembly entryAssembly = Assembly.GetEntryAssembly();
				if (entryAssembly != null)
				{
					result = entryAssembly.Location;
				}
				else
				{
					result = Process.GetCurrentProcess().MainModule.FileName;
				}
			}
			else
			{
				string[] commandLineArgs = Environment.GetCommandLineArgs();
				if (Index > ((commandLineArgs != null) ? commandLineArgs.Length : 0) - 1)
				{
					result = "";
				}
				else
				{
					result = commandLineArgs[Index];
				}
			}
			return result;
		}

		public static int Pos([In] string substr, [In] string str)
		{
			int result;
			if (str == null || str.Length == 0 || substr == null || substr.Length == 0)
			{
				result = 0;
			}
			else
			{
				result = str.IndexOf(substr) + 1;
			}
			return result;
		}

		public static int GetLastError()
		{
			return Marshal.GetLastWin32Error();
		}

		public static byte[] LStrConcat2([In] byte[] L, [In] byte[] R)
		{
			byte[] array = null;
			int num = ((L != null) ? L.Length : 0);
			int num2 = ((R != null) ? R.Length : 0);
			if (num + num2 > 0)
			{
				array = new byte[num + num2];
				if (num > 0)
				{
					Array.Copy(L, 0, array, 0, num);
				}
				if (num2 > 0)
				{
					Array.Copy(R, 0, array, num, num2);
				}
			}
			return array;
		}

		public static byte[] LStrCopy([In] byte[] S, int Index1, int Count)
		{
			byte[] array = null;
			if (Count > 0)
			{
				int num = ((S != null) ? S.Length : 0);
				if (num > 0 && Index1 <= num)
				{
					int idx = ((Index1 <= 0) ? 0 : Index1 - 1);

					if (Count > num - idx)
					{
						Count = num - idx;
					}

					if (Count > 0)
					{
						array = new byte[Count];
						Array.Copy(S, idx, array, 0, Count);
					}
				}
			}
			return array;
		}

		public static string WStrCopy([In] string S, int Index1, int Count)
		{
			string result = "";
			if (!string.IsNullOrEmpty(S) && Count > 0)
			{
				int len = S.Length;
				if (len > 0 && Index1 <= len)
				{
					int idx = ((Index1 <= 0) ? 0 : Index1 - 1);

					if (Count > len - idx)
					{
						Count = len - idx;
					}

					if (Count > 0)
					{
						result = S.Substring(idx, Count);
					}
				}
			}
			return result;
		}

		public static void LStrDelete(ref byte[] Dest, int Index1, int Count)
		{
			if (Count > 0)
			{
				int num = ((Dest != null) ? Dest.Length : 0);
				if (num > 0 && Index1 <= num)
				{
					int num2;
					if (Index1 <= 0)
					{
						num2 = 0;
					}
					else
					{
						num2 = Index1 - 1;
					}
					if (Count > num - num2)
					{
						Count = num - num2;
					}
					if (Count > 0)
					{
						int num3 = num - Count;
						if (num3 < 0)
						{
							num3 = 0;
						}
						byte[] array = new byte[num3];
						if (num2 > 0)
						{
							Array.Copy(Dest, 0, array, 0, num2);
						}
						if (num2 + Count < num)
						{
							Array.Copy(Dest, num2 + Count, array, num2, num - Count - num2);
						}
						Dest = array;
					}
				}
			}
		}

		public static string StrToUtf8([In] string S)
		{
			byte[] src = Encoding.GetEncoding(1251).GetBytes(S);
			return Encoding.UTF8.GetString(src);
		}



		[SuppressUnmanagedCodeSecurity]
		[DllImport("hhctrl.ocx", CharSet = CharSet.Unicode, EntryPoint = "HtmlHelpW", SetLastError = true)]
		public static extern uint HtmlHelp(IntPtr hwndCaller, string pszFile, uint uCommand, uint dwData);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("shell32.dll", CharSet = CharSet.Auto, SetLastError = true)]
		public static extern int ShellExecute(uint hWnd, string Operation, string FileName, string Parameters, string Directory, int ShowCmd);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("user32.dll", CharSet = CharSet.Ansi, SetLastError = true)]
		public static extern LongBool ScrollWindowEx(uint hWnd, int dx, int dy, [In] ref TRect prcScroll, [In] ref TRect prcClip, uint hrgnUpdate, out TRect prcUpdate, uint flags);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("user32.dll", CharSet = CharSet.Ansi, SetLastError = true)]
		public static extern LongBool SetScrollRange(uint hWnd, int nBar, int nMinPos, int nMaxPos, LongBool bRedraw);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("user32.dll", CharSet = CharSet.Ansi, SetLastError = true)]
		public static extern LongBool GetScrollInfo(uint hWnd, int BarFlag, ref TScrollInfo ScrollInfo);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("user32.dll", CharSet = CharSet.Ansi, SetLastError = true)]
		public static extern LongBool InvalidateRect(uint hWnd, [In] ref TRect lpRect, LongBool bErase);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("user32.dll", CharSet = CharSet.Ansi, SetLastError = true)]
		public static extern int SetScrollPos(uint hWnd, int nBar, int nPos, LongBool bRedraw);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("user32.dll", CharSet = CharSet.Ansi, SetLastError = true)]
		public static extern uint GetKeyboardLayout(uint dwLayout);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("user32.dll", CharSet = CharSet.Ansi, SetLastError = true)]
		public static extern uint ActivateKeyboardLayout(uint hkl, uint Flags);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("user32.dll", CharSet = CharSet.Auto, SetLastError = true)]
		public static extern LongBool PostMessage(uint hWnd, uint Msg, int wParam, int lParam);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("user32.dll", CharSet = CharSet.Ansi, SetLastError = true)]
		public static extern LongBool EnableWindow(uint hWnd, LongBool bEnable);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("user32.dll", SetLastError = true)]
		public static extern IntPtr FindWindow(string lpClassName, string lpWindowName);



		public static int HInstance()
		{
			return (int)Marshal.GetHINSTANCE(Assembly.GetCallingAssembly().GetModules()[0]);
		}

		public static string TrimLeft([In] string S)
		{
			int L = (S != null) ? S.Length : 0;
			int I = 1;
			while (I <= L && S[I - 1] <= ' ') I++;

			string Result;
			if (I > L) {
				Result = "";
			} else {
				Result = ((I != 1) ? S.Substring(I - 1) : S);
			}
			return Result;
		}

		public static string TrimRight([In] string S)
		{
			int L = (S != null) ? S.Length : 0;
			int I = L;
			while (I > 0 && S[I - 1] <= ' ') I--;

			string Result = ((I != L) ? S.Substring(0, I) : S);
			return Result;
		}

		private static string ConvertMaskToRegularExpression([In] string Mask)
		{
			string result = "";
			int CurPos = 0;
			int Len = (Mask != null) ? Mask.Length : 0;
			if (CurPos < Len)
			{
				do
				{
					int I = Mask.IndexOfAny("*?".ToCharArray(), CurPos);
					if (I < CurPos) break;
					if (I > CurPos) result += Regex.Escape(WStrCopy(Mask, CurPos + 1, I - CurPos));

					char c = Mask[I];
					switch (c) {
						case '*':
							result += ".*";
							break;
						case '?':
							result += ".";
							break;
					}

					CurPos = I + 1;
				}
				while (CurPos < Len);
			}
			if (CurPos < Len) result += Regex.Escape(WStrCopy(Mask, CurPos + 1, Len - CurPos));
			return result;
		}

		public static bool MatchesMask([In] string S, [In] string Mask)
		{
			bool Result = false;
			Regex regex = new Regex(ConvertMaskToRegularExpression(Mask), RegexOptions.IgnoreCase);
			try
			{
				Match match = regex.Match(S);
				GroupCollection Groups = match.Groups;
				int num = Groups.Count - 1;
				for (int I = 0; I <= num; I++)
				{
					Group Group = Groups[I];
					if (Group.Success)
					{
						int num2 = Group.Captures.Count - 1;
						for (int J = 0; J <= num2; J++)
						{
							Capture Capture = Group.Captures[J];
							if (string.Compare(Capture.Value, S, true) == 0)
							{
								Result = true;
								break;
							}
						}
						if (Result) break;
					}
				}
			}
			finally
			{
				//FMask.Dispose();
			}
			return Result;
		}


		private static void MoveL2S([In] uint Source, ref byte[] Dest, int count)
		{
			byte[] bytes = new byte[4];

			unchecked
			{
				ushort wl = (ushort)(Source);
				ushort wh = (ushort)(Source >> 16);

				bytes[0] = (byte)wl;
				bytes[1] = (byte)(wl >> 8);
				bytes[2] = (byte)wh;
				bytes[3] = (byte)(wh >> 8);
			}

			if (Dest != null) {
				for (int I = 0; I < count; I++) Dest[I] = bytes[I];
			}
		}

		private static void MoveS2L([In] byte[] Source, ref int Dest, int count)
		{
			byte[] bytes = new byte[4];
			for (int I = 1; I <= 4; I++) {
				if (I <= count) {
					bytes[I - 1] = Source[I - 1];
				} else {
					bytes[I - 1] = 0;
				}
			}

			Dest = (int)((bytes[0] | bytes[1] << 8) | (bytes[2] | bytes[3] << 8) << 16);
		}

		private static byte[] Decode([In] byte[] data)
		{
			int num = (data != null) ? data.Length : 0;
			byte[] result = null;
			uint I;

			switch (num) {
				case 2:
					I = (uint)(_Unnamed1.Map[data[0]] + (_Unnamed1.Map[data[1]] << 6));
					result = new byte[1];
					MoveL2S(I, ref result, 1);
					break;
				case 3:
					I = (uint)(_Unnamed1.Map[data[0]] + (_Unnamed1.Map[data[1]] << 6) + (_Unnamed1.Map[data[2]] << 12));
					result = new byte[2];
					MoveL2S(I, ref result, 2);
					break;
				case 4:
					I = (uint)(_Unnamed1.Map[data[0]] + (_Unnamed1.Map[data[1]] << 6) + (_Unnamed1.Map[data[2]] << 12) + (_Unnamed1.Map[data[3]] << 18));
					result = new byte[3];
					MoveL2S(I, ref result, 3);
					break;
			}
			
			return result;
		}

		private static byte[] Encode([In] byte[] data)
		{
			int I = 0;
			int num = (data != null) ? data.Length : 0;
			MoveS2L(data, ref I, num);

			byte[] res = new byte[num + 1];

			switch (num) {
				case 1:
					res[0] = (byte)_Unnamed2.Map[I % 64];
					res[1] = (byte)_Unnamed2.Map[((uint)I >> 6) % 64];
					break;
				case 2:
					res[0] = (byte)_Unnamed2.Map[I % 64];
					res[1] = (byte)_Unnamed2.Map[((uint)I >> 6) % 64];
					res[2] = (byte)_Unnamed2.Map[((uint)I >> 12) % 64];
					break;
				case 3:
					res[0] = (byte)_Unnamed2.Map[I % 64];
					res[1] = (byte)_Unnamed2.Map[((uint)I >> 6) % 64];
					res[2] = (byte)_Unnamed2.Map[((uint)I >> 12) % 64];
					res[3] = (byte)_Unnamed2.Map[((uint)I >> 18) % 64];
					break;
			}
			
			return res;
		}

		public static string scDecrypt([In] string St, ushort Key)
		{
			string res = "";

			if (!string.IsNullOrEmpty(St))
			{
				byte[] SSD = Encoding.ASCII.GetBytes(St);
				byte[] ppd = null;
				while (SSD.Length != 0)
				{
					byte[] sd = LStrCopy(SSD, 1, 4);
					ppd = LStrConcat2(ppd, Decode(sd));
					LStrDelete(ref SSD, 1, 4);
				}

				byte[] tmp = (byte[])ppd.Clone();

				ushort Seed = Key;
				for (int I = 1; I <= ppd.Length; I++)
				{
					tmp[I - 1] = (byte)((uint)tmp[I - 1] ^ (uint)Seed >> 8);
					Seed = unchecked((ushort)(((uint)ppd[I - 1] + (uint)Seed) * 28732u + 28446u));
				}
				res = Encoding.ASCII.GetString(tmp);
			}

			return res;
		}

		public static string scEncrypt([In] string St, ushort Key)
		{
			string res = "";

			if (!string.IsNullOrEmpty(St))
			{
				ushort Seed = Key;
				byte[] idata = Encoding.ASCII.GetBytes(St);
				for (int I = 1; I <= idata.Length; I++)
				{
					idata[I - 1] = (byte)((uint)idata[I - 1] ^ (uint)Seed >> 8);
					Seed = unchecked((ushort)(((uint)idata[I - 1] + (uint)Seed) * 28732u + 28446u));
				}

				byte[] res_data = null;

				while (idata.Length != 0)
				{
					byte[] sd = LStrCopy(idata, 1, 3);
					res_data = LStrConcat2(res_data, Encode(sd));
					LStrDelete(ref idata, 1, 3);
				}
				res = Encoding.ASCII.GetString(res_data);
			}

			return res;
		}

		public static string GetTempDir()
		{
			return Environment.GetEnvironmentVariable("TEMP");
		}

		public static string GetAppPath()
		{
			Module[] mods = System.Reflection.Assembly.GetExecutingAssembly().GetModules();
			string fn = mods[0].FullyQualifiedName;
			return Path.GetDirectoryName(fn) + "\\";
		}

		public static void LoadExtFile([In] string aFileName)
		{
			ShellExecute(0, "open", aFileName, "", "", 5);
		}

		public static double SafeDiv(double aDividend, double aDivisor)
		{
			double Result;
			if (aDivisor == (double)0f)
			{
				Result = 0.0;
			}
			else
			{
				Result = (aDividend / aDivisor);
			}
			return Result;
		}

		public static string GetRome(int N)
		{
			string S = "";
			byte T = 13;
			if (N > 0)
			{
				while (true)
				{
					if (N >= RomeData.Rn_N[(int)T - 1])
					{
						while (N >= RomeData.Rn_N[(int)T - 1])
						{
							N -= RomeData.Rn_N[(int)T - 1];
							S += RomeData.Rn_S[(int)T - 1];
						}
						if (N <= 0)
						{
							break;
						}
					}
					else
					{
						T -= 1;
					}
				}
			}
			return S;
		}

		public static string NumUpdate(int val, int up)
		{
			string Result = val.ToString();
			while (Result.Length < up)
			{
				Result = "0" + Result;
			}
			return Result;
		}

		private static string LogFilename;

		public static void LogInit([In] string aFileName)
		{
			LogFilename = aFileName;
		}

		public static void LogWrite([In] string aMsg)
		{
			StreamWriter Log = new StreamWriter(LogFilename, true, Encoding.GetEncoding(1251));
			Log.WriteLine("[" + DateTime.Now.ToString() + "] -> " + aMsg);
			Log.Flush();
			Log.Close();
		}

		public static string ConStrings(StringList aStrings)
		{
			string Result = "";
			int num = aStrings.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				if (Result != "") Result += " ";
				Result += aStrings[i].Trim();
			}
			return Result;
		}

		private static uint[] Ccitt32Table = new uint[256];

		public static void BuildCRCTable()
		{
			unchecked
			{
				uint i = 0u;
				do
				{
					uint val = i;
					uint j = 4294967288u;
					do
					{
						if ((val & 1u) != 0u)
						{
							val = (val >> 1 ^ 3988292384u);
						}
						else
						{
							val >>= 1;
						}
						j += 1u;
					}
					while (j != 0u);
					Ccitt32Table[(int)i] = val;
					i += 1u;
				}
				while (i != 256u);
			}
		}

		public static uint CrcStr([In] string Str)
		{
			uint crc = 0u;
			int num = (Str != null) ? Str.Length : 0;
			for (int i = 1; i <= num; i++)
			{
				byte c = (byte)Str[i - 1];
				crc = ((crc >> 8 & 16777215u) ^ Ccitt32Table[(int)((crc ^ (uint)c) & 255u)]);
			}
			return crc;
		}

		public static int agCompare(string Str1, string Str2)
		{
			int NumCode;
			double Val = ValExt(Str1, out NumCode);
			bool v = (NumCode == 0);
			double Val2 = ValExt(Str2, out NumCode);
			bool v2 = (NumCode == 0);

			int Result;
			if (v && v2)
			{
				if (Val < Val2)
				{
					Result = -1;
				}
				else
				{
					if (Val > Val2)
					{
						Result = 1;
					}
					else
					{
						Result = 0;
					}
				}
			}
			else
			{
				Result = string.Compare(Str1, Str2, false);
				if (Str1 != "" && Str2 == "")
				{
					Result = -1;
				}
				if (Str1 == "" && Str2 != "")
				{
					Result = 1;
				}
			}
			return Result;
		}

		public static double StrToFloatDef([In] string S, [In] double Default)
		{
			NumberFormatInfo LFormat = Thread.CurrentThread.CurrentCulture.NumberFormat.Clone() as NumberFormatInfo;
			LFormat.NumberDecimalSeparator = ".";
			LFormat.NumberGroupSeparator = " ";

			double Value;
			double Result;
			if (double.TryParse(S, NumberStyles.Float, LFormat, out Value)) {
				Result = Value;
			} else {
				Result = Default;
			}
			return Result;
		}

		public static int DaysBetween([In] DateTime ANow, [In] DateTime AThen)
		{
			TimeSpan span;
			if (ANow < AThen) {
				span = AThen - ANow;
			} else {
				span = ANow - AThen;
			}
			return span.Days;
		}

		public static bool IsDigit(char C)
		{
			return C >= '0' && C <= '9';
		}

		public static bool IsDigits([In] string S)
		{
			bool res = false;

			if (!string.IsNullOrEmpty(S))
			{
				int I;
				for (I = 1; I <= S.Length; I++)
				{
					char c = S[I - 1];
					if (c < '0' || c >= ':')
					{
						break;
					}
				}
				res = (I > S.Length);
			}

			return res;
		}

		public static int StrToInt([In] string S)
		{
			int E;
			int Result = ValLong(S, out E);
			if (E != 0)
			{
				throw new EConvertError(string.Format("'{0}' is not a valid integer value", new object[] { S }));
			}
			return Result;
		}

		public static double DoubleParse(string s) {
			if (s == null || s == "") return 0.0;

			NumberFormatInfo nfi = new NumberFormatInfo();
			nfi.NumberDecimalSeparator = ".";
			return double.Parse(s, nfi);
		}

		public static string SetAsName(string s) {
			string st = s.ToLower();
			char f = Char.ToUpper(st[0]);
			st = f + st.Substring(1);
			return st;
		}
		
		/// <summary>
		/// Swap two values
		/// </summary>
		/// <typeparam name="T">type of values</typeparam>
		/// <param name="a">fisrt value</param>
		/// <param name="b">second value</param>
		private static void Swap<T>(ref T a, ref T b)
		{
			T tmp = a; a = b; b = tmp;
		}

		/// <summary>
		/// Function to sort array by quick sort method
		/// </summary>
		/// <typeparam name="T">type of sorted data</typeparam>
		/// <param name="array">sorted array</param>
		public static void QuickSort<T>(T[] array)
			where T : IComparable<T>
		{
			QuickSort<T>(array, 0, array.Length - 1);
		}

		/// <summary>
		/// Function to sort array by quick sort method
		/// </summary>
		/// <typeparam name="T">type of sorted data</typeparam>
		/// <param name="array">sorted array</param>
		/// <param name="left">begin of unsorted part</param>
		/// <param name="right">end of unsorted part</param>
		public static void QuickSort<T>(T[] array, int left, int right)
			where T : IComparable<T>
		{
			if (left < right)
			{
				System.Random rnd = new Random();
				T x = array[rnd.Next(left, right)];
				int i = left, j = right;
				while (i <= j)
				{
					while (array[i].CompareTo(x) < 0) ++i;
					while (array[j].CompareTo(x) > 0) --j;
					if (i <= j)
					{
						Swap(ref array[i], ref array[j]);
						++i;
						--j;
					}
				}
				QuickSort<T>(array, left, j);
				QuickSort<T>(array, i, right);
			}
		}

		/// <summary>
		/// Sorting array using MergeSort method
		/// </summary>
		/// <typeparam name="T">type of array's elements.</typeparam>
		/// <param name="array">sorting array.</param>
		public static void MergeSort<T>(T[] array)
			where T : IComparable<T>
		{
			MergeSort<T>(array, new T[array.Length], 0, array.Length - 1);
		}

		/// <summary>
		/// Sorting array using MergeSort method.
		/// </summary>
		/// <typeparam name="T">type of array's elements.</typeparam>
		/// <param name="array">sorting array.</param>
		/// <param name="tmp">temporary array that used for sorting.
		/// Size of sorted and temporary arrays must be the same.</param>
		/// <param name="left">left index, from where sorting will be started.</param>
		/// <param name="right">right index, where sorting will be finnished.</param>
		private static void MergeSort<T>(T[] array, T[] tmp, long left, long right)
			where T : IComparable<T>
		{
			if (left >= right)
			{
				return;
			};
			long mid = (left + right) / 2;
			MergeSort<T>(array, tmp, left, mid);
			MergeSort<T>(array, tmp, mid + 1, right);
			Merge<T>(array, tmp, left, mid, right);
		}

		/// <summary>
		/// Sorting array using MergeSort method.
		/// </summary>
		/// <typeparam name="T">type of array's elements.</typeparam>
		/// <param name="array">sorting array.</param>
		/// <param name="tmp">temporary array that used for sorting.
		/// Size of sorted and temporary arrays must be the same.</param>
		/// <param name="left">left index, from where sorting will be started.</param>
		/// <param name="mid">middle of sorted array.</param>
		/// <param name="right">right index, where sorting will be finnished.</param>
		private static void Merge<T>(T[] array, T[] tmp, long left, long mid, long right)
			where T : IComparable<T>
		{
			long i = left,
			j = mid + 1,
			k = left;
			while (i <= mid && j <= right)
			{
				if (array[i].CompareTo(array[j]) < 0)
				{
					tmp[k++] = array[i++];
				}
				else
				{
					tmp[k++] = array[j++];
				}
			}
			while (i <= mid) tmp[k++] = array[i++];
			while (j <= right) tmp[k++] = array[j++];
			for (i = left; i <= right; ++i) array[i] = tmp[i];
		}

		public static string TimeSpanToString(TimeSpan ts)
		{
			return string.Format("{0:00}:{1:00}:{2:00}", new object[] { ts.Hours, ts.Minutes, ts.Seconds });
		}

	}
}
