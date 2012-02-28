using System;
using System.Globalization;
using System.IO;
using System.Net;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Security;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;

using GKCore;
using MapiMail;

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

		public static ushort DaysInAMonth([In] ushort AYear, [In] ushort AMonth)
		{
			return MonthDays[(AMonth == 2 && DateTime.IsLeapYear((int)AYear)) ? 1 : 0][(int)AMonth - 1];
		}

		public static string GetIPAndCompName()
		{
			string hostName = Dns.GetHostName();
			IPHostEntry hostEntry = Dns.GetHostEntry(hostName);
			string str = hostEntry.AddressList[0].ToString();
			return str + "," + hostName;
		}

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
			return ((AValue > (double)0f) ? Math.Floor(AValue) : Math.Ceiling(AValue));
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

		public static int StrToIntDef([In] string S, int Default)
		{
			int res;
			if (!int.TryParse(S, out res)) res = Default;
			return res;
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
			byte[] result = null;
			int num = ((L != null) ? L.Length : 0);
			int num2 = ((R != null) ? R.Length : 0);
			if (num + num2 > 0)
			{
				result = new byte[num + num2];
				if (num > 0) Array.Copy(L, 0, result, 0, num);
				if (num2 > 0) Array.Copy(R, 0, result, num, num2);
			}
			return result;
		}

		public static byte[] LStrCopy([In] byte[] S, int Index1, int Count)
		{
			byte[] result = null;
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
						result = new byte[Count];
						Array.Copy(S, idx, result, 0, Count);
					}
				}
			}
			return result;
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
					int num2 = ((Index1 <= 0) ? 0 : Index1 - 1);

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

		public static Regex InitMaskRegex([In] string Mask)
		{
			// convert mask to regular expression
			string regex_str = "";
			int CurPos = 0;
			int Len = (Mask != null) ? Mask.Length : 0;
			if (CurPos < Len)
			{
				do
				{
					int I = Mask.IndexOfAny("*?".ToCharArray(), CurPos);
					if (I < CurPos) break;
					if (I > CurPos) regex_str += Regex.Escape(WStrCopy(Mask, CurPos + 1, I - CurPos));

					char c = Mask[I];
					switch (c) {
						case '*':
							regex_str += ".*";
							break;
						case '?':
							regex_str += ".";
							break;
					}

					CurPos = I + 1;
				}
				while (CurPos < Len);
			}
			if (CurPos < Len) regex_str += Regex.Escape(WStrCopy(Mask, CurPos + 1, Len - CurPos));

			// create regex
			return new Regex(regex_str, RegexOptions.IgnoreCase);
		}

		public static bool MatchesRegex([In] string S, Regex regex)
		{
			return ((regex != null) ? regex.IsMatch(S) : false);
		}

		public static bool MatchesMask([In] string S, [In] string Mask)
		{
			Regex regex = InitMaskRegex(Mask);
			return MatchesRegex(S, regex);
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

		public static string GetAppDataPath()
		{
			string path = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData) + "\\" + TGenEngine.AppTitle + "\\";
			if (!Directory.Exists(path)) Directory.CreateDirectory(path);
			return path;
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
			return ((aDivisor == (double)0f) ? 0.0 : (aDividend / aDivisor));
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
			string result = val.ToString();
			if (result.Length < up)
			{
				StringBuilder sb = new StringBuilder(result);
				while (sb.Length < up) sb.Insert(0, '0');
				result = sb.ToString();
			}
			return result;
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

		public static void LogSend()
		{
			if (File.Exists(LogFilename)) {
				MapiMailMessage message = new MapiMailMessage("GEDKeeper: error notification", "This automatic notification of error.");
				message.Recipients.Add("serg.zhdanovskih@gmail.com");
				message.Files.Add(LogFilename);
				message.ShowDialog();
			} else {
				TGenEngine.ShowMessage("Журнал ошибок пуст или не существует");
			}
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
			double Val, Val2;
			bool v = double.TryParse(Str1, out Val);
			bool v2 = double.TryParse(Str2, out Val2);

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
			TimeSpan span = ((ANow < AThen) ? AThen - ANow : ANow - AThen);
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

		public static double DoubleParse(string s)
		{
			if (s == null || s == "") return 0.0;

			NumberFormatInfo nfi = new NumberFormatInfo();
			nfi.NumberDecimalSeparator = ".";
			return double.Parse(s, nfi);
		}

		public static string SetAsName(string s)
		{
			string st = s.ToLower();
			char f = Char.ToUpper(st[0]);
			st = f + st.Substring(1);
			return st;
		}

		/*public static void SendMail(string smtpHost, MailAddress from, string to, string subject, string message, Attachment attach)
		{
			char[] charSeparators = new char[0];
			string[] result;

			try
			{
				MailMessage mailMessage = new MailMessage();
				result = to.Split(charSeparators, StringSplitOptions.RemoveEmptyEntries);
				for (int i = 0; i < result.Length; i++)
				{
					mailMessage.To.Add(new MailAddress(result[i]));
				}
				mailMessage.From = from;
				mailMessage.Subject = subject;
				mailMessage.Body = message;
				mailMessage.Priority = MailPriority.Normal;
				mailMessage.BodyEncoding = Encoding.ASCII;
				mailMessage.IsBodyHtml = false;
				if (attach != null)
				{
					mailMessage.Attachments.Add(attach);
				}
				new SmtpClient(smtpHost)
				{
					Credentials = CredentialCache.DefaultNetworkCredentials
				}.Send(mailMessage);

				MessageBox.Show("Your birthday card has been sent");
			}
			catch (Exception ex)
			{
				MessageBox.Show(ex.Message);
			}
		}*/

	}
}
