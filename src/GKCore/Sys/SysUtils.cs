using System;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Security;
using System.Security.Permissions;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
using System.Windows.Forms;

namespace GKCore.Sys
{
	[ComVisible(false)]
	public class TObjectHelper
	{
		public static void Free(object Self)
		{
			if (Self != null && Self is IDisposable)
			{
				((IDisposable)Self).Dispose();
			}
		}
	}

	public class EMaskException : Exception
	{
		public EMaskException()
		{
		}
		public EMaskException(string message) : base(message)
		{
		}
	}

	public enum E23 : byte
	{
		rfReplaceAll, rfIgnoreCase
	}

	[Flags, TSetElementType(typeof(E23))]
	public enum TReplaceFlags : byte
	{
		rfReplaceAll = 1,
		rfIgnoreCase = 2
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
			new ushort[]
			{
				31, 
				28, 
				31, 
				30, 
				31, 
				30, 
				31, 
				31, 
				30, 
				31, 
				30, 
				31
			}, 
			new ushort[]
			{
				31, 
				29, 
				31, 
				30, 
				31, 
				30, 
				31, 
				31, 
				30, 
				31, 
				30, 
				31
			}
		};

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

		public static int PosEx([In] string SubStr, [In] string S, int Offset)
		{
			int Result;
			if (Offset <= 0 || S == null || Offset > ((S != null) ? S.Length : 0))
			{
				Result = 0;
			}
			else
			{
				Result = S.IndexOf(SubStr, Offset - 1) + 1;
			}
			return Result;
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
						array = new byte[Count];
						Array.Copy(S, num2, array, 0, Count);
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
					int num2;

					if (Index1 <= 0) {
						num2 = 0;
					} else {
						num2 = Index1 - 1;
					}

					if (Count > len - num2)
					{
						Count = len - num2;
					}

					if (Count > 0)
					{
						result = S.Substring(num2, Count);
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


		public delegate void TFilePrepareProc(string FileName);
		public delegate int TSortCompareFunc(object Item1, object Item2);

		private static readonly ScrollEventType[] _events;
		private static string LogFilename;
		private static uint[] Ccitt32Table;


		[SuppressUnmanagedCodeSecurity]
		[DllImport("hhctrl.ocx", CharSet = CharSet.Unicode, EntryPoint = "HtmlHelpW", SetLastError = true)]
		[return: HWND]
		public static extern uint HtmlHelp(IntPtr hwndCaller, string pszFile, uint uCommand, uint dwData);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("shell32.dll", CharSet = CharSet.Auto, SetLastError = true)]
		[return: HINST]
		public static extern int ShellExecute([HWND] uint hWnd, string Operation, string FileName, string Parameters, string Directory, int ShowCmd);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("user32.dll", CharSet = CharSet.Ansi, SetLastError = true)]
		public static extern LongBool ScrollWindowEx([HWND] uint hWnd, int dx, int dy, [In] ref TRect prcScroll, [In] ref TRect prcClip, [HRGN] uint hrgnUpdate, out TRect prcUpdate, uint flags);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("user32.dll", CharSet = CharSet.Ansi, SetLastError = true)]
		public static extern LongBool SetScrollRange([HWND] uint hWnd, int nBar, int nMinPos, int nMaxPos, LongBool bRedraw);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("user32.dll", CharSet = CharSet.Ansi, SetLastError = true)]
		public static extern LongBool GetScrollInfo([HWND] uint hWnd, int BarFlag, ref TScrollInfo ScrollInfo);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("user32.dll", CharSet = CharSet.Ansi, SetLastError = true)]
		public static extern LongBool InvalidateRect([HWND] uint hWnd, [In] ref TRect lpRect, LongBool bErase);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("user32.dll", CharSet = CharSet.Ansi, SetLastError = true)]
		public static extern int SetScrollPos([HWND] uint hWnd, int nBar, int nPos, LongBool bRedraw);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("user32.dll", CharSet = CharSet.Ansi, SetLastError = true)]
		[return: HKL]
		public static extern uint GetKeyboardLayout(uint dwLayout);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("user32.dll", CharSet = CharSet.Ansi, SetLastError = true)]
		[return: HKL]
		public static extern uint ActivateKeyboardLayout([HKL] uint hkl, uint Flags);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("user32.dll", CharSet = CharSet.Auto, SetLastError = true)]
		public static extern LongBool PostMessage([HWND] uint hWnd, uint Msg, int wParam, int lParam);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("user32.dll", CharSet = CharSet.Ansi, SetLastError = true)]
		public static extern LongBool EnableWindow([HWND] uint hWnd, LongBool bEnable);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)]
		public static extern uint GetPrivateProfileString(string lpAppName, string lpKeyName, string lpDefault, StringBuilder lpReturnedString, uint nSize, string lpFileName);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)]
		public static extern uint GetPrivateProfileString(string lpAppName, IntPtr lpKeyName, IntPtr lpDefault, [Out] byte[] lpReturnedString, uint nSize, string lpFileName);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)]
		public static extern uint GetPrivateProfileString(IntPtr lpAppName, IntPtr lpKeyName, IntPtr lpDefault, [Out] byte[] lpReturnedString, uint nSize, string lpFileName);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)]
		public static extern LongBool WritePrivateProfileString(string lpAppName, string lpKeyName, string lpString, string lpFileName);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)]
		public static extern LongBool WritePrivateProfileString(string lpAppName, string lpKeyName, IntPtr lpString, string lpFileName);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)]
		public static extern LongBool WritePrivateProfileString(string lpAppName, IntPtr lpKeyName, IntPtr lpString, string lpFileName);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)]
		public static extern LongBool WritePrivateProfileString(IntPtr lpAppName, IntPtr lpKeyName, IntPtr lpString, string lpFileName);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)]
		public static extern int FindFirstFile(string lpFileName, out TWin32FindData lpFindFileData);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)]
		public static extern LongBool FindNextFile(int hFindFile, out TWin32FindData lpFindFileData);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)]
		public static extern LongBool FindClose(int hFindFile);


		public static void FreeAndNil(ref object Obj)
		{
			TObjectHelper.Free(Obj);
			Obj = null;
		}

		public static bool CreateDir([In] string Dir)
		{
			DirectoryInfo LInfo;// = new DirectoryInfo(Dir);
			//LInfo = LInfo.Parent;
			bool Result;// = LInfo != null && LInfo.Exists;
			//if (Result)
			//{
				LInfo = Directory.CreateDirectory(Dir);
				Result = (LInfo != null && LInfo.Exists);
			//}
			return Result;
		}

		public static int Floor([In] double X)
		{
			return Convert.ToInt32(Math.Floor(X));
		}

		public static int Ceil([In] double X)
		{
			return Convert.ToInt32(Math.Ceiling(X));
		}

		[return: HINST]
		public static int HInstance()
		{
			return (int)Marshal.GetHINSTANCE(Assembly.GetCallingAssembly().GetModules()[0]);
		}

		public static bool Supports([In] object Instance, [In] RuntimeTypeHandle IID, out object Intf)
		{
			bool Result = Type.GetTypeFromHandle(IID).IsInstanceOfType(Instance);
			Intf = null;
			if (Result)
			{
				Type typeFromHandle = Type.GetTypeFromHandle(IID);
				if (!typeFromHandle.IsInstanceOfType(Instance))
				{
					throw new Exception(/*Instance, typeFromHandle*/"Invalid cast");
				}
				Intf = (object)Instance;
			}
			return Result;
		}

		public static string TrimLeft([In] string S)
		{
			int L = (S != null) ? S.Length : 0;
			int I = 1;
			while (I <= L && S[I - 1] <= ' ')
			{
				I++;
			}
			string Result;
			if (I > L)
			{
				Result = "";
			}
			else
			{
				if (I != 1)
				{
					Result = WStrCopy(S, I, 2147483647);
				}
				else
				{
					Result = S;
				}
			}
			return Result;
		}

		public static string TrimRight([In] string S)
		{
			int L = (S != null) ? S.Length : 0;
			int I = L;
			while (I > 0 && S[I - 1] <= ' ')
			{
				I--;
			}
			string Result;
			if (I != L)
			{
				Result = WStrCopy(S, 1, I);
			}
			else
			{
				Result = S;
			}
			return Result;
		}

		private static char[] CharArrayOf([In] string AText)
		{
			return ((AText != null) ? AText.ToCharArray() : new char[0]);
		}

		private static int LastDelimiter([In] string Delimiters, [In] string S)
		{
			int Result;
			if (S != null)
			{
				Result = S.LastIndexOfAny(CharArrayOf(Delimiters)) + 1;
			}
			else
			{
				Result = 0;
			}
			return Result;
		}

		public static string ExtractFilePath([In] string FileName)
		{
			int I = LastDelimiter(new string(Path.DirectorySeparatorChar, 1) + new string(Path.VolumeSeparatorChar, 1), FileName);
			return WStrCopy(FileName, 1, I);
		}

		public static string StringReplace([In] string S, [In] string OldPattern, [In] string NewPattern, TReplaceFlags Flags)
		{
			string SearchStr;
			string Patt;
			if ((Flags & TReplaceFlags.rfIgnoreCase) != (TReplaceFlags)0)
			{
				SearchStr = S.ToUpper();
				Patt = OldPattern.ToUpper();
			}
			else
			{
				SearchStr = S;
				Patt = OldPattern;
			}
			string NewStr = S;
			StringBuilder SB = new StringBuilder();
			while (SearchStr != "")
			{
				int Offset = Pos(Patt, SearchStr);
				if (Offset == 0)
				{
					SB.Append(NewStr);
					break;
				}
				SB.Append(NewStr, 0, Offset - 1);
				SB.Append(NewPattern);
				NewStr = WStrCopy(NewStr, Offset + ((OldPattern != null) ? OldPattern.Length : 0), 2147483647);
				if ((Flags & TReplaceFlags.rfReplaceAll) == (TReplaceFlags)0)
				{
					SB.Append(NewStr);
					break;
				}
				SearchStr = WStrCopy(SearchStr, Offset + ((Patt != null) ? Patt.Length : 0), 2147483647);
			}
			return SB.ToString();
		}

		public static int FindDelimiter([In] string Delimiters, [In] string S, int Offset)
		{
			int Result;
			if (S != null)
			{
				if (Offset < 1)
				{
					Offset = 1;
				}
				Result = S.IndexOfAny(CharArrayOf(Delimiters), Offset - 1) + 1;
			}
			else
			{
				Result = 0;
			}
			return Result;
		}

		private static void InvalidMask([In] string Mask)
		{
			throw new EMaskException(string.Format("'{0}' is an invalid mask at (%d)", new object[] { Mask }));
		}

		private static void _ConvertMaskToRegularExpression_CheckPos(int I, int Len, [In] string Mask)
		{
			if (I == Len - 1)
			{
				InvalidMask(Mask);
			}
		}

		private static string ConvertMaskToRegularExpression([In] string Mask)
		{
			string Result = "";
			int CurPos = 0;
			int Len = (Mask != null) ? Mask.Length : 0;
			if (CurPos < Len)
			{
				do
				{
					int I = FindDelimiter("*?[", Mask, CurPos + 1) - 1;
					if (I < CurPos)
					{
						break;
					}
					if (I > CurPos)
					{
						Result += Regex.Escape(WStrCopy(Mask, CurPos + 1, I - CurPos));
					}
					char c = Mask[I];
					if (c != '*')
					{
						if (c != '?')
						{
							if (c == '[')
							{
								_ConvertMaskToRegularExpression_CheckPos(I, Len, Mask);
								if (Mask[I + 2 - 1] == '!')
								{
									Result += "[^";
									I++;
									_ConvertMaskToRegularExpression_CheckPos(I, Len, Mask);
								}
								else
								{
									Result += "[";
								}
								CurPos = I + 1;
								while (Mask[I] != ']')
								{
									I = FindDelimiter("]-", Mask, CurPos + 1) - 1;
									if (I < 0)
									{
										InvalidMask(Mask);
									}
									Result += Regex.Escape(WStrCopy(Mask, CurPos + 1, I - CurPos));
									if (Mask[I] == '-')
									{
										_ConvertMaskToRegularExpression_CheckPos(I, Len, Mask);
										Result += "-";
										CurPos = I + 1;
									}
								}
								Result += "]";
							}
						}
						else
						{
							Result += ".";
						}
					}
					else
					{
						Result += ".*";
					}
					CurPos = I + 1;
				}
				while (CurPos < Len);
			}
			if (CurPos < Len)
			{
				Result += Regex.Escape(WStrCopy(Mask, CurPos + 1, Len - CurPos));
			}
			return Result;
		}

		public static bool MatchesMask([In] string Filename, [In] string Mask)
		{
			Regex FMask = new Regex(ConvertMaskToRegularExpression(Mask), RegexOptions.IgnoreCase);
			bool Result;
			try
			{
				Result = false;
				Match Match = FMask.Match(Filename);
				GroupCollection Groups = Match.Groups;
				int num = Groups.Count - 1;
				int I = 0;
				if (num >= I)
				{
					num++;
					while (true)
					{
						Group Group = Groups[I];
						if (Group.Success)
						{
							int num2 = Group.Captures.Count - 1;
							for (int J = 0; J <= num2; J++)
							{
								Capture Capture = Group.Captures[J];
								if (string.Compare(Capture.Value, Filename, true) == 0)
								{
									goto Block_6;
								}
							}
						}
						I++;
						if (I == num)
						{
							goto IL_A2;
						}
					}
					Block_6:
					Result = true;
				}
				IL_A2:;
			}
			finally
			{
				TObjectHelper.Free(FMask);
			}
			return Result;
		}

		private static int FindMatchingFile(ref TSearchRec F)
		{
			int Result;
			while ((F.FindData.dwFileAttributes & (uint)F.ExcludeAttr) != 0u)
			{
				if (FindNextFile(F.FindHandle, out F.FindData) == (LongBool)0)
				{
					Result = GetLastError();
					return Result;
				}
			}
			F.Attr = (int)F.FindData.dwFileAttributes;
			F.Name = F.FindData.cFileName;
			Result = 0;
			return Result;
		}

		[FileIOPermission(SecurityAction.LinkDemand, Unrestricted=true)]
		public static int FindFirst([In] string Path, int Attr, ref TSearchRec F)
		{
			F.ExcludeAttr = (~Attr & 22);
			F.FindHandle = FindFirstFile(Path, out F.FindData);
			int Result;
			if (F.FindHandle != -1)
			{
				Result = FindMatchingFile(ref F);
				if (Result != 0)
				{
					FindClose(ref F);
				}
			}
			else
			{
				Result = GetLastError();
			}
			return Result;
		}

		[FileIOPermission(SecurityAction.LinkDemand, Unrestricted=true)]
		public static int FindNext(ref TSearchRec F)
		{
			int Result;
			if (FindNextFile(F.FindHandle, out F.FindData) != (LongBool)0)
			{
				Result = FindMatchingFile(ref F);
			}
			else
			{
				Result = GetLastError();
			}
			return Result;
		}

		[FileIOPermission(SecurityAction.LinkDemand, Unrestricted=true)]
		public static void FindClose(ref TSearchRec F)
		{
			if (F.FindHandle != -1)
			{
				FindClose(F.FindHandle);
				F.FindHandle = -1;
			}
		}

		private static void MoveL2S([In] int Source, ref byte[] Dest, int count)
		{
			byte[] bytes = new byte[4];

			ushort wl = (ushort)Source;
			ushort wh = (ushort)((uint)Source >> 16);

			bytes[0] = (byte)wl;
			bytes[1] = (byte)(wl >> 8);
			bytes[2] = (byte)wh;
			bytes[3] = (byte)(wh >> 8);

			if (Dest != null) {
				for (int I = 1; I <= count; I++) {
					Dest[(uint)(I - 1)] = bytes[I - 1];
				}
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
			int I;

			switch (num) {
				case 2:
					I = (int)((uint)_Unnamed1.Map[(int)data[0]] + ((int)((uint)_Unnamed1.Map[(int)data[1]]) << 6));
					result = new byte[1];
					MoveL2S(I, ref result, 1);
					break;
				case 3:
					I = (int)((uint)_Unnamed1.Map[(int)data[0]] + ((int)((uint)_Unnamed1.Map[(int)data[1]]) << 6) + ((int)((uint)_Unnamed1.Map[(int)data[2]]) << 12));
					result = new byte[2];
					MoveL2S(I, ref result, 2);
					break;
				case 4:
					I = (int)((uint)_Unnamed1.Map[(int)data[0]] + ((int)((uint)_Unnamed1.Map[(int)data[1]]) << 6) + ((int)((uint)_Unnamed1.Map[(int)data[2]]) << 12) + ((int)((uint)_Unnamed1.Map[(int)data[3]]) << 18));
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
					Seed = (ushort)(((uint)ppd[I - 1] + (uint)Seed) * 28732u + 28446u);
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
					Seed = (ushort)(((uint)idata[I - 1] + (uint)Seed) * 28732u + 28446u);
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

		private static void IQuickSort(TList SortList, TSortCompareFunc SCompare, int L, int R)
		{
			int I;
			do
			{
				I = L;
				int J = R;
				object P = SortList[(int)((uint)(L + R) >> 1)];
				while (true)
				{
					if (SCompare(SortList[I], P) >= 0)
					{
						while (SCompare(SortList[J], P) > 0)
						{
							J--;
						}
						if (I <= J)
						{
							object T = SortList[I];
							SortList[I] = SortList[J];
							SortList[J] = T;
							I++;
							J--;
						}
						if (I > J)
						{
							break;
						}
					}
					else
					{
						I++;
					}
				}
				if (L < J)
				{
					IQuickSort(SortList, SCompare, L, J);
				}
				L = I;
			}
			while (I < R);
		}

		public static string GetTempDir()
		{
			return Environment.GetEnvironmentVariable("TEMP");
		}

		public static string GetFileVersion()
		{
			AssemblyName name = System.Reflection.Assembly.GetExecutingAssembly().GetName();
			return name.Version.ToString();
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

		public static void ScanDir([In] string aPath, TFilePrepareProc aPrepareProc, bool aIncludeFolders, int FileAttrs, string aMask)
		{
			string PathDelim = new string(Path.DirectorySeparatorChar, 1);
			TSearchRec sr = new TSearchRec();
			if (FindFirst(aPath + PathDelim + aMask, FileAttrs, ref sr) == 0)
			{
				int res;
				do
				{
					if (sr.Name != "." && sr.Name != "..")
					{
						string newf = aPath + PathDelim + sr.Name;
						if ((sr.Attr & 16) == 16)
						{
							if (aIncludeFolders)
							{
								ScanDir(newf, aPrepareProc, aIncludeFolders, FileAttrs, "*.*");
							}
						}
						else
						{
							aPrepareProc(newf);
						}
					}
					res = FindNext(ref sr);
				}
				while (res == 0);
			}
			FindClose(ref sr);
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

		public static string GetToken(string S, char SepChar, int TokenNum)
		{
			string Result = "";
			if (S != "")
			{
				if (S[((S != null) ? S.Length : 0) - 1] != SepChar)
				{
					S += SepChar;
				}
				int sp = 1;
				int cur_tok = 0;
				int num = (S != null) ? S.Length : 0;
				int p = 1;
				if (num >= p)
				{
					num++;
					while (true)
					{
						if (S[p - 1] == SepChar)
						{
							cur_tok++;
							if (cur_tok == TokenNum)
							{
								break;
							}
							sp = p + 1;
						}
						p++;
						if (p == num)
						{
							return Result;
						}
					}
					Result = WStrCopy(S, sp, p - sp);
				}
			}
			return Result;
		}

		public static int GetTokensCount(string S, char SepChar)
		{
			int Result = 0;
			if (S != "")
			{
				int num = (S != null) ? S.Length : 0;
				int p = 1;
				if (num >= p)
				{
					num++;
					do
					{
						if (S[p - 1] == SepChar)
						{
							Result++;
						}
						p++;
					}
					while (p != num);
				}
				Result++;
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

		public static int Hole(ref object A)
		{
			int Result = 0;
			return Result;
		}

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

		public static string ConStrings(TStrings aStrings)
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

		public static void BuildCRCTable()
		{
			uint i = 0u;
			do
			{
				uint value = i;
				uint j = 4294967288u;
				do
				{
					if ((value & 1u) != 0u)
					{
						value = (value >> 1 ^ 3988292384u);
					}
					else
					{
						value >>= 1;
					}
					j += 1u;
				}
				while (j != 0u);
				Ccitt32Table[(int)i] = value;
				i += 1u;
			}
			while (i != 256u);
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

		public static void QuickSort(TList SortList, TSortCompareFunc SCompare)
		{
			if (SortList != null && SortList.Count > 0)
			{
				IQuickSort(SortList, SCompare, 0, SortList.Count - 1);
			}
		}

		public static void MergeSort(TList aList, TSortCompareFunc aCompare)
		{
		}

		public static void ShowMessage([In] string Msg)
		{
			MessageBox.Show(Msg, "GEDKeeper2", MessageBoxButtons.OK, MessageBoxIcon.Asterisk);
		}

		public static void ShowError([In] string Msg)
		{
			MessageBox.Show(Msg, "GEDKeeper2", MessageBoxButtons.OK, MessageBoxIcon.Hand);
		}

		public static DialogResult ShowQuestion([In] string Msg)
		{
			return MessageBox.Show(Msg, "GEDKeeper2", MessageBoxButtons.YesNo, MessageBoxIcon.Question);
		}

		public static ScrollEventType GetScrollEventType(uint wParam)
		{
			ScrollEventType Result;
			if (wParam <= 8u)
			{
				Result = _events[(int)wParam];
			}
			else
			{
				Result = ScrollEventType.EndScroll;
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
			return C >= '0' && C < ':';
		}

		public static bool IsDigits([In] string S)
		{
			bool res = false;
			if (string.IsNullOrEmpty(S)) return res;

			int I;
			for (I = 1; I <= S.Length; I++)
			{
				char c = S[I - 1];
				if (c < '0' || c >= ':')
				{
					break;
				}
			}
			return (I > S.Length);
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
		private static void QuickSort<T>(T[] array)
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
		private static void MergeSort<T>(T[] array)
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

		static SysUtils()
		{
			Ccitt32Table = new uint[256];
			_events = new ScrollEventType[]
			{
				ScrollEventType.SmallDecrement, 
				ScrollEventType.SmallIncrement, 
				ScrollEventType.LargeDecrement, 
				ScrollEventType.LargeIncrement, 
				ScrollEventType.ThumbPosition, 
				ScrollEventType.ThumbTrack, 
				ScrollEventType.First, 
				ScrollEventType.Last, 
				ScrollEventType.EndScroll
			};
		}
	}
}
