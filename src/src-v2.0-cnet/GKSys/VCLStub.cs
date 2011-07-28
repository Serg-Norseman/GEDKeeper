using System;
using System.IO;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Security;
using System.Text;
using System.Text.RegularExpressions;

namespace GKSys
{
	internal class EMaskException : Exception
	{
		public EMaskException()
		{
		}
		public EMaskException(string message) : base(message)
		{
		}
		public EMaskException(string message, Exception innerException) : base(message, innerException)
		{
		}
	}

	public enum E23 : byte
	{
		rfReplaceAll, rfIgnoreCase
	}

	[Flags, TSetElementType(typeof(E23))]
	[Serializable]
	public enum TReplaceFlags : byte
	{
		rfReplaceAll = 1,
		rfIgnoreCase = 2
	}

	public sealed class VCLUtils
	{

		internal static readonly byte[] B2HConvert = new byte[]
		{
			48, 
			49, 
			50, 
			51, 
			52, 
			53, 
			54, 
			55, 
			56, 
			57, 
			65, 
			66, 
			67, 
			68, 
			69, 
			70
		};
		internal static readonly short[] H2BConvert = new short[]
		{
			0, 
			1, 
			2, 
			3, 
			4, 
			5, 
			6, 
			7, 
			8, 
			9, 
			-1, 
			-1, 
			-1, 
			-1, 
			-1, 
			-1, 
			-1, 
			10, 
			11, 
			12, 
			13, 
			14, 
			15, 
			-1, 
			-1, 
			-1, 
			-1, 
			-1, 
			-1, 
			-1, 
			-1, 
			-1, 
			-1, 
			-1, 
			-1, 
			-1, 
			-1, 
			-1, 
			-1, 
			-1, 
			-1, 
			-1, 
			-1, 
			-1, 
			-1, 
			-1, 
			-1, 
			-1, 
			-1, 
			10, 
			11, 
			12, 
			13, 
			14, 
			15
		};

		internal static readonly SeekOrigin[] OriginMap = new SeekOrigin[]
			{ System.IO.SeekOrigin.Begin, System.IO.SeekOrigin.Current, System.IO.SeekOrigin.End };

		internal static readonly string[] Values = new string[]
		{
			"0", 
			"1"
		};

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
		[DllImport("version.dll", CharSet = CharSet.Auto, SetLastError = true)]
		public static extern uint GetFileVersionInfoSize(string lptstrFilename, out uint lpdwHandle);
		[SuppressUnmanagedCodeSecurity]
		[DllImport("version.dll", CharSet = CharSet.Auto, SetLastError = true)]
		public static extern LongBool GetFileVersionInfo(string lptstrFilename, uint dwHandle, uint dwLen, [Out] byte[] lpData);
		[SuppressUnmanagedCodeSecurity]
		[DllImport("version.dll", CharSet = CharSet.Auto, SetLastError = true)]
		public static extern LongBool VerQueryValue([In] byte[] pBlock, string lpSubBlock, out IntPtr lplpBuffer, out uint puLen);
		[SuppressUnmanagedCodeSecurity]
		[DllImport("user32.dll", CharSet = CharSet.Auto, SetLastError = true)]
		public static extern LongBool PostMessage([HWND] uint hWnd, uint Msg, int wParam, int lParam);
		[SuppressUnmanagedCodeSecurity]
		[DllImport("user32.dll", CharSet = CharSet.Ansi, SetLastError = true)]
		public static extern LongBool EnableWindow([HWND] uint hWnd, LongBool bEnable);
		[SuppressUnmanagedCodeSecurity]
		[DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)]
		[return: HRSRC]
		public static extern int FindResource([HMODULE] int hModule, string lpName, string lpType);
		[SuppressUnmanagedCodeSecurity]
		[DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)]
		[return: HRSRC]
		public static extern int FindResource([HMODULE] int hModule, string lpName, int lpType);
		[SuppressUnmanagedCodeSecurity]
		[DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)]
		[return: HRSRC]
		public static extern int FindResource([HMODULE] int hModule, int lpName, int lpType);
		[SuppressUnmanagedCodeSecurity]
		[DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)]
		[return: HRSRC]
		public static extern int FindResource([HMODULE] int hModule, int lpName, string lpType);
		[SuppressUnmanagedCodeSecurity]
		[DllImport("kernel32.dll", CharSet = CharSet.Ansi, SetLastError = true)]
		public static extern int LoadResource([HINST] int hModule, [HRSRC] int hResInfo);
		[SuppressUnmanagedCodeSecurity]
		[DllImport("kernel32.dll", CharSet = CharSet.Ansi, SetLastError = true)]
		public static extern uint SizeofResource([HINST] int hModule, [HRSRC] int hResInfo);
		[SuppressUnmanagedCodeSecurity]
		[DllImport("kernel32.dll", CharSet = CharSet.Ansi, SetLastError = true)]
		public static extern LongBool FreeResource(int hResData);
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
		[DllImport("kernel32.dll", CharSet = CharSet.Ansi, SetLastError = true)]
		public static extern LongBool FindClose(int hFindFile);

		public static TStrings StrArrayToStrings(params string[] StrArr)
		{
			StrArr = (string[])StrArr.Clone();
			TStrings Result = new TStringList();
			int num = ((StrArr != null) ? StrArr.Length : 0) - 1;
			int i = 0;
			if (num >= i)
			{
				num++;
				do
				{
					Result.Add(StrArr[i]);
					i++;
				}
				while (i != num);
			}
			return Result;
		}

		public static void FreeAndNil(ref object Obj)
		{
			TObjectHelper.Free(Obj);
			Obj = null;
		}

		public static bool CreateDir([In] string Dir)
		{
			DirectoryInfo LInfo = new DirectoryInfo(Dir);
			LInfo = LInfo.Parent;
			bool Result = LInfo != null && LInfo.Exists;
			if (Result)
			{
				LInfo = Directory.CreateDirectory(Dir);
				Result = (LInfo != null && LInfo.Exists);
			}
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

		public static int PosEx([In] string SubStr, [In] string S, int Offset)
		{
			int Result;
			if (Offset <= 0 || BDSSystem.WStrCmp(S, null) == 0 || Offset > ((S != null) ? S.Length : 0))
			{
				Result = 0;
			}
			else
			{
				Result = S.IndexOf(SubStr, Offset - 1) + 1;
			}
			return Result;
		}

		public static int CompareText([In] string S1, [In] string S2)
		{
			return string.Compare(S1, S2, true);
		}

		public static int CompareStr([In] string S1, [In] string S2)
		{
			return string.Compare(S1, S2, false);
		}

		public static bool SameText([In] string S1, [In] string S2)
		{
			return CompareText(S1, S2) == 0;
		}

		public static int StrToIntDef([In] string S, int Default)
		{
			int E = 0;
			int Result = BDSSystem.ValLong(S, ref E);
			if (E != 0)
			{
				Result = Default;
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
					Result = BDSSystem.WStrCopy(S, I, 2147483647);
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
				Result = BDSSystem.WStrCopy(S, 1, I);
			}
			else
			{
				Result = S;
			}
			return Result;
		}

		internal static char[] CharArrayOf([In] string AText)
		{
			char[] Result = null;
			if (AText != null)
			{
				Result = AText.ToCharArray();
			}
			else
			{
				char[] array = Result;
				char[] array2;
				char[] expr_1A = array2 = new char[0];
				if (array != null)
				{
					int num;
					if ((num = array.Length) > 0)
					{
						num = 0;
					}
					if (num > 0)
					{
						Array.Copy(array, array2, num);
					}
				}
				Result = expr_1A;
			}
			return Result;
		}

		internal static int LastDelimiter([In] string Delimiters, [In] string S)
		{
			int Result;
			if (S != null)
			{
				Result = S.LastIndexOfAny(VCLUtils.CharArrayOf(Delimiters)) + 1;
			}
			else
			{
				Result = 0;
			}
			return Result;
		}

		public static string ExtractFilePath([In] string FileName)
		{
			int I = VCLUtils.LastDelimiter(BDSSystem.WStrFromWChar(Path.DirectorySeparatorChar) + BDSSystem.WStrFromWChar(Path.VolumeSeparatorChar), FileName);
			return BDSSystem.WStrCopy(FileName, 1, I);
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
			while (BDSSystem.WStrCmp(SearchStr, "") != 0)
			{
				int Offset = BDSSystem.Pos(Patt, SearchStr);
				if (Offset == 0)
				{
					SB.Append(NewStr);
					break;
				}
				SB.Append(NewStr, 0, Offset - 1);
				SB.Append(NewPattern);
				NewStr = BDSSystem.WStrCopy(NewStr, Offset + ((OldPattern != null) ? OldPattern.Length : 0), 2147483647);
				if ((Flags & TReplaceFlags.rfReplaceAll) == (TReplaceFlags)0)
				{
					SB.Append(NewStr);
					break;
				}
				SearchStr = BDSSystem.WStrCopy(SearchStr, Offset + ((Patt != null) ? Patt.Length : 0), 2147483647);
			}
			return SB.ToString();
		}

		internal static void BinToHex([In] byte[] Buffer, int BufOffset, ref byte[] Text, int TextOffset, int Count)
		{
			int num = Count - 1;
			int I = 0;
			if (num >= I)
			{
				num++;
				do
				{
					Text[TextOffset + (I << 1)] = VCLUtils.B2HConvert[(int)((uint)Buffer[BufOffset + I] >> 4)];
					Text[TextOffset + 1 + (I << 1)] = VCLUtils.B2HConvert[(int)(Buffer[BufOffset + I] & 15)];
					I++;
				}
				while (I != num);
			}
		}

		internal static int HexToBin(byte[] Text, int TextOffset, byte[] Buffer, int BufOffset, int Count)
		{
			int C = 0;
			int num = Count - 1;
			int I = 0;
			if (num >= I)
			{
				num++;
				do
				{
					AnsiChar ansiChar = (AnsiChar)Text[TextOffset + (I << 1)];
					if (ansiChar < (AnsiChar)48 || ansiChar >= (AnsiChar)103)
					{
						break;
					}
					AnsiChar ansiChar2 = (AnsiChar)Text[TextOffset + 1 + (I << 1)];
					if (ansiChar2 < (AnsiChar)48 || ansiChar2 >= (AnsiChar)103)
					{
						break;
					}
					Buffer[BufOffset + I] = (byte)((int)VCLUtils.H2BConvert[(int)((ushort)Text[TextOffset + (I << 1)]) - 48] << 4 | (int)VCLUtils.H2BConvert[(int)((ushort)Text[TextOffset + 1 + (I << 1)]) - 48]);
					C++;
					I++;
				}
				while (I != num);
			}
			return C;
		}

		public static int FindDelimiter([In] string Delimiters, [In] string S, int Offset)
		{
			int Result;
			if (BDSSystem.WStrCmp(S, null) != 0)
			{
				if (Offset < 1)
				{
					Offset = 1;
				}
				Result = S.IndexOfAny(VCLUtils.CharArrayOf(Delimiters), Offset - 1) + 1;
			}
			else
			{
				Result = 0;
			}
			return Result;
		}

		internal static void InvalidMask([In] string Mask)
		{
			throw new EMaskException(string.Format("'{0}' is an invalid mask at (%d)", new object[]
			{
				Mask
			}));
		}

		internal static void _ConvertMaskToRegularExpression_CheckPos(int I, int Len, [In] string Mask)
		{
			if (I == Len - 1)
			{
				VCLUtils.InvalidMask(Mask);
			}
		}

		internal static string ConvertMaskToRegularExpression([In] string Mask)
		{
			string Result = "";
			int CurPos = 0;
			int Len = (Mask != null) ? Mask.Length : 0;
			if (CurPos < Len)
			{
				do
				{
					int I = VCLUtils.FindDelimiter("*?[", Mask, CurPos + 1) - 1;
					if (I < CurPos)
					{
						break;
					}
					if (I > CurPos)
					{
						Result += Regex.Escape(BDSSystem.WStrCopy(Mask, CurPos + 1, I - CurPos));
					}
					char c = Mask[I + 1 - 1];
					if (c != '*')
					{
						if (c != '?')
						{
							if (c == '[')
							{
								VCLUtils._ConvertMaskToRegularExpression_CheckPos(I, Len, Mask);
								if (Mask[I + 2 - 1] == '!')
								{
									Result += "[^";
									I++;
									VCLUtils._ConvertMaskToRegularExpression_CheckPos(I, Len, Mask);
								}
								else
								{
									Result += "[";
								}
								CurPos = I + 1;
								while (Mask[I + 1 - 1] != ']')
								{
									I = VCLUtils.FindDelimiter("]-", Mask, CurPos + 1) - 1;
									if (I < 0)
									{
										VCLUtils.InvalidMask(Mask);
									}
									Result += Regex.Escape(BDSSystem.WStrCopy(Mask, CurPos + 1, I - CurPos));
									if (Mask[I + 1 - 1] == '-')
									{
										VCLUtils._ConvertMaskToRegularExpression_CheckPos(I, Len, Mask);
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
				Result += Regex.Escape(BDSSystem.WStrCopy(Mask, CurPos + 1, Len - CurPos));
			}
			return Result;
		}

		public static bool MatchesMask([In] string Filename, [In] string Mask)
		{
			Regex FMask = new Regex(VCLUtils.ConvertMaskToRegularExpression(Mask), RegexOptions.IgnoreCase);
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
							int J = 0;
							if (num2 >= J)
							{
								num2++;
								do
								{
									Capture Capture = Group.Captures[J];
									if (SameText(Capture.Value, Filename))
									{
										goto Block_6;
									}
									J++;
								}
								while (J != num2);
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

		internal static int StringListCompareStrings(TStringList List, int Index1, int Index2)
		{
			return List.CompareStrings(List.FList[Index1].FString, List.FList[Index2].FString);
		}

	}
}
