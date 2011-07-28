using System;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Globalization;
using System.IO;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Security.Permissions;
using System.Text;
using System.Threading;
using System.Windows.Forms;

namespace GKSys
{
	public class EConvertError : Exception
	{
		public EConvertError()
		{
		}
		public EConvertError(string message) : base(message)
		{
		}
		public EConvertError(string message, Exception innerException) : base(message, innerException)
		{
		}
	}

	[StructLayout(LayoutKind.Sequential, Pack = 1, Size = 1)]
	internal struct RomeData
	{
		internal static readonly int[] Rn_N;
		internal static readonly string[] Rn_S;

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

	public class TGKSys
	{
		public delegate void TFilePrepareProc(string FileName);
		public delegate int TSortCompareFunc(object Item1, object Item2);

		internal static readonly ScrollEventType[] _events;
		internal static string LogFilename;
		internal static uint[] Ccitt32Table;


		internal static int FindMatchingFile(ref TSearchRec F)
		{
			int Result;
			while ((F.FindData.dwFileAttributes & (uint)F.ExcludeAttr) != 0u)
			{
				if (VCLUtils.FindNextFile(F.FindHandle, out F.FindData) == (LongBool)0)
				{
					Result = BDSSystem.GetLastError();
					return Result;
				}
			}
			F.Attr = (int)F.FindData.dwFileAttributes;
			F.Name = F.FindData.cFileName;
			Result = 0;
			return Result;
		}

		[FileIOPermission(SecurityAction.LinkDemand, Unrestricted=true)]
		internal static int FindFirst([In] string Path, int Attr, ref TSearchRec F)
		{
			F.ExcludeAttr = (~Attr & 22);
			F.FindHandle = VCLUtils.FindFirstFile(Path, out F.FindData);
			int Result;
			if (F.FindHandle != -1)
			{
				Result = TGKSys.FindMatchingFile(ref F);
				if (Result != 0)
				{
					TGKSys.FindClose(ref F);
				}
			}
			else
			{
				Result = BDSSystem.GetLastError();
			}
			return Result;
		}

		[FileIOPermission(SecurityAction.LinkDemand, Unrestricted=true)]
		internal static int FindNext(ref TSearchRec F)
		{
			int Result;
			if (VCLUtils.FindNextFile(F.FindHandle, out F.FindData) != (LongBool)0)
			{
				Result = TGKSys.FindMatchingFile(ref F);
			}
			else
			{
				Result = BDSSystem.GetLastError();
			}
			return Result;
		}

		[FileIOPermission(SecurityAction.LinkDemand, Unrestricted=true)]
		internal static void FindClose(ref TSearchRec F)
		{
			if (F.FindHandle != -1)
			{
				VCLUtils.FindClose(F.FindHandle);
				F.FindHandle = -1;
			}
		}

		internal static AnsiString Decode([In] AnsiString S)
		{
			byte[] data = S.Data;
			int num = (data != null) ? data.Length : 0;
			AnsiString Result = "";
			if (num != 2)
			{
				if (num != 3)
				{
					if (num == 4)
					{
						int I = (int)((uint)_Unnamed1.Map[(int)S.Data[0]] + ((int)((uint)_Unnamed1.Map[(int)S.Data[1]]) << 6) + ((int)((uint)_Unnamed1.Map[(int)S.Data[2]]) << 12) + ((int)((uint)_Unnamed1.Map[(int)S.Data[3]]) << 18));
						int num2 = 0;
						if (Result.Data != null)
						{
							num2 = Result.Data.Length;
							if (num2 >= 3)
							{
								num2 = 3;
							}
						}
						byte[] array = new byte[3];
						if (Result.Data != null)
						{
							Array.Copy(Result.Data, array, num2);
						}
						Result.Data = array;
						int arg_1E4_0 = I;
						byte[] data2 = Result.Data;
						TGKSys.MoveL2S(arg_1E4_0, ref Result, (data2 != null) ? data2.Length : 0);
					}
				}
				else
				{
					int I = (int)((uint)_Unnamed1.Map[(int)S.Data[0]] + ((int)((uint)_Unnamed1.Map[(int)S.Data[1]]) << 6) + ((int)((uint)_Unnamed1.Map[(int)S.Data[2]]) << 12));
					int num3 = 0;
					if (Result.Data != null)
					{
						num3 = Result.Data.Length;
						if (num3 >= 2)
						{
							num3 = 2;
						}
					}
					byte[] array2 = new byte[2];
					if (Result.Data != null)
					{
						Array.Copy(Result.Data, array2, num3);
					}
					Result.Data = array2;
					int arg_138_0 = I;
					byte[] data3 = Result.Data;
					TGKSys.MoveL2S(arg_138_0, ref Result, (data3 != null) ? data3.Length : 0);
				}
			}
			else
			{
				int I = (int)((uint)_Unnamed1.Map[(int)S.Data[0]] + ((int)((uint)_Unnamed1.Map[(int)S.Data[1]]) << 6));
				int num4 = 0;
				if (Result.Data != null)
				{
					num4 = Result.Data.Length;
					if (num4 >= 1)
					{
						num4 = 1;
					}
				}
				byte[] array3 = new byte[1];
				if (Result.Data != null)
				{
					Array.Copy(Result.Data, array3, num4);
				}
				Result.Data = array3;
				int arg_A1_0 = I;
				byte[] data4 = Result.Data;
				TGKSys.MoveL2S(arg_A1_0, ref Result, (data4 != null) ? data4.Length : 0);
			}
			return Result;
		}

		internal static AnsiString Encode([In] AnsiString S)
		{
			int I = 0;
			AnsiString arg_16_0 = S;
			byte[] data = S.Data;
			TGKSys.MoveS2L(arg_16_0, ref I, (data != null) ? data.Length : 0);
			byte[] data2 = S.Data;
			int num = (data2 != null) ? data2.Length : 0;
			AnsiString Result = "";
			if (num != 1)
			{
				if (num != 2)
				{
					if (num == 3)
					{
						Result.Data = BDSSystem.LStrFromWStr(BDSSystem.WStrFromWChar(_Unnamed2.Map[I % 64]) + BDSSystem.WStrFromWChar(_Unnamed2.Map[((uint)I >> 6) % 64]) + BDSSystem.WStrFromWChar(_Unnamed2.Map[((uint)I >> 12) % 64]) + BDSSystem.WStrFromWChar(_Unnamed2.Map[((uint)I >> 18) % 64]));
					}
				}
				else
				{
					Result.Data = BDSSystem.LStrFromWStr(BDSSystem.WStrFromWChar(_Unnamed2.Map[I % 64]) + BDSSystem.WStrFromWChar(_Unnamed2.Map[((uint)I >> 6) % 64]) + BDSSystem.WStrFromWChar(_Unnamed2.Map[((uint)I >> 12) % 64]));
				}
			}
			else
			{
				Result.Data = BDSSystem.LStrFromWStr(BDSSystem.WStrFromWChar(_Unnamed2.Map[I % 64]) + BDSSystem.WStrFromWChar(_Unnamed2.Map[((uint)I >> 6) % 64]));
			}
			return Result;
		}

		internal static int MakeLong(ushort A, ushort B)
		{
			return (int)((uint)A | (int)((uint)B) << 16);
		}

		internal static ushort MakeWord(byte A, byte B)
		{
			return (ushort)((uint)A | (int)((uint)B) << 8);
		}

		internal static void MoveL2S([In] int Source, ref AnsiString Dest, int count)
		{
			byte[] bytes = new byte[4];
			ushort wl = (ushort)Source;
			ushort wh = (ushort)((uint)Source >> 16);
			bytes[0] = (byte)wl;
			bytes[1] = (byte)((uint)wl >> 8);
			bytes[2] = (byte)wh;
			bytes[3] = (byte)((uint)wh >> 8);
			int I = 1;
			if (count >= I)
			{
				int num = count + 1;
				do
				{
					uint num2 = (uint)(I - 1);
					AnsiChar ansiChar = (AnsiChar)bytes[I - 1];
					if (Dest.Data != null)
					{
						byte[] expr_64 = (byte[])Dest.Data.Clone();
						expr_64[(int)num2] = (byte)ansiChar;
						Dest.Data = expr_64;
					}
					I++;
				}
				while (I != num);
			}
		}

		internal static void MoveS2L([In] AnsiString Source, ref int Dest, int count)
		{
			byte[] bytes = new byte[4];
			int I = 1;
			do
			{
				if (I <= count)
				{
					bytes[I - 1] = Source.Data[I - 1];
				}
				else
				{
					bytes[I - 1] = 0;
				}
				I++;
			}
			while (I != 5);
			Dest = TGKSys.MakeLong(TGKSys.MakeWord(bytes[0], bytes[1]), TGKSys.MakeWord(bytes[2], bytes[3]));
		}

		internal static void IQuickSort(TList SortList, TGKSys.TSortCompareFunc SCompare, int L, int R)
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
					TGKSys.IQuickSort(SortList, SCompare, L, J);
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
			string Result = "";
			string fn = BDSSystem.ParamStr(0);
			uint Wnd;
			uint InfoSize = VCLUtils.GetFileVersionInfoSize(fn, out Wnd);
			if (InfoSize != 0u)
			{
				byte[] VerBuf = null;
				byte[] array = VerBuf;
				uint arg_29_0;
				if ((arg_29_0 = InfoSize) < 0u)
				{
					arg_29_0 = 0u;
				}
				byte[] array2;
				byte[] expr_2E = array2 = new byte[(int)arg_29_0];
				if (InfoSize > 0u && array != null)
				{
					int num;
					if ((num = array.Length) > (int)InfoSize)
					{
						num = (int)InfoSize;
					}
					if (num > 0)
					{
						Array.Copy(array, array2, num);
					}
				}
				VerBuf = expr_2E;
				IntPtr PFI;
				uint VerSize;
				if (VCLUtils.GetFileVersionInfo(fn, Wnd, InfoSize, VerBuf) != (LongBool)0 && VCLUtils.VerQueryValue(VerBuf, "\\", out PFI, out VerSize) != (LongBool)0)
				{
					TVSFixedFileInfo FI = (TVSFixedFileInfo)Marshal.PtrToStructure(PFI, typeof(TVSFixedFileInfo));
					int Ms = (int)FI.dwFileVersionMS;
					int Ls = (int)FI.dwFileVersionLS;
					Result = string.Format("{0}.{1}.{2}.{3}", new object[]
					{
						(ushort)((uint)Ms >> 16), 
						(ushort)Ms, 
						(ushort)((uint)Ls >> 16), 
						(ushort)Ls
					});
				}
			}
			return Result;
		}

		public static string GetAppPath()
		{
			Module[] mods = System.Reflection.Assembly.GetExecutingAssembly().GetModules();
			string fn = mods[0].FullyQualifiedName;
			return Path.GetDirectoryName(fn) + "\\";
		}

		public static void LoadExtFile([In] string aFileName)
		{
			VCLUtils.ShellExecute(0, "open", aFileName, "", "", 5);
		}

		public static void ScanDir([In] string aPath, TGKSys.TFilePrepareProc aPrepareProc, bool aIncludeFolders, int FileAttrs, string aMask)
		{
			string PathDelim = BDSSystem.WStrFromWChar(Path.DirectorySeparatorChar);
			TSearchRec sr = new TSearchRec();
			if (TGKSys.FindFirst(aPath + PathDelim + aMask, FileAttrs, ref sr) == 0)
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
								TGKSys.ScanDir(newf, aPrepareProc, aIncludeFolders, FileAttrs, "*.*");
							}
						}
						else
						{
							aPrepareProc(newf);
						}
					}
					res = TGKSys.FindNext(ref sr);
				}
				while (res == 0);
			}
			TGKSys.FindClose(ref sr);
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
			if (BDSSystem.WStrCmp(S, "") != 0)
			{
				if (S[((S != null) ? S.Length : 0) - 1] != SepChar)
				{
					S += SepChar;
				}
				int sp = 1;
				int cur_tok = 0;
				int arg_4B_0 = 1;
				int num = (S != null) ? S.Length : 0;
				int p = arg_4B_0;
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
					Result = BDSSystem.WStrCopy(S, sp, p - sp);
				}
			}
			return Result;
		}

		public static int GetTokensCount(string S, char SepChar)
		{
			int Result = 0;
			if (BDSSystem.WStrCmp(S, "") != 0)
			{
				int arg_1D_0 = 1;
				int num = (S != null) ? S.Length : 0;
				int p = arg_1D_0;
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
			while (((Result != null) ? Result.Length : 0) < up)
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
			TGKSys.LogFilename = aFileName;
		}

		public static void LogWrite([In] string aMsg)
		{
			StreamWriter Log = new StreamWriter(TGKSys.LogFilename, true, Encoding.GetEncoding(1251));
			Log.WriteLine("[" + DateTime.Now.ToString() + "] -> " + aMsg);
			Log.Flush();
			Log.Close();
		}

		public static string ConStrings(TStrings aStrings)
		{
			string Result = "";
			int num = aStrings.Count - 1;
			int i = 0;
			if (num >= i)
			{
				num++;
				do
				{
					if (Result != "") Result += " ";
					Result += aStrings[i].Trim();
					i++;
				}
				while (i != num);
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
				TGKSys.Ccitt32Table[(int)i] = value;
				i += 1u;
			}
			while (i != 256u);
		}

		public static uint CrcStr([In] string Str)
		{
			uint crc = 0u;
			int arg_10_0 = 1;
			int num = (Str != null) ? Str.Length : 0;
			int i = arg_10_0;
			if (num >= i)
			{
				num++;
				do
				{
					byte c = (byte)Str[i - 1];
					crc = ((crc >> 8 & 16777215u) ^ TGKSys.Ccitt32Table[(int)((crc ^ (uint)c) & 255u)]);
					i++;
				}
				while (i != num);
			}
			return crc;
		}

		public static int agCompare(string Str1, string Str2)
		{
			int NumCode = 0;
			double Val = BDSSystem.ValExt(Str1, ref NumCode);
			bool v = (NumCode == 0);
			double Val2 = BDSSystem.ValExt(Str2, ref NumCode);
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

		public static void QuickSort(TList SortList, TGKSys.TSortCompareFunc SCompare)
		{
			if (SortList != null && SortList.Count > 0)
			{
				TGKSys.IQuickSort(SortList, SCompare, 0, SortList.Count - 1);
			}
		}

		public static void MergeSort(TList aList, TGKSys.TSortCompareFunc aCompare)
		{
		}

		public static void ShowMessage([In] string Msg)
		{
			MessageBox.Show(Msg, "GEDKeeper", MessageBoxButtons.OK, MessageBoxIcon.Asterisk);
		}

		public static void ShowError([In] string Msg)
		{
			MessageBox.Show(Msg, "GEDKeeper", MessageBoxButtons.OK, MessageBoxIcon.Hand);
		}

		public static DialogResult ShowQuestion([In] string Msg)
		{
			return MessageBox.Show(Msg, "GEDKeeper", MessageBoxButtons.YesNo, MessageBoxIcon.Question);
		}

		public static AnsiString scDecrypt([In] AnsiString S, ushort Key)
		{
			//string si;
			//Encoding.ASCII.GetBytes(
			
			AnsiString SS = S;
			AnsiString pp;
			pp.Data = null;
			while (BDSSystem.WStrCmp(BDSSystem.WStrFromLStr(SS.Data), "") != 0)
			{
				byte[] arg_3B_0 = pp.Data;
				AnsiString s;
				s.Data = BDSSystem.LStrCopy(SS.Data, 1, 4);
				pp.Data = BDSSystem.LStrConcat2(arg_3B_0, TGKSys.Decode(s).Data);
				BDSSystem.LStrDelete(ref SS.Data, 1, 4);
			}
			AnsiString Result = pp;
			ushort Seed = Key;
			ushort arg_87_0 = 1;
			byte[] data = Result.Data;
			ushort num = (ushort)((data != null) ? ((ushort)data.Length) : 0);
			ushort I = arg_87_0;
			if (num >= I)
			{
				num += 1;
				do
				{
					uint num2 = (uint)((int)I - 1);
					AnsiChar ansiChar = (AnsiChar)((uint)Result.Data[(int)I - 1] ^ (uint)Seed >> 8);
					if (Result.Data != null)
					{
						byte[] expr_C8 = (byte[])Result.Data.Clone();
						expr_C8[(int)num2] = (byte)ansiChar;
						Result.Data = expr_C8;
					}
					Seed = (ushort)(((uint)pp.Data[(int)I - 1] + (uint)Seed) * 28732u + 28446u);
					I += 1;
				}
				while (I != num);
			}
			return Result;
		}

		public static AnsiString scEncrypt([In] AnsiString S, ushort Key)
		{
			AnsiString @int = S;
			ushort Seed = Key;
			ushort arg_1B_0 = 1;
			byte[] data = @int.Data;
			ushort num = (ushort)((data != null) ? ((ushort)data.Length) : 0);
			ushort I = arg_1B_0;
			if (num >= I)
			{
				num += 1;
				do
				{
					uint num2 = (uint)((int)I - 1);
					AnsiChar ansiChar = (AnsiChar)((uint)@int.Data[(int)I - 1] ^ (uint)Seed >> 8);
					if (@int.Data != null)
					{
						byte[] expr_5C = (byte[])@int.Data.Clone();
						expr_5C[(int)num2] = (byte)ansiChar;
						@int.Data = expr_5C;
					}
					Seed = (ushort)(((uint)@int.Data[(int)I - 1] + (uint)Seed) * 28732u + 28446u);
					I += 1;
				}
				while (I != num);
			}
			AnsiString SS = @int;
			AnsiString Result;
			Result.Data = null;

			while (BDSSystem.WStrCmp(BDSSystem.WStrFromLStr(SS.Data), "") != 0)
			{
				byte[] arg_C4_0 = Result.Data;
				AnsiString s;
				s.Data = BDSSystem.LStrCopy(SS.Data, 1, 3);
				Result.Data = BDSSystem.LStrConcat2(arg_C4_0, TGKSys.Encode(s).Data);
				BDSSystem.LStrDelete(ref SS.Data, 1, 3);
			}
			return Result;
		}

		public static ScrollEventType GetScrollEventType(uint wParam)
		{
			ScrollEventType Result;
			if (wParam <= 8u)
			{
				Result = TGKSys._events[(int)wParam];
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

			if (LFormat.NumberDecimalSeparator != ".")
			{
				LFormat.NumberDecimalSeparator = ".";
			}
			if (LFormat.NumberGroupSeparator != " ")
			{
				LFormat.NumberGroupSeparator = " ";
			}

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
			int I;
			for (I = 1; I <= ((S != null) ? S.Length : 0); I++)
			{
				char c = S[I - 1];
				if (c < '0' || c >= ':')
				{
					break;
				}
			}
			return ((S != null) ? S.Length : 0) > 0 && I > ((S != null) ? S.Length : 0);
		}


		public static int StrToInt([In] string S)
		{
			int E = 0;
			int Result = BDSSystem.ValLong(S, ref E);
			if (E != 0)
			{
				throw new EConvertError(string.Format("'{0}' is not a valid integer value", new object[]
				{
					S
				}));
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
		
		public static GraphicsPath CreateRoundedRectangle(int x, int y, int width, int height, int radius)
	    {
			int xw = x + width;
			int yh = y + height;
			int xwr = xw - radius;
			int yhr = yh - radius;
			int xr = x + radius;
			int yr = y + radius;
			int r2 = radius * 2;
			int xwr2 = xw - r2;
			int yhr2 = yh - r2;

			GraphicsPath p = new GraphicsPath();
			p.StartFigure();

			//Top Left Corner
			p.AddArc(x, y, r2, r2, 180, 90);

			//Top Edge
			p.AddLine(xr, y, xwr, y);

			//Top Right Corner
			p.AddArc(xwr2, y, r2, r2, 270, 90);

			//Right Edge
			p.AddLine(xw, yr, xw, yhr);

			//Bottom Right Corner
			p.AddArc(xwr2, yhr2, r2, r2, 0, 90);

			//Bottom Edge
			p.AddLine(xwr, yh, xr, yh);

			//Bottom Left Corner
			p.AddArc(x, yhr2, r2, r2, 90, 90);

			//Left Edge
			p.AddLine(x, yhr, x, yr);

			p.CloseFigure();
			return p;
		}

		public static string TimeSpanToString(TimeSpan ts)
		{
			return string.Format("{0:00}:{1:00}:{2:00}", new object[]
				{
					ts.Hours, 
					ts.Minutes, 
					ts.Seconds
				});
		}

		public void Free()
		{
			TObjectHelper.Free(this);
		}

		static TGKSys()
		{
			TGKSys.Ccitt32Table = new uint[256];
			TGKSys._events = new ScrollEventType[]
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
