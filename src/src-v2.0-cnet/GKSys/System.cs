using System;
using System.Collections;
using System.Diagnostics;
using System.Reflection;
using System.Resources;
using System.Runtime.InteropServices;
using System.Security;
using System.Text;
using System.Threading;

namespace GKSys
{
	public enum LongBool : int
	{}

	[StructLayout(LayoutKind.Explicit, Pack = 1, Size = 2)]
	internal struct Arr_2
	{
	}

	[StructLayout(LayoutKind.Explicit, Pack = 1, Size = 32)]
	internal struct Arr_32
	{
	}

	public sealed class BDSSystem
	{

		public const int fmCreate         = 0xFFFF;
		public const int fmOpenRead       = 0x0000;
		public const int fmOpenWrite      = 0x0001;
		public const int fmOpenReadWrite  = 0x0002;

		public const int fmShareDenyWrite = 0x0020;
		public const int fmShareDenyRead  = 0x0030; // write-only not supported on all platforms
		public const int fmShareDenyNone  = 0x0040;

		public static Encoding AnsiEncoding = Encoding.GetEncoding(1251);

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

		public const string SInvalidCastString = "Borland.Delphi.System:SInvalidCastString";
		public const string SInvalidCast = "Borland.Delphi.System:SInvalidCast";

		[ThreadStatic]
		internal static Hashtable ResourceManagers;

		public static string LoadResString(Type AType, string ID)
		{
			if (BDSSystem.ResourceManagers == null)
			{
				BDSSystem.ResourceManagers = new Hashtable();
			}
			Assembly assembly = AType.Assembly;
			ResourceManager resourceManager = BDSSystem.ResourceManagers[assembly] as ResourceManager;
			if (resourceManager == null)
			{
				resourceManager = new ResourceManager("_ResourceStrings", assembly);
				Hashtable hashtable = BDSSystem.ResourceManagers.Clone() as Hashtable;
				if (!hashtable.Contains(assembly))
				{
					hashtable.Add(assembly, resourceManager);
				}
				BDSSystem.ResourceManagers = hashtable;
			}
			return resourceManager.GetString(ID);
		}

		public static void InvalidCastError([In] string ACaption)
		{
			throw new InvalidCastException(ACaption);
		}

		public static long Trunc([In] double AValue)
		{
			return Convert.ToInt64(BDSSystem.Int(AValue));
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
			return (AValue - BDSSystem.Int(AValue));
		}

		public static int ValLong([In] string s, ref int code)
		{
			int res;
			if (int.TryParse(s, out res)) { code = 0; } else { code = -1; }
			return res;
		}

		public static double ValExt([In] string s, ref int code)
		{
			double res;
			if (double.TryParse(s, out res)) { code = 0; } else { code = -1; }
			return res;
		}

		public static string StringOf([In] params byte[] Bytes)
		{
			string result;
			if (Bytes != null)
			{
				result = BDSSystem.AnsiEncoding.GetString(Bytes, 0, ((Bytes != null) ? Bytes.Length : 0) - 1 + 1);
			}
			else
			{
				result = "";
			}
			return result;
		}

		public static string PlatformStringOf([In] byte[] Value)
		{
			string result;
			if (Value != null)
			{
				if (Marshal.SystemDefaultCharSize == 1)
				{
					result = BDSSystem.AnsiEncoding.GetString(Value, 0, ((Value != null) ? Value.Length : 0) - 1 + 1);
				}
				else
				{
					result = Encoding.Unicode.GetString(Value, 0, ((Value != null) ? Value.Length : 0) - 1 + 1);
				}
			}
			else
			{
				result = "";
			}
			return result;
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

		public static object GetHelperIntf(object Instance, Type HelperType)
		{
			Type type = HelperType.GetInterfaces()[0];
			object result;
			if (type.IsInstanceOfType(Instance))
			{
				result = Instance;
			}
			else
			{
				result = BDSSystem.GetHelperDelegate(Instance, HelperType);
			}
			return result;
		}

		public static object GetHelperDelegate(object Instance, Type HelperType)
		{
			object[] array2 = null;
			object[] array = array2;
			object[] array3;
			object[] expr_08 = array3 = new object[0];
			if (array != null)
			{
				int num;
				if ((num = array.Length) > 0)
				{
					num = 0;
				}
				if (num > 0)
				{
					Array.Copy(array, array3, num);
				}
			}
			array2 = expr_08;
			object obj = Activator.CreateInstance(HelperType, array2);
			(obj as TClassHelperBase).FInstance = Instance;
			return obj;
		}

		public static object CreateCastException(object Obj, Type CastType)
		{
			return new InvalidCastException(BDSSystem.LoadResString(typeof(BDSSystem), "Borland.Delphi.System:SInvalidCast"));
		}

		public static byte[] SetNew(int Size)
		{
			int arg_07_0 = Size;
			if (Size < 0)
			{
				arg_07_0 = 0;
			}
			return new byte[arg_07_0];
		}

		public static byte[] SetElem(int Elem, int Size)
		{
			byte[] array = BDSSystem.SetNew(Size);
			if (Elem >= 0 && Elem < Size << 3)
			{
				array[Elem / 8] = (byte)(1 << Elem % 8);
			}
			return array;
		}

		public static byte[] SetExpand([In] byte[] Src, int dHi, int dLo, int sHi, int sLo)
		{
			byte[] array;
			if (Src == null)
			{
				array = Src;
			}
			else
			{
				array = BDSSystem.SetNew(dHi - dLo);
				int arg_18_0 = 0;
				int num = sHi - sLo - 1;
				int num2 = arg_18_0;
				if (num >= num2)
				{
					num++;
					do
					{
						array[num2 + sLo - dLo] = Src[num2];
						num2++;
					}
					while (num2 != num);
				}
			}
			return array;
		}

		public static bool SetTest([In] byte[] Src, int Elem, int Size)
		{
			bool result = false;
			if (Src != null && Elem >= 0 && Elem < Size << 3)
			{
				result = (((uint)Src[Elem / 8] & (uint)(1 << Elem % 8)) > 0u);
			}
			return result;
		}

		public static void SetUnion(ref byte[] Dest, [In] byte[] Src, int Size)
		{
			if (Src != null)
			{
				if (Dest == null)
				{
					Dest = BDSSystem.SetNew(Size);
				}
				int arg_14_0 = 0;
				int num = Size - 1;
				int num2 = arg_14_0;
				if (num >= num2)
				{
					num++;
					do
					{
						Dest[num2] |= Src[num2];
						num2++;
					}
					while (num2 != num);
				}
			}
		}

		public static int LStrLen([In] byte[] Dest)
		{
			int result = ((Dest != null) ? Dest.Length : 0);
			return result;
		}

		public static void LStrSetLen(ref byte[] Dest, int Len)
		{
			int arg_08_0 = Len;
			if (Len < 0)
			{
				arg_08_0 = 0;
			}
			Dest = new byte[arg_08_0];
		}

		public static int WStrCmp([In] string L, [In] string R)
		{
			return string.Compare(L, R, false);
		}

		public static byte[] LStrConcat2([In] byte[] L, [In] byte[] R)
		{
			byte[] array = null;
			int num = BDSSystem.LStrLen(L);
			int num2 = BDSSystem.LStrLen(R);
			if (num + num2 > 0)
			{
				BDSSystem.LStrSetLen(ref array, num + num2);
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
				int num = BDSSystem.LStrLen(S);
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
						byte[] array2 = array;
						int arg_39_0;
						if ((arg_39_0 = Count) < 0)
						{
							arg_39_0 = 0;
						}
						byte[] array3;
						byte[] expr_3E = array3 = new byte[arg_39_0];
						if (Count > 0 && array2 != null)
						{
							int num3;
							if ((num3 = array2.Length) > Count)
							{
								num3 = Count;
							}
							if (num3 > 0)
							{
								Array.Copy(array2, array3, num3);
							}
						}
						array = expr_3E;
						Array.Copy(S, num2, array, 0, Count);
					}
				}
			}
			return array;
		}

		public static string WStrCopy([In] string S, int Index1, int Count)
		{
			string result = "";
			if (Count > 0)
			{
				int num = S.Length;
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
				int num = BDSSystem.LStrLen(Dest);
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
						byte[] array = null;
						byte[] arg_3A_0 = array;
						int num3 = num - Count;
						byte[] array2 = arg_3A_0;
						int arg_43_0;
						if ((arg_43_0 = num3) < 0)
						{
							arg_43_0 = 0;
						}
						byte[] array3;
						byte[] expr_48 = array3 = new byte[arg_43_0];
						if (num3 > 0 && array2 != null)
						{
							int num4;
							if ((num4 = array2.Length) > num3)
							{
								num4 = num3;
							}
							if (num4 > 0)
							{
								Array.Copy(array2, array3, num4);
							}
						}
						array = expr_48;
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

		public static void LStrSetElem(ref byte[] Dest, int Index, AnsiChar Val)
		{
			if (Dest != null)
			{
				Dest = (Dest.Clone() as byte[]);
				Dest[Index] = (byte)Val;
			}
		}

		public static string WStrFromWChar(char Val)
		{
			return new string(Val, 1);
		}

		public static byte[] LStrFromWStr([In] string Val)
		{
			byte[] bytes = {};
			if (Val != null && Val != "") // checkit
			{
				bytes = BDSSystem.AnsiEncoding.GetBytes(Val);
			}
			return bytes;
		}

		public static string WStrFromLStr([In] byte[] Val)
		{
			string @string = "";
			if (Val != null && Val.Length > 0) // checkit
			{
				@string = BDSSystem.AnsiEncoding.GetString( Val, 0, ((Val != null) ? Val.Length : 0) - 1 + 1);
			}
			return @string;
		}

		public static byte[] LStrFromLArray(byte[] Val)
		{
			byte[] array = null;
			int num = (Val != null) ? Val.Length : 0;
			if (num > 0)
			{
				byte[] array2 = array;
				int arg_1E_0;
				if ((arg_1E_0 = num) < 0)
				{
					arg_1E_0 = 0;
				}
				byte[] array3;
				byte[] expr_23 = array3 = new byte[arg_1E_0];
				if (num > 0 && array2 != null)
				{
					int num2;
					if ((num2 = array2.Length) > num)
					{
						num2 = num;
					}
					if (num2 > 0)
					{
						Array.Copy(array2, array3, num2);
					}
				}
				array = expr_23;
				Array.Copy(Val, 0, array, 0, num);
			}
			return array;
		}

		public static AnsiString AnsiToUtf8([In] AnsiString S)
		{
			AnsiString result;
			result.Data = null;
			if (S.Data != null)
			{
				result.Data = BDSSystem.LStrFromLArray(Encoding.UTF8.GetBytes(BDSSystem.WStrFromLStr(S.Data)));
			}
			return result;
		}

		public static AnsiString Utf8ToAnsi([In] AnsiString S)
		{
			AnsiString result;
			result.Data = null;
			if (S.Data != null)
			{
				byte[] arg_35_1 = S.Data;
				byte[] data = S.Data;
				result.Data = BDSSystem.LStrFromWStr(Encoding.UTF8.GetString(arg_35_1, 0, ((data != null) ? data.Length : 0)));
			}
			return result;
		}

		public static string StrToUtf8([In] string S)
		{
			byte[] src = Encoding.GetEncoding(1251).GetBytes(S);
			return Encoding.UTF8.GetString(src);
		}
	}
}
