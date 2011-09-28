using System;
using System.Runtime.InteropServices;

namespace GKCore.Sys
{
	public class TSetElementTypeAttribute : Attribute
	{
		private Type FElementType;

		public Type ElementType
		{
			get { return this.FElementType; }
		}

		public TSetElementTypeAttribute(Type AElementType)
		{
			this.FElementType = AElementType;
		}
	}

	public enum LongBool : int
	{}

	public class HINST : Attribute
	{
		public static Type GetImplType()
		{
			return typeof(int);
		}
	}

	public class HKL : Attribute
	{
		public static Type GetImplType()
		{
			return typeof(uint);
		}
	}

	public class HRGN : Attribute
	{
		public static Type GetImplType()
		{
			return typeof(uint);
		}
	}

	public class HRSRC : Attribute
	{
		public static Type GetImplType()
		{
			return typeof(int);
		}
	}

	public class HWND : Attribute
	{
		public static Type GetImplType()
		{
			return typeof(uint);
		}
	}

	public class HMODULE : Attribute
	{
		public static Type GetImplType()
		{
			return typeof(int);
		}
	}

	[StructLayout(LayoutKind.Sequential, Pack = 1)]
	public struct TVSFixedFileInfo
	{
		public uint dwSignature;
		public uint dwStrucVersion;
		public uint dwFileVersionMS;
		public uint dwFileVersionLS;
		public uint dwProductVersionMS;
		public uint dwProductVersionLS;
		public uint dwFileFlagsMask;
		public uint dwFileFlags;
		public uint dwFileOS;
		public uint dwFileType;
		public uint dwFileSubtype;
		public uint dwFileDateMS;
		public uint dwFileDateLS;
	}

	[StructLayout(LayoutKind.Sequential, CharSet = CharSet.Auto)]
	public struct TWin32FindData
	{
		public uint dwFileAttributes;
		public System.Runtime.InteropServices.ComTypes.FILETIME ftCreationTime;
		public System.Runtime.InteropServices.ComTypes.FILETIME ftLastAccessTime;
		public System.Runtime.InteropServices.ComTypes.FILETIME ftLastWriteTime;
		public uint nFileSizeHigh;
		public uint nFileSizeLow;
		public uint dwReserved0;
		public uint dwReserved1;
		[MarshalAs(UnmanagedType.ByValTStr, SizeConst=260)]
		public string cFileName;
		[MarshalAs(UnmanagedType.ByValTStr, SizeConst=14)]
		public string cAlternateFileName;
	}

	public struct TSearchRec
	{
		public int Attr;
		public string Name;
		public int ExcludeAttr;
		public int FindHandle;
		public TWin32FindData FindData;
	}

	[StructLayout(LayoutKind.Sequential, Pack = 1)]
	public struct TScrollInfo
	{
		public uint cbSize;
		public uint fMask;
		public int nMin;
		public int nMax;
		public uint nPage;
		public int nPos;
		public int nTrackPos;
	}


	public delegate void TNotifyEvent(object Sender);


	[StructLayout(LayoutKind.Sequential, Pack = 1, Size = 1)]
	public struct _Unnamed1
	{
		public static readonly byte[] Map;
		static _Unnamed1()
		{
			_Unnamed1.Map = new byte[]
			{
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				62, 
				0, 
				0, 
				0, 
				63, 
				52, 
				53, 
				54, 
				55, 
				56, 
				57, 
				58, 
				59, 
				60, 
				61, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
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
				10, 
				11, 
				12, 
				13, 
				14, 
				15, 
				16, 
				17, 
				18, 
				19, 
				20, 
				21, 
				22, 
				23, 
				24, 
				25, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				26, 
				27, 
				28, 
				29, 
				30, 
				31, 
				32, 
				33, 
				34, 
				35, 
				36, 
				37, 
				38, 
				39, 
				40, 
				41, 
				42, 
				43, 
				44, 
				45, 
				46, 
				47, 
				48, 
				49, 
				50, 
				51, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0
			};
		}
	}


	[StructLayout(LayoutKind.Sequential, Pack = 1, Size = 1)]
	public struct _Unnamed2
	{
		public static readonly char[] Map;
		static _Unnamed2()
		{
			_Unnamed2.Map = new char[]
			{
				'A', 
				'B', 
				'C', 
				'D', 
				'E', 
				'F', 
				'G', 
				'H', 
				'I', 
				'J', 
				'K', 
				'L', 
				'M', 
				'N', 
				'O', 
				'P', 
				'Q', 
				'R', 
				'S', 
				'T', 
				'U', 
				'V', 
				'W', 
				'X', 
				'Y', 
				'Z', 
				'a', 
				'b', 
				'c', 
				'd', 
				'e', 
				'f', 
				'g', 
				'h', 
				'i', 
				'j', 
				'k', 
				'l', 
				'm', 
				'n', 
				'o', 
				'p', 
				'q', 
				'r', 
				's', 
				't', 
				'u', 
				'v', 
				'w', 
				'x', 
				'y', 
				'z', 
				'0', 
				'1', 
				'2', 
				'3', 
				'4', 
				'5', 
				'6', 
				'7', 
				'8', 
				'9', 
				'+', 
				'/'
			};
		}
	}
}
