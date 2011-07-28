using GKSys;
using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GKSys
{
	public class HINST : TUniqueTypeModifier
	{
		public static Type GetImplType()
		{
			return typeof(int);
		}
	}

	public class HKL : TUniqueTypeModifier
	{
		public static Type GetImplType()
		{
			return typeof(uint);
		}
	}

	public class HRGN : TUniqueTypeModifier
	{
		public static Type GetImplType()
		{
			return typeof(uint);
		}
	}

	public class HRSRC : TUniqueTypeModifier
	{
		public static Type GetImplType()
		{
			return typeof(int);
		}
	}

	public class HWND : TUniqueTypeModifier
	{
		public static Type GetImplType()
		{
			return typeof(uint);
		}
	}

	public class HMODULE : TUniqueTypeModifier
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
		public uint nFileSizeHigh;
		public uint nFileSizeLow;
		public uint dwReserved0;
		public uint dwReserved1;
		[MarshalAs(23, SizeConst = 260)]
		public string cFileName;
		[MarshalAs(23, SizeConst = 14)]
		public string cAlternateFileName;
	}
}
