using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GKSys
{

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
}
