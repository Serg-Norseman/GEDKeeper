using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GKSys
{
	[StructLayout(LayoutKind.Auto)]
	public struct TSearchRec
	{
		public int Attr;
		public string Name;
		public int ExcludeAttr;
		public int FindHandle;
		public TWin32FindData FindData;
	}
}
