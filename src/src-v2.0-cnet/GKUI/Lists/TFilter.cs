using GKSys;
using System;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GKUI.Lists
{
	public class TFilter
	{
		public enum TGroupMode : byte
		{
			gmAll,
			gmNone,
			gmAny,
			gmSelected
		}

		public void Free()
		{
			TObjectHelper.Free(this);
		}
	}
}
