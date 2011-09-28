using System;
using GKCore.Sys;

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
