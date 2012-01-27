using System;

using GKSys;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Lists
{
	public abstract class TFilter
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
			SysUtils.Free(this);
		}
	}
}
