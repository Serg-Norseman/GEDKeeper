using System;

using Ext.Utils;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKCore
{
	public abstract class CustomFilter
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
