using System;

using GKSys;

/// <summary>
/// Localization: unknown
/// </summary>

namespace GKCore
{
	public abstract class TCustomCommand
	{
		protected UndoManager FManager;

		public TCustomCommand(UndoManager aManager)
		{
			this.FManager = aManager;
		}

		public abstract bool Redo();

		public abstract void Undo();

		public void Free()
		{
			SysUtils.Free(this);
		}
	}
}
