using System;

using Ext.Utils;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKCore
{
	public abstract class CustomCommand
	{
		protected UndoManager FManager;

		public CustomCommand(UndoManager aManager)
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
