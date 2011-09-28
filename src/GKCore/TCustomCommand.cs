using System;

using GKCore.Sys;

namespace GKCore
{
	public abstract class TCustomCommand
	{
		protected TUndoManager FManager;

		public TCustomCommand(TUndoManager aManager)
		{
			this.FManager = aManager;
		}

		public abstract bool Redo();

		public abstract void Undo();

		public void Free()
		{
			TObjectHelper.Free(this);
		}
	}
}
