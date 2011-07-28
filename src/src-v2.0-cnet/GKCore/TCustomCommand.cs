using GKSys;
using System;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GKCore
{
	public abstract class TCustomCommand
	{
		protected internal TUndoManager FManager;

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
