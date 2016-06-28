using System;

namespace GKCore.Commands
{
	public abstract class CustomCommand
	{
		protected UndoManager fManager;

		public CustomCommand(UndoManager manager)
		{
			this.fManager = manager;
		}

		public abstract bool Redo();

		public abstract void Undo();
	}
}
