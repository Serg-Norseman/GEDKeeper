using System;

namespace GKCore.Sys
{
	public class TObjectList : TList
	{
		private bool FOwnsObjects;

		public bool OwnsObjects
		{
			get { return this.FOwnsObjects; }
			set { this.FOwnsObjects = value; }
		}

		protected override void Notify(object Instance, TListNotification Action)
		{
			if (this.OwnsObjects && Action == TListNotification.lnDeleted)
			{
				TObjectHelper.Free(Instance);
			}
			base.Notify(Instance, Action);
		}

		public TObjectList()
		{
			this.FOwnsObjects = true;
		}

		public TObjectList(bool AOwnsObjects)
		{
			this.FOwnsObjects = AOwnsObjects;
		}
	}
}
