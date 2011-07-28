using System;

namespace GKSys
{
	public class TObjectList : TList
	{
		private bool FOwnsObjects;

		public bool OwnsObjects
		{
			get { return this.FOwnsObjects; }
			set { this.FOwnsObjects = value; }
		}

		protected internal override void Notify(object Instance, TListNotification Action)
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

		public int FindInstanceOf(TClass AClass, bool AExact, int AStartAt)
		{
			/*int Result = -1;
			int num = base.Count - 1;
			int I = AStartAt;
			if (num >= I)
			{
				num++;
				while ((!AExact || !object.Equals(TObjectHelper.ClassType(base[I]), AClass)) && (AExact || !TObjectHelper.InheritsFrom(BDSSystem.GetMetaFromObject(base[I]), AClass)))
				{
					I++;
					if (I == num)
					{
						return Result;
					}
				}
				Result = I;
			}
			return Result;*/
			//alert!!! restore!
			return 0;
		}
	}
}
