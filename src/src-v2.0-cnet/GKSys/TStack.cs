using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GKSys
{
	public class TStack : TOrderedList
	{

		protected internal override void PushItem(object AItem)
		{
			base.List.Add(AItem);
		}

		public void Clear()
		{
			base.List.Clear();
		}

	}
}
