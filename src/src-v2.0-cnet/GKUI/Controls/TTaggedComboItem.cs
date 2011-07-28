using GKSys;
using System;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GKUI.Controls
{
	public class TTaggedComboItem
	{
		public string Caption;
		public int Tag;

		public TTaggedComboItem(string aCaption, int aTag)
		{
			this.Caption = aCaption;
			this.Tag = aTag;
		}

		public override string ToString()
		{
			return this.Caption;
		}

		public void Free()
		{
			TObjectHelper.Free(this);
		}
	}
}
