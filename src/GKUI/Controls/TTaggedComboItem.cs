using System;

using GKSys;

/// <summary>
/// Localization: clean
/// </summary>

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
			SysUtils.Free(this);
		}
	}
}
