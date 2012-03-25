using System;

using Ext.Utils;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Controls
{
	public class GKComboItem
	{
		public string Caption;
		public object Data;

		public GKComboItem(string aCaption, object aData)
		{
			this.Caption = aCaption;
			this.Data = aData;
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
