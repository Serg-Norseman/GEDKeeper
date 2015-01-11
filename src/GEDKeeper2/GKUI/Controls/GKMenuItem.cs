using System;
using System.Windows.Forms;

/// <summary>
/// 
/// </summary>

namespace GKUI.Controls
{
	public class GKMenuItem : MenuItem
	{
		public new int Tag;

		public GKMenuItem(string text, int tag) : base(text)
		{
			this.Tag = tag;
		}
	}
}
