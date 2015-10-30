using System;
using System.Windows.Forms;

namespace GKUI.Controls
{
	/// <summary>
	/// 
	/// </summary>
	public class GKMenuItem : MenuItem
	{
		public new int Tag;

		public GKMenuItem(string text, int tag) : base(text)
		{
			this.Tag = tag;
		}
	}
}
