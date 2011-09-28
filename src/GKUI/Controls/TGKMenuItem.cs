using System;
using System.Windows.Forms;

namespace GKUI.Controls
{
	public class TGKMenuItem : MenuItem
	{
		public new int Tag;

		public TGKMenuItem(string text, int tag) : base(text)
		{
			this.Tag = tag;
		}
	}
}
