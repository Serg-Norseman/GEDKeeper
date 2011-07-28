using GKSys;
using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace GKUI.Controls
{
	public class TGKMenuItem : MenuItem
	{
		public int Tag;

		public TGKMenuItem(string text, int tag) : base(text)
		{
			this.Tag = tag;
		}
	}
}
