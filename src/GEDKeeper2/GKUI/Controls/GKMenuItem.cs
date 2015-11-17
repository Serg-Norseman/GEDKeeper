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

	/// <summary>
	/// 
	/// </summary>
	public class GKToolStripMenuItem : ToolStripMenuItem
	{
		public new int Tag;

		public GKToolStripMenuItem(string text, int tag) : base(text)
		{
			this.Tag = tag;
		}
	}
}
