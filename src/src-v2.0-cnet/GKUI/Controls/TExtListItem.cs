using GKSys;
using System;
using System.Drawing;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace GKUI.Controls
{
	public class TExtListItem : ListViewItem
	{
		public object Data;

		public TExtListItem()
		{
		}

		public TExtListItem(string text) : base(text)
		{
		}

		public TExtListItem(string text, int imageIndex) : base(text, imageIndex)
		{
		}

		public TExtListItem(string[] items) : base(items)
		{
		}

		public TExtListItem(string[] items, int imageIndex) : base(items, imageIndex)
		{
		}

		public TExtListItem(string[] items, int imageIndex, Color foreColor, Color backColor, Font font) : base(items, imageIndex, foreColor, backColor, font)
		{
		}

		public TExtListItem(ListViewItem.ListViewSubItem[] subItems, int imageIndex) : base(subItems, imageIndex)
		{
		}
	}
}
