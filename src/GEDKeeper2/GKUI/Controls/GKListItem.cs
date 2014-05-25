using System;
using System.Drawing;
using System.Windows.Forms;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Controls
{
    [Serializable]
	public class GKListItem : ListViewItem
	{
		public object Data;

		public GKListItem()
		{
		}

		public GKListItem(string text) : base(text)
		{
		}

		public GKListItem(string text, int imageIndex) : base(text, imageIndex)
		{
		}

		public GKListItem(string[] items) : base(items)
		{
		}

		public GKListItem(string[] items, int imageIndex) : base(items, imageIndex)
		{
		}

		public GKListItem(string[] items, int imageIndex, Color foreColor, Color backColor, Font font) : base(items, imageIndex, foreColor, backColor, font)
		{
		}

		public GKListItem(ListViewSubItem[] subItems, int imageIndex) : base(subItems, imageIndex)
		{
		}
	}
}
