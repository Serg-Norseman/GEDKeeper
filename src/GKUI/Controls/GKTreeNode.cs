using System;
using System.Windows.Forms;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Controls
{
	public class GKTreeNode : TreeNode
	{
		public object Data;

		public GKTreeNode(string text, object data) : base(text)
		{
			this.Data = data;
		}
	}
}
