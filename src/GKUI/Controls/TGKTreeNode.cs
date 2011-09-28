using System;
using System.Windows.Forms;

namespace GKUI.Controls
{
	public class TGKTreeNode : TreeNode
	{
		public object Data;

		public TGKTreeNode(string text, object data) : base(text)
		{
			this.Data = data;
		}
	}
}
