using GKSys;
using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
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
