using System;
using System.Windows.Forms;

namespace GKUI.Controls
{
	/// <summary>
	/// 
	/// </summary>
    [Serializable]
	public class GKTreeNode : TreeNode
	{
		public object Data;

		public GKTreeNode(string text, object data) : base(text)
		{
			this.Data = data;
		}
	}
}
