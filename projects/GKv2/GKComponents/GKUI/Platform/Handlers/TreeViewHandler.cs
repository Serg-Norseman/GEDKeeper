/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Windows.Forms;
using GKCore.Design.Controls;

namespace GKUI.Platform.Handlers
{
    public class TreeNodeEx : TreeNode, ITVNode
    {
        public TreeNodeEx(string text, object tag) : base(text)
        {
            Tag = tag;
        }
    }


    public sealed class TreeViewHandler : BaseControlHandler<TreeView, TreeViewHandler>, ITreeView
    {
        public TreeViewHandler(TreeView control) : base(control)
        {
        }

        public ITVNode AddNode(ITVNode parent, string name, object tag)
        {
            var node = new TreeNodeEx(name, tag);
            if (parent == null) {
                Control.Nodes.Add(node);
            } else {
                ((TreeNodeEx)parent).Nodes.Add(node);
            }
            return node;
        }

        public void BeginUpdate()
        {
            Control.BeginUpdate();
        }

        public void Clear()
        {
            Control.Nodes.Clear();
        }

        public void EndUpdate()
        {
            Control.EndUpdate();
        }

        public void Expand(ITVNode node)
        {
            TreeNode treeNode = node as TreeNode;
            if (treeNode != null) {
                treeNode.ExpandAll();
            }
        }

        public object GetSelectedData()
        {
            TreeNodeEx node = Control.SelectedNode as TreeNodeEx;
            return (node == null) ? null : node.Tag;
        }
    }
}
