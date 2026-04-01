/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design.Controls;
using Terminal.Gui;
using Terminal.Gui.Trees;

namespace GKUI.Platform.Handlers
{
    using gkITreeView = GKCore.Design.Controls.ITreeView;

    public sealed class TreeViewHandler : BaseControlHandler<TreeView, TreeViewHandler>, gkITreeView
    {
        private class GKTreeNode : TreeNode, ITVNode
        {
            public GKTreeNode(string text, object tag) : base(text, tag)
            {
            }
        }

        private TreeNode fRootNode;

        public TreeViewHandler(TreeView control) : base(control)
        {
        }

        public ITVNode AddNode(ITVNode parent, string name, object tag)
        {
            var node = new GKTreeNode(name, tag);
            if (parent == null) {
                //fRootNode.Children.Add(node);
                Control.AddObject(node);
            } else {
                ((GKTreeNode)parent).Children.Add(node);
            }
            return node;
        }

        public void BeginUpdate()
        {
            //fRootNode = new TreeItem();
        }

        public void Clear()
        {
            Control.ClearObjects();
        }

        public void EndUpdate()
        {
        }

        public void Expand(ITVNode node)
        {
            GKTreeNode treeNode = node as GKTreeNode;
            if (treeNode != null) {
                Control.Expand(treeNode);
            }
        }

        public object GetSelectedData()
        {
            GKTreeNode node = Control.SelectedObject as GKTreeNode;
            return (node == null) ? null : node.Tag;
        }
    }
}
