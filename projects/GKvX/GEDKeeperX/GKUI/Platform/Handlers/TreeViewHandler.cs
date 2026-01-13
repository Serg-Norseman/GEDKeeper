/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design.Controls;
using GKUI.Components;

namespace GKUI.Platform
{
    public sealed class TreeViewHandler : BaseControlHandler<GKTreeView, TreeViewHandler>, ITreeView
    {
        public TreeViewHandler(GKTreeView control) : base(control)
        {
        }

        public ITVNode AddNode(ITVNode parent, string name, object tag)
        {
            var node = Control.CreateTreeViewNode(name, tag);
            if (parent == null) {
                Control.RootNodes.Add(node);
            } else {
                ((GKTreeNode)parent).Children.Add(node);
            }
            return node;
        }

        public void BeginUpdate()
        {
            Control.BatchBegin();
            /*Control.DataStore = null;
            fRootNode = new TreeItem();*/
        }

        public void Clear()
        {
            Control.RootNodes.Clear();
        }

        public void EndUpdate()
        {
            Control.BatchCommit();
            /*Control.DataStore = fRootNode;
            Control.RefreshData();*/
        }

        public void Expand(ITVNode node)
        {
            /*GKTreeNode treeNode = node as GKTreeNode;
            if (treeNode != null) {
                treeNode.Expanded = true;
            }*/
        }

        public object GetSelectedData()
        {
            return null;
        }
    }
}
