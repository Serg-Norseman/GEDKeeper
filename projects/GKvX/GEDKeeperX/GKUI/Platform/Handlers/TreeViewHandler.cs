/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
