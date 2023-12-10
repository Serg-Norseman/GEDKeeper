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

using System;
using System.Collections.ObjectModel;
using Adapt.Presentation.Controls;
using GKCore.Design.Controls;
using Xamarin.Forms;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public class GKTreeNode : TreeViewNode, ITVNode
    {
        public object Tag { get; set; }

        public string Text { get; set; }

        public GKTreeNode(string text, object tag) : base()
        {
            Text = text;
            Tag = tag;
        }
    }


    public class GKTreeView : TreeView
    {
        public event EventHandler MouseDoubleClick;


        public GKTreeView()
        {
            var rootNodes = new ObservableCollection<TreeViewNode>();
            base.RootNodes = rootNodes;
        }

        public new GKTreeNode CreateTreeViewNode(string text, object tag)
        {
            var label = new Label {
                VerticalOptions = LayoutOptions.Center,
                TextColor = Color.Black
            };
            label.SetBinding(Label.TextProperty, "Text");

            var node = new GKTreeNode(text, tag);
            node.BindingContext = node;
            node.Content = label;

            // set DataTemplate for expand button content
            node.ExpandButtonTemplate = new DataTemplate(() => new ExpandButtonContent { BindingContext = node });

            return node;
        }
    }
}
