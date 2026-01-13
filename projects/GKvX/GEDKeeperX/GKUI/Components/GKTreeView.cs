/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
