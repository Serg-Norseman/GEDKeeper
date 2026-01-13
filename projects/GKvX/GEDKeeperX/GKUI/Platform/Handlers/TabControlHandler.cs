/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GKCore.Design;
using GKCore.Design.Controls;
using Xam.Plugin.TabView;

namespace GKUI.Platform
{
    public sealed class TabPageHandler : ControlHandler<TabItem, TabPageHandler>, ITabPage
    {
        public TabPageHandler(TabItem control) : base(control)
        {
        }

        public bool Enabled
        {
            get { return true; }
            set { }
        }

        public bool Visible
        {
            get { return true; }
            set { }
        }

        public void Activate()
        {
        }

        public string Text
        {
            get { return Control.HeaderText; }
            set { Control.HeaderText = value; }
        }
    }


    public sealed class TabControlHandler : BaseControlHandler<TabViewControl, TabControlHandler>, ITabControl
    {
        private class TabPageItems : ITabPages
        {
            private readonly TabViewControl fTabControl;

            public ITabPage this[int index]
            {
                get {
                    if (index < 0 || index >= fTabControl.ItemSource.Count)
                        throw new ArgumentOutOfRangeException(nameof(index));

                    return new TabPageHandler(fTabControl.ItemSource[index]);
                }
            }

            public int Count
            {
                get { return fTabControl.ItemSource.Count; }
            }

            public TabPageItems(TabViewControl control)
            {
                fTabControl = control;
            }
        }

        private readonly TabPageItems fItems;

        public TabControlHandler(TabViewControl control) : base(control)
        {
            fItems = new TabPageItems(control);
        }

        public int SelectedIndex
        {
            get { return Control.SelectedTabIndex; }
            set { Control.SelectedTabIndex = value; }
        }

        public ITabPages Pages
        {
            get { return fItems; }
        }

        public void SetTabVisible(ITabPage tabPage, bool visible)
        {
            // not supported
        }
    }
}
