/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Linq;
using GKCore.Design.Controls;
using Terminal.Gui.Views;

namespace GKUI.Platform.Handlers
{
    public sealed class TabControlHandler : BaseControlHandler<TabView, TabControlHandler>, ITabControl
    {
        private class TabPageItems : ITabPages
        {
            private TabView fTabControl;

            public ITabPage this[int index]
            {
                get {
                    var tabs = fTabControl.Tabs;
                    if (index < 0 || index >= tabs.Count)
                        throw new ArgumentOutOfRangeException(nameof(index));

                    return new TabPageHandler(tabs.ElementAt(index));
                }
            }

            public int Count
            {
                get { return fTabControl.Tabs.Count; }
            }

            public TabPageItems(TabView control)
            {
                fTabControl = control;
            }
        }

        private TabPageItems fItems;

        public TabControlHandler(TabView control) : base(control)
        {
            fItems = new TabPageItems(control);
        }

        public int SelectedIndex
        {
            get { return Control.Tabs.ToList().IndexOf(Control.SelectedTab); }
            set { }
        }

        public ITabPages Pages
        {
            get { return fItems; }
        }

        public void SetTabVisible(ITabPage tabPage, bool visible)
        {
            if (tabPage != null)
                tabPage.Visible = visible;
        }
    }
}
