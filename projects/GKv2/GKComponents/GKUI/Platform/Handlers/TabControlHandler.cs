/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Windows.Forms;
using GKCore.Design.Controls;

namespace GKUI.Platform.Handlers
{
    public sealed class TabControlHandler : BaseControlHandler<TabControl, TabControlHandler>, ITabControl
    {
        private class TabPageItems : ITabPages
        {
            private TabControl fTabControl;

            public ITabPage this[int index]
            {
                get {
                    if (index < 0 || index >= fTabControl.TabPages.Count)
                        throw new ArgumentOutOfRangeException("index");

                    return new TabPageHandler(fTabControl.TabPages[index]);
                }
            }

            public int Count
            {
                get { return fTabControl.TabPages.Count; }
            }

            public TabPageItems(TabControl control)
            {
                fTabControl = control;
            }
        }

        private TabPageItems fItems;

        public TabControlHandler(TabControl control) : base(control)
        {
            fItems = new TabPageItems(control);
        }

        public int SelectedIndex
        {
            get { return Control.SelectedIndex; }
            set { Control.SelectedIndex = value; }
        }

        public ITabPages Pages
        {
            get { return fItems; }
        }

        public void SetTabVisible(ITabPage tabPage, bool visible)
        {
            if (tabPage == null) return;

            var tabCtl = ((TabPageHandler)tabPage).Control;

            if (visible) {
                if (!Control.TabPages.Contains(tabCtl)) Control.TabPages.Add(tabCtl);
            } else {
                if (Control.TabPages.Contains(tabCtl)) Control.TabPages.Remove(tabCtl);
            }
        }
    }
}
