﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
