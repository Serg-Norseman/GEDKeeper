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
                        throw new ArgumentOutOfRangeException("index");

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
    }
}
