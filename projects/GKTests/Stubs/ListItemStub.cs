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
using GKCore.Design.Graphics;

namespace GKTests.Stubs
{
    public class ListItemStub : IListItem
    {
        public bool Checked
        {
            get { return false; }
            set { }
        }

        public object Data
        {
            get { return null; }
            set { }
        }

        public ListItemStub(object itemValue, object data)
        {
        }

        public void AddSubItem(object itemValue)
        {
        }

        public int CompareTo(object obj)
        {
            return 0;
        }

        public void SetBackColor(IColor color)
        {
        }

        public void SetFont(IFont font)
        {
        }

        public void SetForeColor(IColor color)
        {
        }

        public void SetSubItem(int index, object value)
        {
        }
    }
}
