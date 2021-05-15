/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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

using Eto.Drawing;
using Eto.Forms;

namespace GKUI.Components
{
    public class DefStackLayout : StackLayout
    {
        public DefStackLayout(Orientation orientation) : this(10, 10, orientation)
        {
        }

        public DefStackLayout(int padding, int spacing, Orientation orientation)
        {
            Orientation = orientation;
            Padding = new Padding(padding);
            Spacing = spacing;
        }

        public DefStackLayout(Orientation orientation, int spacing, params Control[] items)
        {
            Orientation = orientation;
            Padding = new Padding(0);
            Spacing = spacing;
            foreach (var item in items)
                Items.Add(item);
        }

        public DefStackLayout(params Control[] items) : this(Orientation.Vertical, 0, items)
        {
        }
    }
}
