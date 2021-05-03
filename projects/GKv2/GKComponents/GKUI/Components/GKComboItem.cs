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

using BSLib.Design;
using BSLib.Design.Graphics;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public class GKComboItem<T> : ComboItem<T>
    {
        public GKComboItem(string text) : base(text)
        {
        }

        public GKComboItem(string text, T tag) : base(text, tag)
        {
        }

        public GKComboItem(string text, T tag, IImage image) : base(text, tag, image)
        {
        }
    }
}
