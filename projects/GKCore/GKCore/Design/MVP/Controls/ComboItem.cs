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

using GKCore.Design.Graphics;

namespace GKCore.Design.MVP.Controls
{
    /// <summary>
    /// 
    /// </summary>
    public class ComboItem<T> : IComboItem
    {
        public string Text { get; private set; }

        public T Tag { get; private set; }

        public IImage Image { get; private set; }

        public ComboItem(string text)
        {
            Text = text;
        }

        public ComboItem(string text, T tag)
        {
            Text = text;
            Tag = tag;
        }

        public ComboItem(string text, T tag, IImage image)
        {
            Text = text;
            Tag = tag;
            Image = image;
        }

        public override string ToString()
        {
            return Text;
        }
    }
}
