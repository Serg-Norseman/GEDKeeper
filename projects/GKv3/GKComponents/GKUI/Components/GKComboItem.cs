/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
    public class GKComboItem<T> : Eto.Forms.IListItem, IComboItem
    {
        public string Text { get; set; }
        public T Tag { get; private set; }
        public IImage Image { get; private set; }

        public string Key
        {
            get { return Text; }
        }

        public GKComboItem(string text)
        {
            Text = text;
        }

        public GKComboItem(string text, T tag)
        {
            Text = text;
            Tag = tag;
        }

        public GKComboItem(string text, T tag, IImage image)
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
