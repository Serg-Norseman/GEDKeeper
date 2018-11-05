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

using System.Collections.Generic;
using GKCore.Interfaces;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public class GKComboItem
    {
        public readonly string Caption;
        public readonly object Tag;
        public readonly IImage Image;

        public string Text
        {
            get { return Caption; }
            set { }
        }

        public string Key
        {
            get { return Caption; }
        }

        public GKComboItem(string caption)
        {
            Caption = caption;
        }

        public GKComboItem(string caption, object tag)
        {
            Caption = caption;
            Tag = tag;
        }

        public GKComboItem(string caption, object tag, IImage image)
        {
            Caption = caption;
            Tag = tag;
            Image = image;
        }

        public override string ToString()
        {
            return Caption;
        }

        public static IEnumerable<GKComboItem> Convert(IEnumerable<string> items)
        {
            var result = new List<GKComboItem>();

            foreach (var item in items) {
                result.Add(new GKComboItem(item));
            }

            return result;
        }
    }
}
