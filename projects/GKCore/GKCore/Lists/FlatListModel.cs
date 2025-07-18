/*
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

using System.Collections.Generic;
using GKCore.Options;

namespace GKCore.Lists
{
    public sealed class FlatItem
    {
        public object Tag { get; set; }
        public object[] Values { get; set; }
    }


    public sealed class FlatListModel : SimpleListModel<FlatItem>
    {
        private readonly List<FlatItem> fItems;


        public FlatListModel() :
            base(null, new ListColumns(GKListType.ltNone))
        {
            fItems = new List<FlatItem>();
            DataSource = fItems;
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object[] values = fFetchedRec.Values;
            object result = (colType >= values.Length) ? null : values[colType];
            return result;
        }

        public void AddItem(object tag, params object[] values)
        {
            var item = new FlatItem() {
                Tag = tag,
                Values = values
            };
            fItems.Add(item);
        }

        public override void Clear()
        {
            fItems.Clear();
        }
    }
}
