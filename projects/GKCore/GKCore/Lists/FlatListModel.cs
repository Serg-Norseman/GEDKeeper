/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
