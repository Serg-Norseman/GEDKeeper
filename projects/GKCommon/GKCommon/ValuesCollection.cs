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

using System;
using System.Collections;
using System.Collections.Specialized;

namespace GKCommon
{
    [Serializable]
    public sealed class ValuesCollection : NameObjectCollectionBase
    {
        public ValuesCollection()
        {
        }

        public void Clear()
        {
            base.BaseClear();
        }

        public void Add(string name, string value, bool excludeDuplicates = false)
        {
            ArrayList arrayList = (ArrayList)base.BaseGet(name);

            if (arrayList == null) {
                arrayList = new ArrayList(1);
                base.BaseAdd(name, arrayList);
            }

            if (value == null) return;

            if (!excludeDuplicates) {
                arrayList.Add(value);
            } else {
                if (!arrayList.Contains(value)) arrayList.Add(value);
            }
        }

        public void Remove(string name)
        {
            base.BaseRemove(name);
        }

        public string[] GetValues(string name)
        {
            ArrayList list = (ArrayList)base.BaseGet(name);
            if (list == null) {
                return null;
            }

            int num = list.Count;
            string[] array = new string[num];
            list.CopyTo(0, array, 0, num);

            return array;
        }
    }
}
