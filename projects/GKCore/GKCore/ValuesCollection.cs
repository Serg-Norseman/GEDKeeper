/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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

namespace GKCore
{
    public sealed class ValuesCollection
    {
        private readonly Dictionary<string, SortedSet<string>> fValues;

        public int Count
        {
            get { return fValues.Count; }
        }

        public ValuesCollection()
        {
            fValues = new Dictionary<string, SortedSet<string>>();
        }

        public void Clear()
        {
            fValues.Clear();
        }

        public void Add(string name, string value)
        {
            SortedSet<string> set;
            if (!fValues.TryGetValue(name, out set)) {
                set = new SortedSet<string>();
                fValues.Add(name, set);
            }

            if (!string.IsNullOrEmpty(value))
                set.Add(value);
        }

        public void Remove(string name)
        {
            fValues.Remove(name);
        }

        public string[] GetValues(string name)
        {
            SortedSet<string> set;
            if (!fValues.TryGetValue(name, out set)) {
                return new string[0];
            }

            int num = set.Count;
            string[] array = new string[num];
            set.CopyTo(array, 0, num);
            return array;
        }
    }
}
