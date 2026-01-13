/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;

namespace GKCore.Utilities
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
