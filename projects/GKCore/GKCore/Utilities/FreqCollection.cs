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

using System.Collections.Generic;
using GKCore.Design.Controls;

namespace GKCore.Utilities
{
    public sealed class FreqItem<X>
    {
        public X Ident;
        public string Name;
        public int Stat;
        public bool Use;

        public FreqItem(X ident, string name, int stat)
        {
            Ident = ident;
            Name = name;
            Stat = stat;
            Use = (stat > 0);
        }
    }

    public sealed class FreqCollection<I>
    {
        private readonly Dictionary<I, int> fStats;

        public int Count
        {
            get {
                return fStats.Count;
            }
        }

        public FreqCollection()
        {
            fStats = new Dictionary<I, int>();
        }

        public void Increment(I ident)
        {
            int val;
            if (fStats.TryGetValue(ident, out val)) {
                val += 1;
            } else {
                val = 1;
            }
            fStats[ident] = val;
        }

        public int GetValue(I key)
        {
            int value;
            return fStats.TryGetValue(key, out value) ? value : 0;
        }

        public IList<I> ToList()
        {
            var result = new List<I>();

            foreach (var pair in fStats) {
                if (pair.Value > 0)
                    result.Add(pair.Key);
            }

            return result;
        }

        public const string LineItem = " ------------------------------ ";

        public static void PopulateCombo<X>(IComboBox comboBox, List<FreqItem<X>> freqList, X defIdent)
        {
            freqList.Sort((x, y) => { return (-10 * x.Use.CompareTo(y.Use)) + x.Name.CompareTo(y.Name); });

            comboBox.Clear();
            bool use = false;
            for (int i = 0; i < freqList.Count; i++) {
                var item = freqList[i];
                if (use != item.Use && i != 0) {
                    comboBox.AddItem(LineItem, defIdent);
                }
                //comboBox.AddItem(string.Format("{0} [{1}]", item.Name, item.Stat), item.Ident);
                comboBox.AddItem(item.Name, item.Ident);
                use = item.Use;
            }
        }
    }
}
