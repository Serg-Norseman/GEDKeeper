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

using System;
using System.Collections.Generic;
using System.Windows.Forms;
using BSLib;
using GDModel;
using GKCore;
using GKCore.Design.Controls;
using GKCore.Interfaces;

namespace GKLocExpertPlugin
{
    public sealed class LocEntry
    {
        public string Text;

        public LocPair Pair;
        public bool HasDateIntersects;
        public GDMLocationLink TopLevelLink;
        public bool TopLevelIntersects;

        public LocEntry(string text)
        {
            Text = text;
        }
    }


    public sealed class LocPair
    {
        public GDMLocationRecord Location;
        public GDMLocationName Name;

        public LocPair(GDMLocationRecord location, GDMLocationName name)
        {
            Location = location;
            Name = name;
        }
    }


    public class LocExpertController
    {
        private readonly IBaseWindow fBase;

        public LocExpertController(IBaseWindow baseWin)
        {
            fBase = baseWin;
        }

        public void Analyze(List<LocEntry> entries, GDMCustomDate date)
        {
            // first pass, get location and their names by text
            for (int i = 0; i < entries.Count; i++) {
                var entry = entries[i];
                entry.Pair = FindLocation(entry.Text, true);
                entry.HasDateIntersects = (entry.Pair == null || entry.Pair.Name == null) ? false : !GDMCustomDate.GetIntersection(date, entry.Pair.Name.Date).IsEmpty();
            }

            // second pass, check links to top levels
            for (int i = 0, num = entries.Count; i < num; i++) {
                var entry = entries[i];
                if (entry.Pair == null) continue;

                var nextEntry = (i < num - 1) ? entries[i + 1] : null;
                if (nextEntry == null || nextEntry.Pair == null) continue;

                var topLevels = entry.Pair.Location.TopLevels;
                for (int j = 0; j < topLevels.Count; j++) {
                    var topLink = topLevels[j];

                    if (topLink.XRef == nextEntry.Pair.Location.XRef) {
                        entry.TopLevelLink = topLink;

                        var interDate = GDMCustomDate.GetIntersection(date, topLink.Date.Value);
                        if (!interDate.IsEmpty()) {
                            entry.TopLevelIntersects = true;
                        }
                    }
                }
            }
        }

        public LocPair FindLocation(string value, bool indistinct = false)
        {
            if (!indistinct) {
                value = "*" + value + "*";
            }

            var tree = fBase.Context.Tree;
            int num = tree.RecordsCount;
            for (int i = 0; i < num; i++) {
                var locRec = tree[i] as GDMLocationRecord;
                if (locRec == null) continue;

                var names = locRec.Names;
                for (int j = 0; j < names.Count; j++) {
                    var locName = names[j];

                    bool res;
                    if (indistinct) {
                        res = (IndistinctMatching.GetSimilarity(locName.StringValue, value) >= 0.85);
                    } else {
                        res = SysUtils.MatchPattern(value, locName.StringValue, false);
                    }

                    if (res) {
                        return new LocPair(locRec, locName);
                    }
                }
            }

            return null;
        }

        public void GetLocationsList(StringList<GDMLocationRecord> list, string filter, bool indistinct = false)
        {
            if (list == null) return;

            list.Clear();

            if (!indistinct) {
                filter = "*" + filter + "*";
            }

            var tree = fBase.Context.Tree;
            int num = tree.RecordsCount;
            for (int i = 0; i < num; i++) {
                var locRec = tree[i] as GDMLocationRecord;
                if (locRec == null) continue;

                var names = locRec.Names;
                for (int j = 0; j < names.Count; j++) {
                    var locName = names[j].StringValue;

                    bool res;
                    if (indistinct) {
                        res = (IndistinctMatching.GetSimilarity(locName, filter) >= 0.75);
                    } else {
                        res = SysUtils.MatchPattern(filter, locName, false);
                    }

                    if (res) {
                        list.AddObject(locName, locRec);
                    }
                }
            }
        }

        public void RefreshCombo(ToolStripComboBox comboBox, StringList<GDMLocationRecord> list)
        {
            comboBox.BeginUpdate();
            try {
                comboBox.Items.Clear();

                for (int i = 0, num = list.Count; i < num; i++) {
                    string st = list[i];
                    comboBox.Items.Add(new ComboItem<GDMLocationRecord>(st, list.GetObject(i)));
                }
            } finally {
                comboBox.EndUpdate();
            }

            comboBox.SelectionStart = comboBox.Text.Length;
        }

        public List<LocEntry> SplitPlaceStr(string place, bool reverseOrder)
        {
            var result = new List<LocEntry>();

            string[] placeParts = place.Split(GKUtils.PLACE_DELIMITERS, StringSplitOptions.None);
            if (placeParts.Length > 1) {
                if (reverseOrder) {
                    for (int i = 0; i < placeParts.Length; i++) {
                        result.Add(new LocEntry(placeParts[i].Trim()));
                    }
                } else {
                    for (int i = placeParts.Length - 1; i >= 0; i--) {
                        result.Add(new LocEntry(placeParts[i].Trim()));
                    }
                }
            }

            return result;
        }
    }
}
