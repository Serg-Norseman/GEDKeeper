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
using GDModel.Providers.GEDCOM;
using GKCore.Calendar;
using GKCore.Types;

namespace GDModel
{
    public sealed class GDMLocationRecord : GDMRecord
    {
        private readonly GDMMap fMap;
        private GDMList<GDMLocationName> fNames;
        private GDMList<GDMLocationLink> fTopLevels;


        public string LocationName
        {
            get { return (fNames.Count == 0) ? string.Empty : fNames[0].StringValue; }
            set {
                GDMLocationName locName;
                if (fNames.Count == 0) {
                    locName = new GDMLocationName();
                    fNames.Add(locName);
                } else {
                    locName = fNames[0];
                }
                locName.StringValue = value;
            }
        }

        public GDMMap Map
        {
            get { return fMap; }
        }

        public GDMList<GDMLocationName> Names
        {
            get { return fNames; }
        }

        public GDMList<GDMLocationLink> TopLevels
        {
            get { return fTopLevels; }
        }


        public GDMLocationRecord(GDMTree tree) : base(tree)
        {
            SetName(GEDCOMTagType._LOC);

            fMap = new GDMMap();
            fNames = new GDMList<GDMLocationName>();
            fTopLevels = new GDMList<GDMLocationLink>();
        }

        internal override void TrimExcess()
        {
            base.TrimExcess();

            fMap.TrimExcess();
            fNames.TrimExcess();
            fTopLevels.TrimExcess();
        }

        public override void Assign(GDMTag source)
        {
            GDMLocationRecord otherLoc = (source as GDMLocationRecord);
            if (otherLoc == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(otherLoc);

            fMap.Assign(otherLoc.fMap);
            AssignList(otherLoc.fNames, Names);
            AssignList(otherLoc.fTopLevels, TopLevels);
        }

        public override void Clear()
        {
            base.Clear();

            fMap.Clear();
            fNames.Clear();
            fTopLevels.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && fMap.IsEmpty() && (fNames.Count == 0) && (fTopLevels.Count == 0);
        }

        // TODO: connect to use
        public override float IsMatch(GDMTag tag, MatchParams matchParams)
        {
            GDMLocationRecord otherLoc = tag as GDMLocationRecord;
            if (otherLoc == null) return 0.0f;

            float match = GetStrMatch(LocationName, otherLoc.LocationName, matchParams);
            return match;
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);
            fMap.ReplaceXRefs(map);
            fNames.ReplaceXRefs(map);
            fTopLevels.ReplaceXRefs(map);
        }

        public IList<GDMLocationName> GetFullNames(GDMTree tree)
        {
            var result = new List<GDMLocationName>();

            if (fTopLevels.Count > 0) {
                var buffer = new List<GDMLocationName>();

                for (int j = 0; j < fTopLevels.Count; j++) {
                    var topLevel = fTopLevels[j];
                    var topLoc = tree.GetPtrValue<GDMLocationRecord>(topLevel);
                    if (topLoc == null) continue;

                    var topNames = topLoc.GetFullNames(tree);
                    for (int i = 0; i < topNames.Count; i++) {
                        var topName = topNames[i];

                        var interDate = GDMCustomDate.GetIntersection(topLevel.Date.Value, topName.Date.Value);
                        if (!interDate.IsEmpty()) {
                            var newLocName = new GDMLocationName();
                            newLocName.StringValue = topName.StringValue;
                            newLocName.Date.ParseString(interDate.StringValue);
                            buffer.Add(newLocName);
                        }
                    }
                }

                for (int j = 0; j < buffer.Count; j++) {
                    var topLocName = buffer[j];
                    var topName = topLocName.StringValue;
                    var topDate = topLocName.Date.Value;

                    for (int i = 0; i < fNames.Count; i++) {
                        var locName = fNames[i];

                        var interDate = GDMCustomDate.GetIntersection(topDate, locName.Date.Value);
                        if (!interDate.IsEmpty()) {
                            string newName = locName.StringValue + ", " + topName;

                            var newLocName = new GDMLocationName();
                            newLocName.StringValue = newName;
                            newLocName.Date.ParseString(interDate.StringValue);
                            result.Add(newLocName);
                        }
                    }
                }
            } else {
                for (int i = 0; i < fNames.Count; i++) {
                    var locName = fNames[i];
                    if (locName.Date.IsEmpty()) continue;

                    result.Add(locName);
                }
            }

            return result;
        }

        public string GetNameByDate(GDMCustomDate date)
        {
            if (date != null && !date.IsEmpty()) {
                for (int i = 1; i < fNames.Count; i++) {
                    var locName = fNames[i];

                    var interDate = GDMCustomDate.GetIntersection(date, locName.Date.Value);
                    if (!interDate.IsEmpty()) {
                        return locName.StringValue;
                    }
                }
            }

            return LocationName;
        }

        public bool ValidateNames()
        {
            GDMCustomDate prevDate = null;
            for (int i = 0; i < fNames.Count; i++) {
                var locName = fNames[i];

                var interDate = GDMCustomDate.GetIntersection(prevDate, locName.Date.Value);
                if (!interDate.IsEmpty()) {
                    return false;
                }

                prevDate = locName.Date.Value;
            }
            return true;
        }

        public bool ValidateLinks()
        {
            GDMCustomDate prevDate = null;
            for (int i = 0; i < fTopLevels.Count; i++) {
                var locLink = fTopLevels[i];

                var interDate = GDMCustomDate.GetIntersection(prevDate, locLink.Date.Value);
                if (!interDate.IsEmpty()) {
                    return false;
                }

                prevDate = locLink.Date.Value;
            }
            return true;
        }

        public void SortNames()
        {
            fNames.Sort(ElementsCompare);
        }

        public void SortTopLevels()
        {
            fTopLevels.Sort(ElementsCompare);
        }

        private static int ElementsCompare(IGDMLocationElement cp1, IGDMLocationElement cp2)
        {
            UDN udn1 = cp1.Date.GetUDN();
            UDN udn2 = cp2.Date.GetUDN();
            return -udn1.CompareTo(udn2);
        }
    }

    public interface IGDMLocationElement
    {
        GDMDateValue Date { get; }
    }
}
