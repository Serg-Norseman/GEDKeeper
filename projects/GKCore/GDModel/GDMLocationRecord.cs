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
using GDModel.Providers.GEDCOM;
using GKCore.Calendar;
using GKCore.Types;

namespace GDModel
{
    public sealed class GDMLocationRecord : GDMRecord
    {
        private readonly GDMMap fMap;
        private GDMList<GDMLocationName> fNames;


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


        public GDMLocationRecord(GDMTree tree) : base(tree)
        {
            SetName(GEDCOMTagType._LOC);

            fMap = new GDMMap();
            fNames = new GDMList<GDMLocationName>();
        }

        internal override void TrimExcess()
        {
            base.TrimExcess();

            fMap.TrimExcess();
            fNames.TrimExcess();
        }

        public override void Assign(GDMTag source)
        {
            GDMLocationRecord otherLoc = (source as GDMLocationRecord);
            if (otherLoc == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(otherLoc);

            fMap.Assign(otherLoc.fMap);
            AssignList(otherLoc.fNames, Names);
        }

        public override void Clear()
        {
            base.Clear();

            fMap.Clear();
            fNames.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && fMap.IsEmpty() && (fNames.Count == 0);
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
        }

        public string GetNameByDate(GDMCustomDate date)
        {
            if (date != null && !date.IsEmpty()) {
                for (int i = 1; i < fNames.Count; i++) {
                    var locName = fNames[i];
                    if (!locName.Date.IsEmpty()) {
                        var interDate = GDMCustomDate.GetIntersection(date, locName.Date.Value);
                        if (!interDate.IsEmpty()) {
                            return locName.StringValue;
                        }
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
                if (!locName.Date.IsEmpty()) {
                    var interDate = GDMCustomDate.GetIntersection(prevDate, locName.Date.Value);
                    if (!interDate.IsEmpty()) {
                        return false;
                    }
                }
                prevDate = locName.Date.Value;
            }
            return true;
        }

        public void SortNames()
        {
            fNames.Sort(ChildrenEventsCompare);
        }

        private static int ChildrenEventsCompare(GDMLocationName cp1, GDMLocationName cp2)
        {
            UDN udn1 = cp1.Date.GetUDN();
            UDN udn2 = cp2.Date.GetUDN();
            return -udn1.CompareTo(udn2);
        }
    }
}
