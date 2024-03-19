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
using GKCore.Options;
using GKCore.Types;

namespace GDModel
{
    /// <summary>
    /// Enumeration of administrative territorial divisions (ATD).
    /// </summary>
    public enum ATDEnumeration
    {
        // from largest to smallest
        fLtS,
        // from smallest to largest
        fStL
    }


    public sealed class GDMLocationRecord : GDMRecord
    {
        private readonly GDMMap fMap;
        private readonly GDMList<GDMLocationName> fNames;
        private readonly GDMList<GDMLocationLink> fTopLevels;


        public string LocationName
        {
            get {
                int num = fNames.Count;
                return (num == 0) ? string.Empty : fNames[num - 1].StringValue;
            }
            set {
                GDMLocationName locName;
                int num = fNames.Count;
                if (num == 0) {
                    locName = new GDMLocationName();
                    fNames.Add(locName);
                } else {
                    locName = fNames[num - 1];
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

        /// <summary>
        /// The MoveTo() merges records and their references, but does not change the text in the target.
        /// </summary>
        /// <param name="targetRecord"></param>
        public override void MoveTo(GDMRecord targetRecord)
        {
            GDMLocationRecord targetLoc = (targetRecord as GDMLocationRecord);
            if (targetLoc == null)
                throw new ArgumentException(@"Argument is null or wrong type", "targetRecord");

            base.MoveTo(targetRecord);

            if (targetLoc.Map.IsEmpty() && !fMap.IsEmpty()) {
                targetLoc.Map.Assign(fMap);
            }

            while (fNames.Count > 0) {
                GDMLocationName obj = fNames.Extract(0);
                targetLoc.Names.Add(obj);
            }
            SortNames();

            while (fTopLevels.Count > 0) {
                GDMLocationLink obj = fTopLevels.Extract(0);
                targetLoc.TopLevels.Add(obj);
            }
            SortTopLevels();
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

        public GDMList<GDMLocationName> GetFullNames(GDMTree tree, ATDEnumeration atdEnum)
        {
            GDMList<GDMLocationName> result;

            if (fTopLevels.Count == 0) {
                result = fNames;
            } else {
                result = new GDMList<GDMLocationName>();

                var buffer = new List<GDMLocationName>();
                for (int j = 0; j < fTopLevels.Count; j++) {
                    var topLevel = fTopLevels[j];
                    var topLoc = tree.GetPtrValue<GDMLocationRecord>(topLevel);
                    if (topLoc == null) continue;

                    bool wasJoin = false;
                    var topNames = topLoc.GetFullNames(tree, atdEnum);
                    for (int i = 0; i < topNames.Count; i++) {
                        var topName = topNames[i];

                        var interDate = GDMCustomDate.GetIntersection(topLevel.Date.Value, topName.Date.Value);
                        if (!interDate.IsEmpty()) {
                            var newLocName = new GDMLocationName();
                            newLocName.SetRawData(topName.StringValue, interDate);
                            buffer.Add(newLocName);
                            wasJoin = true;
                        }
                    }

                    if (!wasJoin && topNames.Count > 0) {
                        var newLocName = new GDMLocationName();
                        newLocName.SetRawData(topNames[topNames.Count - 1].StringValue, topLevel.Date.Value);
                        buffer.Add(newLocName);
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
                            string newName = (atdEnum == ATDEnumeration.fLtS) ? topName + ", " + locName.StringValue : locName.StringValue + ", " + topName;

                            var newLocName = new GDMLocationName();
                            newLocName.SetRawData(newName, interDate);
                            result.Add(newLocName);
                        }
                    }
                }
            }

            return result;
        }

        public string GetNameByDate(GDMCustomDate date, ATDEnumeration atdEnum, bool full = false)
        {
            var namesList = (!full) ? fNames : GetFullNames(Tree, atdEnum);

            if (date != null && !date.IsEmpty()) {
                for (int i = 0; i < namesList.Count; i++) {
                    var locName = namesList[i];

                    var interDate = GDMCustomDate.GetIntersection(date, locName.Date.Value);
                    if (!interDate.IsEmpty()) {
                        return locName.StringValue;
                    }
                }
            }

            int num = namesList.Count;
            return (num == 0) ? string.Empty : namesList[num - 1].StringValue;
        }

        public string GetNameByDate(GDMCustomDate date, bool full = false)
        {
            ATDEnumeration atdEnum = (!GlobalOptions.Instance.ReversePlaceEntitiesOrder) ? ATDEnumeration.fLtS : ATDEnumeration.fStL;

            return GetNameByDate(date, atdEnum, full);
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
            return udn1.CompareTo(udn2);
        }
    }

    public interface IGDMLocationElement
    {
        GDMDateValue Date { get; }
    }
}
