/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2019 by Sergey V. Zhdanovskih.
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
using System.IO;
using BSLib.Calendar;
using GDModel.Providers.GEDCOM;
using GKCore.Types;

namespace GDModel
{
    public enum GEDCOMSex
    {
        svNone,
        svMale,
        svFemale,
        svUndetermined,

        svLast = svUndetermined
    }


    public sealed class GDMIndividualRecord : GDMRecordWithEvents
    {
        private GDMList<GDMAlias> fAliasses;
        private GDMList<GDMAssociation> fAssociations;
        private GDMList<GDMChildToFamilyLink> fChildToFamilyLinks;
        private GDMList<GDMPointer> fGroups;
        private GDMList<GDMPersonalName> fPersonalNames;
        private GDMList<GDMSpouseToFamilyLink> fSpouseToFamilyLinks;
        private GEDCOMSex fSex;


        public GDMList<GDMAlias> Aliases
        {
            get { return fAliasses; }
        }

        public string AncestralFileNumber
        {
            get { return GetTagStringValue(GEDCOMTagType.AFN); }
            set { SetTagStringValue(GEDCOMTagType.AFN, value); }
        }

        public GDMList<GDMAssociation> Associations
        {
            get { return fAssociations; }
        }

        public bool Bookmark
        {
            get {
                return FindTag(GEDCOMTagType._BOOKMARK, 0) != null;
            }
            set {
                if (value) {
                    if (FindTag(GEDCOMTagType._BOOKMARK, 0) == null) {
                        AddTag(GEDCOMTagType._BOOKMARK, "", null);
                    }
                } else {
                    DeleteTag(GEDCOMTagType._BOOKMARK);
                }
            }
        }

        public GDMList<GDMChildToFamilyLink> ChildToFamilyLinks
        {
            get { return fChildToFamilyLinks; }
        }

        public GDMList<GDMPointer> Groups
        {
            get { return fGroups; }
        }

        public bool Patriarch
        {
            get {
                return FindTag(GEDCOMTagType._PATRIARCH, 0) != null;
            }
            set {
                if (value) {
                    if (FindTag(GEDCOMTagType._PATRIARCH, 0) == null) {
                        AddTag(GEDCOMTagType._PATRIARCH, "", null);
                    }
                } else {
                    DeleteTag(GEDCOMTagType._PATRIARCH);
                }
            }
        }

        public string PermanentRecordFileNumber
        {
            get { return GetTagStringValue(GEDCOMTagType.RFN); }
            set { SetTagStringValue(GEDCOMTagType.RFN, value); }
        }

        public GDMList<GDMPersonalName> PersonalNames
        {
            get { return fPersonalNames; }
        }

        public GEDCOMSex Sex
        {
            get { return fSex; }
            set { fSex = value; }
        }

        public GDMList<GDMSpouseToFamilyLink> SpouseToFamilyLinks
        {
            get { return fSpouseToFamilyLinks; }
        }


        public GDMIndividualRecord(GDMObject owner) : base(owner)
        {
            SetRecordType(GEDCOMRecordType.rtIndividual);
            SetName(GEDCOMTagType.INDI);

            fAliasses = new GDMList<GDMAlias>(this);
            fAssociations = new GDMList<GDMAssociation>(this);
            fChildToFamilyLinks = new GDMList<GDMChildToFamilyLink>(this);
            fGroups = new GDMList<GDMPointer>(this);
            fPersonalNames = new GDMList<GDMPersonalName>(this);
            fSpouseToFamilyLinks = new GDMList<GDMSpouseToFamilyLink>(this);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fAliasses.Dispose();
                fAssociations.Dispose();
                fChildToFamilyLinks.Dispose();
                fGroups.Dispose();
                fPersonalNames.Dispose();
                fSpouseToFamilyLinks.Dispose();
            }
            base.Dispose(disposing);
        }

        public override GDMCustomEvent AddEvent(GDMCustomEvent evt)
        {
            if (evt != null) {
                if (evt is GDMIndividualEvent || evt is GDMIndividualAttribute) {
                    Events.Add(evt);
                } else {
                    throw new ArgumentException(@"Event has the invalid type", "evt");
                }
            }

            return evt;
        }

        public GDMPersonalName AddPersonalName(GDMPersonalName value)
        {
            if (value != null) {
                fPersonalNames.Add(value);
            }
            return value;
        }

        public override void Clear()
        {
            base.Clear();

            fSex = GEDCOMSex.svNone;

            for (int i = fChildToFamilyLinks.Count - 1; i >= 0; i--) {
                GDMFamilyRecord family = fChildToFamilyLinks[i].Family;
                family.DeleteChild(this);
            }
            fChildToFamilyLinks.Clear();

            for (int i = fSpouseToFamilyLinks.Count - 1; i >= 0; i--) {
                GDMFamilyRecord family = fSpouseToFamilyLinks[i].Family;
                family.RemoveSpouse(this);
            }
            fSpouseToFamilyLinks.Clear();

            for (int i = fGroups.Count - 1; i >= 0; i--) {
                GDMGroupRecord group = (GDMGroupRecord)fGroups[i].Value;
                group.RemoveMember(this);
            }
            fGroups.Clear();

            fAliasses.Clear();
            fAssociations.Clear();
            fPersonalNames.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && (fSex == GEDCOMSex.svNone) && fPersonalNames.Count == 0
                && fChildToFamilyLinks.Count == 0 && fSpouseToFamilyLinks.Count == 0
                && fAssociations.Count == 0 && fAliasses.Count == 0 && fGroups.Count == 0;
        }

        public int IndexOfGroup(GDMGroupRecord groupRec)
        {
            if (groupRec != null) {
                int num = fGroups.Count;
                for (int i = 0; i < num; i++) {
                    if (fGroups[i].XRef == groupRec.XRef) {
                        return i;
                    }
                }
            }

            return -1;
        }

        public int IndexOfSpouse(GDMFamilyRecord familyRec)
        {
            if (familyRec != null) {
                int num = fSpouseToFamilyLinks.Count;
                for (int i = 0; i < num; i++) {
                    if (fSpouseToFamilyLinks[i].Family == familyRec) {
                        return i;
                    }
                }
            }

            return -1;
        }

        public void DeleteSpouseToFamilyLink(GDMFamilyRecord familyRec)
        {
            int num = fSpouseToFamilyLinks.Count;
            for (int i = 0; i < num; i++) {
                if (fSpouseToFamilyLinks[i].Family == familyRec) {
                    fSpouseToFamilyLinks.DeleteAt(i);
                    break;
                }
            }
        }

        public void DeleteChildToFamilyLink(GDMFamilyRecord familyRec)
        {
            int num = fChildToFamilyLinks.Count;
            for (int i = 0; i < num; i++) {
                if (fChildToFamilyLinks[i].Family == familyRec) {
                    fChildToFamilyLinks.DeleteAt(i);
                    break;
                }
            }
        }

        public void ExchangeSpouses(int index1, int index2)
        {
            fSpouseToFamilyLinks.Exchange(index1, index2);
        }

        public bool IsLive()
        {
            return FindEvent(GEDCOMTagType.DEAT) == null;
        }

        public override void Assign(GDMTag source)
        {
            GDMIndividualRecord sourceRec = source as GDMIndividualRecord;
            if (sourceRec == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(source);

            fSex = sourceRec.fSex;

            foreach (GDMPersonalName srcName in sourceRec.fPersonalNames) {
                GDMPersonalName copyName = new GDMPersonalName(this);
                copyName.Assign(srcName);
                AddPersonalName(copyName);
            }
        }

        public override void MoveTo(GDMRecord targetRecord, bool clearDest)
        {
            GDMIndividualRecord targetIndi = targetRecord as GDMIndividualRecord;
            if (targetIndi == null) {
                throw new ArgumentException(@"Argument is null or wrong type", "targetRecord");
            }

            /*if (!clearDest) {
                DeleteTag(GEDCOMTagType.SEX);
                DeleteTag(GEDCOMTagType._UID);
            }*/

            base.MoveTo(targetRecord, clearDest);

            targetIndi.Sex = fSex;

            while (fPersonalNames.Count > 0) {
                GDMPersonalName obj = fPersonalNames.Extract(0);
                obj.ResetOwner(targetIndi);
                targetIndi.AddPersonalName(obj);
            }

            if (targetIndi.ChildToFamilyLinks.Count == 0 && ChildToFamilyLinks.Count != 0 && fChildToFamilyLinks != null) {
                GDMChildToFamilyLink ctfLink = fChildToFamilyLinks.Extract(0);
                GDMFamilyRecord family = ctfLink.Family;

                int num = family.Children.Count;
                for (int i = 0; i < num; i++) {
                    GDMPointer childPtr = family.Children[i];

                    if (childPtr.StringValue == "@" + XRef + "@") {
                        childPtr.StringValue = "@" + targetRecord.XRef + "@";
                    }
                }

                ctfLink.ResetOwner(targetIndi);
                targetIndi.ChildToFamilyLinks.Add(ctfLink);
            }

            while (fSpouseToFamilyLinks.Count > 0) {
                GDMSpouseToFamilyLink stfLink = fSpouseToFamilyLinks.Extract(0);
                GDMFamilyRecord family = stfLink.Family;

                string targetXRef = "@" + targetRecord.XRef + "@";

                if (family.Husband.StringValue == "@" + XRef + "@") {
                    family.Husband.StringValue = targetXRef;
                } else if (family.Wife.StringValue == "@" + XRef + "@") {
                    family.Wife.StringValue = targetXRef;
                }

                stfLink.ResetOwner(targetIndi);
                targetIndi.SpouseToFamilyLinks.Add(stfLink);
            }

            while (fAssociations.Count > 0) {
                GDMAssociation obj = fAssociations.Extract(0);
                obj.ResetOwner(targetIndi);
                targetIndi.Associations.Add(obj);
            }

            while (fAliasses.Count > 0) {
                GDMAlias obj = fAliasses.Extract(0);
                obj.ResetOwner(targetIndi);
                targetIndi.Aliases.Add(obj);
            }

            while (fGroups.Count > 0) {
                GDMPointer obj = fGroups.Extract(0);
                obj.ResetOwner(targetIndi);
                targetIndi.Groups.Add(obj);
            }
        }

        public override void Pack()
        {
            base.Pack();

            fAliasses.Pack();
            fAssociations.Pack();
            fChildToFamilyLinks.Pack();
            fGroups.Pack();
            fPersonalNames.Pack();
            fSpouseToFamilyLinks.Pack();
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);

            fAliasses.ReplaceXRefs(map);
            fAssociations.ReplaceXRefs(map);
            fChildToFamilyLinks.ReplaceXRefs(map);
            fGroups.ReplaceXRefs(map);
            fPersonalNames.ReplaceXRefs(map);
            fSpouseToFamilyLinks.ReplaceXRefs(map);
        }

        public override void SaveToStream(StreamWriter stream, int level)
        {
            base.SaveToStream(stream, level);

            level += 1;
            WriteTagLine(stream, level, GEDCOMTagType.SEX, GEDCOMUtils.GetSexStr(fSex), true);

            fPersonalNames.SaveToStream(stream, level);
            fChildToFamilyLinks.SaveToStream(stream, level);
            fSpouseToFamilyLinks.SaveToStream(stream, level);
            Events.SaveToStream(stream, level); // for files content compatibility
            fAssociations.SaveToStream(stream, level);
            fAliasses.SaveToStream(stream, level);
            fGroups.SaveToStream(stream, level);
        }

        public sealed class LifeDatesRet
        {
            public readonly GDMCustomEvent BirthEvent;
            public readonly GDMCustomEvent DeathEvent;

            public LifeDatesRet(GDMCustomEvent birthEvent, GDMCustomEvent deathEvent)
            {
                BirthEvent = birthEvent;
                DeathEvent = deathEvent;
            }
        }

        public LifeDatesRet GetLifeDates()
        {
            GDMCustomEvent birthEvent = null;
            GDMCustomEvent deathEvent = null;

            int num = Events.Count;
            for (int i = 0; i < num; i++)
            {
                GDMCustomEvent evt = Events[i];

                if (evt.Name == GEDCOMTagType.BIRT && birthEvent == null) {
                    birthEvent = evt;
                } else if (evt.Name == GEDCOMTagType.DEAT && deathEvent == null) {
                    deathEvent = evt;
                }
            }

            return new LifeDatesRet(birthEvent, deathEvent);
        }

        /// <summary>
        /// Attention: returns or creates only the first marriage!
        /// </summary>
        /// <param name="canCreate">can create if does not exist</param>
        /// <returns></returns>
        public GDMFamilyRecord GetMarriageFamily(bool canCreate = false)
        {
            GDMFamilyRecord result = (fSpouseToFamilyLinks.Count < 1) ? null : fSpouseToFamilyLinks[0].Family;

            if (result == null && canCreate) {
                result = GetTree().CreateFamily();
                result.AddSpouse(this);
            }

            return result;
        }

        /// <summary>
        /// Attention: returns or creates only the first parents family!
        /// </summary>
        /// <param name="canCreate">can create if does not exist</param>
        /// <returns></returns>
        public GDMFamilyRecord GetParentsFamily(bool canCreate = false)
        {
            GDMFamilyRecord result = (fChildToFamilyLinks.Count < 1) ? null : fChildToFamilyLinks[0].Value as GDMFamilyRecord;

            if (result == null && canCreate) {
                result = GetTree().CreateFamily();
                result.AddChild(this);
            }

            return result;
        }

        public string GetPrimaryFullName()
        {
            string result = (fPersonalNames.Count <= 0) ? string.Empty : fPersonalNames[0].FullName;
            return result;
        }

        private string GetComparableName(bool onlyFirstPart)
        {
            string resName;

            if (fPersonalNames.Count > 0) {
                GDMPersonalName np = fPersonalNames[0];

                if (onlyFirstPart) {
                    resName = np.FirstPart;
                } else {
                    resName = np.StringValue;
                }
            } else {
                resName = "";
            }

            return resName;
        }

        public override float IsMatch(GDMTag tag, MatchParams matchParams)
        {
            GDMIndividualRecord indi = tag as GDMIndividualRecord;
            if (indi == null) return 0.0f;

            if (Sex != indi.Sex) return 0.0f;

            bool womanMode = (Sex == GEDCOMSex.svFemale);

            float matchesCount = 0.0f;
            float nameMatch = 0.0f;
            float birthMatch = 0.0f;
            float deathMatch = 0.0f;

            // check name
            /*for (int i = 0; i < indi.PersonalNames.Count; i++)
			{
				for (int k = 0; k < fPersonalNames.Count; k++)
				{
					float currentNameMatch = fPersonalNames[k].IsMatch(indi.PersonalNames[i]);
					nameMatch = Math.Max(nameMatch, currentNameMatch);
				}
			}*/

            string iName = GetComparableName(womanMode);
            string kName = indi.GetComparableName(womanMode);

            if (!string.IsNullOrEmpty(iName) && !string.IsNullOrEmpty(kName)) {
                nameMatch = GetStrMatch(iName, kName, matchParams);
                matchesCount++;
            }

            // 0% name match would be pointless checking other details
            if (nameMatch != 0.0f && matchParams.DatesCheck)
            {
                var dates = GetLifeDates();
                var indiDates = indi.GetLifeDates();

                if (dates.BirthEvent != null && indiDates.BirthEvent != null) {
                    birthMatch = dates.BirthEvent.IsMatch(indiDates.BirthEvent, matchParams);
                    matchesCount++;
                } else if (dates.BirthEvent == null && indiDates.BirthEvent == null) {
                    birthMatch = 100.0f;
                    matchesCount++;
                } else {
                    matchesCount++;
                }

                /*if (death != null && indiDeath != null) {
					deathMatch = death.IsMatch(indiDeath, matchParams);
					matches++;
				} else if (death == null && indiDeath == null) {
					deathMatch = 100.0f;
					matches++;
				} else {
					matches++;
				}*/
            }

            float match = (nameMatch + birthMatch + deathMatch) / matchesCount;
            return match;
        }

        public GDMAssociation AddAssociation(string relation, GDMIndividualRecord relPerson)
        {
            GDMAssociation result = new GDMAssociation(this);
            result.Relation = relation;
            result.Individual = relPerson;
            Associations.Add(result);
            return result;
        }

        public GDMMultimediaLink SetPrimaryMultimediaLink(GDMMultimediaRecord mediaRec)
        {
            if (mediaRec == null) return null;
            GDMMultimediaLink mmLink = null;

            int num = MultimediaLinks.Count;
            for (int i = 0; i < num; i++) {
                GDMMultimediaLink lnk = MultimediaLinks[i];

                if (lnk.Value == mediaRec) {
                    mmLink = lnk;
                    break;
                }
            }

            if (mmLink == null) {
                mmLink = AddMultimedia(mediaRec);
            }

            mmLink.IsPrimary = true;
            return mmLink;
        }

        public GDMMultimediaLink GetPrimaryMultimediaLink()
        {
            GDMMultimediaLink result = null;

            int num = MultimediaLinks.Count;
            for (int i = 0; i < num; i++) {
                GDMMultimediaLink mmLink = MultimediaLinks[i];
                if (mmLink.IsPrimary) {
                    result = mmLink;
                    break;
                }
            }

            return result;
        }

        public int GetTotalChildsCount()
        {
            int result = 0;

            int num = SpouseToFamilyLinks.Count;
            for (int i = 0; i < num; i++) {
                GDMFamilyRecord family = SpouseToFamilyLinks[i].Family;
                result += family.Children.Count;
            }

            return result;
        }

        private static int EventsCompare(GDMPointer cp1, GDMPointer cp2)
        {
            UDN udn1 = ((GDMFamilyRecord)cp1.Value).GetUDN(GEDCOMTagType.MARR);
            UDN udn2 = ((GDMFamilyRecord)cp2.Value).GetUDN(GEDCOMTagType.MARR);
            return udn1.CompareTo(udn2);
        }

        public void SortSpouses()
        {
            fSpouseToFamilyLinks.Sort(EventsCompare);
        }
    }
}
