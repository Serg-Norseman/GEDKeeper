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
using GKCore.Types;

namespace GDModel
{
    // Standard: [M]Male / [F]Female / [U]Unknown (gedcom-5.5.1, p.61)
    // Tamura Jones: +[X]Intersex (GEDCOM 5.5.1 Specification Annotated Edition)
    public enum GDMSex
    {
        svUnknown,
        svMale,
        svFemale,
        svIntersex,

        svLast = svFemale
    }


    public sealed class GDMIndividualRecord : GDMRecordWithEvents, IGDMIndividualRecord
    {
        private GDMList<GDMAssociation> fAssociations;
        private readonly GDMList<GDMChildToFamilyLink> fChildToFamilyLinks;
        private GDMList<GDMPointer> fGroups;
        private readonly GDMList<GDMPersonalName> fPersonalNames;
        private readonly GDMList<GDMSpouseToFamilyLink> fSpouseToFamilyLinks;
        private GDMSex fSex;


        public bool HasAssociations
        {
            get { return fAssociations != null && fAssociations.Count != 0; }
        }

        public GDMList<GDMAssociation> Associations
        {
            get {
                if (fAssociations == null) {
                    fAssociations = new GDMList<GDMAssociation>();
                }

                return fAssociations;
            }
        }

        public bool Bookmark
        {
            get {
                return FindTag(GEDCOMTagName._BOOKMARK, 0) != null;
            }
            set {
                if (value) {
                    if (FindTag(GEDCOMTagName._BOOKMARK, 0) == null) {
                        AddTag(new GDMValueTag((int)GEDCOMTagType._BOOKMARK, ""));
                    }
                } else {
                    DeleteTag(GEDCOMTagName._BOOKMARK);
                }
            }
        }

        public GDMList<GDMChildToFamilyLink> ChildToFamilyLinks
        {
            get { return fChildToFamilyLinks; }
        }

        public bool HasGroups
        {
            get { return fGroups != null && fGroups.Count != 0; }
        }

        public GDMList<GDMPointer> Groups
        {
            get {
                if (fGroups == null) {
                    fGroups = new GDMList<GDMPointer>();
                }

                return fGroups;
            }
        }

        public bool Patriarch
        {
            get {
                return FindTag(GEDCOMTagName._PATRIARCH, 0) != null;
            }
            set {
                if (value) {
                    if (FindTag(GEDCOMTagName._PATRIARCH, 0) == null) {
                        AddTag(new GDMValueTag((int)GEDCOMTagType._PATRIARCH, ""));
                    }
                } else {
                    DeleteTag(GEDCOMTagName._PATRIARCH);
                }
            }
        }

        public GDMList<GDMPersonalName> PersonalNames
        {
            get { return fPersonalNames; }
        }

        public GDMSex Sex
        {
            get { return fSex; }
            set { fSex = value; }
        }

        public GDMList<GDMSpouseToFamilyLink> SpouseToFamilyLinks
        {
            get { return fSpouseToFamilyLinks; }
        }


        public GDMIndividualRecord(GDMTree tree) : base(tree)
        {
            SetName(GEDCOMTagType.INDI);

            fChildToFamilyLinks = new GDMList<GDMChildToFamilyLink>();
            fPersonalNames = new GDMList<GDMPersonalName>();
            fSpouseToFamilyLinks = new GDMList<GDMSpouseToFamilyLink>();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fAssociations != null) fAssociations.Dispose();
                fChildToFamilyLinks.Dispose();
                if (fGroups != null) fGroups.Dispose();
                fPersonalNames.Dispose();
                fSpouseToFamilyLinks.Dispose();
            }
            base.Dispose(disposing);
        }

        internal override void TrimExcess()
        {
            base.TrimExcess();

            if (fAssociations != null) fAssociations.TrimExcess();
            fChildToFamilyLinks.TrimExcess();
            if (fGroups != null) fGroups.TrimExcess();
            fPersonalNames.TrimExcess();
            fSpouseToFamilyLinks.TrimExcess();
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

            fSex = GDMSex.svUnknown;

            for (int i = fChildToFamilyLinks.Count - 1; i >= 0; i--) {
                var family = fTree.GetPtrValue<GDMFamilyRecord>(fChildToFamilyLinks[i]);
                family.DeleteChild(this);
            }
            fChildToFamilyLinks.Clear();

            for (int i = fSpouseToFamilyLinks.Count - 1; i >= 0; i--) {
                var family = fTree.GetPtrValue<GDMFamilyRecord>(fSpouseToFamilyLinks[i]);
                family.RemoveSpouse(this);
            }
            fSpouseToFamilyLinks.Clear();

            if (fGroups != null) {
                for (int i = fGroups.Count - 1; i >= 0; i--) {
                    var group = fTree.GetPtrValue<GDMGroupRecord>(fGroups[i]);
                    group.RemoveMember(this);
                }
                fGroups.Clear();
            }

            if (fAssociations != null) fAssociations.Clear();

            fPersonalNames.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && (fSex == GDMSex.svUnknown) && (fPersonalNames.Count == 0)
                && (fChildToFamilyLinks.Count == 0) && (fSpouseToFamilyLinks.Count == 0)
                && (fAssociations == null || fAssociations.Count == 0)
                && (fGroups == null || fGroups.Count == 0);
        }

        public int IndexOfGroup(GDMGroupRecord groupRec)
        {
            if (groupRec != null && fGroups != null) {
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
                    if (fSpouseToFamilyLinks[i].XRef == familyRec.XRef) {
                        return i;
                    }
                }
            }

            return -1;
        }

        public void DeleteSpouseToFamilyLink(GDMFamilyRecord familyRec)
        {
            if (familyRec == null) return;

            int num = fSpouseToFamilyLinks.Count;
            for (int i = 0; i < num; i++) {
                if (fSpouseToFamilyLinks[i].XRef == familyRec.XRef) {
                    fSpouseToFamilyLinks.RemoveAt(i);
                    break;
                }
            }
        }

        public void DeleteChildToFamilyLink(GDMFamilyRecord familyRec)
        {
            if (familyRec == null) return;

            int num = fChildToFamilyLinks.Count;
            for (int i = 0; i < num; i++) {
                if (fChildToFamilyLinks[i].XRef == familyRec.XRef) {
                    fChildToFamilyLinks.RemoveAt(i);
                    break;
                }
            }
        }

        public GDMChildToFamilyLink FindChildToFamilyLink(GDMFamilyRecord familyRec)
        {
            if (familyRec == null) return null;

            for (int i = 0, num = fChildToFamilyLinks.Count; i < num; i++) {
                var childLink = fChildToFamilyLinks[i];
                if (childLink.XRef == familyRec.XRef) {
                    return childLink;
                }
            }

            return null;
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
                GDMPersonalName copyName = new GDMPersonalName();
                copyName.Assign(srcName);
                AddPersonalName(copyName);
            }
        }

        public override void MoveTo(GDMRecord targetRecord)
        {
            GDMIndividualRecord targetIndi = targetRecord as GDMIndividualRecord;
            if (targetIndi == null) {
                throw new ArgumentException(@"Argument is null or wrong type", "targetRecord");
            }

            base.MoveTo(targetRecord);

            targetIndi.Sex = fSex;

            while (fPersonalNames.Count > 0) {
                GDMPersonalName obj = fPersonalNames.Extract(0);
                targetIndi.AddPersonalName(obj);
            }

            string currentXRef = this.XRef;
            string targetXRef = targetRecord.XRef;

            while (fChildToFamilyLinks.Count > 0) {
                GDMChildToFamilyLink ctfLink = fChildToFamilyLinks.Extract(0);
                var family = fTree.GetPtrValue<GDMFamilyRecord>(ctfLink);

                int num = family.Children.Count;
                for (int i = 0; i < num; i++) {
                    GDMIndividualLink childPtr = family.Children[i];

                    if (childPtr.XRef == currentXRef) {
                        childPtr.XRef = targetXRef;
                    }
                }

                targetIndi.ChildToFamilyLinks.Add(ctfLink);
            }

            while (fSpouseToFamilyLinks.Count > 0) {
                GDMSpouseToFamilyLink stfLink = fSpouseToFamilyLinks.Extract(0);
                var family = fTree.GetPtrValue<GDMFamilyRecord>(stfLink);

                if (family.Husband.XRef == currentXRef) {
                    family.Husband.XRef = targetXRef;
                } else if (family.Wife.XRef == currentXRef) {
                    family.Wife.XRef = targetXRef;
                }

                targetIndi.SpouseToFamilyLinks.Add(stfLink);
            }

            while (fAssociations != null && fAssociations.Count > 0) {
                GDMAssociation obj = fAssociations.Extract(0);
                targetIndi.Associations.Add(obj);
            }

            while (fGroups != null && fGroups.Count > 0) {
                GDMPointer obj = fGroups.Extract(0);
                targetIndi.Groups.Add(obj);
            }
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);

            if (fAssociations != null) fAssociations.ReplaceXRefs(map);
            fChildToFamilyLinks.ReplaceXRefs(map);
            if (fGroups != null) fGroups.ReplaceXRefs(map);
            fPersonalNames.ReplaceXRefs(map);
            fSpouseToFamilyLinks.ReplaceXRefs(map);
        }

        public sealed class LifeEvents
        {
            public readonly GDMCustomEvent BirthEvent;
            public readonly GDMCustomEvent DeathEvent;

            public readonly GDMCustomEvent BaptismEvent;
            public readonly GDMCustomEvent BurialEvent;

            public LifeEvents(GDMCustomEvent birthEvent, GDMCustomEvent deathEvent, GDMCustomEvent baptismEvent, GDMCustomEvent burialEvent)
            {
                BirthEvent = birthEvent;
                DeathEvent = deathEvent;
                BaptismEvent = baptismEvent;
                BurialEvent = burialEvent;
            }
        }

        public LifeEvents GetLifeEvents(bool ext = false)
        {
            GDMCustomEvent birthEvent = null;
            GDMCustomEvent deathEvent = null;
            GDMCustomEvent baptismEvent = null;
            GDMCustomEvent burialEvent = null;

            int num = Events.Count;
            for (int i = 0; i < num; i++) {
                GDMCustomEvent evt = Events[i];
                var evtType = evt.GetTagType();

                if (evtType == GEDCOMTagType.BIRT && birthEvent == null) {
                    birthEvent = evt;
                } else if (evtType == GEDCOMTagType.DEAT && deathEvent == null) {
                    deathEvent = evt;
                } else if (ext) {
                    if (evtType == GEDCOMTagType.BAPM && baptismEvent == null) {
                        baptismEvent = evt;
                    } else if (evtType == GEDCOMTagType.BURI && burialEvent == null) {
                        burialEvent = evt;
                    }
                }
            }

            return new LifeEvents(birthEvent, deathEvent, baptismEvent, burialEvent);
        }

        public GDMPersonalName GetPrimaryPersonalName()
        {
            return (fPersonalNames.Count <= 0) ? null : fPersonalNames[0];
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

            bool womanMode = (Sex == GDMSex.svFemale);

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
                var dates = GetLifeEvents();
                var indiDates = indi.GetLifeEvents();

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
            GDMAssociation result = new GDMAssociation();
            result.Relation = relation;
            result.XRef = (relPerson == null) ? string.Empty : relPerson.XRef;
            Associations.Add(result);
            return result;
        }

        public GDMMultimediaLink SetPrimaryMultimediaLink(GDMMultimediaRecord mediaRec)
        {
            if (mediaRec == null) return null;
            GDMMultimediaLink mmLink = null;

            if (HasMultimediaLinks) {
                int num = MultimediaLinks.Count;
                for (int i = 0; i < num; i++) {
                    GDMMultimediaLink lnk = MultimediaLinks[i];

                    if (lnk.XRef == mediaRec.XRef) {
                        mmLink = lnk;
                        break;
                    }
                }
            }

            if (mmLink == null) {
                mmLink = this.AddMultimedia(mediaRec);
            }

            mmLink.IsPrimary = true;
            return mmLink;
        }

        public GDMMultimediaLink GetPrimaryMultimediaLink()
        {
            GDMMultimediaLink result = null;
            if (!HasMultimediaLinks) return result;

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

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            ProcessHashes(ref hashCode, fAssociations);
            ProcessHashes(ref hashCode, fChildToFamilyLinks);
            ProcessHashes(ref hashCode, fGroups);
            ProcessHashes(ref hashCode, fPersonalNames);
            ProcessHashes(ref hashCode, fSpouseToFamilyLinks);
            hashCode.Add(fSex);
        }
    }
}
