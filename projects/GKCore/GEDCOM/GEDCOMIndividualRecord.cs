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

namespace GKCommon.GEDCOM
{
    public sealed class GEDCOMIndividualRecord : GEDCOMRecordWithEvents
    {
        private static readonly GEDCOMFactory fTagsFactory;
        
        private GEDCOMList<GEDCOMAlias> fAliasses;
        private GEDCOMList<GEDCOMAssociation> fAssociations;
        private GEDCOMList<GEDCOMChildToFamilyLink> fChildToFamilyLinks;
        private GEDCOMList<GEDCOMPointer> fGroups;
        private GEDCOMList<GEDCOMPersonalName> fPersonalNames;
        private GEDCOMList<GEDCOMSpouseToFamilyLink> fSpouseToFamilyLinks;
        private GEDCOMSex fSex;


        public GEDCOMList<GEDCOMAlias> Aliases
        {
            get { return fAliasses; }
        }

        public string AncestralFileNumber
        {
            get { return GetTagStringValue(GEDCOMTagType.AFN); }
            set { SetTagStringValue(GEDCOMTagType.AFN, value); }
        }

        public GEDCOMList<GEDCOMAssociation> Associations
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

        public GEDCOMList<GEDCOMChildToFamilyLink> ChildToFamilyLinks
        {
            get { return fChildToFamilyLinks; }
        }

        public GEDCOMList<GEDCOMPointer> Groups
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

        public GEDCOMList<GEDCOMPersonalName> PersonalNames
        {
            get { return fPersonalNames; }
        }

        public GEDCOMSex Sex
        {
            get { return fSex; }
            set { fSex = value; }
        }

        public GEDCOMList<GEDCOMSpouseToFamilyLink> SpouseToFamilyLinks
        {
            get { return fSpouseToFamilyLinks; }
        }


        public GEDCOMIndividualRecord(GEDCOMObject owner) : base(owner)
        {
            SetRecordType(GEDCOMRecordType.rtIndividual);
            SetName(GEDCOMTagType.INDI);

            fAliasses = new GEDCOMList<GEDCOMAlias>(this);
            fAssociations = new GEDCOMList<GEDCOMAssociation>(this);
            fChildToFamilyLinks = new GEDCOMList<GEDCOMChildToFamilyLink>(this);
            fGroups = new GEDCOMList<GEDCOMPointer>(this);
            fPersonalNames = new GEDCOMList<GEDCOMPersonalName>(this);
            fSpouseToFamilyLinks = new GEDCOMList<GEDCOMSpouseToFamilyLink>(this);
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

        static GEDCOMIndividualRecord()
        {
            GEDCOMFactory f = new GEDCOMFactory();
            fTagsFactory = f;

            f.RegisterTag(GEDCOMTagType.NAME, GEDCOMPersonalName.Create);
            f.RegisterTag(GEDCOMTagType.FAMC, GEDCOMChildToFamilyLink.Create);
            f.RegisterTag(GEDCOMTagType.FAMS, GEDCOMSpouseToFamilyLink.Create);
            f.RegisterTag(GEDCOMTagType.ASSO, GEDCOMAssociation.Create);
            f.RegisterTag(GEDCOMTagType.ALIA, GEDCOMAlias.Create);

            f.RegisterTag(GEDCOMTagType.ADOP, GEDCOMIndividualEvent.Create);
            f.RegisterTag(GEDCOMTagType.BAPM, GEDCOMIndividualEvent.Create);
            f.RegisterTag(GEDCOMTagType.BARM, GEDCOMIndividualEvent.Create);
            f.RegisterTag(GEDCOMTagType.BASM, GEDCOMIndividualEvent.Create);
            f.RegisterTag(GEDCOMTagType.BIRT, GEDCOMIndividualEvent.Create);
            f.RegisterTag(GEDCOMTagType.BLES, GEDCOMIndividualEvent.Create);
            f.RegisterTag(GEDCOMTagType.BURI, GEDCOMIndividualEvent.Create);
            f.RegisterTag(GEDCOMTagType.CENS, GEDCOMIndividualEvent.Create);
            f.RegisterTag(GEDCOMTagType.CHR, GEDCOMIndividualEvent.Create);
            f.RegisterTag(GEDCOMTagType.CHRA, GEDCOMIndividualEvent.Create);
            f.RegisterTag(GEDCOMTagType.CONF, GEDCOMIndividualEvent.Create);
            f.RegisterTag(GEDCOMTagType.CREM, GEDCOMIndividualEvent.Create);
            f.RegisterTag(GEDCOMTagType.DEAT, GEDCOMIndividualEvent.Create);
            f.RegisterTag(GEDCOMTagType.EVEN, GEDCOMIndividualEvent.Create);
            f.RegisterTag(GEDCOMTagType.EMIG, GEDCOMIndividualEvent.Create);
            f.RegisterTag(GEDCOMTagType.FCOM, GEDCOMIndividualEvent.Create);
            f.RegisterTag(GEDCOMTagType.GRAD, GEDCOMIndividualEvent.Create);
            f.RegisterTag(GEDCOMTagType.IMMI, GEDCOMIndividualEvent.Create);
            f.RegisterTag(GEDCOMTagType.NATU, GEDCOMIndividualEvent.Create);
            f.RegisterTag(GEDCOMTagType.ORDN, GEDCOMIndividualEvent.Create);
            f.RegisterTag(GEDCOMTagType.PROB, GEDCOMIndividualEvent.Create);
            f.RegisterTag(GEDCOMTagType.RETI, GEDCOMIndividualEvent.Create);
            f.RegisterTag(GEDCOMTagType.WILL, GEDCOMIndividualEvent.Create);

            f.RegisterTag(GEDCOMTagType.CAST, GEDCOMIndividualAttribute.Create);
            f.RegisterTag(GEDCOMTagType.DSCR, GEDCOMIndividualAttribute.Create);
            f.RegisterTag(GEDCOMTagType.EDUC, GEDCOMIndividualAttribute.Create);
            f.RegisterTag(GEDCOMTagType.FACT, GEDCOMIndividualAttribute.Create);
            f.RegisterTag(GEDCOMTagType.IDNO, GEDCOMIndividualAttribute.Create);
            f.RegisterTag(GEDCOMTagType.NATI, GEDCOMIndividualAttribute.Create);
            f.RegisterTag(GEDCOMTagType.NCHI, GEDCOMIndividualAttribute.Create);
            f.RegisterTag(GEDCOMTagType.NMR, GEDCOMIndividualAttribute.Create);
            f.RegisterTag(GEDCOMTagType.OCCU, GEDCOMIndividualAttribute.Create);
            f.RegisterTag(GEDCOMTagType.PROP, GEDCOMIndividualAttribute.Create);
            f.RegisterTag(GEDCOMTagType.RELI, GEDCOMIndividualAttribute.Create);
            f.RegisterTag(GEDCOMTagType.RESI, GEDCOMIndividualAttribute.Create);
            f.RegisterTag(GEDCOMTagType.SSN, GEDCOMIndividualAttribute.Create);
            f.RegisterTag(GEDCOMTagType.TITL, GEDCOMIndividualAttribute.Create);

            f.RegisterTag(GEDCOMTagType._TRAVEL, GEDCOMIndividualAttribute.Create);
            f.RegisterTag(GEDCOMTagType._HOBBY, GEDCOMIndividualAttribute.Create);
            f.RegisterTag(GEDCOMTagType._AWARD, GEDCOMIndividualAttribute.Create);
            f.RegisterTag(GEDCOMTagType._MILI, GEDCOMIndividualAttribute.Create);
            f.RegisterTag(GEDCOMTagType._MILI_IND, GEDCOMIndividualAttribute.Create);
            f.RegisterTag(GEDCOMTagType._MILI_DIS, GEDCOMIndividualAttribute.Create);
            f.RegisterTag(GEDCOMTagType._MILI_RANK, GEDCOMIndividualAttribute.Create);

            f.RegisterTag(GEDCOMTagType._BGRO, GEDCOMIndividualAttribute.Create);
            f.RegisterTag(GEDCOMTagType._EYES, GEDCOMIndividualAttribute.Create);
            f.RegisterTag(GEDCOMTagType._HAIR, GEDCOMIndividualAttribute.Create);
            f.RegisterTag(GEDCOMTagType._MDNA, GEDCOMIndividualAttribute.Create);
            f.RegisterTag(GEDCOMTagType._YDNA, GEDCOMIndividualAttribute.Create);
        }

        public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
        {
            GEDCOMTag result = fTagsFactory.CreateTag(this, tagName, tagValue);
            if (result != null) {
                if (result is GEDCOMCustomEvent) {
                    result = AddEvent(result as GEDCOMCustomEvent);
                } else if (result is GEDCOMChildToFamilyLink) {
                    result = fChildToFamilyLinks.Add(result as GEDCOMChildToFamilyLink);
                } else if (result is GEDCOMSpouseToFamilyLink) {
                    result = fSpouseToFamilyLinks.Add(result as GEDCOMSpouseToFamilyLink);
                } else if (result is GEDCOMPersonalName) {
                    result = AddPersonalName(result as GEDCOMPersonalName);
                } else if (result is GEDCOMAssociation) {
                    result = fAssociations.Add(result as GEDCOMAssociation);
                } else if (result is GEDCOMAlias) {
                    result = fAliasses.Add(result as GEDCOMAlias);
                }
            } else {
                if (tagName == GEDCOMTagType._GROUP) {
                    result = fGroups.Add(new GEDCOMPointer(this, tagName, tagValue));
                } else if (tagName == GEDCOMTagType.SEX) {
                    fSex = GEDCOMUtils.GetSexVal(tagValue);
                    result = null;
                } else {
                    result = base.AddTag(tagName, tagValue, tagConstructor);
                }
            }

            return result;
        }

        public override GEDCOMCustomEvent AddEvent(GEDCOMCustomEvent evt)
        {
            if (evt != null) {
                if (evt is GEDCOMIndividualEvent || evt is GEDCOMIndividualAttribute) {
                    // SetLevel need for events created outside!
                    evt.SetLevel(Level + 1);
                    Events.Add(evt);
                } else {
                    throw new ArgumentException(@"Event has the invalid type", "evt");
                }
            }

            return evt;
        }

        public GEDCOMPersonalName AddPersonalName(GEDCOMPersonalName value)
        {
            if (value != null) {
                value.SetLevel(Level + 1);
                fPersonalNames.Add(value);
            }
            return value;
        }

        public override void Clear()
        {
            base.Clear();

            fSex = GEDCOMSex.svNone;

            for (int i = fChildToFamilyLinks.Count - 1; i >= 0; i--) {
                GEDCOMFamilyRecord family = fChildToFamilyLinks[i].Family;
                family.DeleteChild(this);
            }
            fChildToFamilyLinks.Clear();

            for (int i = fSpouseToFamilyLinks.Count - 1; i >= 0; i--) {
                GEDCOMFamilyRecord family = fSpouseToFamilyLinks[i].Family;
                family.RemoveSpouse(this);
            }
            fSpouseToFamilyLinks.Clear();

            for (int i = fGroups.Count - 1; i >= 0; i--) {
                GEDCOMGroupRecord group = (GEDCOMGroupRecord)fGroups[i].Value;
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

        public int IndexOfGroup(GEDCOMGroupRecord groupRec)
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

        public int IndexOfSpouse(GEDCOMFamilyRecord familyRec)
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

        public void DeleteSpouseToFamilyLink(GEDCOMFamilyRecord familyRec)
        {
            int num = fSpouseToFamilyLinks.Count;
            for (int i = 0; i < num; i++) {
                if (fSpouseToFamilyLinks[i].Family == familyRec) {
                    fSpouseToFamilyLinks.DeleteAt(i);
                    break;
                }
            }
        }

        public void DeleteChildToFamilyLink(GEDCOMFamilyRecord familyRec)
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

        public override void Assign(GEDCOMTag source)
        {
            GEDCOMIndividualRecord sourceRec = source as GEDCOMIndividualRecord;
            if (sourceRec == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(source);

            fSex = sourceRec.fSex;

            foreach (GEDCOMPersonalName srcName in sourceRec.fPersonalNames) {
                GEDCOMPersonalName copyName = new GEDCOMPersonalName(this);
                copyName.Assign(srcName);
                AddPersonalName(copyName);
            }
        }

        public override void MoveTo(GEDCOMRecord targetRecord, bool clearDest)
        {
            GEDCOMIndividualRecord targetIndi = targetRecord as GEDCOMIndividualRecord;
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
                GEDCOMPersonalName obj = fPersonalNames.Extract(0);
                obj.ResetOwner(targetIndi);
                targetIndi.AddPersonalName(obj);
            }

            if (targetIndi.ChildToFamilyLinks.Count == 0 && ChildToFamilyLinks.Count != 0 && fChildToFamilyLinks != null) {
                GEDCOMChildToFamilyLink ctfLink = fChildToFamilyLinks.Extract(0);
                GEDCOMFamilyRecord family = ctfLink.Family;

                int num = family.Children.Count;
                for (int i = 0; i < num; i++) {
                    GEDCOMPointer childPtr = family.Children[i];

                    if (childPtr.StringValue == "@" + XRef + "@") {
                        childPtr.StringValue = "@" + targetRecord.XRef + "@";
                    }
                }

                ctfLink.ResetOwner(targetIndi);
                targetIndi.ChildToFamilyLinks.Add(ctfLink);
            }

            while (fSpouseToFamilyLinks.Count > 0) {
                GEDCOMSpouseToFamilyLink stfLink = fSpouseToFamilyLinks.Extract(0);
                GEDCOMFamilyRecord family = stfLink.Family;

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
                GEDCOMAssociation obj = fAssociations.Extract(0);
                obj.ResetOwner(targetIndi);
                targetIndi.Associations.Add(obj);
            }

            while (fAliasses.Count > 0) {
                GEDCOMAlias obj = fAliasses.Extract(0);
                obj.ResetOwner(targetIndi);
                targetIndi.Aliases.Add(obj);
            }

            while (fGroups.Count > 0) {
                GEDCOMPointer obj = fGroups.Extract(0);
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

        public override void ReplaceXRefs(XRefReplacer map)
        {
            base.ReplaceXRefs(map);

            fAliasses.ReplaceXRefs(map);
            fAssociations.ReplaceXRefs(map);
            fChildToFamilyLinks.ReplaceXRefs(map);
            fGroups.ReplaceXRefs(map);
            fPersonalNames.ReplaceXRefs(map);
            fSpouseToFamilyLinks.ReplaceXRefs(map);
        }

        public override void SaveToStream(StreamWriter stream)
        {
            base.SaveToStream(stream);

            WriteTagLine(stream, Level + 1, GEDCOMTagType.SEX, GEDCOMUtils.GetSexStr(fSex), true);

            fPersonalNames.SaveToStream(stream);
            fChildToFamilyLinks.SaveToStream(stream);
            fSpouseToFamilyLinks.SaveToStream(stream);
            Events.SaveToStream(stream); // for files content compatibility
            fAssociations.SaveToStream(stream);
            fAliasses.SaveToStream(stream);
            fGroups.SaveToStream(stream);
        }

        #region Auxiliary

        public sealed class LifeDatesRet
        {
            public readonly GEDCOMCustomEvent BirthEvent;
            public readonly GEDCOMCustomEvent DeathEvent;

            public LifeDatesRet(GEDCOMCustomEvent birthEvent, GEDCOMCustomEvent deathEvent)
            {
                BirthEvent = birthEvent;
                DeathEvent = deathEvent;
            }
        }

        public LifeDatesRet GetLifeDates()
        {
            GEDCOMCustomEvent birthEvent = null;
            GEDCOMCustomEvent deathEvent = null;

            int num = Events.Count;
            for (int i = 0; i < num; i++)
            {
                GEDCOMCustomEvent evt = Events[i];

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
        public GEDCOMFamilyRecord GetMarriageFamily(bool canCreate = false)
        {
            GEDCOMFamilyRecord result = (fSpouseToFamilyLinks.Count < 1) ? null : fSpouseToFamilyLinks[0].Family;

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
        public GEDCOMFamilyRecord GetParentsFamily(bool canCreate = false)
        {
            GEDCOMFamilyRecord result = (fChildToFamilyLinks.Count < 1) ? null : fChildToFamilyLinks[0].Value as GEDCOMFamilyRecord;

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
                GEDCOMPersonalName np = fPersonalNames[0];

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

        public override float IsMatch(GEDCOMTag tag, MatchParams matchParams)
        {
            GEDCOMIndividualRecord indi = tag as GEDCOMIndividualRecord;
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

        public GEDCOMAssociation AddAssociation(string relation, GEDCOMIndividualRecord relPerson)
        {
            GEDCOMAssociation result = new GEDCOMAssociation(this);
            result.Relation = relation;
            result.Individual = relPerson;
            Associations.Add(result);
            return result;
        }

        public GEDCOMMultimediaLink SetPrimaryMultimediaLink(GEDCOMMultimediaRecord mediaRec)
        {
            if (mediaRec == null) return null;
            GEDCOMMultimediaLink mmLink = null;

            int num = MultimediaLinks.Count;
            for (int i = 0; i < num; i++)
            {
                GEDCOMMultimediaLink lnk = MultimediaLinks[i];

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

        public GEDCOMMultimediaLink GetPrimaryMultimediaLink()
        {
            GEDCOMMultimediaLink result = null;

            int num = MultimediaLinks.Count;
            for (int i = 0; i < num; i++) {
                GEDCOMMultimediaLink mmLink = MultimediaLinks[i];
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
            for (int i = 0; i < num; i++)
            {
                GEDCOMFamilyRecord family = SpouseToFamilyLinks[i].Family;
                result += family.Children.Count;
            }

            return result;
        }

        private static int EventsCompare(GEDCOMPointer cp1, GEDCOMPointer cp2)
        {
            UDN udn1 = ((GEDCOMFamilyRecord)cp1.Value).GetUDN(GEDCOMTagType.MARR);
            UDN udn2 = ((GEDCOMFamilyRecord)cp2.Value).GetUDN(GEDCOMTagType.MARR);
            return udn1.CompareTo(udn2);
        }

        public void SortSpouses()
        {
            fSpouseToFamilyLinks.Sort(EventsCompare);
        }

        #endregion
    }
}
