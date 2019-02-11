/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using BSLib;
using BSLib.Calendar;

namespace GKCommon.GEDCOM
{
    public sealed class GEDCOMIndividualRecord : GEDCOMRecordWithEvents
    {
        private static readonly GEDCOMFactory fTagsFactory;
        
        private GEDCOMList<GEDCOMPersonalName> fPersonalNames;
        private GEDCOMList<GEDCOMIndividualOrdinance> fIndividualOrdinances;
        private GEDCOMList<GEDCOMChildToFamilyLink> fChildToFamilyLinks;
        private GEDCOMList<GEDCOMSpouseToFamilyLink> fSpouseToFamilyLinks;
        private GEDCOMList<GEDCOMPointer> fSubmittors;
        private GEDCOMList<GEDCOMAssociation> fAssociations;
        private GEDCOMList<GEDCOMAlias> fAliasses;
        private GEDCOMList<GEDCOMPointer> fAncestorsInterest;
        private GEDCOMList<GEDCOMPointer> fDescendantsInterest;
        private GEDCOMList<GEDCOMPointer> fGroups;


        public string AncestralFileNumber
        {
            get { return GetTagStringValue("AFN"); }
            set { SetTagStringValue("AFN", value); }
        }

        public string PermanentRecordFileNumber
        {
            get { return GetTagStringValue("RFN"); }
            set { SetTagStringValue("RFN", value); }
        }

        public GEDCOMList<GEDCOMIndividualOrdinance> IndividualOrdinances
        {
            get { return fIndividualOrdinances; }
        }

        public GEDCOMList<GEDCOMPersonalName> PersonalNames
        {
            get { return fPersonalNames; }
        }

        public GEDCOMRestriction Restriction
        {
            get { return GEDCOMUtils.GetRestrictionVal(GetTagStringValue("RESN")); }
            set { SetTagStringValue("RESN", GEDCOMUtils.GetRestrictionStr(value)); }
        }

        public GEDCOMSex Sex
        {
            get { return GEDCOMUtils.GetSexVal(GetTagStringValue("SEX")); }
            set { SetTagStringValue("SEX", GEDCOMUtils.GetSexStr(value)); }
        }

        public bool Bookmark
        {
            get {
                return FindTag("_BOOKMARK", 0) != null;
            }
            set {
                if (value) {
                    if (FindTag("_BOOKMARK", 0) == null) {
                        AddTag("_BOOKMARK", "", null);
                    }
                } else {
                    DeleteTag("_BOOKMARK");
                }
            }
        }

        public bool Patriarch
        {
            get {
                return FindTag("_PATRIARCH", 0) != null;
            }
            set {
                if (value) {
                    if (FindTag("_PATRIARCH", 0) == null) {
                        AddTag("_PATRIARCH", "", null);
                    }
                } else {
                    DeleteTag("_PATRIARCH");
                }
            }
        }

        public GEDCOMList<GEDCOMChildToFamilyLink> ChildToFamilyLinks
        {
            get { return fChildToFamilyLinks; }
        }

        public GEDCOMList<GEDCOMSpouseToFamilyLink> SpouseToFamilyLinks
        {
            get { return fSpouseToFamilyLinks; }
        }

        public GEDCOMList<GEDCOMPointer> Submittors
        {
            get { return fSubmittors; }
        }

        public GEDCOMList<GEDCOMAssociation> Associations
        {
            get { return fAssociations; }
        }

        public GEDCOMList<GEDCOMAlias> Aliases
        {
            get { return fAliasses; }
        }

        public GEDCOMList<GEDCOMPointer> AncestorsInterest
        {
            get { return fAncestorsInterest; }
        }

        public GEDCOMList<GEDCOMPointer> DescendantsInterest
        {
            get { return fDescendantsInterest; }
        }

        public GEDCOMList<GEDCOMPointer> Groups
        {
            get { return fGroups; }
        }

        protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
        {
            base.CreateObj(owner, parent);
            SetRecordType(GEDCOMRecordType.rtIndividual);
            SetName("INDI");

            fPersonalNames = new GEDCOMList<GEDCOMPersonalName>(this);
            fIndividualOrdinances = new GEDCOMList<GEDCOMIndividualOrdinance>(this);
            fChildToFamilyLinks = new GEDCOMList<GEDCOMChildToFamilyLink>(this);
            fSpouseToFamilyLinks = new GEDCOMList<GEDCOMSpouseToFamilyLink>(this);
            fSubmittors = new GEDCOMList<GEDCOMPointer>(this);
            fAssociations = new GEDCOMList<GEDCOMAssociation>(this);
            fAliasses = new GEDCOMList<GEDCOMAlias>(this);
            fAncestorsInterest = new GEDCOMList<GEDCOMPointer>(this);
            fDescendantsInterest = new GEDCOMList<GEDCOMPointer>(this);
            fGroups = new GEDCOMList<GEDCOMPointer>(this);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                fPersonalNames.Dispose();
                fIndividualOrdinances.Dispose();
                fChildToFamilyLinks.Dispose();
                fSpouseToFamilyLinks.Dispose();
                fSubmittors.Dispose();
                fAssociations.Dispose();
                fAliasses.Dispose();
                fAncestorsInterest.Dispose();
                fDescendantsInterest.Dispose();
                fGroups.Dispose();
            }
            base.Dispose(disposing);
        }

        static GEDCOMIndividualRecord()
        {
            GEDCOMFactory f = new GEDCOMFactory();
            fTagsFactory = f;

            f.RegisterTag("FAMC", GEDCOMChildToFamilyLink.Create);
            f.RegisterTag("FAMS", GEDCOMSpouseToFamilyLink.Create);
            f.RegisterTag("ASSO", GEDCOMAssociation.Create);
            f.RegisterTag("ALIA", GEDCOMAlias.Create);
            
            f.RegisterTag("BAPL", GEDCOMIndividualOrdinance.Create);
            f.RegisterTag("CONL", GEDCOMIndividualOrdinance.Create);
            f.RegisterTag("ENDL", GEDCOMIndividualOrdinance.Create);
            f.RegisterTag("SLGC", GEDCOMIndividualOrdinance.Create);
            
            // // //

            f.RegisterTag("BIRT", GEDCOMIndividualEvent.Create);
            f.RegisterTag("CHR", GEDCOMIndividualEvent.Create);
            f.RegisterTag("DEAT", GEDCOMIndividualEvent.Create);
            f.RegisterTag("BURI", GEDCOMIndividualEvent.Create);
            f.RegisterTag("CREM", GEDCOMIndividualEvent.Create);
            
            f.RegisterTag("ADOP", GEDCOMIndividualEvent.Create);
            f.RegisterTag("BAPM", GEDCOMIndividualEvent.Create);
            f.RegisterTag("BARM", GEDCOMIndividualEvent.Create);
            f.RegisterTag("BASM", GEDCOMIndividualEvent.Create);
            f.RegisterTag("BLES", GEDCOMIndividualEvent.Create);

            f.RegisterTag("CHRA", GEDCOMIndividualEvent.Create);
            f.RegisterTag("CONF", GEDCOMIndividualEvent.Create);
            f.RegisterTag("FCOM", GEDCOMIndividualEvent.Create);
            f.RegisterTag("ORDN", GEDCOMIndividualEvent.Create);
            f.RegisterTag("NATU", GEDCOMIndividualEvent.Create);

            f.RegisterTag("EMIG", GEDCOMIndividualEvent.Create);
            f.RegisterTag("IMMI", GEDCOMIndividualEvent.Create);
            f.RegisterTag("CENS", GEDCOMIndividualEvent.Create);
            f.RegisterTag("PROB", GEDCOMIndividualEvent.Create);
            f.RegisterTag("WILL", GEDCOMIndividualEvent.Create);

            f.RegisterTag("GRAD", GEDCOMIndividualEvent.Create);
            f.RegisterTag("RETI", GEDCOMIndividualEvent.Create);
            f.RegisterTag("EVEN", GEDCOMIndividualEvent.Create);


            f.RegisterTag("CAST", GEDCOMIndividualAttribute.Create);
            f.RegisterTag("DSCR", GEDCOMIndividualAttribute.Create);
            f.RegisterTag("EDUC", GEDCOMIndividualAttribute.Create);
            f.RegisterTag("IDNO", GEDCOMIndividualAttribute.Create);
            f.RegisterTag("NATI", GEDCOMIndividualAttribute.Create);

            f.RegisterTag("NCHI", GEDCOMIndividualAttribute.Create);
            f.RegisterTag("NMR", GEDCOMIndividualAttribute.Create);
            f.RegisterTag("OCCU", GEDCOMIndividualAttribute.Create);
            f.RegisterTag("PROP", GEDCOMIndividualAttribute.Create);
            f.RegisterTag("RELI", GEDCOMIndividualAttribute.Create);

            f.RegisterTag("RESI", GEDCOMIndividualAttribute.Create);
            f.RegisterTag("SSN", GEDCOMIndividualAttribute.Create);
            f.RegisterTag("TITL", GEDCOMIndividualAttribute.Create);
            f.RegisterTag("FACT", GEDCOMIndividualAttribute.Create);
            f.RegisterTag("_TRAVEL", GEDCOMIndividualAttribute.Create);

            f.RegisterTag("_HOBBY", GEDCOMIndividualAttribute.Create);
            f.RegisterTag("_AWARD", GEDCOMIndividualAttribute.Create);
            f.RegisterTag("_MILI", GEDCOMIndividualAttribute.Create);
            f.RegisterTag("_MILI_IND", GEDCOMIndividualAttribute.Create);
            f.RegisterTag("_MILI_DIS", GEDCOMIndividualAttribute.Create);
            f.RegisterTag("_MILI_RANK", GEDCOMIndividualAttribute.Create);

            f.RegisterTag("_BGRO", GEDCOMIndividualAttribute.Create);
            f.RegisterTag("_HAIR", GEDCOMIndividualAttribute.Create);
            f.RegisterTag("_EYES", GEDCOMIndividualAttribute.Create);
            f.RegisterTag("_MDNA", GEDCOMIndividualAttribute.Create);
            f.RegisterTag("_YDNA", GEDCOMIndividualAttribute.Create);
        }

        public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
        {
            GEDCOMTag result;

            if (tagName == "NAME")
            {
                result = AddPersonalName(new GEDCOMPersonalName(Owner, this, tagName, tagValue));
            }
            else if (tagName == "SUBM")
            {
                result = Submittors.Add(new GEDCOMPointer(Owner, this, tagName, tagValue));
            }
            else if (tagName == "ANCI")
            {
                result = AncestorsInterest.Add(new GEDCOMPointer(Owner, this, tagName, tagValue));
            }
            else if (tagName == "DESI")
            {
                result = DescendantsInterest.Add(new GEDCOMPointer(Owner, this, tagName, tagValue));
            }
            else if (tagName == "_GROUP")
            {
                result = fGroups.Add(new GEDCOMPointer(Owner, this, tagName, tagValue));
            }
            else
            {
                result = fTagsFactory.CreateTag(Owner, this, tagName, tagValue);

                if (result != null)
                {
                    if (result is GEDCOMChildToFamilyLink) {
                        result = ChildToFamilyLinks.Add(result as GEDCOMChildToFamilyLink);
                    } else if (result is GEDCOMSpouseToFamilyLink) {
                        result = SpouseToFamilyLinks.Add(result as GEDCOMSpouseToFamilyLink);
                    } else if (result is GEDCOMIndividualOrdinance) {
                        result = IndividualOrdinances.Add(result as GEDCOMIndividualOrdinance);
                    } else if (result is GEDCOMAssociation) {
                        result = Associations.Add(result as GEDCOMAssociation);
                    } else if (result is GEDCOMIndividualEvent) {
                        result = AddEvent(result as GEDCOMCustomEvent);
                    } else if (result is GEDCOMIndividualAttribute) {
                        result = AddEvent(result as GEDCOMCustomEvent);
                    } else if (result is GEDCOMAlias) {
                        result = Aliases.Add(result as GEDCOMAlias);
                    }
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
            if (value != null)
            {
                value.SetLevel(Level + 1);
                fPersonalNames.Add(value);
            }
            return value;
        }

        public override void Clear()
        {
            base.Clear();

            for (int i = fChildToFamilyLinks.Count - 1; i >= 0; i--)
            {
                GEDCOMFamilyRecord family = fChildToFamilyLinks[i].Family;
                family.DeleteChild(this);
            }
            fChildToFamilyLinks.Clear();

            for (int i = fSpouseToFamilyLinks.Count - 1; i >= 0; i--)
            {
                GEDCOMFamilyRecord family = fSpouseToFamilyLinks[i].Family;
                family.RemoveSpouse(this);
            }
            fSpouseToFamilyLinks.Clear();

            for (int i = fGroups.Count - 1; i >= 0; i--)
            {
                GEDCOMGroupRecord group = (GEDCOMGroupRecord)fGroups[i].Value;
                group.RemoveMember(this);
            }
            fGroups.Clear();

            fPersonalNames.Clear();
            fIndividualOrdinances.Clear();
            fSubmittors.Clear();
            fAssociations.Clear();
            fAliasses.Clear();
            fAncestorsInterest.Clear();
            fDescendantsInterest.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && fPersonalNames.Count == 0
                && fIndividualOrdinances.Count == 0 && fChildToFamilyLinks.Count == 0
                && fSpouseToFamilyLinks.Count == 0 && fSubmittors.Count == 0
                && fAssociations.Count == 0 && fAliasses.Count == 0
                && fAncestorsInterest.Count == 0 && fDescendantsInterest.Count == 0
                && fGroups.Count == 0;
        }

        public int IndexOfGroup(GEDCOMGroupRecord groupRec)
        {
            if (groupRec != null) {
                int num = fGroups.Count;
                for (int i = 0; i < num; i++)
                {
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
                for (int i = 0; i < num; i++)
                {
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
            for (int i = 0; i < num; i++)
            {
                if (fSpouseToFamilyLinks[i].Family == familyRec) {
                    fSpouseToFamilyLinks.DeleteAt(i);
                    break;
                }
            }
        }

        public void DeleteChildToFamilyLink(GEDCOMFamilyRecord familyRec)
        {
            int num = fChildToFamilyLinks.Count;
            for (int i = 0; i < num; i++)
            {
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
            return FindEvent("DEAT") == null;
        }

        public override void Assign(GEDCOMTag source)
        {
            GEDCOMIndividualRecord sourceRec = source as GEDCOMIndividualRecord;
            if (sourceRec == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(source);

            foreach (GEDCOMPersonalName srcName in sourceRec.fPersonalNames)
            {
                GEDCOMPersonalName copyName = (GEDCOMPersonalName)GEDCOMPersonalName.Create(Owner, this, "", "");
                copyName.Assign(srcName);
                AddPersonalName(copyName);
            }
        }

        public override void MoveTo(GEDCOMRecord targetRecord, bool clearDest)
        {
            GEDCOMIndividualRecord toRec = targetRecord as GEDCOMIndividualRecord;
            if (toRec == null) {
                throw new ArgumentException(@"Argument is null or wrong type", "targetRecord");
            }

            if (!clearDest) {
                DeleteTag("SEX");
                DeleteTag("_UID");
            }

            base.MoveTo(targetRecord, clearDest);

            while (fPersonalNames.Count > 0) {
                GEDCOMPersonalName obj = fPersonalNames.Extract(0);
                obj.ResetParent(toRec);
                toRec.AddPersonalName(obj);
            }

            if (toRec.ChildToFamilyLinks.Count == 0 && ChildToFamilyLinks.Count != 0 && fChildToFamilyLinks != null) {
                GEDCOMChildToFamilyLink ctfLink = fChildToFamilyLinks.Extract(0);
                GEDCOMFamilyRecord family = ctfLink.Family;

                int num = family.Children.Count;
                for (int i = 0; i < num; i++) {
                    GEDCOMPointer childPtr = family.Children[i];

                    if (childPtr.StringValue == "@" + XRef + "@") {
                        childPtr.StringValue = "@" + targetRecord.XRef + "@";
                    }
                }

                ctfLink.ResetParent(toRec);
                toRec.ChildToFamilyLinks.Add(ctfLink);
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

                stfLink.ResetParent(toRec);
                toRec.SpouseToFamilyLinks.Add(stfLink);
            }

            while (fIndividualOrdinances.Count > 0) {
                GEDCOMIndividualOrdinance ord = fIndividualOrdinances.Extract(0);
                ord.ResetParent(toRec);
                toRec.IndividualOrdinances.Add(ord);
            }

            while (fSubmittors.Count > 0) {
                GEDCOMPointer obj = fSubmittors.Extract(0);
                obj.ResetParent(toRec);
                toRec.Submittors.Add(obj);
            }

            while (fAssociations.Count > 0) {
                GEDCOMAssociation obj = fAssociations.Extract(0);
                obj.ResetParent(toRec);
                toRec.Associations.Add(obj);
            }

            while (fAliasses.Count > 0) {
                GEDCOMAlias obj = fAliasses.Extract(0);
                obj.ResetParent(toRec);
                toRec.Aliases.Add(obj);
            }

            while (fAncestorsInterest.Count > 0) {
                GEDCOMPointer obj = fAncestorsInterest.Extract(0);
                obj.ResetParent(toRec);
                toRec.AncestorsInterest.Add(obj);
            }

            while (fDescendantsInterest.Count > 0) {
                GEDCOMPointer obj = fDescendantsInterest.Extract(0);
                obj.ResetParent(toRec);
                toRec.DescendantsInterest.Add(obj);
            }

            while (fGroups.Count > 0) {
                GEDCOMPointer obj = fGroups.Extract(0);
                obj.ResetParent(toRec);
                toRec.Groups.Add(obj);
            }
        }

        public override void Pack()
        {
            base.Pack();

            fPersonalNames.Pack();
            fChildToFamilyLinks.Pack();
            fSpouseToFamilyLinks.Pack();
            fIndividualOrdinances.Pack();
            fSubmittors.Pack();
            fAssociations.Pack();
            fAliasses.Pack();
            fAncestorsInterest.Pack();
            fDescendantsInterest.Pack();
            fGroups.Pack();
        }

        public override void ReplaceXRefs(XRefReplacer map)
        {
            base.ReplaceXRefs(map);

            fPersonalNames.ReplaceXRefs(map);
            fChildToFamilyLinks.ReplaceXRefs(map);
            fSpouseToFamilyLinks.ReplaceXRefs(map);
            fIndividualOrdinances.ReplaceXRefs(map);
            fSubmittors.ReplaceXRefs(map);
            fAssociations.ReplaceXRefs(map);
            fAliasses.ReplaceXRefs(map);
            fAncestorsInterest.ReplaceXRefs(map);
            fDescendantsInterest.ReplaceXRefs(map);
            fGroups.ReplaceXRefs(map);
        }

        public override void ResetOwner(GEDCOMTree newOwner)
        {
            base.ResetOwner(newOwner);

            fPersonalNames.ResetOwner(newOwner);
            fChildToFamilyLinks.ResetOwner(newOwner);
            fSpouseToFamilyLinks.ResetOwner(newOwner);
            fIndividualOrdinances.ResetOwner(newOwner);
            fSubmittors.ResetOwner(newOwner);
            fAssociations.ResetOwner(newOwner);
            fAliasses.ResetOwner(newOwner);
            fAncestorsInterest.ResetOwner(newOwner);
            fDescendantsInterest.ResetOwner(newOwner);
            fGroups.ResetOwner(newOwner);
        }

        public override void SaveToStream(StreamWriter stream)
        {
            base.SaveToStream(stream);

            fPersonalNames.SaveToStream(stream);
            fChildToFamilyLinks.SaveToStream(stream);
            fSpouseToFamilyLinks.SaveToStream(stream);
            Events.SaveToStream(stream); // for files content compatibility
            fIndividualOrdinances.SaveToStream(stream);
            fSubmittors.SaveToStream(stream);
            fAssociations.SaveToStream(stream);
            fAliasses.SaveToStream(stream);
            fAncestorsInterest.SaveToStream(stream);
            fDescendantsInterest.SaveToStream(stream);
            fGroups.SaveToStream(stream);
        }

        public GEDCOMIndividualRecord(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
        }

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
        {
            return new GEDCOMIndividualRecord(owner, parent, tagName, tagValue);
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

                if (evt.Name == "BIRT" && birthEvent == null) {
                    birthEvent = evt;
                } else if (evt.Name == "DEAT" && deathEvent == null) {
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
                result = Owner.CreateFamily();
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
                result = Owner.CreateFamily();
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
            GEDCOMAssociation result = new GEDCOMAssociation(Owner, this, "", "");
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
            UDN udn1 = ((GEDCOMFamilyRecord)cp1.Value).GetUDN("MARR");
            UDN udn2 = ((GEDCOMFamilyRecord)cp2.Value).GetUDN("MARR");
            return udn1.CompareTo(udn2);
        }

        public void SortSpouses()
        {
            fSpouseToFamilyLinks.Sort(EventsCompare);
        }

        #endregion
    }
}
