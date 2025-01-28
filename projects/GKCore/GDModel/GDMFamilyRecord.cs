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
    /// <summary>
    /// This type of Genealogical Data Model (GDM) defines the status of the marriage.
    /// </summary>
    public enum GDMMarriageStatus
    {
        Unknown,
        MarrRegistered,
        MarrNotRegistered,
        MarrDivorced
    }


    public sealed class GDMFamilyRecord : GDMRecordWithEvents, IGDMFamilyRecord
    {
        private readonly GDMList<GDMChildLink> fChildren;
        private readonly GDMIndividualLink fHusband;
        private readonly GDMIndividualLink fWife;
        private GDMMarriageStatus fStatus;


        public GDMList<GDMChildLink> Children
        {
            get { return fChildren; }
        }

        public GDMIndividualLink Husband
        {
            get { return fHusband; }
        }

        public GDMIndividualLink Wife
        {
            get { return fWife; }
        }

        public GDMMarriageStatus Status
        {
            get { return fStatus; }
            set { fStatus = value; }
        }


        public GDMFamilyRecord(GDMTree tree) : base(tree)
        {
            SetName(GEDCOMTagType.FAM);

            fHusband = new GDMIndividualLink((int)GEDCOMTagType.HUSB);
            fWife = new GDMIndividualLink((int)GEDCOMTagType.WIFE);
            fChildren = new GDMList<GDMChildLink>();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fChildren.Dispose();
            }
            base.Dispose(disposing);
        }

        internal override void TrimExcess()
        {
            base.TrimExcess();

            fHusband.TrimExcess();
            fWife.TrimExcess();
            fChildren.TrimExcess();
        }

        public override GDMCustomEvent AddEvent(GDMCustomEvent evt)
        {
            var famEvent = evt as GDMFamilyEvent;
            if (famEvent != null) {
                Events.Add(evt);
            } else {
                throw new ArgumentException(@"Event has the invalid type", "evt");
            }

            return evt;
        }

        public override void Clear()
        {
            base.Clear();

            RemoveSpouse(fTree.GetPtrValue<GDMIndividualRecord>(fHusband));
            RemoveSpouse(fTree.GetPtrValue<GDMIndividualRecord>(fWife));

            int num = fChildren.Count;
            for (int i = 0; i < num; i++) {
                GDMIndividualRecord child = fTree.GetPtrValue<GDMIndividualRecord>(fChildren[i]);
                child.DeleteChildToFamilyLink(this);
            }
            fChildren.Clear();

            fStatus = GDMMarriageStatus.Unknown;
        }

        public override bool IsEmpty()
        {
            // Status are not checked because they are not important if other fields are empty.
            return base.IsEmpty() && (fChildren.Count == 0) && fHusband.IsEmpty() && fWife.IsEmpty();
        }

        public void DeleteChild(GDMRecord childRec)
        {
            if (childRec == null) return;

            for (int i = fChildren.Count - 1; i >= 0; i--) {
                if (fChildren[i].XRef == childRec.XRef) {
                    fChildren.RemoveAt(i);
                    break;
                }
            }
        }

        public void DeleteChild(GDMPointer childPtr)
        {
            if (childPtr == null) return;

            for (int i = fChildren.Count - 1; i >= 0; i--) {
                if (fChildren[i].XRef == childPtr.XRef) {
                    fChildren.RemoveAt(i);
                    break;
                }
            }
        }

        public int IndexOfChild(GDMRecord childRec)
        {
            int result = -1;
            if (childRec == null) return result;

            for (int i = fChildren.Count - 1; i >= 0; i--) {
                if (fChildren[i].XRef == childRec.XRef) {
                    result = i;
                    break;
                }
            }

            return result;
        }

        public override void Assign(GDMTag source)
        {
            GDMFamilyRecord sourceRec = source as GDMFamilyRecord;
            if (sourceRec == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(source);

            fHusband.Assign(sourceRec.fHusband);
            fWife.Assign(sourceRec.fWife);
            fStatus = sourceRec.fStatus;
            AssignList(sourceRec.fChildren, fChildren);
        }

        public override void MoveTo(GDMRecord targetRecord)
        {
            GDMFamilyRecord targetFamily = targetRecord as GDMFamilyRecord;
            if (targetFamily == null)
                throw new ArgumentException(@"Argument is null or wrong type", "targetRecord");

            base.MoveTo(targetRecord);

            targetFamily.RemoveSpouse(fTree.GetPtrValue<GDMIndividualRecord>(targetFamily.Husband));
            targetFamily.Husband.XRef = fHusband.XRef;

            targetFamily.RemoveSpouse(fTree.GetPtrValue<GDMIndividualRecord>(targetFamily.Wife));
            targetFamily.Wife.XRef = fWife.XRef;

            targetFamily.Status = fStatus;

            while (fChildren.Count > 0) {
                var obj = fChildren.Extract(0);
                targetFamily.Children.Add(obj);
            }
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);

            fHusband.ReplaceXRefs(map);
            fWife.ReplaceXRefs(map);
            fChildren.ReplaceXRefs(map);
        }

        private string GetFamilyString()
        {
            string result = "";

            GDMIndividualRecord spouse = fTree.GetPtrValue<GDMIndividualRecord>(fHusband);
            result += (spouse == null) ? "?" : spouse.GetPrimaryFullName();
            result += " - ";
            spouse = fTree.GetPtrValue<GDMIndividualRecord>(fWife);
            result += (spouse == null) ? "?" : spouse.GetPrimaryFullName();

            return result;
        }

        public override float IsMatch(GDMTag tag, MatchParams matchParams)
        {
            GDMFamilyRecord otherFam = tag as GDMFamilyRecord;
            if (otherFam == null) return 0.0f;

            float match = GetStrMatch(GetFamilyString(), otherFam.GetFamilyString(), matchParams);
            return match;
        }

        public bool AddSpouse(GDMIndividualRecord spouse)
        {
            if (spouse == null) {
                return false;
            }

            switch (spouse.Sex) {
                case GDMSex.svMale:
                    fHusband.XRef = spouse.XRef;
                    break;

                case GDMSex.svFemale:
                    fWife.XRef = spouse.XRef;
                    break;

                case GDMSex.svUnknown:
                case GDMSex.svIntersex:
                    return false;
            }

            spouse.SpouseToFamilyLinks.Add(new GDMSpouseToFamilyLink(this.XRef));

            return true;
        }

        public void RemoveSpouse(GDMIndividualRecord spouse)
        {
            if (spouse == null) return;

            spouse.DeleteSpouseToFamilyLink(this);

            switch (spouse.Sex) {
                case GDMSex.svMale:
                    fHusband.XRef = string.Empty;
                    break;

                case GDMSex.svFemale:
                    fWife.XRef = string.Empty;
                    break;
            }
        }

        public bool HasMember(GDMIndividualRecord member)
        {
            return HasSpouse(member) || HasChild(member);
        }

        public bool HasSpouse(GDMIndividualRecord spouse)
        {
            return (spouse != null) && (fHusband.XRef == spouse.XRef || fWife.XRef == spouse.XRef);
        }

        public bool AddChild(GDMIndividualRecord child)
        {
            if (child == null) return false;

            fChildren.Add(new GDMChildLink(child.XRef));

            child.ChildToFamilyLinks.Add(new GDMChildToFamilyLink(this.XRef));

            return true;
        }

        public bool RemoveChild(GDMIndividualRecord child)
        {
            if (child == null) return false;

            DeleteChild(child);
            child.DeleteChildToFamilyLink(this);

            return true;
        }

        public bool HasChild(GDMIndividualRecord child)
        {
            return IndexOfChild(child) >= 0;
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            ProcessHashes(ref hashCode, fChildren);
            hashCode.Add(fHusband);
            hashCode.Add(fWife);
            hashCode.Add(fStatus);
        }
    }
}
