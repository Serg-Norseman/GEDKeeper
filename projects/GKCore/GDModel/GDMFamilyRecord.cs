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
using BSLib.Calendar;
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


    public sealed class GDMFamilyRecord : GDMRecordWithEvents
    {
        private GDMList<GDMPointer> fChildren;
        private GDMPointer fHusband;
        private GDMPointer fWife;
        private GDMMarriageStatus fStatus;


        public GDMList<GDMPointer> Children
        {
            get { return fChildren; }
        }

        public GDMPointer Husband
        {
            get { return fHusband; }
        }

        public GDMPointer Wife
        {
            get { return fWife; }
        }

        public GDMMarriageStatus Status
        {
            get { return fStatus; }
            set { fStatus = value; }
        }


        public GDMFamilyRecord(GDMObject owner) : base(owner)
        {
            SetRecordType(GDMRecordType.rtFamily);
            SetName(GEDCOMTagType.FAM);

            fHusband = new GDMPointer(this, GEDCOMTagType.HUSB, string.Empty);
            fWife = new GDMPointer(this, GEDCOMTagType.WIFE, string.Empty);
            fChildren = new GDMList<GDMPointer>(this);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fChildren.Dispose();
            }
            base.Dispose(disposing);
        }

        public GDMIndividualRecord GetHusband()
        {
            return fHusband.Value as GDMIndividualRecord;
        }

        public GDMIndividualRecord GetWife()
        {
            return fWife.Value as GDMIndividualRecord;
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

            RemoveSpouse(GetHusband());
            RemoveSpouse(GetWife());

            int num = fChildren.Count;
            for (int i = 0; i < num; i++) {
                GDMIndividualRecord child = (GDMIndividualRecord)fChildren[i].Value;
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
            for (int i = fChildren.Count - 1; i >= 0; i--) {
                if (fChildren[i].Value == childRec) {
                    fChildren.DeleteAt(i);
                    break;
                }
            }
        }

        public int IndexOfChild(GDMRecord childRec)
        {
            int result = -1;

            for (int i = fChildren.Count - 1; i >= 0; i--) {
                if (fChildren[i].Value == childRec) {
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

            // TODO: validate this logic!
            foreach (GDMPointer srcChild in sourceRec.fChildren) {
                GDMPointer copyChild = new GDMPointer(this);
                copyChild.Assign(srcChild);
                fChildren.Add(copyChild);
            }
        }

        public override void MoveTo(GDMRecord targetRecord, bool clearDest)
        {
            GDMFamilyRecord targetFamily = targetRecord as GDMFamilyRecord;
            if (targetFamily == null)
                throw new ArgumentException(@"Argument is null or wrong type", "targetRecord");

            base.MoveTo(targetRecord, clearDest);

            targetFamily.Husband.XRef = fHusband.XRef;
            targetFamily.Wife.XRef = fWife.XRef;
            targetFamily.Status = fStatus;

            while (fChildren.Count > 0) {
                GDMPointer obj = fChildren.Extract(0);
                obj.ResetOwner(targetFamily);
                targetFamily.Children.Add(obj);
            }
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);

            fHusband.StringValue = GEDCOMUtils.EncloseXRef(map.FindNewXRef(fHusband.StringValue));
            fWife.StringValue = GEDCOMUtils.EncloseXRef(map.FindNewXRef(fWife.StringValue));

            fChildren.ReplaceXRefs(map);
        }

        private string GetFamilyString()
        {
            string result = "";

            GDMIndividualRecord spouse = GetHusband();
            result += (spouse == null) ? "?" : spouse.GetPrimaryFullName();
            result += " - ";
            spouse = GetWife();
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

        private static int EventsCompare(GDMPointer cp1, GDMPointer cp2)
        {
            UDN udn1 = ((GDMIndividualRecord)cp1.Value).GetUDN(GEDCOMTagType.BIRT);
            UDN udn2 = ((GDMIndividualRecord)cp2.Value).GetUDN(GEDCOMTagType.BIRT);
            return udn1.CompareTo(udn2);
        }

        public void SortChilds()
        {
            fChildren.Sort(EventsCompare);
        }

        public GDMIndividualRecord GetSpouseBy(GDMIndividualRecord spouse)
        {
            GDMIndividualRecord husb = GetHusband();
            GDMIndividualRecord wife = GetWife();

            return (spouse == husb) ? wife : husb;
        }

        public bool AddSpouse(GDMIndividualRecord spouse)
        {
            if (spouse == null) {
                return false;
            }

            GDMSex sex = spouse.Sex;
            if (sex == GDMSex.svUnknown || sex == GDMSex.svIntersex) {
                return false;
            }

            switch (sex) {
                case GDMSex.svMale:
                    fHusband.Value = spouse;
                    break;

                case GDMSex.svFemale:
                    fWife.Value = spouse;
                    break;
            }

            GDMSpouseToFamilyLink spLink = new GDMSpouseToFamilyLink(spouse);
            spLink.Family = this;
            spouse.SpouseToFamilyLinks.Add(spLink);

            return true;
        }

        public void RemoveSpouse(GDMIndividualRecord spouse)
        {
            if (spouse == null) return;

            spouse.DeleteSpouseToFamilyLink(this);

            switch (spouse.Sex) {
                case GDMSex.svMale:
                    fHusband.Value = null;
                    break;

                case GDMSex.svFemale:
                    fWife.Value = null;
                    break;
            }
        }

        public bool AddChild(GDMIndividualRecord child)
        {
            if (child == null) return false;

            GDMPointer ptr = new GDMPointer(this);
            ptr.SetNameValue(GEDCOMTagType.CHIL, child);
            fChildren.Add(ptr);

            GDMChildToFamilyLink chLink = new GDMChildToFamilyLink(child);
            chLink.Family = this;
            child.ChildToFamilyLinks.Add(chLink);

            return true;
        }

        public bool RemoveChild(GDMIndividualRecord child)
        {
            if (child == null) return false;

            DeleteChild(child);
            child.DeleteChildToFamilyLink(this);

            return true;
        }
    }
}
