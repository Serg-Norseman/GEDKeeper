﻿/*
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
using GKCore.Types;

namespace GKCommon.GEDCOM
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


    public sealed class GEDCOMFamilyRecord : GEDCOMRecordWithEvents
    {
        private GEDCOMList<GEDCOMPointer> fChildren;
        private GDMMarriageStatus fStatus;


        public GEDCOMList<GEDCOMPointer> Children
        {
            get { return fChildren; }
        }

        public GEDCOMPointer Husband
        {
            get { return GetTag(GEDCOMTagType.HUSB, GEDCOMPointer.Create) as GEDCOMPointer; }
        }

        public GEDCOMPointer Wife
        {
            get { return GetTag(GEDCOMTagType.WIFE, GEDCOMPointer.Create) as GEDCOMPointer; }
        }

        public GDMMarriageStatus Status
        {
            get { return fStatus; }
            set { fStatus = value; }
        }


        public GEDCOMFamilyRecord(GEDCOMObject owner) : base(owner)
        {
            SetRecordType(GEDCOMRecordType.rtFamily);
            SetName(GEDCOMTagType.FAM);

            fChildren = new GEDCOMList<GEDCOMPointer>(this);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fChildren.Dispose();
            }
            base.Dispose(disposing);
        }

        public GEDCOMIndividualRecord GetHusband()
        {
            return Husband.Value as GEDCOMIndividualRecord;
        }

        public GEDCOMIndividualRecord GetWife()
        {
            return Wife.Value as GEDCOMIndividualRecord;
        }

        public override GEDCOMCustomEvent AddEvent(GEDCOMCustomEvent evt)
        {
            var famEvent = evt as GEDCOMFamilyEvent;
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

            // Special cleaning of Husband and Wife is not necessary,
            // because these are sub-tags and they will be cleared in the call base.Clear()

            int num = fChildren.Count;
            for (int i = 0; i < num; i++) {
                GEDCOMIndividualRecord child = (GEDCOMIndividualRecord)fChildren[i].Value;
                child.DeleteChildToFamilyLink(this);
            }
            fChildren.Clear();

            fStatus = GDMMarriageStatus.Unknown;
        }

        public override bool IsEmpty()
        {
            // Status are not checked because they are not important if other fields are empty.
            return base.IsEmpty() && (fChildren.Count == 0);
        }

        public void DeleteChild(GEDCOMRecord childRec)
        {
            for (int i = fChildren.Count - 1; i >= 0; i--) {
                if (fChildren[i].Value == childRec) {
                    fChildren.DeleteAt(i);
                    break;
                }
            }
        }

        public int IndexOfChild(GEDCOMRecord childRec)
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

        public override void Assign(GEDCOMTag source)
        {
            GEDCOMFamilyRecord sourceRec = source as GEDCOMFamilyRecord;
            if (sourceRec == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(source);

            fStatus = sourceRec.fStatus;
        }

        public override void MoveTo(GEDCOMRecord targetRecord, bool clearDest)
        {
            GEDCOMFamilyRecord targetFamily = targetRecord as GEDCOMFamilyRecord;
            if (targetFamily == null)
                throw new ArgumentException(@"Argument is null or wrong type", "targetRecord");

            base.MoveTo(targetRecord, clearDest);

            targetFamily.Status = fStatus;

            while (fChildren.Count > 0) {
                GEDCOMPointer obj = fChildren.Extract(0);
                obj.ResetOwner(targetFamily);
                targetFamily.Children.Add(obj);
            }
        }

        public override void Pack()
        {
            base.Pack();

            fChildren.Pack();
        }

        public override void ReplaceXRefs(XRefReplacer map)
        {
            base.ReplaceXRefs(map);

            if (Husband != null) {
                Husband.StringValue = GEDCOMUtils.EncloseXRef(map.FindNewXRef(Husband.StringValue));
            }

            if (Wife != null) {
                Wife.StringValue = GEDCOMUtils.EncloseXRef(map.FindNewXRef(Wife.StringValue));
            }

            fChildren.ReplaceXRefs(map);
        }

        public override void SaveToStream(StreamWriter stream, int level)
        {
            base.SaveToStream(stream, level);

            level += 1;
            WriteTagLine(stream, level, GEDCOMTagType._STAT, GEDCOMUtils.GetMarriageStatusStr(fStatus), true);

            fChildren.SaveToStream(stream, level);
            Events.SaveToStream(stream, level); // for files content compatibility
        }

        private string GetFamilyString()
        {
            string result = "";

            GEDCOMIndividualRecord spouse = GetHusband();
            result += (spouse == null) ? "?" : spouse.GetPrimaryFullName();
            result += " - ";
            spouse = GetWife();
            result += (spouse == null) ? "?" : spouse.GetPrimaryFullName();

            return result;
        }

        public override float IsMatch(GEDCOMTag tag, MatchParams matchParams)
        {
            GEDCOMFamilyRecord otherFam = tag as GEDCOMFamilyRecord;
            if (otherFam == null) return 0.0f;

            float match = GetStrMatch(GetFamilyString(), otherFam.GetFamilyString(), matchParams);
            return match;
        }

        #region Auxiliary

        private static int EventsCompare(GEDCOMPointer cp1, GEDCOMPointer cp2)
        {
            UDN udn1 = ((GEDCOMIndividualRecord)cp1.Value).GetUDN(GEDCOMTagType.BIRT);
            UDN udn2 = ((GEDCOMIndividualRecord)cp2.Value).GetUDN(GEDCOMTagType.BIRT);
            return udn1.CompareTo(udn2);
        }

        public void SortChilds()
        {
            fChildren.Sort(EventsCompare);
        }

        public GEDCOMIndividualRecord GetSpouseBy(GEDCOMIndividualRecord spouse)
        {
            GEDCOMIndividualRecord husb = GetHusband();
            GEDCOMIndividualRecord wife = GetWife();

            return (spouse == husb) ? wife : husb;
        }

        public bool AddSpouse(GEDCOMIndividualRecord spouse)
        {
            if (spouse == null) {
                return false;
            }

            GEDCOMSex sex = spouse.Sex;
            if (sex == GEDCOMSex.svNone || sex == GEDCOMSex.svUndetermined) {
                return false;
            }

            switch (sex) {
                case GEDCOMSex.svMale:
                    Husband.Value = spouse;
                    break;

                case GEDCOMSex.svFemale:
                    Wife.Value = spouse;
                    break;
            }

            GEDCOMSpouseToFamilyLink spLink = new GEDCOMSpouseToFamilyLink(spouse);
            spLink.Family = this;
            spouse.SpouseToFamilyLinks.Add(spLink);

            return true;
        }

        public void RemoveSpouse(GEDCOMIndividualRecord spouse)
        {
            if (spouse == null) return;

            spouse.DeleteSpouseToFamilyLink(this);

            switch (spouse.Sex) {
                case GEDCOMSex.svMale:
                    Husband.Value = null;
                    break;

                case GEDCOMSex.svFemale:
                    Wife.Value = null;
                    break;
            }
        }

        public bool AddChild(GEDCOMIndividualRecord child)
        {
            if (child == null) return false;

            GEDCOMPointer ptr = new GEDCOMPointer(this);
            ptr.SetNameValue(GEDCOMTagType.CHIL, child);
            fChildren.Add(ptr);

            GEDCOMChildToFamilyLink chLink = new GEDCOMChildToFamilyLink(child);
            chLink.Family = this;
            child.ChildToFamilyLinks.Add(chLink);

            return true;
        }

        public bool RemoveChild(GEDCOMIndividualRecord child)
        {
            if (child == null) return false;

            DeleteChild(child);
            child.DeleteChildToFamilyLink(this);

            return true;
        }

        #endregion
    }
}