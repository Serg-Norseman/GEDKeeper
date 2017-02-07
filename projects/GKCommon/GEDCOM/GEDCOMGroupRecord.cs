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

using System.IO;

namespace GKCommon.GEDCOM
{
    public sealed class GEDCOMGroupRecord : GEDCOMRecord
    {
        private GEDCOMList<GEDCOMPointer> fMembers;

        public GEDCOMList<GEDCOMPointer> Members
        {
            get { return this.fMembers; }
        }

        public string GroupName
        {
            get { return base.GetTagStringValue("NAME"); }
            set { base.SetTagStringValue("NAME", value); }
        }

        protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
        {
            base.CreateObj(owner, parent);
            base.SetRecordType(GEDCOMRecordType.rtGroup);
            base.SetName("_GROUP");

            this.fMembers = new GEDCOMList<GEDCOMPointer>(this);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                this.fMembers.Dispose();
            }
            base.Dispose(disposing);
        }

        public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
        {
            GEDCOMTag result;

            if (tagName == "NAME")
            {
                result = base.AddTag(tagName, tagValue, null);
            }
            else if (tagName == "_MEMBER")
            {
                result = this.fMembers.Add(new GEDCOMPointer(base.Owner, this, tagName, tagValue));
            }
            else
            {
                result = base.AddTag(tagName, tagValue, tagConstructor);
            }

            return result;
        }

        public override void Clear()
        {
            base.Clear();
            this.fMembers.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && this.fMembers.Count == 0;
        }

        public override void ReplaceXRefs(XRefReplacer map)
        {
            base.ReplaceXRefs(map);
            this.fMembers.ReplaceXRefs(map);
        }

        public override void ResetOwner(GEDCOMTree newOwner)
        {
            base.ResetOwner(newOwner);
            this.fMembers.ResetOwner(newOwner);
        }

        public override void SaveToStream(StreamWriter stream)
        {
            base.SaveToStream(stream);
            this.fMembers.SaveToStream(stream);
        }

        public GEDCOMGroupRecord(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
        }

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
        {
            return new GEDCOMGroupRecord(owner, parent, tagName, tagValue);
        }

        #region Auxiliary

        public int IndexOfMember(GEDCOMIndividualRecord member)
        {
            if (member != null) {
                int num = this.fMembers.Count;
                for (int i = 0; i < num; i++)
                {
                    if (this.fMembers[i].XRef == member.XRef) {
                        return i;
                    }
                }
            }

            return -1;
        }

        public bool AddMember(GEDCOMIndividualRecord member)
        {
            if (member == null) return false;

            GEDCOMPointer ptr = new GEDCOMPointer(this.Owner, this, "", "");
            ptr.SetNamedValue("_MEMBER", member);
            this.fMembers.Add(ptr);

            ptr = new GEDCOMPointer(this.Owner, member, "", "");
            ptr.SetNamedValue("_GROUP", this);
            member.Groups.Add(ptr);

            return true;
        }

        public bool RemoveMember(GEDCOMIndividualRecord member)
        {
            if (member == null) return false;

            this.fMembers.DeleteAt(this.IndexOfMember(member));
            member.Groups.DeleteAt(member.IndexOfGroup(this));

            return true;
        }

        #endregion
    }
}
