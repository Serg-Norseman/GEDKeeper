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

using System.IO;

namespace GKCommon.GEDCOM
{
    public sealed class GEDCOMGroupRecord : GEDCOMRecord
    {
        private GEDCOMList<GEDCOMPointer> fMembers;

        public GEDCOMList<GEDCOMPointer> Members
        {
            get { return fMembers; }
        }

        public string GroupName
        {
            get { return GetTagStringValue("NAME"); }
            set { SetTagStringValue("NAME", value); }
        }


        public GEDCOMGroupRecord(GEDCOMTree owner, GEDCOMObject parent) : base(owner, parent)
        {
            SetRecordType(GEDCOMRecordType.rtGroup);
            SetName(GEDCOMTagType._GROUP);

            fMembers = new GEDCOMList<GEDCOMPointer>(this);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fMembers.Dispose();
            }
            base.Dispose(disposing);
        }

        public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
        {
            GEDCOMTag result;

            if (tagName == "NAME") {
                result = base.AddTag(tagName, tagValue, null);
            } else if (tagName == GEDCOMTagType._MEMBER) {
                result = fMembers.Add(new GEDCOMPointer(Owner, this, tagName, tagValue));
            } else {
                result = base.AddTag(tagName, tagValue, tagConstructor);
            }

            return result;
        }

        public override void Clear()
        {
            base.Clear();
            fMembers.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && fMembers.Count == 0;
        }

        public override void ReplaceXRefs(XRefReplacer map)
        {
            base.ReplaceXRefs(map);
            fMembers.ReplaceXRefs(map);
        }

        public override void ResetOwner(GEDCOMTree newOwner)
        {
            base.ResetOwner(newOwner);
            fMembers.ResetOwner(newOwner);
        }

        public override void SaveToStream(StreamWriter stream)
        {
            base.SaveToStream(stream);
            fMembers.SaveToStream(stream);
        }

        // TODO: connect to use
        public override float IsMatch(GEDCOMTag tag, MatchParams matchParams)
        {
            GEDCOMGroupRecord otherGroup = tag as GEDCOMGroupRecord;
            if (otherGroup == null) return 0.0f;

            float match = GetStrMatch(GroupName, otherGroup.GroupName, matchParams);
            return match;
        }

        #region Auxiliary

        public int IndexOfMember(GEDCOMIndividualRecord member)
        {
            int result = -1;

            if (member != null) {
                int num = fMembers.Count;
                for (int i = 0; i < num; i++) {
                    if (fMembers[i].XRef == member.XRef) {
                        result = i;
                        break;
                    }
                }
            }

            return result;
        }

        public bool AddMember(GEDCOMIndividualRecord member)
        {
            if (member == null) return false;

            GEDCOMPointer ptr = new GEDCOMPointer(Owner, this);
            ptr.SetNamedValue(GEDCOMTagType._MEMBER, member);
            fMembers.Add(ptr);

            ptr = new GEDCOMPointer(Owner, member);
            ptr.SetNamedValue(GEDCOMTagType._GROUP, this);
            member.Groups.Add(ptr);

            return true;
        }

        public bool RemoveMember(GEDCOMIndividualRecord member)
        {
            if (member == null) return false;

            fMembers.DeleteAt(IndexOfMember(member));
            member.Groups.DeleteAt(member.IndexOfGroup(this));

            return true;
        }

        #endregion
    }
}
