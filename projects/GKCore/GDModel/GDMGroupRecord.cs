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

using GDModel.Providers.GEDCOM;
using GKCore.Types;

namespace GDModel
{
    public sealed class GDMGroupRecord : GDMRecord
    {
        private GDMList<GDMPointer> fMembers;

        public GDMList<GDMPointer> Members
        {
            get { return fMembers; }
        }

        public string GroupName
        {
            get { return GetTagStringValue(GEDCOMTagType.NAME); }
            set { SetTagStringValue(GEDCOMTagType.NAME, value); }
        }


        public GDMGroupRecord(GDMObject owner) : base(owner)
        {
            SetRecordType(GDMRecordType.rtGroup);
            SetName(GEDCOMTagType._GROUP);

            fMembers = new GDMList<GDMPointer>(this);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fMembers.Dispose();
            }
            base.Dispose(disposing);
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

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);
            fMembers.ReplaceXRefs(map);
        }

        // TODO: connect to use
        public override float IsMatch(GDMTag tag, MatchParams matchParams)
        {
            GDMGroupRecord otherGroup = tag as GDMGroupRecord;
            if (otherGroup == null) return 0.0f;

            float match = GetStrMatch(GroupName, otherGroup.GroupName, matchParams);
            return match;
        }

        public int IndexOfMember(GDMIndividualRecord member)
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

        public bool AddMember(GDMIndividualRecord member)
        {
            if (member == null) return false;

            GDMPointer ptr = new GDMPointer(this);
            ptr.SetNameValue(GEDCOMTagType._MEMBER, member);
            fMembers.Add(ptr);

            ptr = new GDMPointer(member);
            ptr.SetNameValue(GEDCOMTagType._GROUP, this);
            member.Groups.Add(ptr);

            return true;
        }

        public bool RemoveMember(GDMIndividualRecord member)
        {
            if (member == null) return false;

            fMembers.DeleteAt(IndexOfMember(member));
            member.Groups.DeleteAt(member.IndexOfGroup(this));

            return true;
        }
    }
}
