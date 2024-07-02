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
    public sealed class GDMGroupRecord : GDMRecord
    {
        private string fGroupName;
        private readonly GDMList<GDMIndividualLink> fMembers;


        public string GroupName
        {
            get { return fGroupName; }
            set { fGroupName = value; }
        }

        public GDMList<GDMIndividualLink> Members
        {
            get { return fMembers; }
        }


        public GDMGroupRecord(GDMTree tree) : base(tree)
        {
            SetName(GEDCOMTagType._GROUP);

            fGroupName = string.Empty;
            fMembers = new GDMList<GDMIndividualLink>();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fMembers.Dispose();
            }
            base.Dispose(disposing);
        }

        internal override void TrimExcess()
        {
            base.TrimExcess();

            fMembers.TrimExcess();
        }

        public override void Assign(GDMTag source)
        {
            GDMGroupRecord sourceObj = source as GDMGroupRecord;
            if (sourceObj == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(sourceObj);

            GroupName = sourceObj.GroupName;
            AssignList(sourceObj.Members, fMembers);
        }

        public override void Clear()
        {
            base.Clear();
            fGroupName = string.Empty;
            fMembers.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && (fMembers.Count == 0) && (string.IsNullOrEmpty(fGroupName));
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

            GDMIndividualLink mbrLink = new GDMIndividualLink((int)GEDCOMTagType._MEMBER);
            mbrLink.XRef = member.XRef;
            fMembers.Add(mbrLink);

            var ptr = new GDMPointer((int)GEDCOMTagType._GROUP);
            ptr.XRef = this.XRef;
            member.Groups.Add(ptr);

            return true;
        }

        public bool RemoveMember(GDMIndividualRecord member)
        {
            if (member == null) return false;

            fMembers.RemoveAt(IndexOfMember(member));
            member.Groups.RemoveAt(member.IndexOfGroup(this));

            return true;
        }

        public override void MoveTo(GDMRecord targetRecord)
        {
            var targetGroup = targetRecord as GDMGroupRecord;
            if (targetGroup == null)
                throw new ArgumentException(@"Argument is null or wrong type", "targetRecord");

            base.MoveTo(targetRecord);

            while (fMembers.Count > 0) {
                var obj = fMembers.Extract(0);
                targetGroup.Members.Add(obj);
            }
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fGroupName);
            ProcessHashes(ref hashCode, fMembers);
        }
    }
}
