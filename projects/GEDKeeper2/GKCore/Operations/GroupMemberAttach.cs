/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using GKCommon.GEDCOM;

namespace GKCore.Operations
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class GroupMemberAttach : CustomOperation
    {
        private string fGroupXRef;
        private string fMemberXRef;

        public GroupMemberAttach(UndoManager manager, GEDCOMGroupRecord group, GEDCOMIndividualRecord member) : base(manager)
        {
            this.fGroupXRef = group.XRef;
            this.fMemberXRef = member.XRef;
        }

        public override bool Redo()
        {
            bool result = true;

            GEDCOMGroupRecord grpRec = this.fManager.Tree.XRefIndex_Find(this.fGroupXRef) as GEDCOMGroupRecord;
            GEDCOMIndividualRecord mbrRec = this.fManager.Tree.XRefIndex_Find(this.fMemberXRef) as GEDCOMIndividualRecord;

            if (grpRec == null || mbrRec == null) {
                result = false;
            } else {
                grpRec.AddMember(mbrRec);
            }

            return result;
        }

        public override void Undo()
        {
            GEDCOMGroupRecord grpRec = this.fManager.Tree.XRefIndex_Find(this.fGroupXRef) as GEDCOMGroupRecord;
            GEDCOMIndividualRecord mbrRec = this.fManager.Tree.XRefIndex_Find(this.fMemberXRef) as GEDCOMIndividualRecord;

            if (grpRec != null && mbrRec != null) {
                grpRec.RemoveMember(mbrRec);
            }
        }
    }
}
