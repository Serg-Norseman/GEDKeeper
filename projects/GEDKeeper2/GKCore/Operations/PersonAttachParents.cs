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
    public sealed class PersonAttachParents : CustomOperation
    {
        private string fPersonXRef;
        private string fFamilyXRef;

        public PersonAttachParents(UndoManager manager, GEDCOMIndividualRecord person, GEDCOMFamilyRecord family) : base(manager)
        {
            this.fPersonXRef = person.XRef;
            this.fFamilyXRef = family.XRef;
        }

        public override bool Redo()
        {
            bool result = true;

            GEDCOMIndividualRecord iRec = this.fManager.Tree.XRefIndex_Find(this.fPersonXRef) as GEDCOMIndividualRecord;
            GEDCOMFamilyRecord familyRec = this.fManager.Tree.XRefIndex_Find(this.fFamilyXRef) as GEDCOMFamilyRecord;

            if (iRec == null || familyRec == null) {
                result = false;
            } else {
                familyRec.AddChild(iRec);
            }

            return result;
        }

        public override void Undo()
        {
            GEDCOMIndividualRecord iRec = this.fManager.Tree.XRefIndex_Find(this.fPersonXRef) as GEDCOMIndividualRecord;
            GEDCOMFamilyRecord familyRec = this.fManager.Tree.XRefIndex_Find(this.fFamilyXRef) as GEDCOMFamilyRecord;

            if (iRec != null && familyRec != null) {
                familyRec.RemoveChild(iRec);
            }
        }
    }
}
