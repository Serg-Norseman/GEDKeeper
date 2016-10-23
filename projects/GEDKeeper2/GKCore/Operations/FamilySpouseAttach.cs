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
    public sealed class FamilySpouseAttach : CustomOperation
    {
        private string fFamilyXRef;
        private string fSpouseXRef;

        public FamilySpouseAttach(UndoManager manager, GEDCOMFamilyRecord family, GEDCOMIndividualRecord spouse) : base(manager)
        {
            this.fFamilyXRef = family.XRef;
            this.fSpouseXRef = spouse.XRef;
        }

        public override bool Redo()
        {
            bool result = true;

            GEDCOMFamilyRecord famRec = this.FindRecord(this.fFamilyXRef) as GEDCOMFamilyRecord;
            GEDCOMIndividualRecord spouseRec = this.FindRecord(this.fSpouseXRef) as GEDCOMIndividualRecord;

            if (famRec == null || spouseRec == null) {
                result = false;
            } else {
                famRec.AddSpouse(spouseRec);
            }

            return result;
        }

        public override void Undo()
        {
            GEDCOMFamilyRecord famRec = this.FindRecord(this.fFamilyXRef) as GEDCOMFamilyRecord;
            GEDCOMIndividualRecord spouseRec = this.FindRecord(this.fSpouseXRef) as GEDCOMIndividualRecord;

            if (famRec != null && spouseRec != null) {
                famRec.RemoveSpouse(spouseRec);
            }
        }
    }
}
