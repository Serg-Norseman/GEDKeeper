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
    public class PersonPatriarchChange : CustomOperation
    {
        private string fPersonXRef;
        private bool fOldVal;
        private bool fNewVal;

        public PersonPatriarchChange(UndoManager manager, GEDCOMIndividualRecord person, bool newValue) : base(manager)
        {
            this.fPersonXRef = person.XRef;
            this.fOldVal = person.Patriarch;
            this.fNewVal = newValue;
        }

        public override bool Redo()
        {
            bool result = true;

            GEDCOMIndividualRecord i_rec = this.fManager.Tree.XRefIndex_Find(this.fPersonXRef) as GEDCOMIndividualRecord;
            if (i_rec == null)
            {
                result = false;
            }
            else
            {
                i_rec.Patriarch = this.fNewVal;
            }

            return result;
        }

        public override void Undo()
        {
            GEDCOMIndividualRecord i_rec = this.fManager.Tree.XRefIndex_Find(this.fPersonXRef) as GEDCOMIndividualRecord;
            if (i_rec != null)
            {
                i_rec.Patriarch = this.fOldVal;
            }
        }
    }
}
