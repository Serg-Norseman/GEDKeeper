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
    public sealed class IndividualNameChange : CustomOperation
    {
        private GEDCOMIndividualRecord fPerson;
        private string fOldSurname;
        private string fNewSurname;
        private string fOldName;
        private string fNewName;
        private string fOldPatronymic;
        private string fNewPatronymic;

        public IndividualNameChange(UndoManager manager, GEDCOMIndividualRecord person,
                                    string surname, string name, string patronymic) : base(manager)
        {
            this.fPerson = person;
            this.fNewSurname = surname;
            this.fNewName = name;
            this.fNewPatronymic = patronymic;
        }

        public override bool Redo()
        {
            bool result = true;

            if (this.fPerson == null) {
                result = false;
            } else {
                GEDCOMPersonalName np = this.fPerson.PersonalNames[0];

                string surname, name, patr;
                GKUtils.GetRusNameParts(np, out surname, out name, out patr);

                this.fOldSurname = surname;
                this.fOldName = name;
                this.fOldPatronymic = patr;

                GKUtils.SetRusNameParts(np, this.fNewSurname, this.fNewName, this.fNewPatronymic);
            }

            return result;
        }

        public override void Undo()
        {
            if (this.fPerson != null) {
                GEDCOMPersonalName np = this.fPerson.PersonalNames[0];
                GKUtils.SetRusNameParts(np, this.fOldSurname, this.fOldName, this.fOldPatronymic);
            }
        }
    }
}
