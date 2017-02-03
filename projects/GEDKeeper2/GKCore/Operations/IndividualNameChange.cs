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

using GKCommon.GEDCOM;

namespace GKCore.Operations
{
    public sealed class IndividualNameChange : CustomOperation
    {
        private readonly GEDCOMIndividualRecord fPerson;
        private string fOldSurname;
        private string fNewSurname;
        private string fOldName;
        private string fNewName;
        private string fOldPatronymic;
        private string fNewPatronymic;

        public IndividualNameChange(UndoManager manager, GEDCOMIndividualRecord person,
                                    string surname, string name, string patronymic) : base(manager)
        {
            fPerson = person;
            fNewSurname = surname;
            fNewName = name;
            fNewPatronymic = patronymic;
        }

        public override bool Redo()
        {
            bool result = true;

            if (fPerson == null) {
                result = false;
            } else {
                GEDCOMPersonalName np = fPerson.PersonalNames[0];

                string surname, name, patr;
                GKUtils.GetRusNameParts(np, out surname, out name, out patr);

                fOldSurname = surname;
                fOldName = name;
                fOldPatronymic = patr;

                GKUtils.SetRusNameParts(np, fNewSurname, fNewName, fNewPatronymic);
            }

            return result;
        }

        public override void Undo()
        {
            if (fPerson != null) {
                GEDCOMPersonalName np = fPerson.PersonalNames[0];
                GKUtils.SetRusNameParts(np, fOldSurname, fOldName, fOldPatronymic);
            }
        }
    }
}
