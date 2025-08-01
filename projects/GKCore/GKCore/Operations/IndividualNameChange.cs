﻿/*
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

using GDModel;

namespace GKCore.Operations
{
    public sealed class IndividualNameChange : CustomOperation
    {
        private readonly GDMIndividualRecord fPerson;
        private readonly string fNewSurname;
        private readonly string fNewName;
        private readonly string fNewPatronymic;
        private readonly string fNewMarriedSurname;
        private readonly string fNewNickname;
        private string fOldSurname;
        private string fOldName;
        private string fOldPatronymic;
        private string fOldMarriedSurname;
        private string fOldNickname;

        public IndividualNameChange(UndoManager manager, GDMIndividualRecord person,
                                    string surname, string name, string patronymic,
                                    string marriedSurname, string nickname) : base(manager)
        {
            fPerson = person;
            fNewSurname = surname;
            fNewName = name;
            fNewPatronymic = patronymic;
            fNewMarriedSurname = marriedSurname;
            fNewNickname = nickname;
        }

        public override bool Redo()
        {
            bool result = true;

            if (fPerson == null) {
                result = false;
            } else {
                IBaseContext baseContext = ((ChangeTracker)fManager).Context;
                GDMPersonalName np = fPerson.PersonalNames[0];
                var parts = GKUtils.GetNameParts(baseContext.Tree, fPerson, np, false);

                fOldSurname = parts.Surname;
                fOldName = parts.Name;
                fOldPatronymic = parts.Patronymic;
                fOldMarriedSurname = np.MarriedName;
                fOldNickname = np.Nickname;

                GKUtils.SetNameParts(np, fNewSurname, fNewName, fNewPatronymic);
                np.MarriedName = fNewMarriedSurname;
                np.Nickname= fNewNickname;
            }

            return result;
        }

        public override void Undo()
        {
            if (fPerson == null) return;

            GDMPersonalName np = fPerson.PersonalNames[0];
            GKUtils.SetNameParts(np, fOldSurname, fOldName, fOldPatronymic);
            np.MarriedName = fOldMarriedSurname;
            np.Nickname = fOldNickname;
        }
    }
}
