/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
                var baseContext = ((ChangeTracker)fManager).Context;
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
