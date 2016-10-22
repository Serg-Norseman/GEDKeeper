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
using GKCore.Interfaces;

namespace GKCore.Operations
{
    /// <summary>
    /// 
    /// </summary>
    public static class ChangeTracker
    {

        public static void ChangeIndividualSex(IUndoManager undoman, GEDCOMIndividualRecord person, GEDCOMSex newSex)
        {
            if (undoman == null)
                throw new ArgumentNullException("undoman");

            if (person == null)
                throw new ArgumentNullException("person");

            if (person.Sex != newSex)
            {
                UndoManager uMan = ((UndoManager) undoman);
                uMan.DoOperation(new PersonSexChange(uMan, person, newSex));
            }
        }

        public static void ChangeIndividualPatriarch(IUndoManager undoman, GEDCOMIndividualRecord person, bool newValue)
        {
            if (undoman == null)
                throw new ArgumentNullException("undoman");

            if (person == null)
                throw new ArgumentNullException("person");

            if (person.Patriarch != newValue)
            {
                UndoManager uMan = ((UndoManager) undoman);
                uMan.DoOperation(new PersonPatriarchChange(uMan, person, newValue));
            }
        }

        public static void ChangeIndividualBookmark(IUndoManager undoman, GEDCOMIndividualRecord person, bool newValue)
        {
            if (undoman == null)
                throw new ArgumentNullException("undoman");

            if (person == null)
                throw new ArgumentNullException("person");

            if (person.Bookmark != newValue)
            {
                UndoManager uMan = ((UndoManager) undoman);
                uMan.DoOperation(new PersonBookmarkChange(uMan, person, newValue));
            }
        }


        public static void AttachIndividualParents(IUndoManager undoman, GEDCOMIndividualRecord person, GEDCOMFamilyRecord family)
        {
            if (undoman == null)
                throw new ArgumentNullException("undoman");

            if (person == null)
                throw new ArgumentNullException("person");

            if (family == null)
                throw new ArgumentNullException("family");

            UndoManager uMan = ((UndoManager) undoman);
            uMan.DoOperation(new PersonParentsAttach(uMan, person, family));
        }

        public static void DetachIndividualParents(IUndoManager undoman, GEDCOMIndividualRecord person, GEDCOMFamilyRecord family)
        {
            if (undoman == null)
                throw new ArgumentNullException("undoman");

            if (person == null)
                throw new ArgumentNullException("person");

            if (family == null)
                throw new ArgumentNullException("family");

            UndoManager uMan = ((UndoManager) undoman);
            uMan.DoOperation(new PersonParentsDetach(uMan, person, family));
        }


        public static void AttachFamilySpouse(IUndoManager undoman, GEDCOMFamilyRecord family, GEDCOMIndividualRecord spouse)
        {
            if (undoman == null)
                throw new ArgumentNullException("undoman");

            if (family == null)
                throw new ArgumentNullException("family");

            if (spouse == null)
                throw new ArgumentNullException("spouse");

            UndoManager uMan = ((UndoManager) undoman);
            uMan.DoOperation(new FamilySpouseAttach(uMan, family, spouse));
        }

        public static void DetachFamilySpouse(IUndoManager undoman, GEDCOMFamilyRecord family, GEDCOMIndividualRecord spouse)
        {
            if (undoman == null)
                throw new ArgumentNullException("undoman");

            if (family == null)
                throw new ArgumentNullException("family");

            if (spouse == null)
                throw new ArgumentNullException("spouse");

            UndoManager uMan = ((UndoManager) undoman);
            uMan.DoOperation(new FamilySpouseDetach(uMan, family, spouse));
        }


        /*public static void AttachGroupMember(IUndoManager undoman, GEDCOMGroupRecord group, GEDCOMIndividualRecord member)
        {
            if (undoman == null)
                throw new ArgumentNullException("undoman");

            if (group == null)
                throw new ArgumentNullException("group");

            if (member == null)
                throw new ArgumentNullException("member");

            UndoManager uMan = ((UndoManager) undoman);
            uMan.DoOperation(new GroupMemberAttach(uMan, group, member));
        }*/

        /*public static void DetachGroupMember(IUndoManager undoman, GEDCOMGroupRecord group, GEDCOMIndividualRecord member)
        {
            if (undoman == null)
                throw new ArgumentNullException("undoman");

            if (group == null)
                throw new ArgumentNullException("group");

            if (member == null)
                throw new ArgumentNullException("member");

            UndoManager uMan = ((UndoManager) undoman);
            uMan.DoOperation(new GroupMemberDetach(uMan, group, member));
        }*/
    }
}
