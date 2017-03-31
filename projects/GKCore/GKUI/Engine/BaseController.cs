/*
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

using System;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Operations;
using GKCore.Types;

namespace GKUI.Engine
{
    /// <summary>
    /// 
    /// </summary>
    public class BaseController : IBaseController
    {
        private IStdDialogs fStdDialogs;

        internal BaseController(IStdDialogs stdDialogs)
        {
            fStdDialogs = stdDialogs;
        }

        public bool AddFather(IBaseWindow baseWin, ChangeTracker localUndoman, GEDCOMIndividualRecord person)
        {
            bool result = false;

            GEDCOMIndividualRecord father = baseWin.SelectPerson(person, TargetMode.tmChild, GEDCOMSex.svMale);
            if (father != null)
            {
                GEDCOMFamilyRecord family = baseWin.GetChildFamily(person, true, father);
                if (family != null)
                {
                    if (family.Husband.Value == null) {
                        // new family
                        result = localUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseAttach, family, father);
                    } else {
                        // selected family with husband
                        result = true;
                    }
                }
            }

            return result;
        }

        public bool DeleteFather(IBaseWindow baseWin, ChangeTracker localUndoman, GEDCOMIndividualRecord person)
        {
            bool result = false;

            if (fStdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachFatherQuery)) == true)
            {
                GEDCOMFamilyRecord family = baseWin.GetChildFamily(person, false, null);
                if (family != null)
                {
                    GEDCOMIndividualRecord father = family.GetHusband();
                    result = localUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseDetach, family, father);
                }
            }

            return result;
        }
    }
}
