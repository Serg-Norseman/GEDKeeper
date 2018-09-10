/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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
using GKCore.Lists;
using GKCore.Options;
using GKCore.Types;
using GKCore.UIContracts;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class CommonFilterDlgController : DialogController<ICommonFilterDlg>
    {
        private readonly string[] fFields;
        private readonly IListManager fListMan;

        public string[] Fields
        {
            get { return fFields; }
        }

        public CommonFilterDlgController(ICommonFilterDlg view, IListManager listMan) : base(view)
        {
            fListMan = listMan;

            ListColumns listColumns = (ListColumns)fListMan.ListColumns;
            fFields = new string[listColumns.Count + 1]; // +empty item
            fFields[0] = "";

            for (int idx = 0; idx < listColumns.Count; idx++) {
                var cs = listColumns[idx];
                fFields[idx + 1] = fListMan.GetColumnName(cs.Id);
            }
        }

        public ConditionKind GetCondByName(string condName)
        {
            ConditionKind res = ConditionKind.ck_NotEq;

            for (ConditionKind pl = ConditionKind.ck_NotEq; pl <= ConditionKind.ck_NotContains; pl++) {
                if (GKData.CondSigns[(int)pl] == condName) {
                    res = pl;
                    break;
                }
            }

            return res;
        }

        public int GetFieldColumnId(string fieldName)
        {
            int idx = -1;
            for (int i = 0; i < fFields.Length; i++) {
                if (fFields[i] == fieldName) {
                    idx = i - 1; // exclude empty item
                    break;
                }
            }

            return idx;
        }

        public override bool Accept()
        {
            try {
                return true;
            } catch (Exception ex) {
                Logger.LogWrite("CommonFilterDlgController.Accept(): " + ex.Message);
                return false;
            }
        }

        public override void UpdateView()
        {
        }
    }
}
