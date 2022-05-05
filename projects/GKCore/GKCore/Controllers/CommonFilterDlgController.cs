/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using GKCore.Interfaces;
using GKCore.MVP;
using GKCore.MVP.Views;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class CommonFilterDlgController : DialogController<ICommonFilterDlg>
    {
        private readonly IListManager fListMan;

        public CommonFilterDlgController(ICommonFilterDlg view, IListManager listMan) : base(view)
        {
            fListMan = listMan;
        }

        public override bool Accept()
        {
            try {
                fListMan.Filter.Clear();

                int num = fView.FilterGrid.Count;
                for (int r = 0; r < num; r++) {
                    FilterCondition fcond = fView.FilterGrid[r];
                    if (fcond != null) {
                        fListMan.AddCondition((byte)fcond.ColumnIndex, fcond.Condition, fcond.Value.ToString());
                    }
                }

                return true;
            } catch (Exception ex) {
                Logger.WriteError("CommonFilterDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            fView.FilterGrid.Clear();
            int num = fListMan.Filter.Conditions.Count;
            for (int i = 0; i < num; i++) {
                FilterCondition fcond = fListMan.Filter.Conditions[i];
                fView.FilterGrid.AddCondition(fcond);
            }
        }

        public override void SetLocale()
        {
        }
    }
}
