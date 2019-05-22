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
using GDModel;
using GKCore.MVP;
using GKCore.MVP.Views;
using GKCore.Types;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class NameEditDlgController : DialogController<INameEditDlg>
    {
        private NameEntry fIName;

        public NameEntry NameEntry
        {
            get { return fIName; }
            set {
                if (fIName != value) {
                    fIName = value;
                    UpdateView();
                }
            }
        }


        public NameEditDlgController(INameEditDlg view) : base(view)
        {
            for (GEDCOMSex sx = GEDCOMSex.svNone; sx <= GEDCOMSex.svLast; sx++) {
                fView.SexCombo.Add(GKUtils.SexStr(sx));
            }
        }

        public override bool Accept()
        {
            try {
                fIName.Name = fView.Name.Text;
                fIName.Sex = (GEDCOMSex)fView.SexCombo.SelectedIndex;
                fIName.F_Patronymic = fView.FPatr.Text;
                fIName.M_Patronymic = fView.MPatr.Text;

                return true;
            } catch (Exception ex) {
                Logger.LogWrite("NameEditDlgController.Accept(): " + ex.Message);
                return false;
            }
        }

        public override void UpdateView()
        {
            if (fIName == null) {
                fView.Name.Text = "";
                fView.SexCombo.SelectedIndex = 0;
                fView.FPatr.Text = "";
                fView.MPatr.Text = "";
            } else {
                fView.Name.Text = fIName.Name;
                fView.SexCombo.SelectedIndex = (sbyte)fIName.Sex;
                fView.FPatr.Text = fIName.F_Patronymic;
                fView.MPatr.Text = fIName.M_Patronymic;
            }
        }
    }
}
