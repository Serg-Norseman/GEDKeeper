﻿/*
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

using GDModel;
using GKCore;
using GKCore.MVP.Views;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class SexCheckDlg : CommonDialog, ISexCheckDlg
    {
        public SexCheckDlg()
        {
            InitializeComponent();

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            // SetLang()
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            Text = LangMan.LS(LSID.LSID_WinCheckSex);
            grpSex.Text = LangMan.LS(LSID.LSID_Sex);
            rbMale.Text = LangMan.LS(LSID.LSID_SexM);
            rbFemale.Text = LangMan.LS(LSID.LSID_SexF);
        }

        public string IndividualName
        {
            get { return txtName.Text; }
            set { txtName.Text = value; }
        }

        public GDMSex Sex
        {
            get {
                if (rbMale.Checked) {
                    return GDMSex.svMale;
                }
                if (rbFemale.Checked) {
                    return GDMSex.svFemale;
                }
                return GDMSex.svUnknown;
            }
            set {
                switch (value) {
                    case GDMSex.svUnknown:
                    case GDMSex.svIntersex:
                        rbNone.Checked = true;
                        break;

                    case GDMSex.svMale:
                        rbMale.Checked = true;
                        break;

                    case GDMSex.svFemale:
                        rbFemale.Checked = true;
                        break;
                }
            }
        }
    }
}
