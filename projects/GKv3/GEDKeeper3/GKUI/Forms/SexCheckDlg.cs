/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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

using Eto.Forms;
using Eto.Serialization.Xaml;
using GDModel;
using GKCore;
using GKCore.Design.Views;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class SexCheckDlg : CommonDialog, ISexCheckDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private TextBox txtName;
        private GroupBox grpSex;
        private RadioButton rbNone;
        private RadioButton rbMale;
        private RadioButton rbFemale;
        private Button btnAccept;
        private Button btnCancel;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        public SexCheckDlg()
        {
            XamlReader.Load(this);

            UIHelper.FixRadioButtons(this, grpSex);

            // SetLocale()
            btnAccept.Text = LangMan.LS(LSID.DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.DlgCancel);
            Title = LangMan.LS(LSID.WinCheckSex);
            grpSex.Text = LangMan.LS(LSID.Sex);
            rbNone.Text = "?";
            rbMale.Text = LangMan.LS(LSID.SexM);
            rbFemale.Text = LangMan.LS(LSID.SexF);
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
