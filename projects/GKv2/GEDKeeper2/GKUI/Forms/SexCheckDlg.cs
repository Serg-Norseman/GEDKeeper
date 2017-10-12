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

using System.Windows.Forms;

using GKCommon.GEDCOM;
using GKCore;
using GKCore.UIContracts;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class SexCheckDlg : Form, ISexCheckDlg
    {
        public SexCheckDlg()
        {
            InitializeComponent();

            btnAccept.Image = GKResources.iBtnAccept;
            btnCancel.Image = GKResources.iBtnCancel;

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

        public GEDCOMSex Sex
        {
            get
            {
                if (rbMale.Checked) {
                    return GEDCOMSex.svMale;
                }
                if (rbFemale.Checked) {
                    return GEDCOMSex.svFemale;
                }
                return GEDCOMSex.svNone;
            }
            set
            {
                switch (value)
                {
                    case GEDCOMSex.svNone:
                    case GEDCOMSex.svUndetermined:
                        rbNone.Checked = true;
                        break;

                    case GEDCOMSex.svMale:
                        rbMale.Checked = true;
                        break;

                    case GEDCOMSex.svFemale:
                        rbFemale.Checked = true;
                        break;
                }
            }
        }

        public bool ShowModalX(object owner)
        {
            return (ShowDialog() == DialogResult.OK);
        }
    }
}
