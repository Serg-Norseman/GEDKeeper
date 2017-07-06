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
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Types;
using GKCore.UIContracts;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class NameEditDlg : Form, INameEditDlg
    {
        private NameEntry fNameEntry;

        public NameEntry IName
        {
            get { return fNameEntry; }
            set { SetIName(value); }
        }

        private void SetIName(NameEntry value)
        {
            fNameEntry = value;
            if (fNameEntry == null)
            {
                txtName.Text = "";
                cmbSex.SelectedIndex = 0;
                txtFPatr.Text = "";
                txtMPatr.Text = "";
            }
            else
            {
                txtName.Text = fNameEntry.Name;
                cmbSex.SelectedIndex = (sbyte)fNameEntry.Sex;
                txtFPatr.Text = fNameEntry.F_Patronymic;
                txtMPatr.Text = fNameEntry.M_Patronymic;
            }
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                fNameEntry.Name = txtName.Text;
                fNameEntry.Sex = (GEDCOMSex)cmbSex.SelectedIndex;
                fNameEntry.F_Patronymic = txtFPatr.Text;
                fNameEntry.M_Patronymic = txtMPatr.Text;
                DialogResult = DialogResult.OK;
            }
            catch (Exception ex)
            {
                Logger.LogWrite("NameEditDlg.btnAccept_Click(): " + ex.Message);
                DialogResult = DialogResult.None;
            }
        }

        private void edName_KeyPress(object sender, KeyPressEventArgs e)
        {
            if (e.KeyChar == '/')
            {
                e.Handled = true;
            }
        }

        public NameEditDlg()
        {
            InitializeComponent();

            btnAccept.Image = GKResources.iBtnAccept;
            btnCancel.Image = GKResources.iBtnCancel;

            for (GEDCOMSex sx = GEDCOMSex.svNone; sx <= GEDCOMSex.svLast; sx++)
            {
                cmbSex.Items.Add(GKUtils.SexStr(sx));
            }

            // SetLang()
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            Text = LangMan.LS(LSID.LSID_Name);
            lblName.Text = LangMan.LS(LSID.LSID_Name);
            lblSex.Text = LangMan.LS(LSID.LSID_Sex);
            grpPatronymics.Text = LangMan.LS(LSID.LSID_Patronymic);
            lblFemale.Text = LangMan.LS(LSID.LSID_PatFemale);
            lblMale.Text = LangMan.LS(LSID.LSID_PatMale);
        }

        public bool ShowModalX(object owner)
        {
            return (ShowDialog() == DialogResult.OK);
        }
    }
}
