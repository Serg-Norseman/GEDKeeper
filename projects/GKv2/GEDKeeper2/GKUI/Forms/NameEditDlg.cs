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
using System.Windows.Forms;
using BSLib.Design.MVP.Controls;
using GKCore;
using GKCore.Controllers;
using GKCore.MVP.Controls;
using GKCore.MVP.Views;
using GKCore.Types;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class NameEditDlg : CommonDialog, INameEditDlg
    {
        private readonly NameEditDlgController fController;

        public NameEntry IName
        {
            get { return fController.NameEntry; }
            set { fController.NameEntry = value; }
        }

        #region View Interface

        ITextBox INameEditDlg.Name
        {
            get { return GetControlHandler<ITextBox>(txtName); }
        }

        ITextBox INameEditDlg.FPatr
        {
            get { return GetControlHandler<ITextBox>(txtFPatr); }
        }

        ITextBox INameEditDlg.MPatr
        {
            get { return GetControlHandler<ITextBox>(txtMPatr); }
        }

        IComboBox INameEditDlg.SexCombo
        {
            get { return GetControlHandler<IComboBox>(cmbSex); }
        }

        #endregion

        private void btnAccept_Click(object sender, EventArgs e)
        {
            DialogResult = fController.Accept() ? DialogResult.OK : DialogResult.None;
        }

        private void edName_KeyPress(object sender, KeyPressEventArgs e)
        {
            if (e.KeyChar == '/') {
                e.Handled = true;
            }
        }

        public NameEditDlg()
        {
            InitializeComponent();

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            // SetLang()
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            Text = LangMan.LS(LSID.LSID_Name);
            lblName.Text = LangMan.LS(LSID.LSID_Name);
            lblSex.Text = LangMan.LS(LSID.LSID_Sex);
            grpPatronymics.Text = LangMan.LS(LSID.LSID_Patronymic);
            lblFemale.Text = LangMan.LS(LSID.LSID_PatFemale);
            lblMale.Text = LangMan.LS(LSID.LSID_PatMale);

            fController = new NameEditDlgController(this);
        }
    }
}
