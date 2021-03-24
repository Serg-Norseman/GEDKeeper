/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2020 by Sergey V. Zhdanovskih.
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
using GDModel;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Views;
using GKUI.Components;

namespace GKUI.Forms
{
    public partial class PersonalNameEditDlg: EditorDialog, IPersonalNameEditDlg
    {
        private readonly PersonalNameEditDlgController fController;

        public GDMIndividualRecord Individual
        {
            get { return fController.Individual; }
            set { fController.Individual = value; }
        }

        public GDMPersonalName PersonalName
        {
            get { return fController.PersonalName; }
            set { fController.PersonalName = value; }
        }

        #region View Interface

        ILabel IPersonalNameEditDlg.SurnameLabel
        {
            get { return GetControlHandler<ILabel>(lblSurname); }
        }

        ITextBox IPersonalNameEditDlg.Surname
        {
            get { return GetControlHandler<ITextBox>(txtSurname); }
        }

        ITextBox IPersonalNameEditDlg.Name
        {
            get { return GetControlHandler<ITextBox>(txtName); }
        }

        ITextBox IPersonalNameEditDlg.Patronymic
        {
            get { return GetControlHandler<ITextBox>(txtPatronymic); }
        }

        IComboBox IPersonalNameEditDlg.NameType
        {
            get { return GetControlHandler<IComboBox>(cmbNameType); }
        }

        ITextBox IPersonalNameEditDlg.NamePrefix
        {
            get { return GetControlHandler<ITextBox>(txtNamePrefix); }
        }

        ITextBox IPersonalNameEditDlg.Nickname
        {
            get { return GetControlHandler<ITextBox>(txtNickname); }
        }

        ITextBox IPersonalNameEditDlg.SurnamePrefix
        {
            get { return GetControlHandler<ITextBox>(txtSurnamePrefix); }
        }

        ITextBox IPersonalNameEditDlg.NameSuffix
        {
            get { return GetControlHandler<ITextBox>(txtNameSuffix); }
        }

        ITextBox IPersonalNameEditDlg.MarriedSurname
        {
            get { return GetControlHandler<ITextBox>(txtMarriedSurname); }
        }

        IComboBox IPersonalNameEditDlg.Language
        {
            get { return GetControlHandler<IComboBox>(cmbLanguage); }
        }

        #endregion

        public PersonalNameEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            SetLang();

            fController = new PersonalNameEditDlgController(this);
            fController.Init(baseWin);
        }

        public void SetLang()
        {
            Text = LangMan.LS(LSID.LSID_Name);
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            lblSurname.Text = LangMan.LS(LSID.LSID_Surname);
            lblMarriedSurname.Text = LangMan.LS(LSID.LSID_MarriedSurname);
            lblName.Text = LangMan.LS(LSID.LSID_Name);
            lblPatronymic.Text = LangMan.LS(LSID.LSID_Patronymic);
            lblNickname.Text = LangMan.LS(LSID.LSID_Nickname);
            lblSurnamePrefix.Text = LangMan.LS(LSID.LSID_SurnamePrefix);
            lblNamePrefix.Text = LangMan.LS(LSID.LSID_NamePrefix);
            lblNameSuffix.Text = LangMan.LS(LSID.LSID_NameSuffix);
            lblType.Text = LangMan.LS(LSID.LSID_Type);
            lblLanguage.Text = LangMan.LS(LSID.LSID_Language);
        }

        private void txtXName_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Down && e.Control) {
                UIHelper.ProcessName(sender);
            }
        }

        private void txtXName_Leave(object sender, EventArgs e)
        {
            UIHelper.ProcessName(sender);
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            DialogResult = fController.Accept() ? DialogResult.OK : DialogResult.None;
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            DialogResult = fController.Cancel() ? DialogResult.Cancel : DialogResult.None;
        }

        protected override void OnFormClosing(FormClosingEventArgs e)
        {
            base.OnFormClosing(e);
            e.Cancel = fController.CheckChangesPersistence();
        }
    }
}
