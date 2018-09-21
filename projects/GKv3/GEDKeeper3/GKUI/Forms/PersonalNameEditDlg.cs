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
using Eto.Forms;

using GKCommon.GEDCOM;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Controls;
using GKCore.MVP.Views;
using GKUI.Components;

namespace GKUI.Forms
{
    public partial class PersonalNameEditDlg: EditorDialog, IPersonalNameEditDlg
    {
        private readonly PersonalNameEditDlgController fController;

        public GEDCOMPersonalName PersonalName
        {
            get { return fController.PersonalName; }
            set { fController.PersonalName = value; }
        }

        #region View Interface

        ILabelHandler IPersonalNameEditDlg.SurnameLabel
        {
            get { return fControlsManager.GetControlHandler<ILabelHandler>(lblSurname); }
        }

        ITextBoxHandler IPersonalNameEditDlg.Surname
        {
            get { return fControlsManager.GetControlHandler<ITextBoxHandler>(txtSurname); }
        }

        ITextBoxHandler IPersonalNameEditDlg.Name
        {
            get { return fControlsManager.GetControlHandler<ITextBoxHandler>(txtName); }
        }

        ITextBoxHandler IPersonalNameEditDlg.Patronymic
        {
            get { return fControlsManager.GetControlHandler<ITextBoxHandler>(txtPatronymic); }
        }

        IComboBoxHandler IPersonalNameEditDlg.NameType
        {
            get { return fControlsManager.GetControlHandler<IComboBoxHandler>(cmbNameType); }
        }

        ITextBoxHandler IPersonalNameEditDlg.NamePrefix
        {
            get { return fControlsManager.GetControlHandler<ITextBoxHandler>(txtNamePrefix); }
        }

        ITextBoxHandler IPersonalNameEditDlg.Nickname
        {
            get { return fControlsManager.GetControlHandler<ITextBoxHandler>(txtNickname); }
        }

        ITextBoxHandler IPersonalNameEditDlg.SurnamePrefix
        {
            get { return fControlsManager.GetControlHandler<ITextBoxHandler>(txtSurnamePrefix); }
        }

        ITextBoxHandler IPersonalNameEditDlg.NameSuffix
        {
            get { return fControlsManager.GetControlHandler<ITextBoxHandler>(txtNameSuffix); }
        }

        ITextBoxHandler IPersonalNameEditDlg.MarriedSurname
        {
            get { return fControlsManager.GetControlHandler<ITextBoxHandler>(txtMarriedSurname); }
        }

        IComboBoxHandler IPersonalNameEditDlg.Language
        {
            get { return fControlsManager.GetControlHandler<IComboBoxHandler>(cmbLanguage); }
        }

        #endregion

        private void edName_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyChar == '/') {
                e.Handled = true;
            }
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            DialogResult = fController.Accept() ? DialogResult.Ok : DialogResult.None;
        }

        public PersonalNameEditDlg()
        {
            InitializeComponent();

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            SetLang();

            fController = new PersonalNameEditDlgController(this);
        }

        public override void InitDialog(IBaseWindow baseWin)
        {
            base.InitDialog(baseWin);
            fController.Init(baseWin);
        }

        public void SetLang()
        {
            Title = LangMan.LS(LSID.LSID_Name);
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
    }
}
