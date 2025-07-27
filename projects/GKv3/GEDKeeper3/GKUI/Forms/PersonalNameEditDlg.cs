/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using Eto.Serialization.Xaml;
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKUI.Components;

namespace GKUI.Forms
{
    public partial class PersonalNameEditDlg: CommonDialog<IPersonalNameEditDlg, PersonalNameEditDlgController>, IPersonalNameEditDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private TextBox txtMarriedSurname;
        private Label lblMarriedSurname;
        private ComboBox cmbNameType;
        private Label lblType;
        private TextBox txtNickname;
        private TextBox txtNameSuffix;
        private TextBox txtNamePrefix;
        private TextBox txtSurnamePrefix;
        private TextBox txtPatronymic;
        private TextBox txtName;
        private TextBox txtSurname;
        private Label lblNickname;
        private Label lblNameSuffix;
        private Label lblNamePrefix;
        private Label lblSurnamePrefix;
        private Button btnCancel;
        private Label lblPatronymic;
        private Label lblName;
        private Button btnAccept;
        private Label lblSurname;
        private ComboBox cmbLanguage;
        private Label lblLanguage;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        public GDMIndividualRecord IndividualRecord
        {
            get { return fController.IndividualRecord; }
            set { fController.IndividualRecord = value; }
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
            XamlReader.Load(this);

            fController = new PersonalNameEditDlgController(this);
            fController.Init(baseWin);
        }

        private void edName_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.Key == Keys.Down && e.Control) {
                UIHelper.ProcessName(sender);
            } else if (e.KeyChar == '/') {
                e.Handled = true;
            }
        }

        private void txtXName_Leave(object sender, EventArgs e)
        {
            UIHelper.ProcessName(sender);
        }

        private void cmbLanguage_SelectedIndexChanged(object sender, EventArgs e)
        {
            fController.UpdateLanguage();
        }
    }
}
