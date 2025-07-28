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
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKUI.Components;
using GKUI.Platform;

namespace GKUI.Forms
{
    public partial class PersonalNameEditDlg: CommonDialog<IPersonalNameEditDlg, PersonalNameEditDlgController>, IPersonalNameEditDlg
    {
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
            InitializeComponent();

            txtSurname.Behaviors.Add(new NameValidationBehavior());
            txtMarriedSurname.Behaviors.Add(new NameValidationBehavior());
            txtName.Behaviors.Add(new NameValidationBehavior());
            txtPatronymic.Behaviors.Add(new NameValidationBehavior());

            fController = new PersonalNameEditDlgController(this);
            fController.Init(baseWin);
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
