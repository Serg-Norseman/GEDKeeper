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

using Eto.Forms;
using Eto.Serialization.Xaml;
using GKCore.Controllers;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Names;

namespace GKUI.Forms
{
    public sealed partial class NameEditDlg : CommonDialog<INameEditDlg, NameEditDlgController>, INameEditDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Label lblName;
        private TextBox txtName;
        private Label lblSex;
        private ComboBox cmbSex;
        private Button btnAccept;
        private Button btnCancel;
        private GroupBox grpPatronymics;
        private Label lblFemale;
        private TextBox txtFPatr;
        private Label lblMale;
        private TextBox txtMPatr;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

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

        public NameEditDlg()
        {
            XamlReader.Load(this);

            fController = new NameEditDlgController(this);
        }

        private void edName_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyChar == '/') {
                e.Handled = true;
            }
        }
    }
}
