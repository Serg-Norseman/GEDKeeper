/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
