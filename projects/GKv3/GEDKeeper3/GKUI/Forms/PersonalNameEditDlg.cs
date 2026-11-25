/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
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

        public PersonalNameEditDlg(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            fController = new PersonalNameEditDlgController(this);
            fController.InitView(baseWin);
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
