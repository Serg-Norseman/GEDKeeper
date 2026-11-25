/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Views;
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

        public PersonalNameEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            txtSurname.Behaviors.Add(new NameValidationBehavior());
            txtMarriedSurname.Behaviors.Add(new NameValidationBehavior());
            txtName.Behaviors.Add(new NameValidationBehavior());
            txtPatronymic.Behaviors.Add(new NameValidationBehavior());

            fController = new PersonalNameEditDlgController(this);
            fController.InitView(baseWin);
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
