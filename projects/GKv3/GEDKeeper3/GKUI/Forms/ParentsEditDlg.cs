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
using GKCore.Design.Controls;
using GKCore.Design.Views;

namespace GKUI.Forms
{
    public partial class ParentsEditDlg : CommonDialog<IParentsEditDlg, ParentsEditDlgController>, IParentsEditDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Button btnAccept;
        private Button btnCancel;
        private GroupBox GroupBox1;
        private Label lblChildName;
        private TextBox txtChildName;
        private Label lblLinkageType;
        private ComboBox cmbLinkageType;
        private Panel panCtlParents;
        private Label lblParents;
        private TextBox txtFather;
        private TextBox txtMother;
        private Button btnParentsEdit;
        private Button btnFatherAdd;
        private Button btnFatherDelete;
        private Button btnMotherAdd;
        private Button btnMotherDelete;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        public GDMChildToFamilyLink ChildLink
        {
            get { return fController.ChildLink; }
            set { fController.ChildLink = value; }
        }

        public GDMIndividualRecord IndividualRecord
        {
            get { return fController.IndividualRecord; }
            set { fController.IndividualRecord = value; }
        }

        #region View Interface

        ITextBox IParentsEditDlg.Father
        {
            get { return GetControlHandler<ITextBox>(txtFather); }
        }

        ITextBox IParentsEditDlg.Mother
        {
            get { return GetControlHandler<ITextBox>(txtMother); }
        }

        ITextBox IParentsEditDlg.ChildName
        {
            get { return GetControlHandler<ITextBox>(txtChildName); }
        }

        IComboBox IParentsEditDlg.LinkageTypeCombo
        {
            get { return GetControlHandler<IComboBox>(cmbLinkageType); }
        }

        #endregion

        public ParentsEditDlg(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            fController = new ParentsEditDlgController(this);
            fController.Init(baseWin);
        }

        private void btnFatherAdd_Click(object sender, EventArgs e)
        {
            fController.AddFather();
        }

        private void btnFatherDelete_Click(object sender, EventArgs e)
        {
            fController.DeleteFather();
        }

        private void btnMotherAdd_Click(object sender, EventArgs e)
        {
            fController.AddMother();
        }

        private void btnMotherDelete_Click(object sender, EventArgs e)
        {
            fController.DeleteMother();
        }

        private void btnParentsEdit_Click(object sender, EventArgs e)
        {
            fController.EditParents();
        }
    }
}
