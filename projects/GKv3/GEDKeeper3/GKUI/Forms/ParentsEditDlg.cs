/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using System.ComponentModel;
using BSLib.Design.MVP.Controls;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GDModel;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Views;

namespace GKUI.Forms
{
    public partial class ParentsEditDlg : EditorDialog, IParentsEditDlg
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

        private readonly ParentsEditDlgController fController;

        public GDMChildToFamilyLink Link
        {
            get { return fController.Link; }
            set { fController.Link = value; }
        }

        public GDMIndividualRecord Person
        {
            get { return fController.Person; }
            set { fController.Person = value; }
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

        public void SetParentsAvl(bool avail)
        {
            btnParentsEdit.Enabled = avail;
        }

        public void SetFatherAvl(bool avail)
        {
            btnFatherAdd.Enabled = !avail;
            btnFatherDelete.Enabled = avail;
        }

        public void SetMotherAvl(bool avail)
        {
            btnMotherAdd.Enabled = !avail;
            btnMotherDelete.Enabled = avail;
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            DialogResult = fController.Accept() ? DialogResult.Ok : DialogResult.None;
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            DialogResult = fController.Cancel() ? DialogResult.Cancel : DialogResult.None;
        }

        protected override void OnClosing(CancelEventArgs e)
        {
            base.OnClosing(e);
            e.Cancel = fController.CheckChangesPersistence();
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
