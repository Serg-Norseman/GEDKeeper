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
using BSLib.Design.MVP.Controls;
using GDModel;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Views;
using GKUI.Components;

namespace GKUI.Forms
{
    public partial class ParentsEditDlg : CommonDialog<IParentsEditDlg, ParentsEditDlgController>, IParentsEditDlg
    {
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
            InitializeComponent();

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");
            btnParentsEdit.Image = UIHelper.LoadResourceImage("Resources.btn_rec_edit.gif");
            btnFatherAdd.Image = UIHelper.LoadResourceImage("Resources.btn_rec_new.gif");
            btnFatherDelete.Image = UIHelper.LoadResourceImage("Resources.btn_rec_delete.gif");
            btnMotherAdd.Image = UIHelper.LoadResourceImage("Resources.btn_rec_new.gif");
            btnMotherDelete.Image = UIHelper.LoadResourceImage("Resources.btn_rec_delete.gif");

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
