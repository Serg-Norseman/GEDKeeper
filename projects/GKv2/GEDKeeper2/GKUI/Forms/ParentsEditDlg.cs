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
using System.Windows.Forms;

using GKCommon.GEDCOM;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Controls;
using GKCore.MVP.Views;
using GKUI.Components;

namespace GKUI.Forms
{
    public partial class ParentsEditDlg : EditorDialog, IParentsEditDlg
    {
        private readonly ParentsEditDlgController fController;

        public GEDCOMChildToFamilyLink Link
        {
            get { return fController.Link; }
            set { fController.Link = value; }
        }

        public GEDCOMIndividualRecord Person
        {
            get { return fController.Person; }
            set { fController.Person = value; }
        }

        #region View Interface

        ITextBoxHandler IParentsEditDlg.Father
        {
            get { return GetControlHandler<ITextBoxHandler>(txtFather); }
        }

        ITextBoxHandler IParentsEditDlg.Mother
        {
            get { return GetControlHandler<ITextBoxHandler>(txtMother); }
        }

        ITextBoxHandler IParentsEditDlg.ChildName
        {
            get { return GetControlHandler<ITextBoxHandler>(txtChildName); }
        }

        IComboBoxHandler IParentsEditDlg.LinkageTypeCombo
        {
            get { return GetControlHandler<IComboBoxHandler>(cmbLinkageType); }
        }

        #endregion

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
            DialogResult = fController.Accept() ? DialogResult.OK : DialogResult.None;
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            try {
                fController.Cancel();
            } catch (Exception ex) {
                Logger.LogWrite("ParentsEditDlg.btnCancel_Click(): " + ex.Message);
            }
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

        public ParentsEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");
            btnParentsEdit.Image = UIHelper.LoadResourceImage("Resources.btn_rec_edit.gif");
            btnFatherAdd.Image = UIHelper.LoadResourceImage("Resources.btn_rec_new.gif");
            btnFatherDelete.Image = UIHelper.LoadResourceImage("Resources.btn_rec_edit.gif");
            btnMotherAdd.Image = UIHelper.LoadResourceImage("Resources.btn_rec_new.gif");
            btnMotherDelete.Image = UIHelper.LoadResourceImage("Resources.btn_rec_edit.gif");

            SetLang();

            fController = new ParentsEditDlgController(this);
            fController.Init(baseWin);
        }

        public void SetLang()
        {
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            Text = LangMan.LS(LSID.LSID_WinPersonEdit);
            lblChildName.Text = LangMan.LS(LSID.LSID_Name);
            lblParents.Text = LangMan.LS(LSID.LSID_Parents);
            lblLinkageType.Text = LangMan.LS(LSID.LSID_LinkageType);

            SetToolTip(btnParentsEdit, LangMan.LS(LSID.LSID_ParentsEditTip));
            SetToolTip(btnFatherAdd, LangMan.LS(LSID.LSID_FatherAddTip));
            SetToolTip(btnFatherDelete, LangMan.LS(LSID.LSID_FatherDeleteTip));
            SetToolTip(btnMotherAdd, LangMan.LS(LSID.LSID_MotherAddTip));
            SetToolTip(btnMotherDelete, LangMan.LS(LSID.LSID_MotherDeleteTip));
        }
    }
}
