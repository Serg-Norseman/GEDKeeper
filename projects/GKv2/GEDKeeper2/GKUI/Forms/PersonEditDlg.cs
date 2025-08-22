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
using System.Drawing;
using System.Windows.Forms;
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using GKCore.Design.Views;
using GKCore.Lists;
using GKCore.Locales;
using GKUI.Components;
using GKUI.Platform.Handlers;

namespace GKUI.Forms
{
    public partial class PersonEditDlg : CommonDialog<IStdPersonEditDlg, StdPersonEditDlgController>, IStdPersonEditDlg
    {
        #region Design components

        private GKSheetList fEventsList;
        private GKSheetList fSpousesList;
        private GKSheetList fAssociationsList;
        private GKSheetList fGroupsList;
        private GKSheetList fNotesList;
        private GKSheetList fMediaList;
        private GKSheetList fSourcesList;
        private GKSheetList fUserRefList;
        private GKSheetList fNamesList;
        private GKSheetList fParentsList;
        private GKSheetList fChildrenList;
        private GKSheetList fDNATestsList;

        #endregion

        public GDMIndividualRecord IndividualRecord
        {
            get { return fController.IndividualRecord; }
            set { fController.IndividualRecord = value; }
        }

        public GDMIndividualRecord Target
        {
            get { return fController.Target; }
            set { fController.Target = value; }
        }

        public TargetMode TargetMode
        {
            get { return fController.TargetMode; }
            set { fController.TargetMode = value; }
        }


        #region View Interface

        ISheetList IPersonEditDlg.EventsList
        {
            get { return fEventsList; }
        }

        ISheetList IPersonEditDlg.SpousesList
        {
            get { return fSpousesList; }
        }

        ISheetList IPersonEditDlg.AssociationsList
        {
            get { return fAssociationsList; }
        }

        ISheetList IPersonEditDlg.GroupsList
        {
            get { return fGroupsList; }
        }

        ISheetList IPersonEditDlg.UserRefList
        {
            get { return fUserRefList; }
        }

        ISheetList IPersonEditDlg.NamesList
        {
            get { return fNamesList; }
        }

        ISheetList IPersonEditDlg.NotesList
        {
            get { return fNotesList; }
        }

        ISheetList IPersonEditDlg.MediaList
        {
            get { return fMediaList; }
        }

        ISheetList IPersonEditDlg.SourcesList
        {
            get { return fSourcesList; }
        }

        ISheetList IPersonEditDlg.ParentsList
        {
            get { return fParentsList; }
        }

        ISheetList IStdPersonEditDlg.ChildrenList
        {
            get { return fChildrenList; }
        }

        ISheetList IStdPersonEditDlg.DNATestsList
        {
            get { return fDNATestsList; }
        }

        IPortraitControl IPersonEditDlg.Portrait
        {
            get { return imgPortrait; }
        }

        ITextBox IPersonEditDlg.Father
        {
            get { return GetControlHandler<ITextBox>(txtFather); }
        }

        ITextBox IPersonEditDlg.Mother
        {
            get { return GetControlHandler<ITextBox>(txtMother); }
        }

        ILabel IPersonEditDlg.SurnameLabel
        {
            get { return GetControlHandler<ILabel>(lblSurname); }
        }

        ITextBox IPersonEditDlg.Surname
        {
            get { return GetControlHandler<ITextBox>(txtSurname); }
        }

        ITextBox IPersonEditDlg.Name
        {
            get { return GetControlHandler<ITextBox>(txtName); }
        }

        IComboBox IPersonEditDlg.Patronymic
        {
            get { return GetControlHandler<IComboBox>(cmbPatronymic); }
        }

        ITextBox IPersonEditDlg.Nickname
        {
            get { return GetControlHandler<ITextBox>(txtNickname); }
        }

        ITextBox IPersonEditDlg.MarriedSurname
        {
            get { return GetControlHandler<ITextBox>(txtMarriedSurname); }
        }

        IComboBox IPersonEditDlg.RestrictionCombo
        {
            get { return GetControlHandler<IComboBox>(cmbRestriction); }
        }

        IComboBox IPersonEditDlg.SexCombo
        {
            get { return GetControlHandler<IComboBox>(cmbSex); }
        }

        ICheckBox IPersonEditDlg.Patriarch
        {
            get { return GetControlHandler<ICheckBox>(chkPatriarch); }
        }

        ICheckBox IPersonEditDlg.Bookmark
        {
            get { return GetControlHandler<ICheckBox>(chkBookmark); }
        }

        #endregion

        public PersonEditDlg()
        {
            InitializeComponent();
        }

        public PersonEditDlg(IBaseWindow baseWin) : this()
        {
            tabsData.SelectedIndexChanged += tabControl_SelectedIndexChanged;

            txtMarriedSurname.TextChanged += Names_TextChanged;
            txtSurname.TextChanged += Names_TextChanged;
            txtName.TextChanged += Names_TextChanged;
            cmbPatronymic.TextChanged += Names_TextChanged;

            fEventsList = new GKSheetList(pageEvents);
            fSpousesList = new GKSheetList(pageSpouses);
            fNamesList = new GKSheetList(pageNames);
            fAssociationsList = new GKSheetList(pageAssociations);
            fNotesList = new GKSheetList(pageNotes);
            fMediaList = new GKSheetList(pageMultimedia);
            fSourcesList = new GKSheetList(pageSources);
            fGroupsList = new GKSheetList(pageGroups);
            fUserRefList = new GKSheetList(pageUserRefs);
            fParentsList = new GKSheetList(pageParents);
            fChildrenList = new GKSheetList(pageChilds);
            fDNATestsList = new GKSheetList(pageDNATests);

            imgPortrait.AddButton(btnPortraitAdd);
            imgPortrait.AddButton(btnPortraitDelete);

            fController = new StdPersonEditDlgController(this);
            fController.Init(baseWin);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fController.Dispose();
            }
            base.Dispose(disposing);
        }

        private void cbSex_SelectedIndexChanged(object sender, EventArgs e)
        {
            fController.ChangeSex();
        }

        public void SetParentsAvl(bool avail, bool locked)
        {
            btnParentsAdd.Enabled = !avail && !locked;
            btnParentsEdit.Enabled = avail && !locked;
            btnParentsDelete.Enabled = avail && !locked;
        }

        public void SetFatherAvl(bool avail, bool locked)
        {
            btnFatherAdd.Enabled = !avail && !locked;
            btnFatherDelete.Enabled = avail && !locked;
            btnFatherSel.Enabled = avail && !locked;
        }

        public void SetMotherAvl(bool avail, bool locked)
        {
            btnMotherAdd.Enabled = !avail && !locked;
            btnMotherDelete.Enabled = avail && !locked;
            btnMotherSel.Enabled = avail && !locked;
        }

        public void SetPortrait(IImage portrait)
        {
            Image img = (portrait == null) ? null : ((ImageHandler)portrait).Handle;
            imgPortrait.Image = img;
        }

        private void cbRestriction_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (cmbRestriction.Focused) {
                fController.AcceptTempData();

                fController.UpdateControls();
            }
        }

        private void Names_TextChanged(object sender, EventArgs e)
        {
            SetTitle(string.Format("{0} \"{1} {2} {3}\" [{4}]", LangMan.LS(LSID.Person), txtSurname.Text, txtName.Text,
                                  cmbPatronymic.Text, fController.IndividualRecord.GetXRefNum()));
        }

        private void btnFatherAdd_Click(object sender, EventArgs e)
        {
            fController.AddFather();
        }

        private void btnFatherDelete_Click(object sender, EventArgs e)
        {
            fController.DeleteFather();
        }

        private void btnFatherSel_Click(object sender, EventArgs e)
        {
            fController.JumpToFather();
        }

        private void btnMotherAdd_Click(object sender, EventArgs e)
        {
            fController.AddMother();
        }

        private void btnMotherDelete_Click(object sender, EventArgs e)
        {
            fController.DeleteMother();
        }

        private void btnMotherSel_Click(object sender, EventArgs e)
        {
            fController.JumpToMother();
        }

        private void btnParentsAdd_Click(object sender, EventArgs e)
        {
            fController.AddParents();
        }

        private void btnParentsEdit_Click(object sender, EventArgs e)
        {
            fController.EditParents();
        }

        private void btnParentsDelete_Click(object sender, EventArgs e)
        {
            fController.DeleteParents();
        }

        private void btnNameCopy_Click(object sender, EventArgs e)
        {
            fController.CopyPersonName();
        }

        private void btnPortraitAdd_Click(object sender, EventArgs e)
        {
            fController.AddPortrait();
        }

        private void btnPortraitDelete_Click(object sender, EventArgs e)
        {
            fController.DeletePortrait();
        }

        private void txtXName_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Down && e.Control) {
                UIHelper.ProcessName(sender);
            }
        }

        private void txtXName_Leave(object sender, EventArgs e)
        {
            UIHelper.ProcessName(sender);
        }

        private void edSurname_KeyPress(object sender, KeyPressEventArgs e)
        {
            if (e.KeyChar == '/') {
                e.Handled = true;
            }
        }

        public void SetNeedSex(GDMSex needSex)
        {
            cmbSex.SelectedIndex = (int)needSex;
        }
    }
}
