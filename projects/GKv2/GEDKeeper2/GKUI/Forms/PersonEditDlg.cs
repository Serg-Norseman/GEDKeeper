/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using GKCore;
using GKCore.Controllers;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Types;
using GKUI.Components;
using GKUI.Platform.Handlers;

namespace GKUI.Forms
{
    public partial class PersonEditDlg : CommonDialog<IStdPersonEditDlg, StdPersonEditDlgController>, IStdPersonEditDlg
    {
        private readonly GKSheetList fEventsList;
        private readonly GKSheetList fSpousesList;
        private readonly GKSheetList fAssociationsList;
        private readonly GKSheetList fGroupsList;
        private readonly GKSheetList fNotesList;
        private readonly GKSheetList fMediaList;
        private readonly GKSheetList fSourcesList;
        private readonly GKSheetList fUserRefList;
        private readonly GKSheetList fNamesList;
        private readonly GKSheetList fParentsList;
        private readonly GKSheetList fChildrenList;

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

        ITextBox IStdPersonEditDlg.NamePrefix
        {
            get { return GetControlHandler<ITextBox>(txtNamePrefix); }
        }

        ITextBox IPersonEditDlg.Nickname
        {
            get { return GetControlHandler<ITextBox>(txtNickname); }
        }

        ITextBox IStdPersonEditDlg.SurnamePrefix
        {
            get { return GetControlHandler<ITextBox>(txtSurnamePrefix); }
        }

        ITextBox IStdPersonEditDlg.NameSuffix
        {
            get { return GetControlHandler<ITextBox>(txtNameSuffix); }
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

        public PersonEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            tabsData.SelectedIndexChanged += tabControl_SelectedIndexChanged;

            txtMarriedSurname.TextChanged += Names_TextChanged;
            txtSurname.TextChanged += Names_TextChanged;
            txtName.TextChanged += Names_TextChanged;
            cmbPatronymic.TextChanged += Names_TextChanged;

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");
            btnPortraitAdd.Image = UIHelper.LoadResourceImage("Resources.btn_rec_new.gif");
            btnPortraitDelete.Image = UIHelper.LoadResourceImage("Resources.btn_rec_delete.gif");
            btnParentsAdd.Image = UIHelper.LoadResourceImage("Resources.btn_rec_new.gif");
            btnParentsEdit.Image = UIHelper.LoadResourceImage("Resources.btn_rec_edit.gif");
            btnParentsDelete.Image = UIHelper.LoadResourceImage("Resources.btn_rec_delete.gif");
            btnFatherAdd.Image = UIHelper.LoadResourceImage("Resources.btn_rec_new.gif");
            btnFatherDelete.Image = UIHelper.LoadResourceImage("Resources.btn_rec_delete.gif");
            btnFatherSel.Image = UIHelper.LoadResourceImage("Resources.btn_jump.gif");
            btnMotherAdd.Image = UIHelper.LoadResourceImage("Resources.btn_rec_new.gif");
            btnMotherDelete.Image = UIHelper.LoadResourceImage("Resources.btn_rec_delete.gif");
            btnMotherSel.Image = UIHelper.LoadResourceImage("Resources.btn_jump.gif");
            btnNameCopy.Image = UIHelper.LoadResourceImage("Resources.btn_copy.gif");

            fEventsList = new GKSheetList(pageEvents);
            fEventsList.SetControlName("fEventsList"); // for purpose of tests

            fSpousesList = new GKSheetList(pageSpouses);
            fSpousesList.SetControlName("fSpousesList"); // for purpose of tests
            fSpousesList.OnModify += ModifySpousesSheet;
            fSpousesList.OnBeforeChange += BeforeChangeSpousesSheet;

            fNamesList = new GKSheetList(pageNames);
            fNamesList.OnModify += ModifyNamesSheet;
            fNamesList.SetControlName("fNamesList"); // for purpose of tests

            fAssociationsList = new GKSheetList(pageAssociations);
            fAssociationsList.OnModify += ModifyAssociationsSheet;
            fAssociationsList.SetControlName("fAssociationsList"); // for purpose of tests

            fNotesList = new GKSheetList(pageNotes);
            fNotesList.SetControlName("fNotesList"); // for purpose of tests

            fMediaList = new GKSheetList(pageMultimedia);
            fMediaList.SetControlName("fMediaList"); // for purpose of tests

            fSourcesList = new GKSheetList(pageSources);
            fSourcesList.SetControlName("fSourcesList"); // for purpose of tests

            fGroupsList = new GKSheetList(pageGroups);
            fGroupsList.SetControlName("fGroupsList"); // for purpose of tests
            fGroupsList.OnModify += ModifyGroupsSheet;

            fUserRefList = new GKSheetList(pageUserRefs);
            fUserRefList.SetControlName("fUserRefList"); // for purpose of tests

            fParentsList = new GKSheetList(pageParents);
            fParentsList.SetControlName("fParentsList"); // for purpose of tests
            fParentsList.OnModify += ModifyParentsSheet;

            fChildrenList = new GKSheetList(pageChilds);
            fChildrenList.SetControlName("fChildsList"); // for purpose of tests
            fChildrenList.OnItemValidating += PersonEditDlg_ItemValidating;
            fChildrenList.OnModify += ModifyChildrenSheet;

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

        public void SetPortraitAvl(bool avail, bool locked)
        {
            btnPortraitAdd.Enabled = !avail && !locked;
            btnPortraitDelete.Enabled = avail && !locked;
        }

        private void cbRestriction_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (cmbRestriction.Focused) {
                fController.AcceptTempData();

                fController.UpdateControls();
            }
        }

        private void ModifyNamesSheet(object sender, ModifyEventArgs eArgs)
        {
            if (eArgs.Action == RecordAction.raMoveUp || eArgs.Action == RecordAction.raMoveDown || eArgs.Action == RecordAction.raEdit) {
                fController.UpdateNameControls(fController.IndividualRecord.PersonalNames[0]);
            }
        }

        private void ModifyAssociationsSheet(object sender, ModifyEventArgs eArgs)
        {
            GDMAssociation ast = eArgs.ItemData as GDMAssociation;
            if (eArgs.Action == RecordAction.raJump && ast != null) {
                fController.JumpToRecord(ast);
            }
        }

        private void BeforeChangeSpousesSheet(object sender, ModifyEventArgs eArgs)
        {
            if (eArgs.Action == RecordAction.raAdd || eArgs.Action == RecordAction.raEdit) {
                fController.AcceptTempData();
            }
        }

        private void ModifySpousesSheet(object sender, ModifyEventArgs eArgs)
        {
            GDMFamilyRecord family = eArgs.ItemData as GDMFamilyRecord;
            if (eArgs.Action == RecordAction.raJump && family != null) {
                fController.JumpToPersonSpouse(family);
            }
        }

        private void ModifyParentsSheet(object sender, ModifyEventArgs eArgs)
        {
            fController.UpdateParents();
        }

        private void ModifyGroupsSheet(object sender, ModifyEventArgs eArgs)
        {
            if (eArgs.Action == RecordAction.raJump) {
                fController.JumpToRecord(eArgs.ItemData as GDMGroupRecord);
            }
        }

        private void PersonEditDlg_ItemValidating(object sender, ItemValidatingEventArgs e)
        {
            var record = e.Item as GDMRecord;
            e.IsAvailable = record == null || fController.Base.Context.IsAvailableRecord(record);
        }

        private void ModifyChildrenSheet(object sender, ModifyEventArgs eArgs)
        {
            if (eArgs.Action == RecordAction.raJump) {
                fController.JumpToRecord(eArgs.ItemData as GDMIndividualRecord);
            }
        }

        private void Names_TextChanged(object sender, EventArgs e)
        {
            Title = string.Format("{0} \"{1} {2} {3}\" [{4}]", LangMan.LS(LSID.Person), txtSurname.Text, txtName.Text,
                                  cmbPatronymic.Text, fController.IndividualRecord.GetXRefNum());
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
