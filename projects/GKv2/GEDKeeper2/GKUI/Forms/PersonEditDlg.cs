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
using System.Drawing;
using System.Windows.Forms;
using BSLib;
using GDModel;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.MVP.Controls;
using GKCore.MVP.Views;
using GKCore.Types;
using GKUI.Components;

namespace GKUI.Forms
{
    public partial class PersonEditDlg : EditorDialog, IPersonEditDlg
    {
        private readonly PersonEditDlgController fController;

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

        public GDMIndividualRecord Person
        {
            get { return fController.Person; }
            set { fController.Person = value; }
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

        IPortraitControl IPersonEditDlg.Portrait
        {
            get { return imgPortrait; }
        }

        ITextBoxHandler IPersonEditDlg.Father
        {
            get { return GetControlHandler<ITextBoxHandler>(txtFather); }
        }

        ITextBoxHandler IPersonEditDlg.Mother
        {
            get { return GetControlHandler<ITextBoxHandler>(txtMother); }
        }

        ILabelHandler IPersonEditDlg.SurnameLabel
        {
            get { return GetControlHandler<ILabelHandler>(lblSurname); }
        }

        ITextBoxHandler IPersonEditDlg.Surname
        {
            get { return GetControlHandler<ITextBoxHandler>(txtSurname); }
        }

        ITextBoxHandler IPersonEditDlg.Name
        {
            get { return GetControlHandler<ITextBoxHandler>(txtName); }
        }

        IComboBoxHandler IPersonEditDlg.Patronymic
        {
            get { return GetControlHandler<IComboBoxHandler>(cmbPatronymic); }
        }

        ITextBoxHandler IPersonEditDlg.NamePrefix
        {
            get { return GetControlHandler<ITextBoxHandler>(txtNamePrefix); }
        }

        ITextBoxHandler IPersonEditDlg.Nickname
        {
            get { return GetControlHandler<ITextBoxHandler>(txtNickname); }
        }

        ITextBoxHandler IPersonEditDlg.SurnamePrefix
        {
            get { return GetControlHandler<ITextBoxHandler>(txtSurnamePrefix); }
        }

        ITextBoxHandler IPersonEditDlg.NameSuffix
        {
            get { return GetControlHandler<ITextBoxHandler>(txtNameSuffix); }
        }

        ITextBoxHandler IPersonEditDlg.MarriedSurname
        {
            get { return GetControlHandler<ITextBoxHandler>(txtMarriedSurname); }
        }

        IComboBoxHandler IPersonEditDlg.RestrictionCombo
        {
            get { return GetControlHandler<IComboBoxHandler>(cmbRestriction); }
        }

        IComboBoxHandler IPersonEditDlg.SexCombo
        {
            get { return GetControlHandler<IComboBoxHandler>(cmbSex); }
        }

        ICheckBoxHandler IPersonEditDlg.Patriarch
        {
            get { return GetControlHandler<ICheckBoxHandler>(chkPatriarch); }
        }

        ICheckBoxHandler IPersonEditDlg.Bookmark
        {
            get { return GetControlHandler<ICheckBoxHandler>(chkBookmark); }
        }

        #endregion

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
            fController.UpdateControls();
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
                Logger.LogException(ex);
            }
        }

        private void ModifyNamesSheet(object sender, ModifyEventArgs eArgs)
        {
            if (eArgs.Action == RecordAction.raMoveUp || eArgs.Action == RecordAction.raMoveDown) {
                fController.UpdateNameControls(fController.Person.PersonalNames[0]);
            }
        }

        private void ModifyAssociationsSheet(object sender, ModifyEventArgs eArgs)
        {
            GDMAssociation ast = eArgs.ItemData as GDMAssociation;
            if (eArgs.Action == RecordAction.raJump && ast != null) {
                fController.JumpToRecord(ast.Individual);
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
                GDMIndividualRecord spouse = null;
                switch (fController.Person.Sex) {
                    case GDMSex.svMale:
                        spouse = family.Wife.Individual;
                        break;

                    case GDMSex.svFemale:
                        spouse = family.Husband.Individual;
                        break;
                }

                fController.JumpToRecord(spouse);
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

        private void Names_TextChanged(object sender, EventArgs e)
        {
            Text = string.Format("{0} \"{1} {2} {3}\" [{4}]", LangMan.LS(LSID.LSID_Person), txtSurname.Text, txtName.Text,
                                 cmbPatronymic.Text, fController.Person.GetXRefNum());
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
            UIHelper.SetClipboardText(fController.Person.GetNameString(true, false));
        }

        private void btnPortraitAdd_Click(object sender, EventArgs e)
        {
            fController.AddPortrait();
        }

        private void btnPortraitDelete_Click(object sender, EventArgs e)
        {
            fController.DeletePortrait();
        }

        private void edSurname_KeyDown(object sender, KeyEventArgs e)
        {
            TextBox tb = (sender as TextBox);
            if (tb != null && e.KeyCode == Keys.Down && e.Control) {
                tb.Text = ConvertHelper.UniformName(tb.Text);
            }
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

        public PersonEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

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

            fGroupsList = new GKSheetList(pageGroups);
            fGroupsList.SetControlName("fGroupsList"); // for purpose of tests
            fGroupsList.OnModify += ModifyGroupsSheet;

            fNotesList = new GKSheetList(pageNotes);
            fNotesList.SetControlName("fNotesList"); // for purpose of tests

            fMediaList = new GKSheetList(pageMultimedia);
            fMediaList.SetControlName("fMediaList"); // for purpose of tests

            fSourcesList = new GKSheetList(pageSources);
            fSourcesList.SetControlName("fSourcesList"); // for purpose of tests

            fUserRefList = new GKSheetList(pageUserRefs);
            fUserRefList.SetControlName("fUserRefList"); // for purpose of tests

            fParentsList = new GKSheetList(pageParents);
            fParentsList.SetControlName("fParentsList"); // for purpose of tests
            fParentsList.OnModify += ModifyParentsSheet;

            imgPortrait.AddButton(btnPortraitAdd);
            imgPortrait.AddButton(btnPortraitDelete);
            imgPortrait.SizeMode = PictureBoxSizeMode.CenterImage;

            SetLang();

            fController = new PersonEditDlgController(this);
            fController.Init(baseWin);

            fEventsList.ListModel = new EventsListModel(baseWin, fController.LocalUndoman, true);
            fNotesList.ListModel = new NoteLinksListModel(baseWin, fController.LocalUndoman);
            fMediaList.ListModel = new MediaLinksListModel(baseWin, fController.LocalUndoman);
            fSourcesList.ListModel = new SourceCitationsListModel(baseWin, fController.LocalUndoman);
            fAssociationsList.ListModel = new AssociationsListModel(baseWin, fController.LocalUndoman);

            fGroupsList.ListModel = new GroupsSublistModel(baseWin, fController.LocalUndoman);
            fNamesList.ListModel = new NamesSublistModel(baseWin, fController.LocalUndoman);
            fSpousesList.ListModel = new SpousesSublistModel(baseWin, fController.LocalUndoman);
            fUserRefList.ListModel = new URefsSublistModel(baseWin, fController.LocalUndoman);
            fParentsList.ListModel = new ParentsSublistModel(baseWin, fController.LocalUndoman);
        }

        public void SetLang()
        {
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            Text = LangMan.LS(LSID.LSID_WinPersonEdit);
            lblSurname.Text = LangMan.LS(LSID.LSID_Surname);
            lblMarriedSurname.Text = LangMan.LS(LSID.LSID_MarriedSurname);
            lblName.Text = LangMan.LS(LSID.LSID_Name);
            lblPatronymic.Text = LangMan.LS(LSID.LSID_Patronymic);
            lblSex.Text = LangMan.LS(LSID.LSID_Sex);
            lblNickname.Text = LangMan.LS(LSID.LSID_Nickname);
            lblSurnamePrefix.Text = LangMan.LS(LSID.LSID_SurnamePrefix);
            lblNamePrefix.Text = LangMan.LS(LSID.LSID_NamePrefix);
            lblNameSuffix.Text = LangMan.LS(LSID.LSID_NameSuffix);
            chkPatriarch.Text = LangMan.LS(LSID.LSID_Patriarch);
            chkBookmark.Text = LangMan.LS(LSID.LSID_Bookmark);
            lblParents.Text = LangMan.LS(LSID.LSID_Parents);
            pageEvents.Text = LangMan.LS(LSID.LSID_Events);
            pageSpouses.Text = LangMan.LS(LSID.LSID_Spouses);
            pageAssociations.Text = LangMan.LS(LSID.LSID_Associations);
            pageGroups.Text = LangMan.LS(LSID.LSID_RPGroups);
            pageNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
            pageMultimedia.Text = LangMan.LS(LSID.LSID_RPMultimedia);
            pageSources.Text = LangMan.LS(LSID.LSID_RPSources);
            pageUserRefs.Text = LangMan.LS(LSID.LSID_UserRefs);
            lblRestriction.Text = LangMan.LS(LSID.LSID_Restriction);
            pageNames.Text = LangMan.LS(LSID.LSID_Names);
            pageParents.Text = LangMan.LS(LSID.LSID_Parents);

            SetToolTip(btnPortraitAdd, LangMan.LS(LSID.LSID_PortraitAddTip));
            SetToolTip(btnPortraitDelete, LangMan.LS(LSID.LSID_PortraitDeleteTip));
            SetToolTip(btnParentsAdd, LangMan.LS(LSID.LSID_ParentsAddTip));
            SetToolTip(btnParentsEdit, LangMan.LS(LSID.LSID_ParentsEditTip));
            SetToolTip(btnParentsDelete, LangMan.LS(LSID.LSID_ParentsDeleteTip));
            SetToolTip(btnFatherAdd, LangMan.LS(LSID.LSID_FatherAddTip));
            SetToolTip(btnFatherDelete, LangMan.LS(LSID.LSID_FatherDeleteTip));
            SetToolTip(btnFatherSel, LangMan.LS(LSID.LSID_FatherSelTip));
            SetToolTip(btnMotherAdd, LangMan.LS(LSID.LSID_MotherAddTip));
            SetToolTip(btnMotherDelete, LangMan.LS(LSID.LSID_MotherDeleteTip));
            SetToolTip(btnMotherSel, LangMan.LS(LSID.LSID_MotherSelTip));
            SetToolTip(btnNameCopy, LangMan.LS(LSID.LSID_NameCopyTip));
        }
    }
}
