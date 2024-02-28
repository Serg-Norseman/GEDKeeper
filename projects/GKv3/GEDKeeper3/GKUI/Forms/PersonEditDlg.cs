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
using Eto.Drawing;
using Eto.Forms;
using Eto.Serialization.Xaml;
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
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private TabControl tabsData;
        private TabPage pageEvents;
        private TabPage pageNotes;
        private TabPage pageMultimedia;
        private TabPage pageSources;
        private TabPage pageSpouses;
        private TabPage pageAssociations;
        private TabPage pageGroups;
        private Button btnAccept;
        private Button btnCancel;
        private Label lblRestriction;
        private ComboBox cmbRestriction;
        private GroupBox GroupBox1;
        private Label lblSurname;
        private Label lblName;
        private Label lblPatronymic;
        private Label lblSex;
        private TextBox txtSurname;
        private TextBox txtName;
        private ComboBox cmbPatronymic;
        public DropDown cmbSex;
        private CheckBox chkPatriarch;
        private TabPage pageUserRefs;
        private Panel panCtlParents;
        private Label lblParents;
        private TextBox txtFather;
        private TextBox txtMother;
        private Button btnParentsAdd;
        private Button btnParentsEdit;
        private Button btnParentsDelete;
        private CheckBox chkBookmark;
        private Label lblSurnamePrefix;
        private TextBox txtSurnamePrefix;
        private Label lblNamePrefix;
        private TextBox txtNamePrefix;
        private Label lblNameSuffix;
        private TextBox txtNameSuffix;
        private Label lblNickname;
        private TextBox txtNickname;
        private GKPortrait imgPortrait;
        private Button btnNameCopy;
        private Button btnPortraitAdd;
        private Button btnPortraitDelete;
        private Button btnFatherAdd;
        private Button btnFatherDelete;
        private Button btnFatherSel;
        private Button btnMotherAdd;
        private Button btnMotherDelete;
        private Button btnMotherSel;
        private TabPage pageNames;
        private TextBox txtMarriedSurname;
        private Label lblMarriedSurname;
        private TabPage pageParents;
        private TabPage pageChilds;
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

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
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
            XamlReader.Load(this);

            tabsData.SelectedIndexChanged += tabControl_SelectedIndexChanged;

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
            if (cmbRestriction.HasFocus) {
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

        public void SetNeedSex(GDMSex needSex)
        {
            cmbSex.SelectedIndex = (int)needSex;
        }
    }
}
