/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using GKUI.Platform;

namespace GKUI.Forms
{
    public partial class PersonEditDlg : CommonDialog<IMobPersonEditDlg, MobPersonEditDlgController>, IMobPersonEditDlg
    {
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

        public PersonEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            txtSurname.Behaviors.Add(new NameValidationBehavior());
            txtName.Behaviors.Add(new NameValidationBehavior());
            txtMarriedSurname.Behaviors.Add(new NameValidationBehavior());

            imgPortrait.AddButton(btnPortraitAdd);
            imgPortrait.AddButton(btnPortraitDelete);

            fController = new MobPersonEditDlgController(this);
            fController.Init(baseWin);
        }

        private void cbSex_SelectedIndexChanged(object sender, EventArgs e)
        {
            fController.ChangeSex();
        }

        public void SetParentsAvl(bool avail, bool locked)
        {
            btnParentsAdd.IsEnabled = !avail && !locked;
            btnParentsEdit.IsEnabled = avail && !locked;
            btnParentsDelete.IsEnabled = avail && !locked;
        }

        public void SetFatherAvl(bool avail, bool locked)
        {
            btnFatherAdd.IsEnabled = !avail && !locked;
            btnFatherDelete.IsEnabled = avail && !locked;
            btnFatherSel.IsEnabled = avail && !locked;
        }

        public void SetMotherAvl(bool avail, bool locked)
        {
            btnMotherAdd.IsEnabled = !avail && !locked;
            btnMotherDelete.IsEnabled = avail && !locked;
            btnMotherSel.IsEnabled = avail && !locked;
        }

        public void SetPortrait(IImage portrait)
        {
            var img = (portrait == null) ? null : ((SKImageHandler)portrait).Handle;
            imgPortrait.Image = img;
        }

        public void SetPortraitAvl(bool avail, bool locked)
        {
            btnPortraitAdd.IsEnabled = !avail && !locked;
            btnPortraitDelete.IsEnabled = avail && !locked;
        }

        private void cbRestriction_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (cmbRestriction.IsFocused) {
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
