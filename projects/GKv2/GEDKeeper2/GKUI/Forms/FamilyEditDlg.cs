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
using GDModel;
using GKCore;
using GKCore.Controllers;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Types;
using GKUI.Components;

namespace GKUI.Forms
{
    public partial class FamilyEditDlg : CommonDialog<IFamilyEditDlg, FamilyEditDlgController>, IFamilyEditDlg
    {
        private readonly GKSheetList fChildrenList;
        private readonly GKSheetList fEventsList;
        private readonly GKSheetList fNotesList;
        private readonly GKSheetList fMediaList;
        private readonly GKSheetList fSourcesList;
        private readonly GKSheetList fUserRefList;

        public GDMFamilyRecord FamilyRecord
        {
            get { return fController.FamilyRecord; }
            set { fController.FamilyRecord = value; }
        }

        #region View Interface

        ISheetList IFamilyEditDlg.NotesList
        {
            get { return fNotesList; }
        }

        ISheetList IFamilyEditDlg.MediaList
        {
            get { return fMediaList; }
        }

        ISheetList IFamilyEditDlg.SourcesList
        {
            get { return fSourcesList; }
        }

        ISheetList IFamilyEditDlg.ChildrenList
        {
            get { return fChildrenList; }
        }

        ISheetList IFamilyEditDlg.EventsList
        {
            get { return fEventsList; }
        }

        ISheetList IFamilyEditDlg.UserRefList
        {
            get { return fUserRefList; }
        }

        IComboBox IFamilyEditDlg.MarriageStatus
        {
            get { return GetControlHandler<IComboBox>(cmbMarriageStatus); }
        }

        IComboBox IFamilyEditDlg.Restriction
        {
            get { return GetControlHandler<IComboBox>(cmbRestriction); }
        }

        ITextBox IFamilyEditDlg.Husband
        {
            get { return GetControlHandler<ITextBox>(txtHusband); }
        }

        ITextBox IFamilyEditDlg.Wife
        {
            get { return GetControlHandler<ITextBox>(txtWife); }
        }

        #endregion

        public FamilyEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            tabsData.SelectedIndexChanged += tabControl_SelectedIndexChanged;

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");
            btnHusbandAdd.Image = UIHelper.LoadResourceImage("Resources.btn_rec_new.gif");
            btnHusbandDelete.Image = UIHelper.LoadResourceImage("Resources.btn_rec_delete.gif");
            btnHusbandSel.Image = UIHelper.LoadResourceImage("Resources.btn_jump.gif");
            btnWifeAdd.Image = UIHelper.LoadResourceImage("Resources.btn_rec_new.gif");
            btnWifeDelete.Image = UIHelper.LoadResourceImage("Resources.btn_rec_delete.gif");
            btnWifeSel.Image = UIHelper.LoadResourceImage("Resources.btn_jump.gif");

            txtHusband.TextChanged += EditSpouse_TextChanged;
            txtWife.TextChanged += EditSpouse_TextChanged;

            fChildrenList = new GKSheetList(pageChilds);
            fChildrenList.SetControlName("fChildsList"); // for purpose of tests
            fChildrenList.OnItemValidating += FamilyEditDlg_ItemValidating;
            fChildrenList.OnModify += ModifyChildrenSheet;

            fEventsList = new GKSheetList(pageEvents);
            fEventsList.SetControlName("fEventsList"); // for purpose of tests

            fNotesList = new GKSheetList(pageNotes);
            fNotesList.SetControlName("fNotesList"); // for purpose of tests

            fMediaList = new GKSheetList(pageMultimedia);
            fMediaList.SetControlName("fMediaList"); // for purpose of tests

            fSourcesList = new GKSheetList(pageSources);
            fSourcesList.SetControlName("fSourcesList"); // for purpose of tests

            fUserRefList = new GKSheetList(pageUserRefs);
            fUserRefList.SetControlName("fUserRefList"); // for purpose of tests

            fController = new FamilyEditDlgController(this);
            fController.Init(baseWin);
        }

        public void LockEditor(bool locked)
        {
            btnHusbandAdd.Enabled = (btnHusbandAdd.Enabled && !locked);
            btnHusbandDelete.Enabled = (btnHusbandDelete.Enabled && !locked);
            btnWifeAdd.Enabled = (btnWifeAdd.Enabled && !locked);
            btnWifeDelete.Enabled = (btnWifeDelete.Enabled && !locked);

            cmbMarriageStatus.Enabled = (cmbMarriageStatus.Enabled && !locked);

            fChildrenList.ReadOnly = locked;
            fEventsList.ReadOnly = locked;
            fNotesList.ReadOnly = locked;
            fMediaList.ReadOnly = locked;
            fSourcesList.ReadOnly = locked;
            fUserRefList.ReadOnly = locked;
        }

        public void SetHusband(string value)
        {
            bool res = !string.IsNullOrEmpty(value);
            txtHusband.Text = (res) ? value : LangMan.LS(LSID.UnkMale);
            btnHusbandAdd.Enabled = (!res);
            btnHusbandDelete.Enabled = (res);
            btnHusbandSel.Enabled = (res);
        }

        public void SetWife(string value)
        {
            bool res = !string.IsNullOrEmpty(value);
            txtWife.Text = (res) ? value : LangMan.LS(LSID.UnkFemale);
            btnWifeAdd.Enabled = (!res);
            btnWifeDelete.Enabled = (res);
            btnWifeSel.Enabled = (res);
        }

        private void cbRestriction_SelectedIndexChanged(object sender, EventArgs e)
        {
            LockEditor(cmbRestriction.SelectedIndex == (int)GDMRestriction.rnLocked);
        }

        private void ModifyChildrenSheet(object sender, ModifyEventArgs eArgs)
        {
            if (eArgs.Action == RecordAction.raJump) {
                fController.JumpToRecord(eArgs.ItemData as GDMIndividualRecord);
            }
        }

        public void SetTarget(TargetMode targetType, GDMIndividualRecord target)
        {
            fController.SetTarget(targetType, target);
        }

        private void btnHusbandAddClick(object sender, EventArgs e)
        {
            fController.AddHusband();
        }

        private void btnHusbandDeleteClick(object sender, EventArgs e)
        {
            fController.DeleteHusband();
        }

        private void btnHusbandSelClick(object sender, EventArgs e)
        {
            fController.JumpToHusband();
        }

        private void btnWifeAddClick(object sender, EventArgs e)
        {
            fController.AddWife();
        }

        private void btnWifeDeleteClick(object sender, EventArgs e)
        {
            fController.DeleteWife();
        }

        private void btnWifeSelClick(object sender, EventArgs e)
        {
            fController.JumpToWife();
        }

        private void EditSpouse_TextChanged(object sender, EventArgs e)
        {
            Title = string.Format("{0} \"{1} - {2}\"", LangMan.LS(LSID.Family), txtHusband.Text, txtWife.Text);
        }

        private void FamilyEditDlg_ItemValidating(object sender, ItemValidatingEventArgs e)
        {
            var record = e.Item as GDMRecord;
            e.IsAvailable = record == null || fController.Base.Context.IsAvailableRecord(record);
        }
    }
}
