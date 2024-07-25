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
using Eto.Forms;
using Eto.Serialization.Xaml;
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
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private TabControl tabsData;
        private TabPage pageEvents;
        private TabPage pageNotes;
        private TabPage pageMultimedia;
        private TabPage pageSources;
        private TabPage pageChilds;
        private Button btnAccept;
        private Button btnCancel;
        private GroupBox GroupBox1;
        private Label lblHusband;
        private TextBox txtHusband;
        private Button btnHusbandAdd;
        private Button btnHusbandDelete;
        private Button btnHusbandSel;
        private Button btnWifeSel;
        private Button btnWifeDelete;
        private Button btnWifeAdd;
        private TextBox txtWife;
        private Label lblWife;
        private Label lblStatus;
        private ComboBox cmbMarriageStatus;
        private Label lblRestriction;
        private ComboBox cmbRestriction;
        private GKSheetList fChildrenList;
        private GKSheetList fEventsList;
        private GKSheetList fNotesList;
        private GKSheetList fMediaList;
        private GKSheetList fSourcesList;
        private GKSheetList fUserRefList;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

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
            XamlReader.Load(this);

            tabsData.SelectedIndexChanged += tabControl_SelectedIndexChanged;

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
