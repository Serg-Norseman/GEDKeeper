﻿/*
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
using BSLib.Design.MVP.Controls;
using Eto.Forms;
using GDModel;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.MVP.Views;
using GKCore.Types;
using GKUI.Components;

namespace GKUI.Forms
{
    public partial class FamilyEditDlg : EditorDialog, IFamilyEditDlg
    {
        private readonly FamilyEditDlgController fController;

        private readonly GKSheetList fChildrenList;
        private readonly GKSheetList fEventsList;
        private readonly GKSheetList fNotesList;
        private readonly GKSheetList fMediaList;
        private readonly GKSheetList fSourcesList;

        public GDMFamilyRecord Family
        {
            get { return fController.Family; }
            set { fController.Family = value; }
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
            fChildrenList.OnItemValidating += FamilyEditDlg_ItemValidating;
            fChildrenList.OnModify += ModifyChildrenSheet;

            fEventsList = new GKSheetList(pageEvents);

            fNotesList = new GKSheetList(pageNotes);

            fMediaList = new GKSheetList(pageMultimedia);

            fSourcesList = new GKSheetList(pageSources);

            // SetLang()
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            GroupBox1.Text = LangMan.LS(LSID.LSID_Family);
            lblHusband.Text = LangMan.LS(LSID.LSID_Husband);
            lblWife.Text = LangMan.LS(LSID.LSID_Wife);
            lblStatus.Text = LangMan.LS(LSID.LSID_Status);
            pageChilds.Text = LangMan.LS(LSID.LSID_Childs);
            pageEvents.Text = LangMan.LS(LSID.LSID_Events);
            pageNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
            pageMultimedia.Text = LangMan.LS(LSID.LSID_RPMultimedia);
            pageSources.Text = LangMan.LS(LSID.LSID_RPSources);
            lblRestriction.Text = LangMan.LS(LSID.LSID_Restriction);

            SetToolTip(btnHusbandAdd, LangMan.LS(LSID.LSID_HusbandAddTip));
            SetToolTip(btnHusbandDelete, LangMan.LS(LSID.LSID_HusbandDeleteTip));
            SetToolTip(btnHusbandSel, LangMan.LS(LSID.LSID_HusbandSelTip));
            SetToolTip(btnWifeAdd, LangMan.LS(LSID.LSID_WifeAddTip));
            SetToolTip(btnWifeDelete, LangMan.LS(LSID.LSID_WifeDeleteTip));
            SetToolTip(btnWifeSel, LangMan.LS(LSID.LSID_WifeSelTip));

            fController = new FamilyEditDlgController(this);
            fController.Init(baseWin);

            fChildrenList.ListModel = new ChildrenListModel(baseWin, fController.LocalUndoman);
            fEventsList.ListModel = new EventsListModel(baseWin, fController.LocalUndoman, false);
            fNotesList.ListModel = new NoteLinksListModel(baseWin, fController.LocalUndoman);
            fMediaList.ListModel = new MediaLinksListModel(baseWin, fController.LocalUndoman);
            fSourcesList.ListModel = new SourceCitationsListModel(baseWin, fController.LocalUndoman);
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
        }

        public void SetHusband(string value)
        {
            bool res = !string.IsNullOrEmpty(value);
            txtHusband.Text = (res) ? value : LangMan.LS(LSID.LSID_UnkMale);
            btnHusbandAdd.Enabled = (!res);
            btnHusbandDelete.Enabled = (res);
            btnHusbandSel.Enabled = (res);
        }

        public void SetWife(string value)
        {
            bool res = !string.IsNullOrEmpty(value);
            txtWife.Text = (res) ? value : LangMan.LS(LSID.LSID_UnkFemale);
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

        private void btnAccept_Click(object sender, EventArgs e)
        {
            DialogResult = fController.Accept() ? DialogResult.Ok : DialogResult.None;
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            try {
                fController.Cancel();
                CancelClickHandler(sender, e);
            } catch (Exception ex) {
                Logger.LogWrite("FamilyEditDlg.btnCancel_Click(): " + ex.Message);
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
            Title = string.Format("{0} \"{1} - {2}\"", LangMan.LS(LSID.LSID_Family), txtHusband.Text, txtWife.Text);
        }

        private void FamilyEditDlg_ItemValidating(object sender, ItemValidatingEventArgs e)
        {
            if (e.Item is GDMRecord && !fController.Base.Context.IsAvailableRecord((GDMRecord)e.Item)) {
                e.IsAvailable = false;
            } else {
                e.IsAvailable = true;
            }
        }
    }
}
