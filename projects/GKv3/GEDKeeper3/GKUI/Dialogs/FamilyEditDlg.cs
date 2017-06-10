/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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

using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Operations;
using GKCore.Types;
using GKCore.UIContracts;
using GKUI.Components;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public partial class FamilyEditDlg : EditorDialog, IFamilyEditDlg
    {
        private readonly GKSheetList fChildrenList;
        private readonly GKSheetList fEventsList;
        private readonly GKSheetList fNotesList;
        private readonly GKSheetList fMediaList;
        private readonly GKSheetList fSourcesList;

        private GEDCOMFamilyRecord fFamily;

        public GEDCOMFamilyRecord Family
        {
            get	{ return fFamily; }
            set	{ SetFamily(value); }
        }

        private void SetFamily(GEDCOMFamilyRecord value)
        {
            fFamily = value;
            try
            {
                fChildrenList.ListModel.DataOwner = fFamily;
                fEventsList.ListModel.DataOwner = fFamily;
                fNotesList.ListModel.DataOwner = fFamily;
                fMediaList.ListModel.DataOwner = fFamily;
                fSourcesList.ListModel.DataOwner = fFamily;

                if (fFamily == null)
                {
                    cmbMarriageStatus.Enabled = false;
                    cmbMarriageStatus.SelectedIndex = 0;
                    cmbRestriction.SelectedIndex = 0;
                }
                else
                {
                    string stat = fFamily.GetTagStringValue("_STAT");
                    int statIdx = GKUtils.GetMarriageStatusIndex(stat);
                    cmbMarriageStatus.Enabled = true;
                    cmbMarriageStatus.SelectedIndex = statIdx;
                    cmbRestriction.SelectedIndex = (sbyte)fFamily.Restriction;
                }

                UpdateControls();
            }
            catch (Exception ex)
            {
                Logger.LogWrite("FamilyEditDlg.SetFamily(): " + ex.Message);
            }
        }

        public FamilyEditDlg()
        {
            InitializeComponent();

            txtHusband.TextChanged += EditSpouse_TextChanged;
            txtWife.TextChanged += EditSpouse_TextChanged;

            for (GEDCOMRestriction res = GEDCOMRestriction.rnNone; res <= GEDCOMRestriction.rnLast; res++)
            {
                cmbRestriction.Items.Add(LangMan.LS(GKData.Restrictions[(int)res]));
            }

            for (int i = 0; i < GKData.MarriageStatus.Length; i++)
            {
                cmbMarriageStatus.Items.Add(LangMan.LS(GKData.MarriageStatus[i].Name));
            }

            fChildrenList = new GKSheetList(pageChilds);
            fChildrenList.OnItemValidating += FamilyEditDlg_ItemValidating;
            fChildrenList.OnModify += ModifyChildrenSheet;

            fEventsList = new GKSheetList(pageEvents);

            fNotesList = new GKSheetList(pageNotes);

            fMediaList = new GKSheetList(pageMultimedia);

            fSourcesList = new GKSheetList(pageSources);

            btnAccept.Image = Bitmap.FromResource("Resources.btn_accept.gif");
            btnCancel.Image = Bitmap.FromResource("Resources.btn_cancel.gif");
            btnHusbandAdd.Image = Bitmap.FromResource("Resources.btn_rec_new.gif");
            btnHusbandDelete.Image = Bitmap.FromResource("Resources.btn_rec_delete.gif");
            btnHusbandSel.Image = Bitmap.FromResource("Resources.btn_jump.gif");
            btnWifeAdd.Image = Bitmap.FromResource("Resources.btn_rec_new.gif");
            btnWifeDelete.Image = Bitmap.FromResource("Resources.btn_rec_delete.gif");
            btnWifeSel.Image = Bitmap.FromResource("Resources.btn_jump.gif");

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

            btnHusbandAdd.ToolTip = LangMan.LS(LSID.LSID_HusbandAddTip);
            btnHusbandDelete.ToolTip = LangMan.LS(LSID.LSID_HusbandDeleteTip);
            btnHusbandSel.ToolTip = LangMan.LS(LSID.LSID_HusbandSelTip);
            btnWifeAdd.ToolTip = LangMan.LS(LSID.LSID_WifeAddTip);
            btnWifeDelete.ToolTip = LangMan.LS(LSID.LSID_WifeDeleteTip);
            btnWifeSel.ToolTip = LangMan.LS(LSID.LSID_WifeSelTip);
        }

        private void UpdateControls()
        {
            GEDCOMIndividualRecord husband, wife;

            if (fFamily == null) {
                husband = null;
                wife = null;

                LockEditor(true);
            } else {
                husband = fFamily.GetHusband();
                wife = fFamily.GetWife();

                LockEditor(fFamily.Restriction == GEDCOMRestriction.rnLocked);
            }

            txtHusband.Text = (husband != null) ? GKUtils.GetNameString(husband, true, false) : LangMan.LS(LSID.LSID_UnkMale);
            btnHusbandAdd.Enabled = (husband == null);
            btnHusbandDelete.Enabled = (husband != null);
            btnHusbandSel.Enabled = (husband != null);

            txtWife.Text = (wife != null) ? GKUtils.GetNameString(wife, true, false) : LangMan.LS(LSID.LSID_UnkFemale);
            btnWifeAdd.Enabled = (wife == null);
            btnWifeDelete.Enabled = (wife != null);
            btnWifeSel.Enabled = (wife != null);

            fChildrenList.UpdateSheet();
            fEventsList.UpdateSheet();
            fNotesList.UpdateSheet();
            fMediaList.UpdateSheet();
            fSourcesList.UpdateSheet();
        }

        private void LockEditor(bool locked)
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

        private void cbRestriction_SelectedIndexChanged(object sender, EventArgs e)
        {
            LockEditor(cmbRestriction.SelectedIndex == (int)GEDCOMRestriction.rnLocked);
        }

        private void ModifyChildrenSheet(object sender, ModifyEventArgs eArgs)
        {
            GEDCOMIndividualRecord child = eArgs.ItemData as GEDCOMIndividualRecord;
            if (eArgs.Action == RecordAction.raJump && child != null) {
                AcceptChanges();
                fBase.SelectRecordByXRef(child.XRef);
                Close();
            }
        }

        private void AcceptChanges()
        {
            string stat = GKData.MarriageStatus[cmbMarriageStatus.SelectedIndex].StatSign;
            fFamily.SetTagStringValue("_STAT", stat);

            fFamily.Restriction = (GEDCOMRestriction)cmbRestriction.SelectedIndex;

            fFamily.SortChilds();

            fLocalUndoman.Commit();

            fBase.NotifyRecord(fFamily, RecordAction.raEdit);
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                AcceptChanges();
                DialogResult = DlgResult.OK;
            }
            catch (Exception ex)
            {
                Logger.LogWrite("FamilyEditDlg.btnAccept_Click(): " + ex.Message);
                DialogResult = DlgResult.None;
            }
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            try
            {
                fLocalUndoman.Rollback();
            }
            catch (Exception ex)
            {
                Logger.LogWrite("FamilyEditDlg.btnCancel_Click(): " + ex.Message);
            }
        }

        // TODO: rework
        public void SetTarget(TargetMode targetType, GEDCOMIndividualRecord target)
        {
            if (targetType == TargetMode.tmNone || target == null) return;

            bool result = false;
            if (targetType == TargetMode.tmFamilySpouse) {
                result = fLocalUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseAttach, fFamily, target);
            } else if (targetType == TargetMode.tmFamilyChild) {
                result = fLocalUndoman.DoOrdinaryOperation(OperationType.otIndividualParentsAttach, target, fFamily);
            }

            if (result) UpdateControls();
        }

        private void btnHusbandAddClick(object sender, EventArgs e)
        {
            if (BaseController.AddFamilyHusband(fBase, fLocalUndoman, fFamily)) {
                UpdateControls();
            }
        }

        private void btnHusbandDeleteClick(object sender, EventArgs e)
        {
            if (BaseController.DeleteFamilyHusband(fBase, fLocalUndoman, fFamily)) {
                UpdateControls();
            }
        }

        private void btnHusbandSelClick(object sender, EventArgs e)
        {
            GEDCOMIndividualRecord spouse = fFamily.GetHusband();
            if (spouse == null) return;

            AcceptChanges();
            fBase.SelectRecordByXRef(spouse.XRef);
            Close();
        }

        private void btnWifeAddClick(object sender, EventArgs e)
        {
            if (BaseController.AddFamilyWife(fBase, fLocalUndoman, fFamily)) {
                UpdateControls();
            }
        }

        private void btnWifeDeleteClick(object sender, EventArgs e)
        {
            if (BaseController.DeleteFamilyWife(fBase, fLocalUndoman, fFamily)) {
                UpdateControls();
            }
        }

        private void btnWifeSelClick(object sender, EventArgs e)
        {
            GEDCOMIndividualRecord spouse = fFamily.GetWife();
            if (spouse == null) return;

            AcceptChanges();
            fBase.SelectRecordByXRef(spouse.XRef);
            Close();
        }

        private void EditSpouse_TextChanged(object sender, EventArgs e)
        {
            Title = string.Format("{0} \"{1} - {2}\"", LangMan.LS(LSID.LSID_Family), txtHusband.Text, txtWife.Text);
        }

        private void FamilyEditDlg_ItemValidating(object sender, ItemValidatingEventArgs e)
        {
            if (e.Item is GEDCOMRecord && !fBase.Context.IsAvailableRecord((GEDCOMRecord) e.Item)) {
                e.IsAvailable = false;
            } else {
                e.IsAvailable = true;
            }
        }

        public override void InitDialog(IBaseWindow baseWin)
        {
            base.InitDialog(baseWin);

            fChildrenList.ListModel = new ChildrenListModel(fBase, fLocalUndoman);
            fEventsList.ListModel = new EventsListModel(fBase, fLocalUndoman, false);
            fNotesList.ListModel = new NoteLinksListModel(fBase, fLocalUndoman);
            fMediaList.ListModel = new MediaLinksListModel(fBase, fLocalUndoman);
            fSourcesList.ListModel = new SourceCitationsListModel(fBase, fLocalUndoman);
        }
    }
}
