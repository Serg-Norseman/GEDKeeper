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
using System.Windows.Forms;

using GKCommon;
using GKCommon.Controls;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Operations;
using GKCore.Types;
using GKUI.Controls;
using GKUI.Sheets;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public partial class FamilyEditDlg : EditorDialog
    {
        private readonly GKSheetList fChildsList;
        private readonly GKEventsSheet fEventsList;
        private readonly GKNotesSheet fNotesList;
        private readonly GKMediaSheet fMediaList;
        private readonly GKSourcesSheet fSourcesList;

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
                if (fFamily == null)
                {
                    btnHusbandSel.Enabled = false;
                    btnWifeSel.Enabled = false;
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

                    UpdateControls();
                }
            }
            catch (Exception ex)
            {
                fBase.Host.LogWrite("FamilyEditDlg.SetFamily(): " + ex.Message);
            }
        }

        public FamilyEditDlg(IBaseWindow baseWin) : base(baseWin)
        {
            InitializeComponent();

            for (GEDCOMRestriction res = GEDCOMRestriction.rnNone; res <= GEDCOMRestriction.rnLast; res++)
            {
                cmbRestriction.Items.Add(LangMan.LS(GKData.Restrictions[(int)res]));
            }

            for (int i = 0; i < GKData.MarriageStatus.Length; i++)
            {
                cmbMarriageStatus.Items.Add(LangMan.LS(GKData.MarriageStatus[i].Name));
            }

            fChildsList = CreateChildsSheet(pageChilds);
            fChildsList.SetControlName("fChildsList"); // for purpose of tests
            fEventsList = new GKEventsSheet(this, pageEvents, false, fLocalUndoman);
            fEventsList.SetControlName("fEventsList"); // for purpose of tests
            fNotesList = new GKNotesSheet(this, pageNotes, fLocalUndoman);
            fNotesList.SetControlName("fNotesList"); // for purpose of tests
            fMediaList = new GKMediaSheet(this, pageMultimedia, fLocalUndoman);
            fMediaList.SetControlName("fMediaList"); // for purpose of tests
            fSourcesList = new GKSourcesSheet(this, pageSources, fLocalUndoman);
            fSourcesList.SetControlName("fSourcesList"); // for purpose of tests

            GKResourceManager resourceManager = MainWin.ResourceManager;
            if (resourceManager != null) {
                btnAccept.Image = ((System.Drawing.Image)(MainWin.ResourceManager.GetObjectEx("iBtnAccept")));
                btnCancel.Image = ((System.Drawing.Image)(MainWin.ResourceManager.GetObjectEx("iBtnCancel")));
                btnHusbandAdd.Image = ((System.Drawing.Image)(MainWin.ResourceManager.GetObjectEx("iRecNew")));
                btnHusbandDelete.Image = ((System.Drawing.Image)(MainWin.ResourceManager.GetObjectEx("iRecDelete")));
                btnHusbandSel.Image = ((System.Drawing.Image)(MainWin.ResourceManager.GetObjectEx("iToMan")));
                btnWifeAdd.Image = ((System.Drawing.Image)(MainWin.ResourceManager.GetObjectEx("iRecNew")));
                btnWifeDelete.Image = ((System.Drawing.Image)(MainWin.ResourceManager.GetObjectEx("iRecDelete")));
                btnWifeSel.Image = ((System.Drawing.Image)(MainWin.ResourceManager.GetObjectEx("iToMan")));
            }

            fChildsList.OnItemValidating += FamilyEditDlg_ItemValidating;

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

            toolTip1.SetToolTip(btnHusbandAdd, LangMan.LS(LSID.LSID_HusbandAddTip));
            toolTip1.SetToolTip(btnHusbandDelete, LangMan.LS(LSID.LSID_HusbandDeleteTip));
            toolTip1.SetToolTip(btnHusbandSel, LangMan.LS(LSID.LSID_HusbandSelTip));
            toolTip1.SetToolTip(btnWifeAdd, LangMan.LS(LSID.LSID_WifeAddTip));
            toolTip1.SetToolTip(btnWifeDelete, LangMan.LS(LSID.LSID_WifeDeleteTip));
            toolTip1.SetToolTip(btnWifeSel, LangMan.LS(LSID.LSID_WifeSelTip));
        }

        private void UpdateControls()
        {
            GEDCOMIndividualRecord husband = fFamily.GetHusband();
            txtHusband.Text = (husband != null) ? GKUtils.GetNameString(husband, true, false) : LangMan.LS(LSID.LSID_UnkMale);

            btnHusbandAdd.Enabled = (husband == null);
            btnHusbandDelete.Enabled = (husband != null);
            btnHusbandSel.Enabled = (husband != null);

            GEDCOMIndividualRecord wife = fFamily.GetWife();
            txtWife.Text = (wife != null) ? GKUtils.GetNameString(wife, true, false) : LangMan.LS(LSID.LSID_UnkFemale);

            btnWifeAdd.Enabled = (wife == null);
            btnWifeDelete.Enabled = (wife != null);
            btnWifeSel.Enabled = (wife != null);

            fEventsList.DataList = fFamily.Events.GetEnumerator();
            fNotesList.DataList = fFamily.Notes.GetEnumerator();
            fMediaList.DataList = fFamily.MultimediaLinks.GetEnumerator();
            fSourcesList.DataList = fFamily.SourceCitations.GetEnumerator();
            UpdateChildsSheet();

            LockEditor(fFamily.Restriction == GEDCOMRestriction.rnLocked);
        }

        private void LockEditor(bool locked)
        {
            btnHusbandAdd.Enabled = (btnHusbandAdd.Enabled && !locked);
            btnHusbandDelete.Enabled = (btnHusbandDelete.Enabled && !locked);
            btnWifeAdd.Enabled = (btnWifeAdd.Enabled && !locked);
            btnWifeDelete.Enabled = (btnWifeDelete.Enabled && !locked);

            cmbMarriageStatus.Enabled = (cmbMarriageStatus.Enabled && !locked);

            fChildsList.ReadOnly = locked;
            fEventsList.ReadOnly = locked;
            fNotesList.ReadOnly = locked;
            fMediaList.ReadOnly = locked;
            fSourcesList.ReadOnly = locked;
        }

        private void cbRestriction_SelectedIndexChanged(object sender, EventArgs e)
        {
            LockEditor(cmbRestriction.SelectedIndex == (int)GEDCOMRestriction.rnLocked);
        }

        private GKSheetList CreateChildsSheet(Control owner)
        {
            GKSheetList sheet = new GKSheetList(owner);
            
            sheet.Columns_BeginUpdate();
            sheet.AddColumn("№", 25, false);
            sheet.AddColumn(LangMan.LS(LSID.LSID_Name), 300, false);
            sheet.AddColumn(LangMan.LS(LSID.LSID_BirthDate), 100, false);
            sheet.Columns_EndUpdate();

            sheet.Buttons = EnumSet<SheetButton>.Create(SheetButton.lbAdd, SheetButton.lbEdit, SheetButton.lbDelete, SheetButton.lbJump);
            sheet.OnModify += ModifyChildsSheet;

            return sheet;
        }

        private void UpdateChildsSheet()
        {
            try
            {
                fChildsList.SwitchSorter();
                fChildsList.BeginUpdate();
                fChildsList.ClearItems();

                int idx = 0;
                foreach (GEDCOMPointer ptr in fFamily.Children) {
                    idx += 1;

                    GEDCOMIndividualRecord child = (GEDCOMIndividualRecord)ptr.Value;

                    GKListItem item = fChildsList.AddItem(idx, child);
                    item.AddSubItem(GKUtils.GetNameString(child, true, false));
                    item.AddSubItem(new GEDCOMDateItem(GKUtils.GetBirthDate(child)));
                }

                fChildsList.EndUpdate();
                fChildsList.SwitchSorter();
            }
            catch (Exception ex)
            {
                Logger.LogWrite("FamilyEditDlg.UpdateChildsSheet(): " + ex.Message);
            }
        }

        private void ModifyChildsSheet(object sender, ModifyEventArgs eArgs)
        {
            bool result = false;

            GEDCOMIndividualRecord child = eArgs.ItemData as GEDCOMIndividualRecord;

            switch (eArgs.Action)
            {
                case RecordAction.raAdd:
                    child = fBase.SelectPerson(fFamily.GetHusband(), TargetMode.tmParent, GEDCOMSex.svNone);
                    result = (child != null && fBase.IsAvailableRecord(child));
                    if (result) {
                        //result = this.fFamily.AddChild(child);
                        result = fLocalUndoman.DoOrdinaryOperation(OperationType.otIndividualParentsAttach, child, fFamily);
                    }
                    break;

                case RecordAction.raEdit:
                    result = (fBase.ModifyPerson(ref child, null, TargetMode.tmNone, GEDCOMSex.svNone));
                    break;

                case RecordAction.raDelete:
                    result = (child != null && GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachChildQuery)) != DialogResult.No);
                    if (result) {
                        //result = this.fFamily.RemoveChild(child);
                        result = fLocalUndoman.DoOrdinaryOperation(OperationType.otIndividualParentsDetach, child, fFamily);
                    }
                    break;

                case RecordAction.raJump:
                    if (child != null) {
                        AcceptChanges();
                        fBase.SelectRecordByXRef(child.XRef);
                        Close();
                    }
                    break;
            }

            if (result) UpdateChildsSheet();
        }

        private void AcceptChanges()
        {
            string stat = GKData.MarriageStatus[cmbMarriageStatus.SelectedIndex].StatSign;
            fFamily.SetTagStringValue("_STAT", stat);

            fFamily.Restriction = (GEDCOMRestriction)cmbRestriction.SelectedIndex;

            fFamily.SortChilds();

            fLocalUndoman.Commit();

            fBase.ChangeRecord(fFamily);
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                AcceptChanges();
                DialogResult = DialogResult.OK;
            }
            catch (Exception ex)
            {
                fBase.Host.LogWrite("FamilyEditDlg.btnAccept_Click(): " + ex.Message);
                DialogResult = DialogResult.None;
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
                fBase.Host.LogWrite("FamilyEditDlg.btnCancel_Click(): " + ex.Message);
            }
        }

        private void SetTitle()
        {
            Text = string.Format("{0} \"{1} - {2}\"", LangMan.LS(LSID.LSID_Family), txtHusband.Text, txtWife.Text);
        }

        internal void SetTarget(FamilyTarget targetType, GEDCOMIndividualRecord target)
        {
            if (targetType == FamilyTarget.None || target == null) return;

            bool result = false;
            if (targetType == FamilyTarget.Spouse) {
                //this.fFamily.AddSpouse(target);
                result = fLocalUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseAttach, fFamily, target);
            } else if (targetType == FamilyTarget.Child) {
                //this.fFamily.AddChild(target);
                result = fLocalUndoman.DoOrdinaryOperation(OperationType.otIndividualParentsAttach, target, fFamily);
            }

            if (result) UpdateControls();
        }

        private void btnHusbandAddClick(object sender, EventArgs e)
        {
            GEDCOMIndividualRecord husband = fBase.SelectPerson(null, TargetMode.tmNone, GEDCOMSex.svMale);
            if (husband != null && fFamily.Husband.StringValue == "")
            {
                //this.fFamily.AddSpouse(husband);
                fLocalUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseAttach, fFamily, husband);
                UpdateControls();
            }
        }

        private void btnHusbandDeleteClick(object sender, EventArgs e)
        {
            GEDCOMIndividualRecord husband = fFamily.GetHusband();
            if (!fBase.IsAvailableRecord(husband)) return;

            if (GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachHusbandQuery)) != DialogResult.No)
            {
                //this.fFamily.RemoveSpouse(husband);
                fLocalUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseDetach, fFamily, husband);
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
            GEDCOMIndividualRecord wife = fBase.SelectPerson(null, TargetMode.tmNone, GEDCOMSex.svFemale);
            if (wife != null && fFamily.Wife.StringValue == "")
            {
                //this.fFamily.AddSpouse(wife);
                fLocalUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseAttach, fFamily, wife);
                UpdateControls();
            }
        }

        private void btnWifeDeleteClick(object sender, EventArgs e)
        {
            GEDCOMIndividualRecord wife = fFamily.GetWife();
            if (!fBase.IsAvailableRecord(wife)) return;

            if (GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachWifeQuery)) != DialogResult.No)
            {
                //this.fFamily.RemoveSpouse(this.fFamily.GetWife());
                fLocalUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseDetach, fFamily, wife);
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

        private void EditHusband_TextChanged(object sender, EventArgs e)
        {
            SetTitle();
        }

        private void EditWife_TextChanged(object sender, EventArgs e)
        {
            SetTitle();
        }

        private void FamilyEditDlg_ItemValidating(object sender, ItemValidatingEventArgs e)
        {
            if (e.Item is GEDCOMRecord && !fBase.IsAvailableRecord((GEDCOMRecord) e.Item)) {
                e.IsAvailable = false;
            } else {
                e.IsAvailable = true;
            }
        }
    }
}
