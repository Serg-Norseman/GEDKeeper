/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
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
            get	{ return this.fFamily; }
            set	{ this.SetFamily(value); }
        }

        private void SetFamily(GEDCOMFamilyRecord value)
        {
            this.fFamily = value;
            try
            {
                if (this.fFamily == null)
                {
                    this.btnHusbandSel.Enabled = false;
                    this.btnWifeSel.Enabled = false;
                    this.cmbMarriageStatus.Enabled = false;
                    this.cmbMarriageStatus.SelectedIndex = 0;
                    this.cmbRestriction.SelectedIndex = 0;
                }
                else
                {
                    string stat = this.fFamily.GetTagStringValue("_STAT");
                    int statIdx = GKUtils.GetMarriageStatusIndex(stat);
                    this.cmbMarriageStatus.Enabled = true;
                    this.cmbMarriageStatus.SelectedIndex = statIdx;
                    this.cmbRestriction.SelectedIndex = (sbyte)this.fFamily.Restriction;

                    this.UpdateControls();
                }
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("FamilyEditDlg.SetFamily(): " + ex.Message);
            }
        }

        public FamilyEditDlg(IBaseWindow baseWin) : base(baseWin)
        {
            this.InitializeComponent();

            for (GEDCOMRestriction res = GEDCOMRestriction.rnNone; res <= GEDCOMRestriction.rnLast; res++)
            {
                this.cmbRestriction.Items.Add(LangMan.LS(GKData.Restrictions[(int)res]));
            }

            for (int i = 0; i < GKData.MarriageStatus.Length; i++)
            {
                this.cmbMarriageStatus.Items.Add(LangMan.LS(GKData.MarriageStatus[i].Name));
            }

            this.fChildsList = this.CreateChildsSheet(this.pageChilds);
            this.fEventsList = new GKEventsSheet(this, this.pageEvents, false, this.fLocalUndoman);
            this.fNotesList = new GKNotesSheet(this, this.pageNotes, this.fLocalUndoman);
            this.fMediaList = new GKMediaSheet(this, this.pageMultimedia, this.fLocalUndoman);
            this.fSourcesList = new GKSourcesSheet(this, this.pageSources, this.fLocalUndoman);

            this.btnAccept.Image = ((System.Drawing.Image)(MainWin.ResourceManager.GetObjectEx("iBtnAccept")));
            this.btnCancel.Image = ((System.Drawing.Image)(MainWin.ResourceManager.GetObjectEx("iBtnCancel")));
            this.btnHusbandAdd.Image = ((System.Drawing.Image)(MainWin.ResourceManager.GetObjectEx("iRecNew")));
            this.btnHusbandDelete.Image = ((System.Drawing.Image)(MainWin.ResourceManager.GetObjectEx("iRecDelete")));
            this.btnHusbandSel.Image = ((System.Drawing.Image)(MainWin.ResourceManager.GetObjectEx("iToMan")));
            this.btnWifeAdd.Image = ((System.Drawing.Image)(MainWin.ResourceManager.GetObjectEx("iRecNew")));
            this.btnWifeDelete.Image = ((System.Drawing.Image)(MainWin.ResourceManager.GetObjectEx("iRecDelete")));
            this.btnWifeSel.Image = ((System.Drawing.Image)(MainWin.ResourceManager.GetObjectEx("iToMan")));

            this.fChildsList.OnItemValidating += this.FamilyEditDlg_ItemValidating;

            // SetLang()
            this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            this.GroupBox1.Text = LangMan.LS(LSID.LSID_Family);
            this.lblHusband.Text = LangMan.LS(LSID.LSID_Husband);
            this.lblWife.Text = LangMan.LS(LSID.LSID_Wife);
            this.lblStatus.Text = LangMan.LS(LSID.LSID_Status);
            this.pageChilds.Text = LangMan.LS(LSID.LSID_Childs);
            this.pageEvents.Text = LangMan.LS(LSID.LSID_Events);
            this.pageNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
            this.pageMultimedia.Text = LangMan.LS(LSID.LSID_RPMultimedia);
            this.pageSources.Text = LangMan.LS(LSID.LSID_RPSources);
            this.lblRestriction.Text = LangMan.LS(LSID.LSID_Restriction);

            this.toolTip1.SetToolTip(this.btnHusbandAdd, LangMan.LS(LSID.LSID_HusbandAddTip));
            this.toolTip1.SetToolTip(this.btnHusbandDelete, LangMan.LS(LSID.LSID_HusbandDeleteTip));
            this.toolTip1.SetToolTip(this.btnHusbandSel, LangMan.LS(LSID.LSID_HusbandSelTip));
            this.toolTip1.SetToolTip(this.btnWifeAdd, LangMan.LS(LSID.LSID_WifeAddTip));
            this.toolTip1.SetToolTip(this.btnWifeDelete, LangMan.LS(LSID.LSID_WifeDeleteTip));
            this.toolTip1.SetToolTip(this.btnWifeSel, LangMan.LS(LSID.LSID_WifeSelTip));
        }

        private void UpdateControls()
        {
            GEDCOMIndividualRecord husband = this.fFamily.GetHusband();
            this.txtHusband.Text = (husband != null) ? husband.GetNameString(true, false) : LangMan.LS(LSID.LSID_UnkMale);

            this.btnHusbandAdd.Enabled = (husband == null);
            this.btnHusbandDelete.Enabled = (husband != null);
            this.btnHusbandSel.Enabled = (husband != null);

            GEDCOMIndividualRecord wife = this.fFamily.GetWife();
            this.txtWife.Text = (wife != null) ? wife.GetNameString(true, false) : LangMan.LS(LSID.LSID_UnkFemale);

            this.btnWifeAdd.Enabled = (wife == null);
            this.btnWifeDelete.Enabled = (wife != null);
            this.btnWifeSel.Enabled = (wife != null);

            this.fEventsList.DataList = this.fFamily.Events.GetEnumerator();
            this.fNotesList.DataList = this.fFamily.Notes.GetEnumerator();
            this.fMediaList.DataList = this.fFamily.MultimediaLinks.GetEnumerator();
            this.fSourcesList.DataList = this.fFamily.SourceCitations.GetEnumerator();
            this.UpdateChildsSheet();

            this.LockEditor(this.fFamily.Restriction == GEDCOMRestriction.rnLocked);
        }

        private void LockEditor(bool locked)
        {
            this.btnHusbandAdd.Enabled = (this.btnHusbandAdd.Enabled && !locked);
            this.btnHusbandDelete.Enabled = (this.btnHusbandDelete.Enabled && !locked);
            this.btnWifeAdd.Enabled = (this.btnWifeAdd.Enabled && !locked);
            this.btnWifeDelete.Enabled = (this.btnWifeDelete.Enabled && !locked);

            this.cmbMarriageStatus.Enabled = (this.cmbMarriageStatus.Enabled && !locked);

            this.fChildsList.ReadOnly = locked;
            this.fEventsList.ReadOnly = locked;
            this.fNotesList.ReadOnly = locked;
            this.fMediaList.ReadOnly = locked;
            this.fSourcesList.ReadOnly = locked;
        }

        private void cbRestriction_SelectedIndexChanged(object sender, EventArgs e)
        {
            this.LockEditor(this.cmbRestriction.SelectedIndex == (int)GEDCOMRestriction.rnLocked);
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
            sheet.OnModify += this.ModifyChildsSheet;

            return sheet;
        }

        private void UpdateChildsSheet()
        {
            try
            {
                this.fChildsList.SwitchSorter();
                this.fChildsList.BeginUpdate();
                this.fChildsList.ClearItems();

                int idx = 0;
                foreach (GEDCOMPointer ptr in this.fFamily.Childrens) {
                    idx += 1;

                    GEDCOMIndividualRecord child = (GEDCOMIndividualRecord)ptr.Value;

                    GKListItem item = this.fChildsList.AddItem(idx, child);
                    item.AddSubItem(child.GetNameString(true, false));
                    item.AddSubItem(GKUtils.GetBirthDate(child));
                }

                this.fChildsList.EndUpdate();
                this.fChildsList.SwitchSorter();
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
                    child = this.fBase.SelectPerson(this.fFamily.GetHusband(), TargetMode.tmParent, GEDCOMSex.svNone);
                    result = (child != null && this.fBase.IsAvailableRecord(child));
                    if (result) {
                        //result = this.fFamily.AddChild(child);
                        result = this.fLocalUndoman.DoOrdinaryOperation(OperationType.otIndividualParentsAttach, child, this.fFamily);
                    }
                    break;

                case RecordAction.raEdit:
                    result = (this.fBase.ModifyPerson(ref child));
                    break;

                case RecordAction.raDelete:
                    result = (child != null && GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachChildQuery)) != DialogResult.No);
                    if (result) {
                        //result = this.fFamily.RemoveChild(child);
                        result = this.fLocalUndoman.DoOrdinaryOperation(OperationType.otIndividualParentsDetach, child, this.fFamily);
                    }
                    break;

                case RecordAction.raJump:
                    if (child != null) {
                        this.AcceptChanges();
                        this.fBase.SelectRecordByXRef(child.XRef);
                        base.Close();
                    }
                    break;
            }

            if (result) this.UpdateChildsSheet();
        }

        private void AcceptChanges()
        {
            string stat = GKData.MarriageStatus[this.cmbMarriageStatus.SelectedIndex].StatSign;
            this.fFamily.SetTagStringValue("_STAT", stat);

            this.fFamily.Restriction = (GEDCOMRestriction)this.cmbRestriction.SelectedIndex;

            this.fFamily.SortChilds();

            this.fLocalUndoman.Commit();

            this.fBase.ChangeRecord(this.fFamily);
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                this.AcceptChanges();
                base.DialogResult = DialogResult.OK;
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("FamilyEditDlg.btnAccept_Click(): " + ex.Message);
                base.DialogResult = DialogResult.None;
            }
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            try
            {
                this.fLocalUndoman.Rollback();
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("FamilyEditDlg.btnCancel_Click(): " + ex.Message);
            }
        }

        private void SetTitle()
        {
            this.Text = string.Format("{0} \"{1} - {2}\"", LangMan.LS(LSID.LSID_Family), this.txtHusband.Text, this.txtWife.Text);
        }

        private void btnHusbandAddClick(object sender, EventArgs e)
        {
            GEDCOMIndividualRecord husband = this.fBase.SelectPerson(null, TargetMode.tmNone, GEDCOMSex.svMale);
            if (husband != null && this.fFamily.Husband.StringValue == "")
            {
                //this.fFamily.AddSpouse(husband);
                this.fLocalUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseAttach, this.fFamily, husband);
                this.UpdateControls();
            }
        }

        private void btnHusbandDeleteClick(object sender, EventArgs e)
        {
            GEDCOMIndividualRecord husband = this.fFamily.GetHusband();
            if (!this.fBase.IsAvailableRecord(husband)) return;

            if (GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachHusbandQuery)) != DialogResult.No)
            {
                //this.fFamily.RemoveSpouse(husband);
                this.fLocalUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseDetach, this.fFamily, husband);
                this.UpdateControls();
            }
        }

        private void btnHusbandSelClick(object sender, EventArgs e)
        {
            GEDCOMIndividualRecord spouse = this.fFamily.GetHusband();
            if (spouse != null)
            {
                this.AcceptChanges();
                this.fBase.SelectRecordByXRef(spouse.XRef);
                base.Close();
            }
        }

        private void btnWifeAddClick(object sender, EventArgs e)
        {
            GEDCOMIndividualRecord wife = this.fBase.SelectPerson(null, TargetMode.tmNone, GEDCOMSex.svFemale);
            if (wife != null && this.fFamily.Wife.StringValue == "")
            {
                //this.fFamily.AddSpouse(wife);
                this.fLocalUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseAttach, this.fFamily, wife);
                this.UpdateControls();
            }
        }

        private void btnWifeDeleteClick(object sender, EventArgs e)
        {
            GEDCOMIndividualRecord wife = this.fFamily.GetWife();
            if (!this.fBase.IsAvailableRecord(wife)) return;

            if (GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachWifeQuery)) != DialogResult.No)
            {
                //this.fFamily.RemoveSpouse(this.fFamily.GetWife());
                this.fLocalUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseDetach, this.fFamily, wife);
                this.UpdateControls();
            }
        }

        private void btnWifeSelClick(object sender, EventArgs e)
        {
            GEDCOMIndividualRecord spouse = this.fFamily.GetWife();
            if (spouse != null)
            {
                this.AcceptChanges();
                this.fBase.SelectRecordByXRef(spouse.XRef);
                base.Close();
            }
        }

        private void EditHusband_TextChanged(object sender, EventArgs e)
        {
            this.SetTitle();
        }

        private void EditWife_TextChanged(object sender, EventArgs e)
        {
            this.SetTitle();
        }

        private void FamilyEditDlg_ItemValidating(object sender, ItemValidatingEventArgs e)
        {
            if (e.Item is GEDCOMRecord && !this.fBase.IsAvailableRecord(e.Item as GEDCOMRecord)) {
                e.IsAvailable = false;
            } else {
                e.IsAvailable = true;
            }
        }
    }
}
