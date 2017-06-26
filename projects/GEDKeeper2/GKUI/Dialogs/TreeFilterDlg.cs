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
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Charts;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Types;
using GKUI.Components;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class TreeFilterDlg : Form
    {
        private readonly IBaseWindow fBase;
        private readonly GKSheetList fPersonsList;

        private ChartFilter fFilter;
        private string fTemp;

        public IBaseWindow Base
        {
            get	{ return fBase; }
        }

        public ChartFilter Filter
        {
            get	{ return fFilter; }
            set	{ fFilter = value;	}
        }

        private void ListModify(object sender, ModifyEventArgs eArgs)
        {
            if (sender == fPersonsList) {
                GEDCOMIndividualRecord iRec = eArgs.ItemData as GEDCOMIndividualRecord;

                switch (eArgs.Action)
                {
                    case RecordAction.raAdd:
                        iRec = fBase.Context.SelectPerson(null, TargetMode.tmNone, GEDCOMSex.svNone);
                        if (iRec != null) {
                            fTemp = fTemp + iRec.XRef + ";";
                        }
                        break;

                    case RecordAction.raDelete:
                        if (iRec != null) {
                            fTemp = fTemp.Replace(iRec.XRef + ";", "");
                        }
                        break;
                }
            }

            UpdateControls();
        }

        private void UpdateControls()
        {
            switch (fFilter.BranchCut) {
                case ChartFilter.BranchCutType.Persons:
                    rbCutPersons.Checked = true;
                    break;

                case ChartFilter.BranchCutType.Years:
                    rbCutYears.Checked = true;
                    break;

                case ChartFilter.BranchCutType.None:
                    rbCutNone.Checked = true;
                    break;
            }

            edYear.Enabled = (fFilter.BranchCut == ChartFilter.BranchCutType.Years);
            fPersonsList.Enabled = (fFilter.BranchCut == ChartFilter.BranchCutType.Persons);
            edYear.Text = fFilter.BranchYear.ToString();
            fPersonsList.ClearItems();

            if (!string.IsNullOrEmpty(fTemp)) {
                string[] tmpRefs = fTemp.Split(';');

                int num = tmpRefs.Length;
                for (int i = 0; i < num; i++)
                {
                    string xref = tmpRefs[i];
                    GEDCOMIndividualRecord p = fBase.Context.Tree.XRefIndex_Find(xref) as GEDCOMIndividualRecord;
                    if (p != null) fPersonsList.AddItem(p, GKUtils.GetNameString(p, true, false));
                }
            }

            if (fFilter.SourceMode != FilterGroupMode.Selected) {
                cmbSource.SelectedIndex = (sbyte)fFilter.SourceMode;
            } else {
                GEDCOMSourceRecord srcRec = fBase.Context.Tree.XRefIndex_Find(fFilter.SourceRef) as GEDCOMSourceRecord;
                if (srcRec != null) cmbSource.Text = srcRec.FiledByEntry;
            }
        }

        private void rbCutNoneClick(object sender, EventArgs e)
        {
            if (rbCutNone.Checked)
            {
                fFilter.BranchCut = ChartFilter.BranchCutType.None;
            }
            else
            {
                if (rbCutYears.Checked)
                {
                    fFilter.BranchCut = ChartFilter.BranchCutType.Years;
                }
                else
                {
                    if (rbCutPersons.Checked)
                    {
                        fFilter.BranchCut = ChartFilter.BranchCutType.Persons;
                    }
                }
            }
            UpdateControls();
        }

        private void AcceptChanges()
        {
            if (rbCutNone.Checked)
            {
                fFilter.BranchCut = ChartFilter.BranchCutType.None;
            }
            else
            {
                if (rbCutYears.Checked)
                {
                    fFilter.BranchCut = ChartFilter.BranchCutType.Years;
                    fFilter.BranchYear = int.Parse(edYear.Text);
                }
                else
                {
                    if (rbCutPersons.Checked)
                    {
                        fFilter.BranchCut = ChartFilter.BranchCutType.Persons;
                        fFilter.BranchPersons = fTemp;
                    }
                }
            }

            int selectedIndex = cmbSource.SelectedIndex;
            if (selectedIndex >= 0 && selectedIndex < 3)
            {
                fFilter.SourceMode = (FilterGroupMode)cmbSource.SelectedIndex;
                fFilter.SourceRef = "";
            }
            else
            {
                GKComboItem item = (GKComboItem)cmbSource.Items[cmbSource.SelectedIndex];
                GEDCOMRecord rec = item.Tag as GEDCOMRecord;
                if (rec != null)
                {
                    fFilter.SourceMode = FilterGroupMode.Selected;
                    fFilter.SourceRef = rec.XRef;
                }
                else
                {
                    fFilter.SourceMode = FilterGroupMode.All;
                    fFilter.SourceRef = "";
                }
            }
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
                Logger.LogWrite("TreeFilterDlg.btnAccept_Click(): " + ex.Message);
                DialogResult = DialogResult.None;
            }
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            fFilter.Reset();
        }

        private void TreeFilterDlg_Load(object sender, EventArgs e)
        {
            GEDCOMTree tree = fBase.Context.Tree;
            fTemp = fFilter.BranchPersons;

            cmbSource.Sorted = true;
            int num = tree.RecordsCount;
            for (int i = 0; i < num; i++) {
                GEDCOMRecord rec = tree[i];
                if (rec is GEDCOMSourceRecord) {
                    cmbSource.Items.Add(new GKComboItem((rec as GEDCOMSourceRecord).FiledByEntry, rec));
                }
            }
            cmbSource.Sorted = false;

            cmbSource.Items.Insert(0, LangMan.LS(LSID.LSID_SrcAll));
            cmbSource.Items.Insert(1, LangMan.LS(LSID.LSID_SrcNot));
            cmbSource.Items.Insert(2, LangMan.LS(LSID.LSID_SrcAny));

            UpdateControls();
        }

        public TreeFilterDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            btnAccept.Image = GKResources.iBtnAccept;
            btnCancel.Image = GKResources.iBtnCancel;

            fBase = baseWin;
            fPersonsList = new GKSheetList(Panel1);
            fPersonsList.Buttons = EnumSet<SheetButton>.Create(SheetButton.lbAdd, SheetButton.lbDelete);
            fPersonsList.OnModify += ListModify;
            fPersonsList.AddColumn(LangMan.LS(LSID.LSID_RPIndividuals), 350, false);

            // SetLang()
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            Text = LangMan.LS(LSID.LSID_MIFilter);
            rgBranchCut.Text = LangMan.LS(LSID.LSID_BranchCut);
            rbCutNone.Text = LangMan.LS(LSID.LSID_Not);
            rbCutYears.Text = LangMan.LS(LSID.LSID_BCut_Years);
            lblYear.Text = LangMan.LS(LSID.LSID_Year);
            rbCutPersons.Text = LangMan.LS(LSID.LSID_BCut_Persons);
            lblRPSources.Text = LangMan.LS(LSID.LSID_RPSources);
        }
    }
}
