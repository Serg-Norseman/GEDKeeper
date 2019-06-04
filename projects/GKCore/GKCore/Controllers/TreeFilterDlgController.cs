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
using BSLib;
using GDModel;
using GKCore.Charts;
using GKCore.MVP;
using GKCore.MVP.Views;
using GKCore.Types;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class TreeFilterDlgController : DialogController<ITreeFilterDlg>
    {
        private ChartFilter fFilter;
        private string fTemp;

        public ChartFilter Filter
        {
            get { return fFilter; }
            set { fFilter = value; }
        }

        public TreeFilterDlgController(ITreeFilterDlg view) : base(view)
        {
        }

        public override bool Accept()
        {
            try {
                fFilter.BranchCut = (ChartFilter.BranchCutType)fView.GetCutModeRadio();
                if (fFilter.BranchCut == ChartFilter.BranchCutType.Years) {
                    fFilter.BranchYear = (int)fView.YearNum.Value;
                } else if (fFilter.BranchCut == ChartFilter.BranchCutType.Persons) {
                    fFilter.BranchPersons = fTemp;
                }

                int selectedIndex = fView.SourceCombo.SelectedIndex;
                if (selectedIndex >= 0 && selectedIndex < 3) {
                    fFilter.SourceMode = (FilterGroupMode)fView.SourceCombo.SelectedIndex;
                    fFilter.SourceRef = "";
                } else {
                    GDMRecord rec = fView.SourceCombo.SelectedTag as GDMRecord;
                    if (rec != null) {
                        fFilter.SourceMode = FilterGroupMode.Selected;
                        fFilter.SourceRef = rec.XRef;
                    } else {
                        fFilter.SourceMode = FilterGroupMode.All;
                        fFilter.SourceRef = "";
                    }
                }

                return true;
            } catch (Exception ex) {
                Logger.LogWrite("TreeFilterDlgController.Accept(): " + ex.Message);
                return false;
            }
        }

        public override void UpdateView()
        {
            GDMTree tree = fBase.Context.Tree;
            fTemp = fFilter.BranchPersons;

            var values = new StringList();
            int num = tree.RecordsCount;
            for (int i = 0; i < num; i++) {
                GDMRecord rec = tree[i];
                if (rec.RecordType == GDMRecordType.rtSource) {
                    values.AddObject((rec as GDMSourceRecord).ShortTitle, rec);
                }
            }
            values.Sort();
            fView.SourceCombo.AddItem(LangMan.LS(LSID.LSID_SrcAll), null);
            fView.SourceCombo.AddItem(LangMan.LS(LSID.LSID_SrcNot), null);
            fView.SourceCombo.AddItem(LangMan.LS(LSID.LSID_SrcAny), null);
            fView.SourceCombo.AddStrings(values);

            UpdateControls();
        }

        public void UpdateControls()
        {
            fView.SetCutModeRadio((int)fFilter.BranchCut);
            fView.YearNum.Enabled = (fFilter.BranchCut == ChartFilter.BranchCutType.Years);
            fView.PersonsList.Enabled = (fFilter.BranchCut == ChartFilter.BranchCutType.Persons);
            fView.YearNum.Text = fFilter.BranchYear.ToString();
            fView.PersonsList.ClearItems();

            if (!string.IsNullOrEmpty(fTemp)) {
                string[] tmpRefs = fTemp.Split(';');

                int num = tmpRefs.Length;
                for (int i = 0; i < num; i++) {
                    string xref = tmpRefs[i];
                    GDMIndividualRecord p = fBase.Context.Tree.XRefIndex_Find(xref) as GDMIndividualRecord;
                    if (p != null) fView.PersonsList.AddItem(p, GKUtils.GetNameString(p, true, false));
                }
            }

            if (fFilter.SourceMode != FilterGroupMode.Selected) {
                fView.SourceCombo.SelectedIndex = (sbyte)fFilter.SourceMode;
            } else {
                GDMSourceRecord srcRec = fBase.Context.Tree.XRefIndex_Find(fFilter.SourceRef) as GDMSourceRecord;
                if (srcRec != null) fView.SourceCombo.Text = srcRec.ShortTitle;
            }
        }

        public void ModifyPersons(RecordAction action, object itemData)
        {
            GDMIndividualRecord iRec = itemData as GDMIndividualRecord;

            switch (action) {
                case RecordAction.raAdd:
                    iRec = fBase.Context.SelectPerson(null, TargetMode.tmNone, GDMSex.svUnknown);
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

            UpdateControls();
        }
    }
}
