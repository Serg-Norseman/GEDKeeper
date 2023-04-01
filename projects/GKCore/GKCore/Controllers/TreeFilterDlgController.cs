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
using BSLib;
using GDModel;
using GKCore.Charts;
using GKCore.Design.Controls;
using GKCore.Lists;
using GKCore.Design;
using GKCore.Design.Views;
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
            fView.PersonsList.Buttons = EnumSet<SheetButton>.Create(SheetButton.lbAdd, SheetButton.lbDelete);
        }

        public override bool Accept()
        {
            try {
                fFilter.BranchCut = GetCutModeRadio();
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
                    GDMRecord rec = fView.SourceCombo.GetSelectedTag<GDMRecord>();
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
                Logger.WriteError("TreeFilterDlgController.Accept()", ex);
                return false;
            }
        }

        public override bool Cancel()
        {
            fFilter.Reset();
            return true;
        }

        public override void UpdateView()
        {
            GDMTree tree = fBase.Context.Tree;
            fTemp = fFilter.BranchPersons;

            fView.SourceCombo.Clear();
            fView.SourceCombo.AddItem<GDMRecord>(LangMan.LS(LSID.LSID_SrcAll), null);
            fView.SourceCombo.AddItem<GDMRecord>(LangMan.LS(LSID.LSID_SrcNot), null);
            fView.SourceCombo.AddItem<GDMRecord>(LangMan.LS(LSID.LSID_SrcAny), null);
            var sources = GKUtils.GetSources(tree);
            foreach (var item in sources) {
                fView.SourceCombo.AddItem<GDMRecord>(item.ShortTitle, item);
            }

            UpdateControls();
        }

        public void UpdateControls()
        {
            SetCutModeRadio(fFilter.BranchCut);
            fView.YearNum.Enabled = (fFilter.BranchCut == ChartFilter.BranchCutType.Years);
            fView.PersonsList.Enabled = (fFilter.BranchCut == ChartFilter.BranchCutType.Persons);
            fView.YearNum.Text = fFilter.BranchYear.ToString();
            fView.PersonsList.ListView.ClearItems();

            if (!string.IsNullOrEmpty(fTemp)) {
                string[] tmpRefs = fTemp.Split(';');

                int num = tmpRefs.Length;
                for (int i = 0; i < num; i++) {
                    var p = fBase.Context.Tree.FindXRef<GDMIndividualRecord>(tmpRefs[i]);
                    if (p != null) fView.PersonsList.ListView.AddItem(p, GKUtils.GetNameString(p, true, false));
                }
            }

            if (fFilter.SourceMode != FilterGroupMode.Selected) {
                fView.SourceCombo.SelectedIndex = (sbyte)fFilter.SourceMode;
            } else {
                var srcRec = fBase.Context.Tree.FindXRef<GDMSourceRecord>(fFilter.SourceRef);
                if (srcRec != null) fView.SourceCombo.Text = srcRec.ShortTitle;
            }
        }

        public void ModifyPersons(RecordAction action, object itemData)
        {
            GDMIndividualRecord iRec = itemData as GDMIndividualRecord;

            switch (action) {
                case RecordAction.raAdd:
                    iRec = fBase.Context.SelectPerson(fView, null, TargetMode.tmNone, GDMSex.svUnknown);
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


        private void SetCutModeRadio(ChartFilter.BranchCutType cutMode)
        {
            switch (cutMode) {
                case ChartFilter.BranchCutType.None:
                    GetControl<IRadioButton>("rbCutNone").Checked = true;
                    break;
                case ChartFilter.BranchCutType.Years:
                    GetControl<IRadioButton>("rbCutYears").Checked = true;
                    break;
                case ChartFilter.BranchCutType.Persons:
                    GetControl<IRadioButton>("rbCutPersons").Checked = true;
                    break;
            }
        }

        private ChartFilter.BranchCutType GetCutModeRadio()
        {
            var cutMode = ChartFilter.BranchCutType.None;
            if (GetControl<IRadioButton>("rbCutNone").Checked)
                cutMode = ChartFilter.BranchCutType.None;
            if (GetControl<IRadioButton>("rbCutYears").Checked)
                cutMode = ChartFilter.BranchCutType.Years;
            if (GetControl<IRadioButton>("rbCutPersons").Checked)
                cutMode = ChartFilter.BranchCutType.Persons;
            return cutMode;
        }

        public void ChangeCutMode()
        {
            fFilter.BranchCut = GetCutModeRadio();
            UpdateControls();
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.LSID_MIFilter);

            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.LSID_DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.LSID_DlgCancel);
            GetControl<IGroupBox>("rgBranchCut").Text = LangMan.LS(LSID.LSID_BranchCut);
            GetControl<IRadioButton>("rbCutNone").Text = LangMan.LS(LSID.LSID_Not);
            GetControl<IRadioButton>("rbCutYears").Text = LangMan.LS(LSID.LSID_BCut_Years);
            GetControl<ILabel>("lblYear").Text = LangMan.LS(LSID.LSID_Year);
            GetControl<IRadioButton>("rbCutPersons").Text = LangMan.LS(LSID.LSID_BCut_Persons);
            GetControl<ILabel>("lblRPSources").Text = LangMan.LS(LSID.LSID_RPSources);

            fView.PersonsList.ListView.AddColumn(LangMan.LS(LSID.LSID_RPIndividuals), 350, false);
        }
    }
}
