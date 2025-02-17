/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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

using System.Collections.Generic;
using System.Threading.Tasks;
using BSLib;
using GDModel;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Tools;
using GKCore.Types;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public class RecMergeController : DialogController<IRecMergeDlg>
    {
        private bool fBookmark;
        private List<GDMRecord> fFilteredRecords;
        private GDMRecordType fMergeMode;
        private readonly StringList fRMSkip;
        private int fRMIndex;
        private GDMRecord fRec1;
        private GDMRecord fRec2;

        public RecMergeController(IRecMergeDlg view) : base(view)
        {
            fRMSkip = new StringList();

            SetRec1(null);
            SetRec2(null);
        }

        public override void Init(IBaseWindow baseWin)
        {
            base.Init(baseWin);
            if (fBase != null) {
                SetMergeMode(GDMRecordType.rtIndividual);
            }
        }

        public override void UpdateView()
        {
        }

        private bool CheckPersonsEx(GDMIndividualRecord rec1, GDMIndividualRecord rec2)
        {
            var tree = fBase.Context.Tree;
            GDMFamilyRecord fam1 = tree.GetParentsFamily(rec1);
            GDMFamilyRecord fam2 = tree.GetParentsFamily(rec2);

            return (!Equals(fam1, fam2));
        }

        private void SetMergeMode(GDMRecordType value)
        {
            fMergeMode = value;
            fFilteredRecords = fBase.GetContentList(fMergeMode);
        }

        public void Skip()
        {
            if (fRec1 != null && fRec2 != null) {
                fRMSkip.Add(fRec1.XRef + "-" + fRec2.XRef);
            }
            SearchDuplicates();
        }

        public void Reset()
        {
            fRMIndex = 0;
            fRMSkip.Clear();
        }

        public void SearchDuplicates()
        {
            SetRec1(null);
            SetRec2(null);

            MatchParams mParams;
            //mParams.IndistinctNameMatching = fView.IndistinctMatchingCheck.Checked; // FIXME!
            mParams.NamesIndistinctThreshold = ((float)fView.NameAccuracyNum.Value) / 100.0f;
            mParams.DatesCheck = fView.BirthYearChk.Checked;
            mParams.YearsInaccuracy = (int)fView.YearInaccuracyNum.Value;
            mParams.CheckEventPlaces = false;

            bool res = false;
            fView.SkipBtn.Enabled = false;

            try {
                int recNum = fFilteredRecords.Count;

                fView.ProgressBar.Minimum = 0;
                fView.ProgressBar.Maximum = recNum;
                fView.ProgressBar.Value = fRMIndex;

                for (int i = fRMIndex; i < recNum; i++) {
                    GDMRecord iRec = fFilteredRecords[i];

                    for (int j = i + 1; j < recNum; j++) {
                        GDMRecord kRec = fFilteredRecords[j];

                        if (iRec == kRec) continue;
                        if (fRMSkip.IndexOf(iRec.XRef + "-" + kRec.XRef) >= 0) continue;

                        res = iRec.IsMatch(kRec, mParams) >= 100.0f;

                        if (res && fMergeMode == GDMRecordType.rtIndividual) {
                            res = CheckPersonsEx((GDMIndividualRecord)iRec, (GDMIndividualRecord)kRec);
                        }

                        if (res) {
                            SetRec1(iRec);
                            SetRec2(kRec);
                            break;
                        }
                    }

                    fRMIndex = i;
                    fView.ProgressBar.Increment(1);

                    if (res) break;
                }
            } finally {
                fView.SkipBtn.Enabled = true;
            }
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.MergeDuplicates);

            GetControl<IGroupBox>("grpSearchPersons").Text = LangMan.LS(LSID.RM_SearchPersons);
            GetControl<IGroupBox>("grpMergeOther").Text = LangMan.LS(LSID.Other);
            GetControl<IGroupBox>("rgMode").Text = LangMan.LS(LSID.RM_Records);

            GetControl<ITabPage>("pageMerge").Text = LangMan.LS(LSID.RecMerge);
            GetControl<ITabPage>("pageMergeOptions").Text = LangMan.LS(LSID.MIOptions);
            GetControl<IButton>("btnAutoSearch").Text = LangMan.LS(LSID.RM_Search);
            GetControl<IButton>("btnSkip").Text = LangMan.LS(LSID.RM_Skip);
            GetControl<IRadioButton>("radPersons").Text = LangMan.LS(LSID.RPIndividuals);
            GetControl<IRadioButton>("radNotes").Text = LangMan.LS(LSID.RPNotes);
            GetControl<IRadioButton>("radFamilies").Text = LangMan.LS(LSID.RPFamilies);
            GetControl<IRadioButton>("radSources").Text = LangMan.LS(LSID.RPSources);
            GetControl<ICheckBox>("chkIndistinctMatching").Text = LangMan.LS(LSID.RM_IndistinctMatching);
            GetControl<ICheckBox>("chkBirthYear").Text = LangMan.LS(LSID.RM_BirthYear);
            GetControl<ILabel>("lblNameAccuracy").Text = LangMan.LS(LSID.RM_NameAccuracy);
            GetControl<ILabel>("lblYearInaccuracy").Text = LangMan.LS(LSID.RM_YearInaccuracy);
            GetControl<ICheckBox>("chkBookmarkMerged").Text = LangMan.LS(LSID.BookmarkMerged);
            GetControl<IButton>("btnRec1Select").Text = LangMan.LS(LSID.DlgSelect) + @"...";
            GetControl<IButton>("btnRec2Select").Text = LangMan.LS(LSID.DlgSelect) + @"...";
            GetControl<IButton>("btnEditLeft").Text = LangMan.LS(LSID.DoEdit);
            GetControl<IButton>("btnEditRight").Text = LangMan.LS(LSID.DoEdit);
        }

        public override void ApplyTheme()
        {
            // dummy
        }

        public void ChangeOption()
        {
            var mergeMode = GDMRecordType.rtNone;
            if (GetControl<IRadioButton>("radPersons").Checked) mergeMode = GDMRecordType.rtIndividual;
            if (GetControl<IRadioButton>("radNotes").Checked) mergeMode = GDMRecordType.rtNote;
            if (GetControl<IRadioButton>("radFamilies").Checked) mergeMode = GDMRecordType.rtFamily;
            if (GetControl<IRadioButton>("radSources").Checked) mergeMode = GDMRecordType.rtSource;
            SetMergeMode(mergeMode);

            fBookmark = GetControl<ICheckBox>("chkBookmarkMerged").Checked;
        }

        private void UpdateMergeButtons()
        {
            GetControl<IButton>("btnMergeToLeft").Enabled = (fRec1 != null && fRec2 != null);
            GetControl<IButton>("btnMergeToRight").Enabled = (fRec1 != null && fRec2 != null);

            GetControl<IButton>("btnEditLeft").Enabled = GetControl<IButton>("btnMergeToLeft").Enabled;
            GetControl<IButton>("btnEditRight").Enabled = GetControl<IButton>("btnMergeToRight").Enabled;
        }

        public void SetRec1(GDMRecord value)
        {
            fRec1 = value;
            UpdateMergeButtons();

            if (fRec1 == null) {
                GetControl<ILabel>("Lab1").Text = @"XXX1";
                GetControl<ITextBox>("Edit1").Text = "";
                fView.View1.Lines.Clear();
            } else {
                GetControl<ILabel>("Lab1").Text = fRec1.XRef;
                GetControl<ITextBox>("Edit1").Text = GKUtils.GetRecordName(fBase.Context.Tree, fRec1, false);
                fView.View1.Lines.Assign(fBase.GetRecordContent(fRec1, RecordContentType.Quick));
            }
        }

        public void SetRec2(GDMRecord value)
        {
            fRec2 = value;
            UpdateMergeButtons();

            if (fRec2 == null) {
                GetControl<ILabel>("Lab2").Text = @"XXX2";
                GetControl<ITextBox>("Edit2").Text = "";
                fView.View2.Lines.Clear();
            } else {
                GetControl<ILabel>("Lab2").Text = fRec2.XRef;
                GetControl<ITextBox>("Edit2").Text = GKUtils.GetRecordName(fBase.Context.Tree, fRec2, false);
                fView.View2.Lines.Assign(fBase.GetRecordContent(fRec2, RecordContentType.Quick));
            }
        }

        public async void SelectRec1()
        {
            GDMRecord irec = await fBase.Context.SelectRecord(fView, fMergeMode, null);
            if (irec != null)
                SetRec1(irec);
        }

        public async void SelectRec2()
        {
            GDMRecord irec = await fBase.Context.SelectRecord(fView, fMergeMode, null);
            if (irec != null)
                SetRec2(irec);
        }

        public async void MergeToLeft()
        {
            await TreeTools.MergeRecord(fBase, fRec1, fRec2, fBookmark);
            SetRec1(fRec1);
            SetRec2(null);
        }

        public async void MergeToRight()
        {
            await TreeTools.MergeRecord(fBase, fRec2, fRec1, fBookmark);
            SetRec1(null);
            SetRec2(fRec2);
        }

        private async Task<bool> EditRecord(GDMRecord record)
        {
            return (record != null && await BaseController.EditRecord(fView, fBase, record));
        }

        public async void EditLeft()
        {
            if (await EditRecord(fRec1))
                SetRec1(fRec1);
        }

        public async void EditRight()
        {
            if (await EditRecord(fRec2))
                SetRec2(fRec2);
        }
    }
}
