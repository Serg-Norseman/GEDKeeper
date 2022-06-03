﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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

using BSLib;
using BSLib.Design.MVP.Controls;
using GDModel;
using GKCore.MVP;
using GKCore.MVP.Views;
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
        private GDMRecordType fMergeMode;
        private readonly StringList fRMSkip;
        private int fRMIndex;
        private GDMRecord fRec1;
        private GDMRecord fRec2;

        public RecMergeController(IRecMergeDlg view) : base(view)
        {
            fRMSkip = new StringList();
            fMergeMode = GDMRecordType.rtIndividual;

            SetRec1(null);
            SetRec2(null);
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
                var tree = fBase.Context.Tree;

                fView.ProgressBar.Minimum = 0;
                fView.ProgressBar.Maximum = tree.RecordsCount;
                fView.ProgressBar.Value = fRMIndex;

                int recNum = tree.RecordsCount;
                for (int i = fRMIndex; i < recNum; i++) {
                    fRMIndex = i;
                    fView.ProgressBar.Increment(1);

                    GDMRecord iRec = tree[i];
                    if (iRec.RecordType != fMergeMode) continue;

                    for (int j = i + 1; j < recNum; j++) {
                        GDMRecord kRec = tree[j];
                        if (kRec.RecordType != fMergeMode) continue;

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

                    if (res) break;
                }
            } finally {
                fView.SkipBtn.Enabled = true;
            }
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.LSID_ToolOp_4);

            GetControl<ITabPage>("pageMerge").Text = LangMan.LS(LSID.LSID_RecMerge);
            GetControl<ITabPage>("pageMergeOptions").Text = LangMan.LS(LSID.LSID_MIOptions);
            GetControl<IButton>("btnAutoSearch").Text = LangMan.LS(LSID.LSID_RM_Search);
            GetControl<IButton>("btnSkip").Text = LangMan.LS(LSID.LSID_RM_Skip);
            GetControl<IGroupBox>("rgMode").Text = LangMan.LS(LSID.LSID_RM_Records);
            GetControl<IRadioButton>("radPersons").Text = LangMan.LS(LSID.LSID_RPIndividuals);
            GetControl<IRadioButton>("radNotes").Text = LangMan.LS(LSID.LSID_RPNotes);
            GetControl<IRadioButton>("radFamilies").Text = LangMan.LS(LSID.LSID_RPFamilies);
            GetControl<IRadioButton>("radSources").Text = LangMan.LS(LSID.LSID_RPSources);
            GetControl<IGroupBox>("grpSearchPersons").Text = LangMan.LS(LSID.LSID_RM_SearchPersons);
            GetControl<ICheckBox>("chkIndistinctMatching").Text = LangMan.LS(LSID.LSID_RM_IndistinctMatching);
            GetControl<ICheckBox>("chkBirthYear").Text = LangMan.LS(LSID.LSID_RM_BirthYear);
            GetControl<ILabel>("lblNameAccuracy").Text = LangMan.LS(LSID.LSID_RM_NameAccuracy);
            GetControl<ILabel>("lblYearInaccuracy").Text = LangMan.LS(LSID.LSID_RM_YearInaccuracy);
            GetControl<IGroupBox>("grpMergeOther").Text = LangMan.LS(LSID.LSID_Other);
            GetControl<ICheckBox>("chkBookmarkMerged").Text = LangMan.LS(LSID.LSID_BookmarkMerged);
            GetControl<IButton>("btnRec1Select").Text = LangMan.LS(LSID.LSID_DlgSelect) + @"...";
            GetControl<IButton>("btnRec2Select").Text = LangMan.LS(LSID.LSID_DlgSelect) + @"...";
        }

        public void ChangeOption()
        {
            if (GetControl<IRadioButton>("radPersons").Checked) fMergeMode = GDMRecordType.rtIndividual;
            if (GetControl<IRadioButton>("radNotes").Checked) fMergeMode = GDMRecordType.rtNote;
            if (GetControl<IRadioButton>("radFamilies").Checked) fMergeMode = GDMRecordType.rtFamily;
            if (GetControl<IRadioButton>("radSources").Checked) fMergeMode = GDMRecordType.rtSource;

            fBookmark = GetControl<ICheckBox>("chkBookmarkMerged").Checked;
        }

        private void UpdateMergeButtons()
        {
            GetControl<IButton>("btnMergeToLeft").Enabled = (fRec1 != null && fRec2 != null);
            GetControl<IButton>("btnMergeToRight").Enabled = (fRec1 != null && fRec2 != null);
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
                fView.View1.Lines.Assign(fBase.GetRecordContent(fRec1));
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
                fView.View2.Lines.Assign(fBase.GetRecordContent(fRec2));
            }
        }

        public void SelectRec1()
        {
            GDMRecord irec = fBase.Context.SelectRecord(fMergeMode, null);
            if (irec != null)
                SetRec1(irec);
        }

        public void SelectRec2()
        {
            GDMRecord irec = fBase.Context.SelectRecord(fMergeMode, null);
            if (irec != null)
                SetRec2(irec);
        }

        public void MergeToLeft()
        {
            TreeTools.MergeRecord(fBase, fRec1, fRec2, fBookmark);
            SetRec1(fRec1);
            SetRec2(null);
        }

        public void MergeToRight()
        {
            TreeTools.MergeRecord(fBase, fRec2, fRec1, fBookmark);
            SetRec1(null);
            SetRec2(fRec2);
        }
    }
}
