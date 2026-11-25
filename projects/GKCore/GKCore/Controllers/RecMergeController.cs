/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using System.Threading.Tasks;
using BSLib;
using GDModel;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Locales;
using GKCore.Tools;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public class RecMergeController : DialogController<IRecMergeDlg>
    {
        #region Design components
#pragma warning disable CS0649, IDE0044
        private ITabPage _pageMerge;
        private IButton _btnAutoSearch;
        private IButton _btnSkip;
        private IProgressBar _ProgressBar1;
        private ITabPage _pageMergeOptions;
        private IGroupBox _rgMode;
        private IGroupBox _grpSearchPersons;
        private ILabel _lblNameAccuracy;
        private ILabel _lblYearInaccuracy;
        private INumericBox _edNameAccuracy;
        private INumericBox _edYearInaccuracy;
        private ICheckBox _chkBirthYear;
        private IRadioButton _radPersons;
        private IRadioButton _radNotes;
        private IRadioButton _radFamilies;
        private IRadioButton _radSources;
        private ICheckBox _chkBookmarkMerged;
        private IGroupBox _grpMergeOther;
        private ICheckBox _chkIndistinctMatching;
        private IButton _btnMergeToRight;
        private IButton _btnMergeToLeft;
        private IButton _btnRec2Select;
        private IButton _btnRec1Select;
        private IButton _btnEditLeft;
        private IButton _btnEditRight;
        private ITextBox _Edit2;
        private ITextBox _Edit1;
        private ILabel _Lab2;
        private ILabel _Lab1;
        private IHyperView _fView1;
        private IHyperView _fView2;
#pragma warning restore CS0649, IDE0044
        #endregion

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
        }

        public override void InitView(IBaseWindow baseWin)
        {
            base.InitView(baseWin);

            if (fBase != null) {
                SetMergeMode(GDMRecordType.rtIndividual);
            }

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
            //mParams.IndistinctNameMatching = _chkIndistinctMatching.Checked; // FIXME!
            mParams.IndistinctThreshold = ((float)_edNameAccuracy.Value) / 100.0f;
            mParams.DatesCheck = _chkBirthYear.Checked;
            mParams.YearsInaccuracy = (int)_edYearInaccuracy.Value;
            mParams.CheckEventPlaces = false;

            bool res = false;
            _btnSkip.Enabled = false;

            try {
                int recNum = fFilteredRecords.Count;

                _ProgressBar1.Minimum = 0;
                _ProgressBar1.Maximum = recNum;
                _ProgressBar1.Value = fRMIndex;

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
                    _ProgressBar1.Increment(1);

                    if (res) break;
                }
            } finally {
                _btnSkip.Enabled = true;
            }
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.MergeDuplicates));

            _grpSearchPersons.Text = LangMan.LS(LSID.RM_SearchPersons);
            _grpMergeOther.Text = LangMan.LS(LSID.Other);
            _rgMode.Text = LangMan.LS(LSID.RM_Records);

            _pageMerge.Text = LangMan.LS(LSID.RecMerge);
            _pageMergeOptions.Text = LangMan.LS(LSID.MIOptions);
            _btnAutoSearch.Text = LangMan.LS(LSID.RM_Search);
            _btnSkip.Text = LangMan.LS(LSID.RM_Skip);
            _radPersons.Text = LangMan.LS(LSID.RPIndividuals);
            _radNotes.Text = LangMan.LS(LSID.RPNotes);
            _radFamilies.Text = LangMan.LS(LSID.RPFamilies);
            _radSources.Text = LangMan.LS(LSID.RPSources);
            _chkIndistinctMatching.Text = LangMan.LS(LSID.RM_IndistinctMatching);
            _chkBirthYear.Text = LangMan.LS(LSID.RM_BirthYear);
            _lblNameAccuracy.Text = LangMan.LS(LSID.RM_NameAccuracy);
            _lblYearInaccuracy.Text = LangMan.LS(LSID.RM_YearInaccuracy);
            _chkBookmarkMerged.Text = LangMan.LS(LSID.BookmarkMerged);
            _btnRec1Select.Text = LangMan.LS(LSID.DlgSelect) + @"...";
            _btnRec2Select.Text = LangMan.LS(LSID.DlgSelect) + @"...";
            _btnEditLeft.Text = LangMan.LS(LSID.DoEdit);
            _btnEditRight.Text = LangMan.LS(LSID.DoEdit);
        }

        public override void ApplyTheme()
        {
            // dummy
        }

        public void ChangeOption()
        {
            var mergeMode = GDMRecordType.rtNone;
            if (_radPersons.Checked) mergeMode = GDMRecordType.rtIndividual;
            if (_radNotes.Checked) mergeMode = GDMRecordType.rtNote;
            if (_radFamilies.Checked) mergeMode = GDMRecordType.rtFamily;
            if (_radSources.Checked) mergeMode = GDMRecordType.rtSource;
            SetMergeMode(mergeMode);

            fBookmark = _chkBookmarkMerged.Checked;
        }

        private void UpdateMergeButtons()
        {
            _btnMergeToLeft.Enabled = (fRec1 != null && fRec2 != null);
            _btnMergeToRight.Enabled = (fRec1 != null && fRec2 != null);

            _btnEditLeft.Enabled = _btnMergeToLeft.Enabled;
            _btnEditRight.Enabled = _btnMergeToRight.Enabled;
        }

        public void SetRec1(GDMRecord value)
        {
            fRec1 = value;
            UpdateMergeButtons();

            if (fRec1 == null) {
                _Lab1.Text = @"XXX1";
                _Edit1.Text = "";
                _fView1.Lines.Clear();
            } else {
                _Lab1.Text = fRec1.XRef;
                _Edit1.Text = GKUtils.GetRecordName(fBase.Context.Tree, fRec1, false);
                _fView1.Lines.Assign(fBase.GetRecordContent(fRec1, RecordContentType.Quick));
            }
        }

        public void SetRec2(GDMRecord value)
        {
            fRec2 = value;
            UpdateMergeButtons();

            if (fRec2 == null) {
                _Lab2.Text = @"XXX2";
                _Edit2.Text = "";
                _fView2.Lines.Clear();
            } else {
                _Lab2.Text = fRec2.XRef;
                _Edit2.Text = GKUtils.GetRecordName(fBase.Context.Tree, fRec2, false);
                _fView2.Lines.Assign(fBase.GetRecordContent(fRec2, RecordContentType.Quick));
            }
        }

        public async void SelectRec1()
        {
            GDMRecord irec = await BaseController.SelectRecord(fView, fBase, fMergeMode, null);
            if (irec != null)
                SetRec1(irec);
        }

        public async void SelectRec2()
        {
            GDMRecord irec = await BaseController.SelectRecord(fView, fBase, fMergeMode, null);
            if (irec != null)
                SetRec2(irec);
        }

        public async void MergeToLeft()
        {
            await TreeTools.MergeRecord(fBase.Context, fRec1, fRec2, fBookmark);
            SetRec1(fRec1);
            SetRec2(null);
        }

        public async void MergeToRight()
        {
            await TreeTools.MergeRecord(fBase.Context, fRec2, fRec1, fBookmark);
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
