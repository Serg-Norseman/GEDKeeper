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
using Eto.Forms;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKUI.Components;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class TTRecMergeDlg : Dialog
    {
        private readonly IBaseWindow fBase;
        private readonly GEDCOMTree fTree;
        private GEDCOMRecordType fRMMode;
        private readonly StringList fRMSkip;
        private int fRMIndex;

        public TTRecMergeDlg(IBaseWindow baseWin)
        {
            InitializeComponent();
            btnClose.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            fBase = baseWin;
            fTree = fBase.Context.Tree;
            fRMSkip = new StringList();
            fRMMode = GEDCOMRecordType.rtIndividual;

            MergeCtl.Base = fBase;
            MergeCtl.MergeMode = fRMMode;

            SetLang();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fRMSkip.Dispose();
            }
            base.Dispose(disposing);
        }

        public void SetLang()
        {
            Title = LangMan.LS(LSID.LSID_MITreeTools);
            pageRecMerge.Text = LangMan.LS(LSID.LSID_ToolOp_4);
            btnClose.Text = LangMan.LS(LSID.LSID_DlgClose);
            pageMerge.Text = LangMan.LS(LSID.LSID_RecMerge);
            pageMergeOptions.Text = LangMan.LS(LSID.LSID_MIOptions);
            btnAutoSearch.Text = LangMan.LS(LSID.LSID_RM_Search);
            btnSkip.Text = LangMan.LS(LSID.LSID_RM_Skip);
            rgMode.Text = LangMan.LS(LSID.LSID_RM_Records);
            radPersons.Text = LangMan.LS(LSID.LSID_RPIndividuals);
            radNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
            radFamilies.Text = LangMan.LS(LSID.LSID_RPFamilies);
            radSources.Text = LangMan.LS(LSID.LSID_RPSources);
            grpSearchPersons.Text = LangMan.LS(LSID.LSID_RM_SearchPersons);
            chkIndistinctMatching.Text = LangMan.LS(LSID.LSID_RM_IndistinctMatching);
            chkBirthYear.Text = LangMan.LS(LSID.LSID_RM_BirthYear);
            lblNameAccuracy.Text = LangMan.LS(LSID.LSID_RM_NameAccuracy);
            lblYearInaccuracy.Text = LangMan.LS(LSID.LSID_RM_YearInaccuracy);
            grpMergeOther.Text = LangMan.LS(LSID.LSID_Other);
            chkBookmarkMerged.Text = LangMan.LS(LSID.LSID_BookmarkMerged);
        }

        private static bool CheckPersonsEx(GEDCOMIndividualRecord rec1, GEDCOMIndividualRecord rec2)
        {
            GEDCOMFamilyRecord fam1 = rec1.GetParentsFamily();
            GEDCOMFamilyRecord fam2 = rec2.GetParentsFamily();

            return (!Equals(fam1, fam2));
        }

        private void SearchDuplicates()
        {
            MergeCtl.Base = fBase;
            MergeCtl.MergeMode = fRMMode;
            
            MergeCtl.SetRec1(null);
            MergeCtl.SetRec2(null);
            
            MatchParams mParams;
            //mParams.IndistinctNameMatching = chkIndistinctMatching.Checked;
            mParams.NamesIndistinctThreshold = (float)(edNameAccuracy.Value / 100.0f);
            mParams.DatesCheck = chkBirthYear.Checked.GetValueOrDefault();
            mParams.YearsInaccuracy = (int)edYearInaccuracy.Value;
            mParams.CheckEventPlaces = false;

            bool res = false;
            btnSkip.Enabled = false;

            try {
                ProgressBar1.MinValue = 0;
                ProgressBar1.MaxValue = fTree.RecordsCount;
                ProgressBar1.Value = fRMIndex;

                int recNum = fTree.RecordsCount;
                for (int i = fRMIndex; i < recNum; i++) {
                    fRMIndex = i;
                    ProgressBar1.Value += 1;

                    GEDCOMRecord iRec = fTree[i];
                    if (iRec.RecordType != fRMMode) continue;

                    for (int j = i + 1; j < recNum; j++) {
                        GEDCOMRecord kRec = fTree[j];
                        if (kRec.RecordType != fRMMode) continue;

                        if (iRec == kRec) continue;
                        if (fRMSkip.IndexOf(iRec.XRef + "-" + kRec.XRef) >= 0) continue;

                        res = iRec.IsMatch(kRec, mParams) >= 100.0f;

                        if (res && fRMMode == GEDCOMRecordType.rtIndividual) {
                            res = CheckPersonsEx((GEDCOMIndividualRecord)iRec, (GEDCOMIndividualRecord)kRec);
                        }

                        if (res) {
                            MergeCtl.SetRec1(iRec);
                            MergeCtl.SetRec2(kRec);
                            break;
                        }
                    }

                    if (res) break;
                }
            } finally {
                btnSkip.Enabled = true;
            }
        }

        private void radMergeMode_Click(object sender, EventArgs e)
        {
            if (radPersons.Checked) fRMMode = GEDCOMRecordType.rtIndividual;
            if (radNotes.Checked) fRMMode = GEDCOMRecordType.rtNote;
            if (radFamilies.Checked) fRMMode = GEDCOMRecordType.rtFamily;
            if (radSources.Checked) fRMMode = GEDCOMRecordType.rtSource;

            MergeCtl.MergeMode = fRMMode;
        }

        private void chkBookmarkMerged_CheckedChanged(object sender, EventArgs e)
        {
            MergeCtl.Bookmark = chkBookmarkMerged.Checked.GetValueOrDefault();
        }

        private void btnSkip_Click(object sender, EventArgs e)
        {
            if (MergeCtl.Rec1 != null && MergeCtl.Rec2 != null) {
                fRMSkip.Add(MergeCtl.Rec1.XRef + "-" + MergeCtl.Rec2.XRef);
            }
            SearchDuplicates();
        }

        private void btnSearch_Click(object sender, EventArgs e)
        {
            fRMIndex = 0;
            fRMSkip.Clear();
            SearchDuplicates();
        }
    }
}
