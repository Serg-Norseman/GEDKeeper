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

using BSLib;
using GDModel;
using GKCore.MVP;
using GKCore.MVP.Views;
using GKCore.Types;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public class RecMergeController : DialogController<IRecMergeDlg>
    {
        private GDMRecordType fRMMode;
        private readonly StringList fRMSkip;
        private int fRMIndex;

        public GDMRecordType RMMode
        {
            get { return fRMMode; }
            set { fRMMode = value; }
        }

        public RecMergeController(IRecMergeDlg view) : base(view)
        {
            fRMSkip = new StringList();
            fRMMode = GDMRecordType.rtIndividual;
        }

        public override void UpdateView()
        {
        }

        private static bool CheckPersonsEx(GDMIndividualRecord rec1, GDMIndividualRecord rec2)
        {
            GDMFamilyRecord fam1 = rec1.GetParentsFamily();
            GDMFamilyRecord fam2 = rec2.GetParentsFamily();

            return (!Equals(fam1, fam2));
        }

        public void Skip()
        {
            if (fView.MergeCtl.Rec1 != null && fView.MergeCtl.Rec2 != null) {
                fRMSkip.Add(fView.MergeCtl.Rec1.XRef + "-" + fView.MergeCtl.Rec2.XRef);
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
            fView.MergeCtl.Base = fBase;
            fView.MergeCtl.MergeMode = fRMMode;

            fView.MergeCtl.SetRec1(null);
            fView.MergeCtl.SetRec2(null);

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
                    if (iRec.RecordType != fRMMode) continue;

                    for (int j = i + 1; j < recNum; j++) {
                        GDMRecord kRec = tree[j];
                        if (kRec.RecordType != fRMMode) continue;

                        if (iRec == kRec) continue;
                        if (fRMSkip.IndexOf(iRec.XRef + "-" + kRec.XRef) >= 0) continue;

                        res = iRec.IsMatch(kRec, mParams) >= 100.0f;

                        if (res && fRMMode == GDMRecordType.rtIndividual) {
                            res = CheckPersonsEx((GDMIndividualRecord)iRec, (GDMIndividualRecord)kRec);
                        }

                        if (res) {
                            fView.MergeCtl.SetRec1(iRec);
                            fView.MergeCtl.SetRec2(kRec);
                            break;
                        }
                    }

                    if (res) break;
                }
            } finally {
                fView.SkipBtn.Enabled = true;
            }
        }
    }
}
