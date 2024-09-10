/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using System.Collections.Generic;
using BSLib;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Export;
using GKCore.Interfaces;

namespace GKCore.Stats
{

    /// <summary>
    /// Class for calculate various types of statistics.
    /// </summary>
    public sealed class TreeStats
    {
        private readonly IBaseContext fContext;
        private readonly GDMTree fTree;
        private readonly List<GDMRecord> fSelectedRecords;

        public TreeStats(IBaseContext context, List<GDMRecord> selectedRecords)
        {
            fContext = context;
            fTree = context.Tree;
            fSelectedRecords = selectedRecords;
        }

        public CommonStats GetCommonStats()
        {
            CommonStats stats = new CommonStats();

            int num = fSelectedRecords.Count;
            for (int i = 0; i < num; i++) {
                GDMRecord rec = fSelectedRecords[i];
                if (rec.RecordType != GDMRecordType.rtIndividual) continue;

                GDMIndividualRecord ind = (GDMIndividualRecord)rec;
                stats.persons++;

                switch (ind.Sex) {
                    case GDMSex.svFemale:
                        stats.persons_f++;
                        if (ind.IsLive()) {
                            stats.lives_f++;
                            stats.lives++;
                        }
                        break;

                    case GDMSex.svMale:
                        stats.persons_m++;
                        if (ind.IsLive()) {
                            stats.lives_m++;
                            stats.lives++;
                        }
                        break;
                }

                string vAge = GKUtils.GetAgeStr(ind, -1);
                stats.age.TakeVal(vAge, ind.Sex, true);

                string vLife = GKUtils.GetLifeExpectancyStr(ind);
                stats.life.TakeVal(vLife, ind.Sex, true);

                int chCnt = fTree.GetTotalChildrenCount(ind);
                stats.childs.TakeVal(chCnt, ind.Sex, true);

                int vFba = GKUtils.GetFirstbornAge(ind, GKUtils.GetFirstborn(fTree, ind));
                stats.fba.TakeVal(vFba, ind.Sex, true);

                int mCnt = GKUtils.GetMarriagesCount(ind);
                stats.marr.TakeVal(mCnt, ind.Sex, true);

                int vMAge = GKUtils.GetMarriageAge(fTree, ind);
                stats.mage.TakeVal(vMAge, ind.Sex, true);

                float vCI = GKUtils.GetCertaintyAssessment(ind);
                stats.cIndex.TakeVal(vCI, ind.Sex, false);
            }

            return stats;
        }

        private static void CheckVal(List<StatsItem> valsList, string val, GDMSex sex = GDMSex.svUnknown)
        {
            if (sex == GDMSex.svUnknown) {
                if (val == "-1" || val == "" || val == "0") {
                    val = "?";
                }
            }

            int vIdx = valsList.FindIndex(delegate (StatsItem lv) { return (lv.Caption == val); });

            StatsItem lvi;
            if (vIdx == -1) {
                lvi = new StatsItem(val, sex != GDMSex.svUnknown);
                valsList.Add(lvi);
            } else {
                lvi = valsList[vIdx];
            }

            switch (sex) {
                case GDMSex.svFemale:
                    lvi.ValF = lvi.ValF + 1;
                    break;

                case GDMSex.svMale:
                    lvi.ValM = lvi.ValM + 1;
                    break;

                case GDMSex.svUnknown:
                case GDMSex.svIntersex:
                    lvi.Value = lvi.Value + 1;
                    break;
            }
        }

        private void GetEventField(StatsMode mode, List<StatsItem> values, GDMIndividualRecord iRec, string evtName)
        {
            string v = "?";
            GDMCustomEvent evt = iRec.FindEvent(evtName);
            if (evt == null) return;

            int dtY = evt.GetChronologicalYear();
            if (dtY == 0 && (mode != StatsMode.smBirthPlaces && mode != StatsMode.smDeathPlaces)) return;

            switch (mode) {
                case StatsMode.smBirthYears:
                case StatsMode.smDeathYears:
                    v = Convert.ToString(dtY);
                    break;

                case StatsMode.smBirthTenYears:
                case StatsMode.smDeathTenYears:
                    v = Convert.ToString(dtY / 10 * 10);
                    break;

                case StatsMode.smBirthPlaces:
                case StatsMode.smDeathPlaces:
                    if (evt.HasPlace) {
                        v = evt.Place.StringValue;
                    }
                    break;
            }

            CheckVal(values, v);
        }

        private void GetIndiName(StatsMode mode, List<StatsItem> values, GDMIndividualRecord iRec)
        {
            string v = "";
            var parts = GKUtils.GetNameParts(fTree, iRec);

            switch (mode) {
                case StatsMode.smSurnames:
                    v = fContext.Culture.NormalizeSurname(parts.Surname, iRec.Sex == GDMSex.svFemale);
                    break;

                case StatsMode.smNames:
                    v = parts.Name;
                    break;

                case StatsMode.smPatronymics:
                    v = parts.Patronymic;
                    break;
            }

            CheckVal(values, v);
        }

        private void GetSimplePersonStat(StatsMode mode, List<StatsItem> values, GDMIndividualRecord iRec)
        {
            string iName = GKUtils.GetNameString(iRec, true, false);

            switch (mode) {
                case StatsMode.smAncestors:
                    values.Add(new StatsItem(iName, GKUtils.GetAncestorsCount(fTree, iRec) - 1));
                    break;

                case StatsMode.smDescendants:
                    values.Add(new StatsItem(iName, GKUtils.GetDescendantsCount(fTree, iRec) - 1));
                    break;

                case StatsMode.smDescGenerations:
                    values.Add(new StatsItem(iName, GKUtils.GetDescGenerations(fTree, iRec)));
                    break;

                case StatsMode.smChildsCount:
                    values.Add(new StatsItem(iName, fTree.GetTotalChildrenCount(iRec)));
                    break;

                case StatsMode.smFirstbornAge:
                    values.Add(new StatsItem(iName, GKUtils.GetFirstbornAge(iRec, GKUtils.GetFirstborn(fTree, iRec))));
                    break;

                case StatsMode.smMarriages:
                    values.Add(new StatsItem(iName, GKUtils.GetMarriagesCount(iRec)));
                    break;

                case StatsMode.smMarriageAge:
                    values.Add(new StatsItem(iName, GKUtils.GetMarriageAge(fTree, iRec)));
                    break;

                case StatsMode.smSurnames:
                case StatsMode.smNames:
                case StatsMode.smPatronymics:
                    GetIndiName(mode, values, iRec);
                    break;

                case StatsMode.smAge:
                    CheckVal(values, GKUtils.GetAgeStr(iRec, -1));
                    break;

                case StatsMode.smLifeExpectancy:
                    CheckVal(values, GKUtils.GetLifeExpectancyStr(iRec));
                    break;

                case StatsMode.smBirthYears:
                case StatsMode.smBirthTenYears:
                case StatsMode.smBirthPlaces:
                    GetEventField(mode, values, iRec, GEDCOMTagName.BIRT);
                    break;

                case StatsMode.smDeathYears:
                case StatsMode.smDeathTenYears:
                case StatsMode.smDeathPlaces:
                    GetEventField(mode, values, iRec, GEDCOMTagName.DEAT);
                    break;

                case StatsMode.smChildsDistribution:
                    CheckVal(values, fTree.GetTotalChildrenCount(iRec).ToString());
                    break;

                case StatsMode.smResidences:
                    CheckVal(values, GKUtils.GetResidencePlace(iRec, false));
                    break;

                case StatsMode.smOccupation:
                    CheckVal(values, GKUtils.GetAttributeValue(iRec, GEDCOMTagName.OCCU));
                    break;

                case StatsMode.smReligious:
                    CheckVal(values, GKUtils.GetAttributeValue(iRec, GEDCOMTagName.RELI));
                    break;

                case StatsMode.smNational:
                    CheckVal(values, GKUtils.GetAttributeValue(iRec, GEDCOMTagName.NATI));
                    break;

                case StatsMode.smEducation:
                    CheckVal(values, GKUtils.GetAttributeValue(iRec, GEDCOMTagName.EDUC));
                    break;

                case StatsMode.smCaste:
                    CheckVal(values, GKUtils.GetAttributeValue(iRec, GEDCOMTagName.CAST));
                    break;

                case StatsMode.smHobby:
                    CheckVal(values, GKUtils.GetAttributeValue(iRec, GEDCOMTagName._HOBBY));
                    break;

                case StatsMode.smAward:
                    CheckVal(values, GKUtils.GetAttributeValue(iRec, GEDCOMTagName._AWARD));
                    break;

                case StatsMode.smMili:
                    CheckVal(values, GKUtils.GetAttributeValue(iRec, GEDCOMTagName._MILI));
                    break;

                case StatsMode.smMiliInd:
                    CheckVal(values, GKUtils.GetAttributeValue(iRec, GEDCOMTagName._MILI_IND));
                    break;

                case StatsMode.smMiliDis:
                    CheckVal(values, GKUtils.GetAttributeValue(iRec, GEDCOMTagName._MILI_DIS));
                    break;

                case StatsMode.smMiliRank:
                    CheckVal(values, GKUtils.GetAttributeValue(iRec, GEDCOMTagName._MILI_RANK));
                    break;

                case StatsMode.smCertaintyIndex:
                    CheckVal(values, string.Format("{0:0.00}", GKUtils.GetCertaintyAssessment(iRec)));
                    break;

                case StatsMode.smBirthByMonth:
                    GDMCustomEvent ev = iRec.FindEvent(GEDCOMTagType.BIRT);
                    if (ev != null) {
                        var dtx = ev.Date.Value as GDMDate;
                        if (dtx != null && dtx.Month > 0) {
                            CheckVal(values, dtx.Month.ToString());
                        }
                    }
                    break;

                case StatsMode.smDemography: {
                        int lifeExp = GKUtils.GetLifeExpectancy(iRec);
                        if (lifeExp > -1) {
                            string v = Convert.ToString(lifeExp / 5 * 5);
                            CheckVal(values, v, iRec.Sex);
                        }
                        break;
                    }
            }
        }

        public void GetSpecStats(StatsMode mode, List<StatsItem> values)
        {
            if (values == null)
                throw new ArgumentNullException("values");

            // special buffers for difficult calculations with averaged ages
            var xvals = new Dictionary<string, List<int>>();

            int num = fTree.RecordsCount;
            for (int i = 0; i < num; i++) {
                GDMRecord rec = fTree[i];

                if (rec.RecordType == GDMRecordType.rtIndividual && mode != StatsMode.smSpousesDiff && fSelectedRecords.Contains(rec)) {
                    GDMIndividualRecord iRec = rec as GDMIndividualRecord;

                    switch (mode) {
                        case StatsMode.smAAF_1:
                        case StatsMode.smAAF_2:
                            ProcessAAFBuffer(mode, xvals, iRec);
                            break;

                        case StatsMode.smParentsAge:
                            ProcessParentsAgeStats(values, iRec);
                            break;

                        default:
                            GetSimplePersonStat(mode, values, iRec);
                            break;
                    }
                } else if (rec.RecordType == GDMRecordType.rtFamily && mode == StatsMode.smSpousesDiff) {
                    GDMFamilyRecord fRec = rec as GDMFamilyRecord;

                    int diff = GKUtils.GetSpousesDiff(fTree, fRec);
                    if (diff != -1) {
                        values.Add(new StatsItem(GKUtils.GetFamilyString(fTree, fRec), diff));
                    }
                }
            }

            if (mode == StatsMode.smAAF_1 || mode == StatsMode.smAAF_2) {
                CalcAAFResult(values, xvals);
            }
        }

        private void ProcessParentsAgeStats(List<StatsItem> values, GDMIndividualRecord iRec)
        {
            GDMCustomEvent evt = iRec.FindEvent(GEDCOMTagName.BIRT);
            if (evt == null) return;

            int parentYear = evt.GetChronologicalYear();
            if (parentYear == 0) return;

            int num = iRec.SpouseToFamilyLinks.Count;
            for (int i = 0; i < num; i++) {
                GDMFamilyRecord family = fTree.GetPtrValue(iRec.SpouseToFamilyLinks[i]);
                if (family == null) continue;

                int num2 = family.Children.Count;
                for (int j = 0; j < num2; j++) {
                    GDMIndividualRecord child = fTree.GetPtrValue(family.Children[j]);
                    if (child == null) continue;

                    evt = child.FindEvent(GEDCOMTagType.BIRT);
                    if (evt == null) continue;

                    int childYear = evt.GetChronologicalYear();

                    if (parentYear != 0 && childYear != 0) {
                        int age = (childYear - parentYear);

                        // exclude errors
                        if (age >= GKData.MIN_PARENT_AGE) {
                            CheckVal(values, age.ToString(), iRec.Sex);
                        }
                    }
                }
            }
        }

        private void ProcessAAFBuffer(StatsMode mode, Dictionary<string, List<int>> xvals, GDMIndividualRecord iRec)
        {
            GDMIndividualRecord iChild = GKUtils.GetFirstborn(fTree, iRec);
            int fba = GKUtils.GetFirstbornAge(iRec, iChild);
            if (fba > 0 && iChild != null) {
                string key;
                List<int> valsList;

                switch (mode) {
                    case StatsMode.smAAF_1:
                        int dty1 = iRec.GetChronologicalYear(GEDCOMTagName.BIRT);
                        if (dty1 != 0) {
                            key = MathHelper.Trunc(dty1 / 10 * 10).ToString();

                            if (!xvals.TryGetValue(key, out valsList)) {
                                valsList = new List<int>();
                                xvals.Add(key, valsList);
                            }
                            valsList.Add(fba);
                        }

                        break;

                    case StatsMode.smAAF_2:
                        int dty2 = iChild.GetChronologicalYear(GEDCOMTagName.BIRT);
                        if (dty2 != 0) {
                            key = MathHelper.Trunc(dty2 / 10 * 10).ToString();

                            if (!xvals.TryGetValue(key, out valsList)) {
                                valsList = new List<int>();
                                xvals.Add(key, valsList);
                            }
                            valsList.Add(fba);
                        }

                        break;
                }
            }
        }

        private static void CalcAAFResult(List<StatsItem> values, Dictionary<string, List<int>> xvals)
        {
            foreach (KeyValuePair<string, List<int>> kvp in xvals) {
                List<int> valsList = kvp.Value;
                int count = valsList.Count;

                int avg;
                if (count == 0) {
                    avg = 0;
                } else {
                    double sum = 0;
                    for (int i = 0; i < count; i++) sum += valsList[i];
                    avg = (int)Math.Round(sum / count);
                }

                values.Add(new StatsItem(kvp.Key, avg));
            }
        }

        public void WriteStatsReport(string title, string cap1, string cap2, List<StatsItem> vals, string fileName, TableWriter writer, IProgressController progress)
        {
            if (vals == null) return;

            try {
                int rowsCount = vals.Count;
                progress.Begin(LangMan.LS(LSID.MIExport) + "...", rowsCount);

                try {
                    writer.BeginWrite();

                    writer.BeginTable(2, rowsCount);
                    writer.AddTableCell(cap1);
                    writer.AddTableCell(cap2);
                    for (int i = 0; i < rowsCount; i++) {
                        StatsItem item = vals[i];
                        writer.AddTableCell(item.Caption);
                        writer.AddTableCell(item.GetDisplayString());
                        progress.Increment();
                    }
                    writer.EndTable();

                    writer.EndWrite();

                    GKUtils.LoadExtFile(fileName);
                } finally {
                    progress.End();
                }
            } catch (Exception ex) {
                Logger.WriteError("TreeStats.WriteStatsReport()", ex);
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.UploadErrorInExcel));
            }
        }
    }
}
