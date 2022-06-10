/*
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

using System.Collections.Generic;
using GDModel;
using GKCore.Interfaces;
using GKCore.Options;

namespace GKCore.Export
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class ExcelExporter : Exporter
    {
        private readonly List<GDMRecord> fSelectedRecords;

        public ExcelExporter(IBaseWindow baseWin) : base(baseWin)
        {
            fSelectedRecords = baseWin.GetContentList(GDMRecordType.rtIndividual);
        }

#if !NETSTANDARD
        private void GenerateInt(IProgressController progress)
        {
            int recordsCount = fTree.RecordsCount;
            progress.Begin(LangMan.LS(LSID.LSID_MIExport) + "...", recordsCount);
            try {
                fWriter.BeginTable(12, recordsCount + 1);

                fWriter.AddTableCell("№");
                fWriter.AddTableCell(LangMan.LS(LSID.LSID_Surname));
                fWriter.AddTableCell(LangMan.LS(LSID.LSID_Name));
                fWriter.AddTableCell(LangMan.LS(LSID.LSID_Patronymic));
                fWriter.AddTableCell(LangMan.LS(LSID.LSID_Sex));
                fWriter.AddTableCell(LangMan.LS(LSID.LSID_BirthDate));
                fWriter.AddTableCell(LangMan.LS(LSID.LSID_DeathDate));
                fWriter.AddTableCell(LangMan.LS(LSID.LSID_BirthPlace));
                fWriter.AddTableCell(LangMan.LS(LSID.LSID_DeathPlace));
                fWriter.AddTableCell(LangMan.LS(LSID.LSID_Residence));
                fWriter.AddTableCell(LangMan.LS(LSID.LSID_Age));
                fWriter.AddTableCell(LangMan.LS(LSID.LSID_LifeExpectancy));

                var dateFormat = GlobalOptions.Instance.DefDateFormat;
                for (int i = 0; i < recordsCount; i++) {
                    GDMRecord rec = fTree[i];
                    if (rec.RecordType == GDMRecordType.rtIndividual) {
                        GDMIndividualRecord ind = (GDMIndividualRecord)rec;

                        if (fSelectedRecords == null || fSelectedRecords.IndexOf(rec) >= 0) {
                            var parts = GKUtils.GetNameParts(fTree, ind);
                            string sx = "" + GKUtils.SexStr(ind.Sex)[0];

                            fWriter.AddTableCell(ind.GetXRefNum());
                            fWriter.AddTableCell(parts.Surname);
                            fWriter.AddTableCell(parts.Name);
                            fWriter.AddTableCell(parts.Patronymic);
                            fWriter.AddTableCell(sx);
                            fWriter.AddTableCell(GKUtils.GetBirthDate(ind, dateFormat, false));
                            fWriter.AddTableCell(GKUtils.GetDeathDate(ind, dateFormat, false));
                            fWriter.AddTableCell(GKUtils.GetBirthPlace(ind));
                            fWriter.AddTableCell(GKUtils.GetDeathPlace(ind));
                            fWriter.AddTableCell(GKUtils.GetResidencePlace(ind, fOptions.PlacesWithAddress));
                            fWriter.AddTableCell(GKUtils.GetAgeStr(ind, -1));
                            fWriter.AddTableCell(GKUtils.GetLifeExpectancyStr(ind));
                        }
                    }
                    progress.Increment();
                }
            } finally {
                progress.End();
            }
        }
#endif

        public override void Generate(bool show)
        {
#if !NETSTANDARD
            fPath = AppHost.StdDialogs.GetSaveFile("Excel files (*.xls)|*.xls");
            if (string.IsNullOrEmpty(fPath)) return;

            fWriter = new XLSWriter();
            fWriter.BeginWrite();

            AppHost.Instance.ExecuteWork((controller) => {
                GenerateInt(controller);
            });

            fWriter.SetFileName(fPath);
            fWriter.EndWrite();

#if !CI_MODE
            if (show) ShowResult();
#endif
#endif
        }
    }
}
