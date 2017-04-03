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

using System.Collections.Generic;
using ExcelLibrary.SpreadSheet;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Types;
using GKUI.Engine;

namespace GKCore.Export
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class ExcelExporter : Exporter
    {
        private readonly List<GEDCOMRecord> fSelectedRecords;

        public ExcelExporter(IBaseWindow baseWin) : base(baseWin)
        {
            fSelectedRecords = baseWin.GetContentList(GEDCOMRecordType.rtIndividual);
        }

        public override void Generate(bool show)
        {
            fPath = AppHub.StdDialogs.GetSaveFile("Excel files (*.xls)|*.xls");
            if (string.IsNullOrEmpty(fPath)) return;

            Workbook workbook = new Workbook();
            Worksheet worksheet = new Worksheet("First Sheet");

            fBase.ProgressInit(LangMan.LS(LSID.LSID_MIExport) + "...", fTree.RecordsCount);

            //TCellAttributeSet cas = (TCellAttributeSet.acBottomBorder | TCellAttributeSet.acTopBorder | TCellAttributeSet.acRightBorder | TCellAttributeSet.acLeftBorder);

            try
            {
                worksheet.Cells[1,  1] = new Cell("№");
                worksheet.Cells[1,  2] = new Cell(LangMan.LS(LSID.LSID_Surname));
                worksheet.Cells[1,  3] = new Cell(LangMan.LS(LSID.LSID_Name));
                worksheet.Cells[1,  4] = new Cell(LangMan.LS(LSID.LSID_Patronymic));
                worksheet.Cells[1,  5] = new Cell(LangMan.LS(LSID.LSID_Sex));
                worksheet.Cells[1,  6] = new Cell(LangMan.LS(LSID.LSID_BirthDate));
                worksheet.Cells[1,  7] = new Cell(LangMan.LS(LSID.LSID_DeathDate));
                worksheet.Cells[1,  8] = new Cell(LangMan.LS(LSID.LSID_BirthPlace));
                worksheet.Cells[1,  9] = new Cell(LangMan.LS(LSID.LSID_DeathPlace));
                worksheet.Cells[1, 10] = new Cell(LangMan.LS(LSID.LSID_Residence));
                worksheet.Cells[1, 11] = new Cell(LangMan.LS(LSID.LSID_Age));
                worksheet.Cells[1, 12] = new Cell(LangMan.LS(LSID.LSID_LifeExpectancy));

                ushort row = 1;
                int num = fTree.RecordsCount;
                for (int i = 0; i < num; i++)
                {
                    GEDCOMRecord rec = fTree[i];
                    if (rec is GEDCOMIndividualRecord) {
                        GEDCOMIndividualRecord ind = rec as GEDCOMIndividualRecord;
                        if (fSelectedRecords == null || fSelectedRecords.IndexOf(rec) >= 0) {
                            string fam, nam, pat;
                            GKUtils.GetNameParts(ind, out fam, out nam, out pat);

                            string sx = "" + GKUtils.SexStr(ind.Sex)[0];
                            row++;

                            worksheet.Cells[row, 1] = new Cell(ind.GetXRefNum());
                            worksheet.Cells[row, 2] = new Cell(fam);
                            worksheet.Cells[row, 3] = new Cell(nam);
                            worksheet.Cells[row, 4] = new Cell(pat);
                            worksheet.Cells[row, 5] = new Cell(sx);
                            worksheet.Cells[row, 6] = new Cell(GKUtils.GetBirthDate(ind, DateFormat.dfDD_MM_YYYY, false));
                            worksheet.Cells[row, 7] = new Cell(GKUtils.GetDeathDate(ind, DateFormat.dfDD_MM_YYYY, false));
                            worksheet.Cells[row, 8] = new Cell(GKUtils.GetBirthPlace(ind));
                            worksheet.Cells[row, 9] = new Cell(GKUtils.GetDeathPlace(ind));
                            worksheet.Cells[row,10] = new Cell(GKUtils.GetResidencePlace(ind, fOptions.PlacesWithAddress));
                            worksheet.Cells[row,11] = new Cell(GKUtils.GetAge(ind, -1));
                            worksheet.Cells[row,12] = new Cell(GKUtils.GetLifeExpectancy(ind));
                        }
                    }

                    fBase.ProgressStep();
                }

                workbook.Worksheets.Add(worksheet);
                workbook.Save(fPath);

                #if !CI_MODE
                if (show) ShowResult();
                #endif
            }
            finally
            {
                fBase.ProgressDone();
            }
        }
    }
}
