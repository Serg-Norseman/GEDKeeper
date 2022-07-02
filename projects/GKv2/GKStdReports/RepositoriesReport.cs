/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2018-2022 by Sergey V. Zhdanovskih.
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
using BSLib;
using BSLib.Design.Graphics;
using GDModel;
using GKCore;
using GKCore.Export;
using GKCore.Interfaces;

namespace GKStdReports
{
    public sealed class RepositoriesReport : ReportExporter
    {
        public RepositoriesReport(IBaseWindow baseWin)
            : base(baseWin, false)
        {
            fTitle = SRLangMan.LS(RLS.LSID_Repositories_Title);
        }

        protected override void InternalGenerate()
        {
            IColor clrBlack = AppHost.GfxProvider.CreateColor(0x000000);

            var titleFont = fWriter.CreateFont("", 22f, true, false, clrBlack);
            var textFont = fWriter.CreateFont("", 10f, false, false, clrBlack);
            var headerFont = fWriter.CreateFont("", 12f, true, false, clrBlack);

            fWriter.AddParagraph(fTitle, titleFont, TextAlignment.taLeft);
            fWriter.NewLine();

            var repositories = new List<GDMRepositoryRecord>();

            GDMTree tree = fBase.Context.Tree;
            var enumer = tree.GetEnumerator<GDMRepositoryRecord>();
            GDMRepositoryRecord repoRec;
            while (enumer.MoveNext(out repoRec)) {
                repositories.Add(repoRec);
            }

            SortHelper.QuickSort(repositories, ItemsCompare);

            fWriter.BeginTable(2, repositories.Count + 1);

            fWriter.BeginTableRow(true);
            fWriter.AddTableCell(SRLangMan.LS(RLS.LSID_Name), headerFont, TextAlignment.taLeft);
            fWriter.AddTableCell(SRLangMan.LS(RLS.LSID_Address), headerFont, TextAlignment.taLeft);
            fWriter.EndTableRow();

            for (int i = 0; i < repositories.Count; i++) {
                repoRec = repositories[i];

                fWriter.BeginTableRow(false);
                fWriter.AddTableCell(repoRec.RepositoryName, textFont, TextAlignment.taLeft);

                if (repoRec.HasAddress)
                    fWriter.AddTableCell(repoRec.Address.Lines.Text, textFont, TextAlignment.taLeft);

                fWriter.EndTableRow();
            }
            fWriter.EndTable();
        }

        private static int ItemsCompare(GDMRepositoryRecord item1, GDMRepositoryRecord item2)
        {
            return item1.RepositoryName.CompareTo(item2.RepositoryName);
        }
    }
}
