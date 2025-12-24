/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2018-2023 by Sergey V. Zhdanovskih.
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
using GDModel;
using GKCore;
using GKCore.Design;
using GKCore.Design.Graphics;
using GKCore.Export;
using GKCore.Export.Formats;

namespace GKStdReports
{
    public sealed class SourcesReport : ReportExporter
    {
        public SourcesReport(IBaseWindow baseWin)
            : base(baseWin, false)
        {
            fTitle = SRLangMan.LS(PLS.SourcesReport);
        }

        protected override void InternalGenerate()
        {
            IColor clrBlack = AppHost.GfxProvider.CreateColor(0x000000);

            var titleFont = fWriter.CreateFont("", 22f, true, false, clrBlack);
            var textFont = fWriter.CreateFont("", 10f, false, false, clrBlack);
            var headerFont = fWriter.CreateFont("", 12f, true, false, clrBlack);

            fWriter.AddParagraph(fTitle, titleFont, TextAlignment.taLeft);
            fWriter.NewLine();

            var sources = new List<GDMSourceRecord>();

            GDMTree tree = fBase.Context.Tree;
            var enumer = tree.GetEnumerator<GDMSourceRecord>();
            GDMSourceRecord sourceRec;
            while (enumer.MoveNext(out sourceRec)) {
                sources.Add(sourceRec);
            }

            SortHelper.QuickSort(sources, ItemsCompare);

            fWriter.BeginTable(3, sources.Count + 1);

            fWriter.BeginTableRow(true);
            fWriter.AddTableCell(SRLangMan.LS(PLS.Name), headerFont, TextAlignment.taLeft);
            fWriter.AddTableCell(SRLangMan.LS(PLS.Title), headerFont, TextAlignment.taLeft);
            fWriter.AddTableCell(SRLangMan.LS(PLS.Repository), headerFont, TextAlignment.taLeft);
            fWriter.EndTableRow();

            for (int i = 0; i < sources.Count; i++) {
                sourceRec = sources[i];
                var repoCit = (sourceRec.RepositoryCitations.Count > 0) ? sourceRec.RepositoryCitations[0] : null;
                var repoRec = tree.GetPtrValue<GDMRepositoryRecord>(repoCit);
                var repoName = (repoRec != null) ? repoRec.RepositoryName : string.Empty;

                fWriter.BeginTableRow(false);
                fWriter.AddTableCell(sourceRec.ShortTitle, textFont, TextAlignment.taLeft);
                fWriter.AddTableCell(sourceRec.Title.Lines.Text, textFont, TextAlignment.taLeft);
                fWriter.AddTableCell(repoName, textFont, TextAlignment.taLeft);
                fWriter.EndTableRow();
            }
            fWriter.EndTable();
        }

        private static int ItemsCompare(GDMSourceRecord item1, GDMSourceRecord item2)
        {
            return item1.ShortTitle.CompareTo(item2.ShortTitle);
        }
    }
}
