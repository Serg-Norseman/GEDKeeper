/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2018-2021 by Sergey V. Zhdanovskih.
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
    public sealed class SourcesReport : ReportExporter
    {
        private IFont fTitleFont, fTextFont, fHeaderFont;

        public SourcesReport(IBaseWindow baseWin)
            : base(baseWin, false)
        {
            fTitle = SRLangMan.LS(RLS.LSID_Sources_Title);
        }

        protected override void InternalGenerate()
        {
            IColor clrBlack = AppHost.GfxProvider.CreateColor(0x000000);
            IColor clrBlue = AppHost.GfxProvider.CreateColor(0x0000FF);

            fTitleFont = fWriter.CreateFont("", 22f, true, false, clrBlack);
            fTextFont = fWriter.CreateFont("", 10f, false, false, clrBlack);
            fHeaderFont = fWriter.CreateFont("", 12f, true, false, clrBlack);

            fWriter.AddParagraph(fTitle, fTitleFont, TextAlignment.taLeft);
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
            fWriter.AddTableCell(SRLangMan.LS(RLS.LSID_Name), fHeaderFont, TextAlignment.taLeft);
            fWriter.AddTableCell(SRLangMan.LS(RLS.LSID_Title), fHeaderFont, TextAlignment.taLeft);
            fWriter.AddTableCell(SRLangMan.LS(RLS.LSID_Repository), fHeaderFont, TextAlignment.taLeft);
            fWriter.EndTableRow();

            for (int i = 0; i < sources.Count; i++) {
                sourceRec = sources[i];
                var repoCit = (sourceRec.RepositoryCitations.Count > 0) ? sourceRec.RepositoryCitations[0] : null;
                var repoRec = tree.GetPtrValue<GDMRepositoryRecord>(repoCit);
                var repoName = (repoRec != null) ? repoRec.RepositoryName : string.Empty;

                fWriter.BeginTableRow(false);
                fWriter.AddTableCell(sourceRec.ShortTitle, fTextFont, TextAlignment.taLeft);
                fWriter.AddTableCell(sourceRec.Title.Lines.Text, fTextFont, TextAlignment.taLeft);
                fWriter.AddTableCell(repoName, fTextFont, TextAlignment.taLeft);
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
