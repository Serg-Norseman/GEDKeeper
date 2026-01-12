/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
    public sealed class RepositoriesReport : ReportExporter
    {
        public RepositoriesReport(IBaseWindow baseWin)
            : base(baseWin, false)
        {
            fTitle = SRLangMan.LS(PLS.RepositoriesReport);
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
            fWriter.AddTableCell(SRLangMan.LS(PLS.Name), headerFont, TextAlignment.taLeft);
            fWriter.AddTableCell(SRLangMan.LS(PLS.Address), headerFont, TextAlignment.taLeft);
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
