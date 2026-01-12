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
    public sealed class NamesFreqReport : ReportExporter
    {
        private class NameItem
        {
            public string Name;
            public int Amount;

            public NameItem(string name)
            {
                Name = name;
                Amount = 1;
            }
        }

        public NamesFreqReport(IBaseWindow baseWin)
            : base(baseWin, false)
        {
            fTitle = SRLangMan.LS(PLS.NamesFreqReport);
        }

        protected override void InternalGenerate()
        {
            IColor clrBlack = AppHost.GfxProvider.CreateColor(0x000000);

            var titleFont = fWriter.CreateFont("", 22f, true, false, clrBlack);
            var chapFont = fWriter.CreateFont("", 16f, true, false, clrBlack);
            var textFont = fWriter.CreateFont("", 10f, false, false, clrBlack);

            fWriter.AddParagraph(fTitle, titleFont, TextAlignment.taLeft);

            var names = new List<NameItem>();
            var surnames = new List<NameItem>();

            GDMTree tree = fBase.Context.Tree;
            var enumer = tree.GetEnumerator<GDMIndividualRecord>();
            GDMIndividualRecord iRec;
            while (enumer.MoveNext(out iRec)) {
                var nameParts = GKUtils.GetNameParts(tree, iRec, false);

                var item = names.Find(x => x.Name.Equals(nameParts.Name));
                if (item != null) {
                    item.Amount += 1;
                } else {
                    names.Add(new NameItem(nameParts.Name));
                }

                item = surnames.Find(x => x.Name.Equals(nameParts.Surname));
                if (item != null) {
                    item.Amount += 1;
                } else {
                    surnames.Add(new NameItem(nameParts.Surname));
                }
            }

            SortHelper.QuickSort(names, ItemsCompare);
            SortHelper.QuickSort(surnames, ItemsCompare);

            fWriter.AddParagraph(SRLangMan.LS(PLS.Names), chapFont, TextAlignment.taLeft);
            fWriter.BeginList();
            foreach (var item in names) {
                fWriter.AddListItem(" " + item.Name + "\t" + item.Amount, textFont);
            }
            fWriter.EndList();

            fWriter.AddParagraph(SRLangMan.LS(PLS.Surnames), chapFont, TextAlignment.taLeft);
            fWriter.BeginList();
            foreach (var item in surnames) {
                fWriter.AddListItem(" " + item.Name + "\t" + item.Amount, textFont);
            }
            fWriter.EndList();
        }

        private static int ItemsCompare(NameItem item1, NameItem item2)
        {
            return -item1.Amount.CompareTo(item2.Amount);
        }
    }
}
