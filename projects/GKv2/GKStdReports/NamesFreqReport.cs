/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2018 by Sergey V. Zhdanovskih.
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
using GKCore;
using GKCore.Export;
using GKCore.Interfaces;

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

        private IFont fTitleFont, fChapFont, fTextFont;

        public NamesFreqReport(IBaseWindow baseWin)
            : base(baseWin, false)
        {
            fTitle = SRLangMan.LS(RLS.LSID_NFR_Title);
        }

        protected override void InternalGenerate()
        {
            IColor clrBlack = AppHost.GfxProvider.CreateColor(0x000000);
            IColor clrBlue = AppHost.GfxProvider.CreateColor(0x0000FF);

            fTitleFont = fWriter.CreateFont("", 22f, true, false, clrBlack);
            fChapFont = fWriter.CreateFont("", 16f, true, false, clrBlack);
            fTextFont = fWriter.CreateFont("", 10f, false, false, clrBlack);

            fWriter.AddParagraph(fTitle, fTitleFont, TextAlignment.taLeft);

            var names = new List<NameItem>();
            var surnames = new List<NameItem>();

            GDMTree tree = fBase.Context.Tree;
            var enumer = tree.GetEnumerator(GEDCOMRecordType.rtIndividual);
            GDMRecord record;
            while (enumer.MoveNext(out record)) {
                var iRec = record as GDMIndividualRecord;
                var nameParts = GKUtils.GetNameParts(iRec, false);

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

            fWriter.AddParagraph(SRLangMan.LS(RLS.LSID_Names), fChapFont, TextAlignment.taLeft);
            fWriter.BeginList();
            foreach (var item in names) {
                fWriter.AddListItem(" " + item.Name + "\t" + item.Amount, fTextFont);
            }
            fWriter.EndList();

            fWriter.AddParagraph(SRLangMan.LS(RLS.LSID_Surnames), fChapFont, TextAlignment.taLeft);
            fWriter.BeginList();
            foreach (var item in surnames) {
                fWriter.AddListItem(" " + item.Name + "\t" + item.Amount, fTextFont);
            }
            fWriter.EndList();
        }

        private static int ItemsCompare(NameItem item1, NameItem item2)
        {
            return -item1.Amount.CompareTo(item2.Amount);
        }
    }
}
