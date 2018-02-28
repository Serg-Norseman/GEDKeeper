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
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Export;
using GKCore.Interfaces;

namespace GKStdReports
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class PersonalEventsReport : ReportExporter
    {
        private IFont fTitleFont;
        private IFont fTextFont;

        public PersonalEventsReport(IBaseWindow baseWin) : base(baseWin)
        {
            fTitle = "Personal Events";
        }

        protected override void InternalGenerate()
        {
            IColor clrBlack = AppHost.GfxProvider.CreateColor(0x000000);
            IColor clrBlue = AppHost.GfxProvider.CreateColor(0x0000FF);

            fTitleFont = fWriter.CreateFont("", 16f/*20f*/, true, false, clrBlack);
            fTextFont = fWriter.CreateFont("", 10f/*8f*/, false, false, clrBlack);

            fWriter.addParagraph(fTitle, fTitleFont, CustomWriter.TextAlignment.taLeft);

            /*var names = new List<NameItem>();

            GEDCOMTree tree = fBase.Context.Tree;
            var enumer = tree.GetEnumerator(GEDCOMRecordType.rtIndividual);
            GEDCOMRecord record;
            while (enumer.MoveNext(out record)) {
                var iRec = record as GEDCOMIndividualRecord;
                var nameParts = GKUtils.GetNameParts(iRec, false);

                var item = names.Find(x => x.Name.Equals(nameParts.Name));
                if (item != null) {
                    item.Amount += 1;
                } else {
                    names.Add(new NameItem(nameParts.Name));
                }
            }

            SortHelper.QuickSort(names, ItemsCompare);

            fWriter.beginList();
            for (int i = 0; i < names.Count; i++) {
                fWriter.addListItem(" " + names[i].Name + "\t" + names[i].Amount, fTextFont);
            }
            fWriter.endList();*/
        }

        /*private static int ItemsCompare(NameItem item1, NameItem item2)
        {
            return -item1.Amount.CompareTo(item2.Amount);
        }*/
    }
}
