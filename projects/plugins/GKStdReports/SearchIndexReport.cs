/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using BSLib;
using GDModel;
using GKCore;
using GKCore.Design;
using GKCore.Design.Graphics;
using GKCore.Export;
using GKCore.Export.Formats;
using GKCore.Locales;
using GKCore.Tools;
using static GKCore.Tools.TreeTools;

namespace GKStdReports
{
    public sealed class SearchIndexReport : TableExporter
    {
        public SearchIndexReport(IBaseWindow baseWin) : base(baseWin)
        {
            //fTitle = SRLangMan.LS(PLS.SearchIndexReport);
        }

        protected override void GenerateInt(IProgressController progress)
        {
            IColor clrBlack = AppHost.GfxProvider.CreateColor(0x000000);

            var titleFont = fWriter.CreateFont("", 22f, true, false, clrBlack);
            var textFont = fWriter.CreateFont("", 10f, false, false, clrBlack);
            var headerFont = fWriter.CreateFont("", 12f, true, false, clrBlack);

            //fWriter.AddParagraph(fTitle, titleFont, TextAlignment.taLeft);
            //fWriter.NewLine();

            var patList = PatriarchsMan.GetPatriarchsList(fBase.Context, 3, false, progress, true);
            if (patList == null) return;

            GDMTree tree = fBase.Context.Tree;
            for (int i = 0, num = patList.Count; i < num; i++) {
                var patObj = patList[i];
                var ext = new PatExt(patObj);
                ext.Name = GKUtils.GetNameString(patObj.IRec, true, false);
                patObj.ExtData = ext;

                // search for the earliest place
                TreeTools.WalkTree(tree, patObj.IRec, TreeWalkMode.twmDescendants, DescWalkProc, ext);
            }
            SortHelper.QuickSort(patList, PatriarchsCompare);

            fWriter.BeginTable(5, patList.Count + 1);

            fWriter.BeginTableRow(true);
            fWriter.AddTableCell(LangMan.LS(LSID.Patriarch), headerFont, TextAlignment.taLeft);
            fWriter.AddTableCell(LangMan.LS(LSID.Birth), headerFont, TextAlignment.taCenter);
            fWriter.AddTableCell(LangMan.LS(LSID.Descendants), headerFont, TextAlignment.taRight);
            fWriter.AddTableCell(LangMan.LS(LSID.Generations), headerFont, TextAlignment.taRight);
            fWriter.AddTableCell(SRLangMan.LS(PLS.EarliestPlace), headerFont, TextAlignment.taLeft);
            fWriter.EndTableRow();

            for (int i = 0, num = patList.Count; i < num; i++) {
                var patObj = patList[i];
                var ext = patObj.ExtData as PatExt;

                fWriter.BeginTableRow(false);
                fWriter.AddTableCell(ext.Name, textFont, TextAlignment.taLeft);
                fWriter.AddTableCell(patObj.BirthYear.ToString(), textFont, TextAlignment.taCenter);
                fWriter.AddTableCell(patObj.DescendantsCount.ToString(), textFont, TextAlignment.taRight);
                fWriter.AddTableCell(patObj.DescGenerations.ToString(), textFont, TextAlignment.taRight);
                fWriter.AddTableCell(string.Format("{0} [{1}]", ext.EarliestPlace, ext.EarliestPlaceYear), textFont, TextAlignment.taLeft);
                fWriter.EndTableRow();
            }
            fWriter.EndTable();
        }

        private static bool DescWalkProc(GDMIndividualRecord iRec, TreeWalkMode mode, object extData)
        {
            bool resContinue = true;

            var ext = (PatExt)extData;
            if (iRec.HasEvents) {
                for (int j = iRec.Events.Count - 1; j >= 0; j--) {
                    var evt = iRec.Events[j];
                    if (evt.GetChronoPlace(out int eYear, out string ePlace)) {
                        if (ext.EarliestPlaceYear > eYear) {
                            ext.EarliestPlaceYear = eYear;
                            ext.EarliestPlace = ePlace;
                        } else {
                            ext.ExcessNum++;
                            if (ext.ExcessNum >= 100) {
                                resContinue = false;
                                break;
                            }
                        }
                    }
                }
            }

            return resContinue;
        }

        private static int PatriarchsCompare(PatriarchObj item1, PatriarchObj item2)
        {
            return ((PatExt)item1.ExtData).Name.CompareTo(((PatExt)item2.ExtData).Name);
        }

        private sealed class PatExt
        {
            public PatriarchObj Patriarch { get; private set; }

            public string Name;
            public string EarliestPlace;
            public int EarliestPlaceYear;
            public int ExcessNum;

            public PatExt(PatriarchObj patriarch)
            {
                Patriarch = patriarch;

                Name = string.Empty;
                EarliestPlace = string.Empty;
                EarliestPlaceYear = int.MaxValue;
                ExcessNum = 0;
            }
        }
    }
}
