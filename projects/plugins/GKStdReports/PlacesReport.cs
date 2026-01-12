/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using BSLib;
using GKCore;
using GKCore.Design;
using GKCore.Design.Graphics;
using GKCore.Export;
using GKCore.Export.Formats;
using GKCore.Tools;

namespace GKStdReports
{
    public sealed class PlacesReport : ReportExporter
    {
        public PlacesReport(IBaseWindow baseWin)
            : base(baseWin, false)
        {
            fTitle = SRLangMan.LS(PLS.PlacesReport);
        }

        protected override void InternalGenerate()
        {
            IColor clrBlack = AppHost.GfxProvider.CreateColor(0x000000);

            var titleFont = fWriter.CreateFont("", 22f, true, false, clrBlack);
            var textFont = fWriter.CreateFont("", 10f, false, false, clrBlack);
            var headerFont = fWriter.CreateFont("", 12f, true, false, clrBlack);

            fWriter.AddParagraph(fTitle, titleFont, TextAlignment.taLeft);
            fWriter.NewLine();

            var places = new StringList();

            AppHost.Instance.ExecuteWork((controller) => {
                TreeTools.SearchPlaces(fBase.Context.Tree, places, controller, "*", false);
            });

            places.Sort();

            fWriter.BeginTable(1, places.Count + 1);

            fWriter.BeginTableRow(true);
            fWriter.AddTableCell(SRLangMan.LS(PLS.Name), headerFont, TextAlignment.taLeft);
            fWriter.EndTableRow();

            for (int i = 0; i < places.Count; i++) {
                PlaceObj placeObj = (PlaceObj)places.GetObject(i);

                fWriter.BeginTableRow(false);
                fWriter.AddTableCell(placeObj.Name, textFont, TextAlignment.taLeft);
                fWriter.EndTableRow();
            }
            fWriter.EndTable();
        }
    }
}
