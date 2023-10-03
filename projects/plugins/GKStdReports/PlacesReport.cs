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

using BSLib;
using GKCore;
using GKCore.Design.Graphics;
using GKCore.Export;
using GKCore.Interfaces;
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
