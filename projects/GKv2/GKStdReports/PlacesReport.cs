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

using BSLib;
using BSLib.Design.Graphics;
using GKCore;
using GKCore.Export;
using GKCore.Interfaces;
using GKCore.Tools;

namespace GKStdReports
{
    public sealed class PlacesReport : ReportExporter
    {
        private IFont fTitleFont, fTextFont, fHeaderFont;

        public PlacesReport(IBaseWindow baseWin)
            : base(baseWin, false)
        {
            fTitle = SRLangMan.LS(RLS.LSID_Places_Title);
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

            var places = new StringList();
            TreeTools.SearchPlaces(fBase.Context.Tree, places, AppHost.Progress, false);
            places.Sort();

            fWriter.BeginTable(1, places.Count + 1);

            fWriter.BeginTableRow(true);
            fWriter.AddTableCell(SRLangMan.LS(RLS.LSID_Name), fHeaderFont, TextAlignment.taLeft);
            fWriter.EndTableRow();

            for (int i = 0; i < places.Count; i++) {
                PlaceObj placeObj = (PlaceObj)places.GetObject(i);

                fWriter.BeginTableRow(false);
                fWriter.AddTableCell(placeObj.Name, fTextFont, TextAlignment.taLeft);
                fWriter.EndTableRow();
            }
            fWriter.EndTable();
        }
    }
}
