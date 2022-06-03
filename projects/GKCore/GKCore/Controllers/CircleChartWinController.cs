﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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

using GKCore.Charts;
using GKCore.MVP;
using GKCore.MVP.Views;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class CircleChartWinController : FormController<ICircleChartWin>
    {

        public CircleChartWinController(ICircleChartWin view) : base(view)
        {
        }

        public override void UpdateView()
        {
        }

        public void SaveSnapshot()
        {
            string filters = GKUtils.GetImageFilter(true);
            string fileName = AppHost.StdDialogs.GetSaveFile("", "", filters, 2, "jpg", "");
            if (!string.IsNullOrEmpty(fileName)) {
                fView.CircleChart.SaveSnapshot(fileName);
            }
        }

        public override void SetLocale()
        {
            if (fView.CircleChart.ChartType == CircleChartType.Ancestors) {
                fView.Title = LangMan.LS(LSID.LSID_AncestorsCircle);
            } else {
                fView.Title = LangMan.LS(LSID.LSID_DescendantsCircle);
            }

            SetToolTip("tbImageSave", LangMan.LS(LSID.LSID_ImageSaveTip));
            SetToolTip("tbDocPrint", LangMan.LS(LSID.LSID_DocPrint));
            SetToolTip("tbDocPreview", LangMan.LS(LSID.LSID_DocPreview));
            SetToolTip("tbPrev", LangMan.LS(LSID.LSID_PrevRec));
            SetToolTip("tbNext", LangMan.LS(LSID.LSID_NextRec));
        }
    }
}
