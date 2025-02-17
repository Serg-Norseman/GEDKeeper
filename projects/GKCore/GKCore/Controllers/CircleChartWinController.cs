/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using GKCore.Design.Controls;
using GKCore.Design;
using GKCore.Design.Views;
using GKCore.Options;
using System.IO;
using GKCore.Types;
using GKUI.Themes;

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

        public async void SaveSnapshot()
        {
            string filters = GKUtils.GetImageFilter(true);
            string fileName = await AppHost.StdDialogs.GetSaveFile("", GlobalOptions.Instance.ImageExportLastDir, filters, 2, "jpg", "");
            if (!string.IsNullOrEmpty(fileName)) {
                GlobalOptions.Instance.ImageExportLastDir = Path.GetDirectoryName(fileName);

                fView.CircleChart.SaveSnapshot(fileName);
            }
        }

        public override void SetLocale()
        {
            if (fView.CircleChart.ChartType == CircleChartType.Ancestors) {
                fView.Title = LangMan.LS(LSID.AncestorsCircle);
            } else {
                fView.Title = LangMan.LS(LSID.DescendantsCircle);
            }

            if (AppHost.Instance.HasFeatureSupport(Feature.Mobile)) return;

            GetControl<IButtonToolItem>("tbModes").Text = LangMan.LS(LSID.ModesTip);
            GetControl<IMenuItem>("miFanMode").Text = LangMan.LS(LSID.FanMode);

            SetToolTip("tbImageSave", LangMan.LS(LSID.ImageSaveTip));
            SetToolTip("tbDocPrint", LangMan.LS(LSID.DocPrint));
            SetToolTip("tbDocPreview", LangMan.LS(LSID.DocPreview));
            SetToolTip("tbPrev", LangMan.LS(LSID.PrevRec));
            SetToolTip("tbNext", LangMan.LS(LSID.NextRec));
            SetToolTip("tbOptions", LangMan.LS(LSID.MIOptions));
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IToolItem>("tbImageSave").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_ImageSave, true);
            GetControl<IToolItem>("tbDocPrint").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_DocPrint, true);
            GetControl<IToolItem>("tbDocPreview").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_DocPreview, true);
            GetControl<IToolItem>("tbPrev").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Prev, true);
            GetControl<IToolItem>("tbNext").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Next, true);
            GetControl<IToolItem>("tbOptions").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Settings, true);
        }
    }
}
