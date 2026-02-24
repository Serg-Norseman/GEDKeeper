/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.IO;
using GKCore.Charts;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Locales;
using GKCore.Options;
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
            GetControl<IToolItem>("tbDocPreview").Enabled = AppHost.Instance.HasFeatureSupport(Feature.PrintPreview);
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
                fView.SetTitle(LangMan.LS(LSID.AncestorsCircle));
            } else {
                fView.SetTitle(LangMan.LS(LSID.DescendantsCircle));
            }

            if (AppHost.Instance.HasFeatureSupport(Feature.Mobile)) return;

            GetControl<IToolItem>("tbModes").Text = LangMan.LS(LSID.ModesTip);
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
