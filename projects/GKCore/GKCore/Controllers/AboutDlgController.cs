/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

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
    public sealed class AboutDlgController : DialogController<IAboutDlg>
    {
        public AboutDlgController(IAboutDlg view) : base(view)
        {
        }

        public override void UpdateView()
        {
#if !GK3
            GetControl<ILabel>("lblProduct").Text = GKData.APP_TITLE;
#endif

            GetControl<ILabel>("lblVersion").Text = @"Version " + AppHost.GetAppVersion();
            GetControl<ILabel>("lblCopyright").Text = AppHost.GetAppCopyright();

            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                if (GlobalOptions.Instance.GetLanguageSign() == "rus") {
                    GetControl<ILabel>("lblForum").Text = GKData.APP_FORUM_RU;
                    GetControl<ILabel>("lblChannel").Text = GKData.APP_CHANNEL_RU;
                } else {
                    GetControl<ILabel>("lblForum").Text = GKData.APP_FORUM_EN;
                    GetControl<ILabel>("lblChannel").Text = GKData.APP_CHANNEL_EN;
                }
            } else {
                if (GlobalOptions.Instance.GetLanguageSign() == "rus") {
                    GetControl<IButton>("lblForum").Text = GKData.APP_FORUM_RU;
                    GetControl<IButton>("lblChannel").Text = GKData.APP_CHANNEL_RU;
                } else {
                    GetControl<IButton>("lblForum").Text = GKData.APP_FORUM_EN;
                    GetControl<IButton>("lblChannel").Text = GKData.APP_CHANNEL_EN;
                }
            }
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.MIAbout));

            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                GetControl<IButton>("btnClose").Text = LangMan.LS(LSID.DlgClose);
            }
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnClose").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
        }
    }
}
