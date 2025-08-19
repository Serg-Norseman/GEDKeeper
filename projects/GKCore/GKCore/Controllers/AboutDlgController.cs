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
