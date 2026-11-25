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
        #region Design components
#pragma warning disable CS0649, IDE0044
        private ILabel _lblVersion;
        private ILabel _lblCopyright;
        private IButton _btnClose;
        private ILabel _lblForum;
        private ILabel _lblChannel;
#pragma warning restore CS0649, IDE0044
        #endregion

        public AboutDlgController(IAboutDlg view) : base(view)
        {
        }

        public override void UpdateView()
        {
            if (AppHost.Instance.HasFeatureSupport(Feature.DesktopV2) || !AppHost.Instance.HasFeatureSupport(Feature.Graphics)) {
                GetControl<ILabel>("lblProduct").Text = GKData.APP_TITLE;
            }

            _lblVersion.Text = @"Version " + AppHost.GetAppVersion();
            _lblCopyright.Text = AppHost.GetAppCopyright();

            if (GlobalOptions.Instance.GetLanguageSign() == "rus") {
                _lblForum.Text = GKData.APP_FORUM_RU;
                _lblChannel.Text = GKData.APP_CHANNEL_RU;
            } else {
                _lblForum.Text = GKData.APP_FORUM_EN;
                _lblChannel.Text = GKData.APP_CHANNEL_EN;
            }
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.MIAbout));

            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                _btnClose.Text = LangMan.LS(LSID.DlgClose);
            }
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _btnClose.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
        }
    }
}
