/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using BSLib;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Locales;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public class DayTipsDlgController : DialogController<IDayTipsDlg>
    {
        #region Design components
#pragma warning disable CS0649, IDE0044
        private ICheckBox _chkShow;
        private IButton _btnNextTip;
        private IButton _btnClose;
        private ILabel _lblTitle;
        private ITextContainer _txtTip;
#pragma warning restore CS0649, IDE0044
        #endregion

        private readonly StringList fTips;

        public DayTipsDlgController(IDayTipsDlg view) : base(view)
        {
            fTips = new StringList();
        }

        public void GetNextTip()
        {
            if (fTips.Count > 0) {
                string tip = fTips[0];

                // processing "title's directives"
                if (!string.IsNullOrEmpty(tip) && tip[0] == '#') {
                    tip = tip.Substring(1);
                    _lblTitle.Text = tip;

                    fTips.Delete(0);
                    tip = fTips[0];
                }

                _txtTip.Text = tip;
                fTips.Delete(0);
            }
            _btnNextTip.Enabled = (fTips.Count > 0);
        }

        public void InitTips(string caption, bool showTipsChecked, StringList tips)
        {
            fView.SetTitle(caption);

            _chkShow.Checked = showTipsChecked;
            _lblTitle.Text = caption;

            fTips.Assign(tips);

            GetNextTip();
        }

        public override void UpdateView()
        {
        }

        public override void SetLocale()
        {
            _btnClose.Text = LangMan.LS(LSID.DlgClose);
            _chkShow.Text = LangMan.LS(LSID.StartupTips);
            _btnNextTip.Text = LangMan.LS(LSID.Next);
            _lblTitle.Text = LangMan.LS(LSID.YouKnowWhat);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _btnClose.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
