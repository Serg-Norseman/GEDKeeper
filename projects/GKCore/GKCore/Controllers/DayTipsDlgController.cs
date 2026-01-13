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
                    fView.TitleLabel.Text = tip;

                    fTips.Delete(0);
                    tip = fTips[0];
                }

                fView.TipText.Text = tip;
                fTips.Delete(0);
            }
            fView.NextButton.Enabled = (fTips.Count > 0);
        }

        public void InitTips(string caption, bool showTipsChecked, StringList tips)
        {
            fView.SetTitle(caption);

            GetControl<ICheckBox>("chkShow").Checked = showTipsChecked;
            GetControl<ILabel>("lblTitle").Text = caption;

            fTips.Assign(tips);

            GetNextTip();
        }

        public override void UpdateView()
        {
        }

        public override void SetLocale()
        {
            GetControl<IButton>("btnClose").Text = LangMan.LS(LSID.DlgClose);
            GetControl<ICheckBox>("chkShow").Text = LangMan.LS(LSID.StartupTips);
            GetControl<IButton>("btnNextTip").Text = LangMan.LS(LSID.Next);
            GetControl<ILabel>("lblTitle").Text = LangMan.LS(LSID.YouKnowWhat);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnClose").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
