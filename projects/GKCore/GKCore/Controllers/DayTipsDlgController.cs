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

using BSLib;
using GKCore.Design.Controls;
using GKCore.Design;
using GKCore.Design.Views;
using GKCore.Types;
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
            fView.Title = caption;

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
