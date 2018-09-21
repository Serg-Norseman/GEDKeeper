/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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
using GKCore.MVP;
using GKCore.MVP.Views;

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

        public void SetTips(StringList tips)
        {
            fTips.Assign(tips);
        }

        public override void UpdateView()
        {
        }
    }
}
