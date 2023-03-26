/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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

using System;
using BSLib;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GKCore.Controllers;
using GKCore.Design.Controls;
using GKCore.Design.Views;

namespace GKUI.Forms
{
    public partial class DayTipsDlg : CommonDialog, IDayTipsDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private CheckBox chkShow;
        private Button btnNextTip;
        private Button btnClose;
        private Label lblTitle;
        private Eto.Forms.ImageView Image1;
        private Label txtTip;
        private Scrollable Shape1;
        private Scrollable Shape2;
        private Scrollable Shape3;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        private readonly DayTipsDlgController fController;

        public bool ShowTipsChecked
        {
            get { return chkShow.Checked.GetValueOrDefault(); }
            set { chkShow.Checked = value; }
        }

        #region View Interface

        ILabel IDayTipsDlg.TitleLabel
        {
            get { return GetControlHandler<ILabel>(lblTitle); }
        }

        ITextContainer IDayTipsDlg.TipText
        {
            get { return GetControlHandler<ITextContainer>(txtTip); }
        }

        IButton IDayTipsDlg.NextButton
        {
            get { return GetControlHandler<IButton>(btnNextTip); }
        }

        #endregion

        public DayTipsDlg()
        {
            XamlReader.Load(this);

            fController = new DayTipsDlgController(this);
        }

        private void btnNextTip_Click(object sender, EventArgs e)
        {
            fController.GetNextTip();
        }

        public void Init(string caption, bool showTipsChecked, StringList tips)
        {
            fController.InitTips(caption, showTipsChecked, tips);
        }

        public override bool SkipTheme(IDisposable component)
        {
            if (component == Shape1 || component == Shape2 || component == Shape3 ||
                component == lblTitle || component == txtTip || component == chkShow) {
                return true;
            }
            return false;
        }
    }
}
