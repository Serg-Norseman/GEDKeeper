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

using System;
using BSLib;
using BSLib.Design.MVP.Controls;
using GKCore;
using GKCore.Controllers;
using GKCore.MVP.Views;
using GKUI.Components;

namespace GKUI.Forms
{
    public partial class DayTipsDlg : CommonDialog, IDayTipsDlg
    {
        private readonly DayTipsDlgController fController;

        public bool ShowTipsChecked
        {
            get { return chkShow.Checked; }
            set { chkShow.Checked = value; }
        }

        #region View Interface

        ILabelHandler IDayTipsDlg.TitleLabel
        {
            get { return GetControlHandler<ILabelHandler>(lblTitle); }
        }

        ITextBoxHandler IDayTipsDlg.TipText
        {
            get { return GetControlHandler<ITextBoxHandler>(txtTip); }
        }

        IButtonHandler IDayTipsDlg.NextButton
        {
            get { return GetControlHandler<IButtonHandler>(btnNextTip); }
        }

        #endregion

        public DayTipsDlg()
        {
            InitializeComponent();

            Image1.Image = UIHelper.LoadResourceImage("Resources.image_tips_light.png");
            btnClose.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            fController = new DayTipsDlgController(this);

            // SetLang()
            btnClose.Text = LangMan.LS(LSID.LSID_DlgClose);
            chkShow.Text = LangMan.LS(LSID.LSID_StartupTips);
            btnNextTip.Text = LangMan.LS(LSID.LSID_Next);
            lblTitle.Text = LangMan.LS(LSID.LSID_YouKnowWhat);
        }

        private void btnNextTip_Click(object sender, EventArgs e)
        {
            fController.GetNextTip();
        }

        public void Init(string caption, bool showTipsChecked, StringList tips)
        {
            chkShow.Checked = showTipsChecked;
            Text = caption;
            lblTitle.Text = caption;
            fController.SetTips(tips);
            fController.GetNextTip();
        }
    }
}
