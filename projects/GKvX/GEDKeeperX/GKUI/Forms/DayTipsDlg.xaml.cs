/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using BSLib;
using GKCore.Controllers;
using GKCore.Design.Views;

namespace GKUI.Forms
{
    public partial class DayTipsDlg : CommonDialog<IDayTipsDlg, DayTipsDlgController>, IDayTipsDlg
    {
        public bool ShowTipsChecked
        {
            get { return chkShow.IsChecked; }
            set { chkShow.IsChecked = value; }
        }

        public DayTipsDlg()
        {
            InitializeComponent();

            fController = new DayTipsDlgController(this);
            fController.InitView(null);
        }

        private void btnNextTip_Click(object sender, EventArgs e)
        {
            fController.GetNextTip();
        }

        public void Init(string caption, bool showTipsChecked, StringList tips)
        {
            fController.InitTips(caption, showTipsChecked, tips);
        }

        public override void ApplyTheme()
        {
            base.ApplyTheme();
            fController.ApplyTheme();
        }
    }
}
