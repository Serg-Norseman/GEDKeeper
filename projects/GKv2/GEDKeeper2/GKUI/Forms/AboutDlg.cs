/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Windows.Forms;
using GKCore;
using GKCore.Controllers;
using GKCore.Design.Views;

namespace GKUI.Forms
{
    public sealed partial class AboutDlg : CommonDialog, IAboutDlg
    {
        private readonly AboutDlgController fController;

        public AboutDlg()
        {
            InitializeComponent();

            fController = new AboutDlgController(this);
            fController.SetLocale();
            fController.UpdateView();
        }

        public override void ApplyTheme()
        {
            base.ApplyTheme();
            fController.ApplyTheme();
        }

        private void LabelMail_Click(object sender, EventArgs e)
        {
            if (sender is Label lbl)
                GKUtils.LoadExtFile(lbl.Text);
        }

        public override bool SkipTheme(IDisposable component)
        {
            if (component == lblProduct || component == lblVersion || component == lblCopyright ||
                component == lblMail || component == lblProjSite || component == lblForum || component == lblChannel) {
                return true;
            }
            return false;
        }
    }
}
