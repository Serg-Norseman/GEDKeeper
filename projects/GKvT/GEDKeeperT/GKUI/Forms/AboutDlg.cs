/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore;
using GKCore.Controllers;
using GKCore.Design.Views;
using Terminal.Gui.Input;
using Terminal.Gui.Views;

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

        private void LabelMail_Click(object sender, Mouse e)
        {
            if (sender is Label lbl && e.Flags.HasFlag(MouseFlags.LeftButtonClicked)) {
                GKUtils.LoadExtFile(lbl.Text.ToString());
                e.Handled = true;
            }
        }
    }
}
