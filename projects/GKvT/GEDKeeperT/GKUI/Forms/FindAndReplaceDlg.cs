/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GKCore;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Views;
using GKCore.Plugins;
using Terminal.Gui;

namespace GKUI.Forms
{
    public partial class FindAndReplaceDlg : CommonWindow<IFARDlg, FARDlgController>, IFARDlg
    {
        #region View Interface
        #endregion

        public IWindow OwnerWindow
        {
            get { return fController.Base; }
        }

        public FindAndReplaceDlg()
        {
            InitializeComponent();
        }

        public FindAndReplaceDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            fController = new FARDlgController(this);
            fController.Init(baseWin);
            fController.UpdateView();

            AppHost.Instance.WidgetLocate(this, WidgetLocation.HRight | WidgetLocation.VBottom);
        }

        private void Form_KeyDown(object sender, KeyEventEventArgs e)
        {
            switch (e.KeyEvent.Key) {
                case Key.Esc:
                    e.Handled = true;
                    Close();
                    break;
            }
        }

        private void btnPrev_Click(object sender, EventArgs e)
        {
            fController.Prev();
        }

        private void btnNext_Click(object sender, EventArgs e)
        {
            fController.Next();
        }

        private void btnReplace_Click(object sender, EventArgs e)
        {
            fController.Replace();
        }

        private void btnReplaceAll_Click(object sender, EventArgs e)
        {
            fController.ReplaceAll();
        }
    }
}
