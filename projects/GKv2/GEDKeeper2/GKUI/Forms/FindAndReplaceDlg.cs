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
using System.Windows.Forms;
using GKCore;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Views;
using GKCore.Plugins;

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
        }

        protected override void OnShown(EventArgs e)
        {
            base.OnShown(e);
            AppHost.Instance.WidgetLocate(this, WidgetLocation.HRight | WidgetLocation.VBottom);
        }

        private void Form_KeyDown(object sender, KeyEventArgs e)
        {
            switch (e.KeyCode) {
                case Keys.Escape:
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
