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
using GKCore.Design.MVP.Controls;
using GKCore.Interfaces;
using GKCore.MVP.Views;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class QuickSearchDlg : CommonForm, IQuickSearchDlg
    {
        private readonly QuickSearchDlgController fController;

        #region View Interface

        ITextBox IQuickSearchDlg.SearchPattern
        {
            get { return GetControlHandler<ITextBox>(txtSearchPattern); }
        }

        #endregion

        public QuickSearchDlg(IWorkWindow workWindow)
        {
            InitializeComponent();

            btnPrev.Image = UIHelper.LoadResourceImage("Resources.btn_left.gif");
            btnNext.Image = UIHelper.LoadResourceImage("Resources.btn_right.gif");

            fController = new QuickSearchDlgController(this, workWindow);
        }

        protected override void OnShown(EventArgs e)
        {
            base.OnShown(e);
            AppHost.Instance.WidgetLocate(this, WidgetLocation.HLeft | WidgetLocation.VBottom);
        }

        private void SearchPattern_TextChanged(object sender, EventArgs e)
        {
            fController.ChangeText();
        }

        private void FindNext_Click(object sender, EventArgs e)
        {
            fController.FindNext();
        }

        private void FindPrev_Click(object sender, EventArgs e)
        {
            fController.FindPrev();
        }

        private void SearchPanel_KeyDown(object sender, KeyEventArgs e)
        {
            switch (e.KeyCode) {
                case Keys.Enter:
                    e.Handled = true;
                    if (e.Shift)
                        fController.FindPrev();
                    else
                        fController.FindNext();
                    break;

                case Keys.Escape:
                    e.Handled = true;
                    Close();
                    break;
            }
        }

        public void SetLocale()
        {
            fController.SetLocale();
        }
    }
}
