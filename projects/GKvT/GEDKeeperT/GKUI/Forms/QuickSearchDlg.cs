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
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Plugins;
using Terminal.Gui;

namespace GKUI.Forms
{
    public sealed partial class QuickSearchDlg : CommonForm, IQuickSearchDlg
    {
        private readonly QuickSearchDlgController fController;

        public IWindow OwnerWindow
        {
            get { return fController.WorkWindow; }
        }

        #region View Interface

        ITextBox IQuickSearchDlg.SearchPattern
        {
            get { return GetControlHandler<ITextBox>(txtSearchPattern); }
        }

        #endregion

        public QuickSearchDlg(IWorkWindow workWindow)
        {
            InitializeComponent();

            fController = new QuickSearchDlgController(this, workWindow);

            AppHost.Instance.WidgetLocate(this, WidgetLocation.HLeft | WidgetLocation.VBottom);
        }

        private void SearchPattern_TextChanged(object sender, string e)
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

        private void SearchPanel_KeyDown(object sender, KeyEventEventArgs e)
        {
            switch (e.KeyEvent.Key) {
                case Key.Enter:
                    e.Handled = true;
                    if (e.KeyEvent.IsShift)
                        fController.FindPrev();
                    else
                        fController.FindNext();
                    break;

                case Key.Esc:
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
