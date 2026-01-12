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
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Plugins;

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

        public override void ApplyTheme()
        {
            base.ApplyTheme();
            fController.ApplyTheme();
        }
    }
}
