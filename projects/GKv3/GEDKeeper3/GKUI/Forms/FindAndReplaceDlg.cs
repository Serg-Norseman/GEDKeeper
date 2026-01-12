/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using Eto.Forms;
using Eto.Serialization.Xaml;
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
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Label lblPattern;
        private ComboBox cmbPattern;
        private Label lblReplacement;
        private ComboBox cmbReplacement;
        private CheckBox chkMatchCase;
        private CheckBox chkMatchWildcards;
        private CheckBox chkWholeWord;
        private GroupBox gbFilters;
        private Label lblRecord;
        private ComboBox cmbRecord;
        private Label lblProperty;
        private ComboBox cmbProperty;
        private Button btnPrev;
        private Button btnNext;
        private Button btnReplace;
        private Button btnReplaceAll;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        public IWindow OwnerWindow
        {
            get { return fController.Base; }
        }

        public FindAndReplaceDlg()
        {
            XamlReader.Load(this);
        }

        public FindAndReplaceDlg(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

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
            switch (e.Key) {
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
