/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using Eto.Forms;
using Eto.Serialization.Xaml;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Views;

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
            AppHost.Instance.WidgetLocate(this, WidgetHorizontalLocation.Right, WidgetVerticalLocation.Bottom);
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
