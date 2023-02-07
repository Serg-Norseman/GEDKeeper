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
using GKCore.Controllers;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class TTTreeCompareDlg : CommonDialog<ITreeCompareDlg, TreeCompareController>, ITreeCompareDlg
    {
        #region View Interface

        ITextBox ITreeCompareDlg.ExternalBase
        {
            get { return GetControlHandler<ITextBox>(txtCompareFile); }
        }

        ITextBox ITreeCompareDlg.CompareOutput
        {
            get { return GetControlHandler<ITextBox>(ListCompare); }
        }

        #endregion

        public TTTreeCompareDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            btnClose.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            fController = new TreeCompareController(this);
            fController.Init(baseWin);
        }

        private void btnFileChoose_Click(object sender, EventArgs e)
        {
            fController.SelectExternalFile();
        }

        private void btnMatch_Click(object sender, EventArgs e)
        {
            fController.Match();
        }

        private void rbtnMatch_CheckedChanged(object sender, EventArgs e)
        {
            fController.ChangeTreeMatchType();
        }
    }
}
