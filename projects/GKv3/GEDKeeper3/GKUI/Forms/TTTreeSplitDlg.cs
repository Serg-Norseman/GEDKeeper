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
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Views;
using GKCore.Tools;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class TTTreeSplitDlg : CommonDialog, ITreeSplitDlg
    {
        #region Design components

        private TabControl tabsTools;
        private Button btnClose;
        private TabPage pageTreeSplit;
        private Button btnSelectAll;
        private GKListView ListSelected;
        private GKListView ListSkipped;
        private Button btnSelectFamily;
        private Button btnSelectAncestors;
        private Button btnSelectDescendants;
        private Button btnDelete;
        private Button btnSave;

        #endregion

        private readonly TreeSplitController fController;

        #region View Interface

        IListViewEx ITreeSplitDlg.SelectedList
        {
            get { return ListSelected; }
        }

        IListViewEx ITreeSplitDlg.SkippedList
        {
            get { return ListSkipped; }
        }

        #endregion

        public TTTreeSplitDlg(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            btnClose.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            fController = new TreeSplitController(this);
            fController.Init(baseWin);
        }

        private void btnSelectFamily_Click(object sender, EventArgs e)
        {
            fController.Select(TreeTools.TreeWalkMode.twmFamily);
        }

        private void btnSelectAncestors_Click(object sender, EventArgs e)
        {
            fController.Select(TreeTools.TreeWalkMode.twmAncestors);
        }

        private void btnSelectDescendants_Click(object sender, EventArgs e)
        {
            fController.Select(TreeTools.TreeWalkMode.twmDescendants);
        }

        private void btnSelectAll_Click(object sender, EventArgs e)
        {
            fController.Select(TreeTools.TreeWalkMode.twmAll);
        }

        private void btnDelete_Click(object sender, EventArgs e)
        {
            fController.Delete();
        }

        private void btnSave_Click(object sender, EventArgs e)
        {
            fController.Save();
        }
    }
}
