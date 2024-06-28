/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Tools;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class TTTreeSplitDlg : CommonDialog<ITreeSplitDlg, TreeSplitController>, ITreeSplitDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Button btnClose;
        private TabPage pageTreeSplit;
        private Button btnSelectAll;
        private GKListView ListSelected;
        private GKListView ListSkipped;
        private Button btnSelectFamily;
        private Button btnSelectAncestors;
        private Button btnSelectDescendants;
        private Button btnSelectList;
        private Button btnDelete;
        private Button btnSave;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        #region View Interface

        IListView ITreeSplitDlg.SelectedList
        {
            get { return ListSelected; }
        }

        IListView ITreeSplitDlg.SkippedList
        {
            get { return ListSkipped; }
        }

        #endregion

        public TTTreeSplitDlg(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

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

        private void btnSelectList_Click(object sender, EventArgs e)
        {
            fController.SelectList();
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
