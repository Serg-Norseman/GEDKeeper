/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Tools;

namespace GKUI.Forms
{
    public sealed partial class TTTreeSplitDlg : CommonDialog<ITreeSplitDlg, TreeSplitController>, ITreeSplitDlg
    {
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
            InitializeComponent();

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
