/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Views;

namespace GKUI.Forms
{
    public sealed partial class TTRecMergeDlg : CommonDialog<IRecMergeDlg, RecMergeController>, IRecMergeDlg
    {
        public TTRecMergeDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            fController = new RecMergeController(this);
            fController.InitView(baseWin);
        }

        private void radMergeMode_Click(object sender, EventArgs e)
        {
            fController.ChangeOption();
        }

        private void chkBookmarkMerged_CheckedChanged(object sender, EventArgs e)
        {
            fController.ChangeOption();
        }

        private void btnSkip_Click(object sender, EventArgs e)
        {
            fController.Skip();
        }

        private void btnSearch_Click(object sender, EventArgs e)
        {
            fController.Reset();
            fController.SearchDuplicates();
        }

        private void btnRec1Select_Click(object sender, EventArgs e)
        {
            fController.SelectRec1();
        }

        private void btnRec2Select_Click(object sender, EventArgs e)
        {
            fController.SelectRec2();
        }

        private void btnMergeToLeft_Click(object sender, EventArgs e)
        {
            fController.MergeToLeft();
        }

        private void btnMergeToRight_Click(object sender, EventArgs e)
        {
            fController.MergeToRight();
        }

        public void SetRec1(GDMRecord value)
        {
            fController.SetRec1(value);
        }

        public void SetRec2(GDMRecord value)
        {
            fController.SetRec2(value);
        }

        private void btnEditLeft_Click(object sender, EventArgs e)
        {
            fController.EditLeft();
        }

        private void btnEditRight_Click(object sender, EventArgs e)
        {
            fController.EditRight();
        }
    }
}
