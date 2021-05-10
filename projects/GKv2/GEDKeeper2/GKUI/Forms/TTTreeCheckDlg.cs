/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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
using System.Text;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Views;
using GKCore.Tools;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class TTTreeCheckDlg : CommonDialog, ITreeCheckDlg
    {
        private readonly TreeCheckController fController;

        private GKListView ListChecks;

        #region View Interface

        IListViewEx ITreeCheckDlg.ChecksList
        {
            get { return ListChecks; }
        }

        #endregion

        public TTTreeCheckDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            btnClose.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            fController = new TreeCheckController(this);
            fController.Init(baseWin);

            ListChecks = UIHelper.CreateListView(panProblemsContainer);
            ListChecks.DoubleClick += ListChecks_DblClick;
            ListChecks.CheckBoxes = true;
            ListChecks.AddColumn(LangMan.LS(LSID.LSID_Record), 400, false);
            ListChecks.AddColumn(LangMan.LS(LSID.LSID_Problem), 200, false);
            ListChecks.AddColumn(LangMan.LS(LSID.LSID_Solve), 200, false);
            ListChecks.ContextMenuStrip = contextMenu;

            SetLang();
        }

        public void SetLang()
        {
            Title = LangMan.LS(LSID.LSID_ToolOp_7);
            pageTreeCheck.Text = LangMan.LS(LSID.LSID_ToolOp_7);
            btnClose.Text = LangMan.LS(LSID.LSID_DlgClose);
            btnAnalyseBase.Text = LangMan.LS(LSID.LSID_Analyze);
            btnBaseRepair.Text = LangMan.LS(LSID.LSID_Repair);
            miDetails.Text = LangMan.LS(LSID.LSID_Details);
            miGoToRecord.Text = LangMan.LS(LSID.LSID_GoToPersonRecord);
            miCopyXRef.Text = LangMan.LS(LSID.LSID_CopyXRef);
        }

        private void btnAnalyseBase_Click(object sender, EventArgs e)
        {
            fController.CheckBase();
        }

        private void btnBaseRepair_Click(object sender, EventArgs e)
        {
            fController.Repair();
        }

        private void ListChecks_DblClick(object sender, EventArgs e)
        {
            fController.SelectRecord();
        }

        private void miDetails_Click(object sender, EventArgs e)
        {
            fController.ShowDetails();
        }

        private void miGoToRecord_Click(object sender, EventArgs e)
        {
            fController.SelectRecord();
        }

        private void contextMenu_Opening(object sender, System.ComponentModel.CancelEventArgs e)
        {
            var rec = fController.GetSelectedRecord();
            miDetails.Enabled = (rec != null);
            miGoToRecord.Enabled = (rec != null);
            miCopyXRef.Enabled = (rec != null);
        }

        public void miCopyXRef_Click(object sender, EventArgs e)
        {
            var list = ListChecks.GetSelectedItems();
            var text = new StringBuilder();
            foreach (var item in list) {
                var checkObj = (TreeTools.CheckObj)item;
                text.Append(checkObj.Rec.XRef);
                text.Append("\r\n");
            }

            UIHelper.SetClipboardText(text.ToString());
        }
    }
}
