/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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
using System.Collections.Generic;
using System.Windows.Forms;

using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Tools;
using GKUI.Components;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class TTTreeCheckDlg : CommonDialog
    {
        private readonly IBaseWindow fBase;
        private readonly GEDCOMTree fTree;
        private readonly List<TreeTools.CheckObj> fChecksList;

        private GKListView ListChecks;

        public TTTreeCheckDlg(IBaseWindow baseWin)
        {
            InitializeComponent();
            btnClose.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            fBase = baseWin;
            fTree = fBase.Context.Tree;
            fChecksList = new List<TreeTools.CheckObj>();

            ListChecks = UIHelper.CreateListView(Panel1);
            ListChecks.CheckBoxes = true;
            ListChecks.DoubleClick += ListChecks_DblClick;
            ListChecks.AddColumn(LangMan.LS(LSID.LSID_Record), 400, false);
            ListChecks.AddColumn(LangMan.LS(LSID.LSID_Problem), 200, false);
            ListChecks.AddColumn(LangMan.LS(LSID.LSID_Solve), 200, false);

            SetLang();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                //fChecksList.Dispose();
            }
            base.Dispose(disposing);
        }

        public void SetLang()
        {
            Text = LangMan.LS(LSID.LSID_MITreeTools);
            pageTreeCheck.Text = LangMan.LS(LSID.LSID_ToolOp_7);
            pageTreeCheckOptions.Text = LangMan.LS(LSID.LSID_MIOptions);
            btnClose.Text = LangMan.LS(LSID.LSID_DlgClose);
            btnAnalyseBase.Text = LangMan.LS(LSID.LSID_Analysis);
            btnBaseRepair.Text = LangMan.LS(LSID.LSID_Repair);
        }

        private void btnAnalyseBase_Click(object sender, EventArgs e)
        {
            CheckBase();
        }

        private void CheckBase()
        {
            TreeTools.CheckBase(fBase, fChecksList);

            ListChecks.BeginUpdate();
            try {
                ListChecks.ClearItems();

                foreach (TreeTools.CheckObj checkObj in fChecksList) {
                    ListChecks.AddItem(checkObj, new object[] { checkObj.GetRecordName(),
                        checkObj.Comment,
                        LangMan.LS(GKData.CheckSolveNames[(int)checkObj.Solve])
                    });
                }

                ListChecks.AutoResizeColumns(ColumnHeaderAutoResizeStyle.ColumnContent);
            } finally {
                ListChecks.EndUpdate();
            }
        }

        private void btnBaseRepair_Click(object sender, EventArgs e)
        {
            try {
                int num = ListChecks.Items.Count;
                for (int i = 0; i < num; i++) {
                    GKListItem item = (GKListItem)ListChecks.Items[i];
                    if (!item.Checked) continue;

                    TreeTools.CheckObj checkObj = item.Data as TreeTools.CheckObj;
                    TreeTools.RepairProblem(fBase, checkObj);
                }
            } finally {
                fBase.RefreshLists(false);
                CheckBase();
            }
        }

        private void ListChecks_DblClick(object sender, EventArgs e)
        {
            GKListItem item = ListChecks.GetSelectedItem();
            if (item == null) return;

            GEDCOMRecord rec = ((TreeTools.CheckObj)item.Data).Rec;
            if (rec == null) return;

            fBase.SelectRecordByXRef(rec.XRef);
            Close();
        }
    }
}
