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

using GKCommon.GEDCOM;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Views;
using GKCore.Tools;
using GKUI.Components;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class TTTreeCheckDlg : CommonDialog, ITreeCheckDlg
    {
        private readonly TreeCheckController fController;

        private GKListView ListChecks;

        #region View Interface

        IListView ITreeCheckDlg.ChecksList
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

            SetLang();
        }

        public void SetLang()
        {
            Text = LangMan.LS(LSID.LSID_ToolOp_7);
            pageTreeCheck.Text = LangMan.LS(LSID.LSID_ToolOp_7);
            btnClose.Text = LangMan.LS(LSID.LSID_DlgClose);
            btnAnalyseBase.Text = LangMan.LS(LSID.LSID_Analysis);
            btnBaseRepair.Text = LangMan.LS(LSID.LSID_Repair);
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
            GEDCOMRecord rec = ((TreeTools.CheckObj)ListChecks.GetSelectedData()).Rec;
            if (rec == null) return;

            fController.Base.SelectRecordByXRef(rec.XRef);
            Close();
        }
    }
}
