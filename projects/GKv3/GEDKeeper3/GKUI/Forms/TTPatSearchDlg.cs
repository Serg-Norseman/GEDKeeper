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
using Eto.Forms;
using BSLib;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;
using GKUI.Components;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class TTPatSearchDlg : Dialog
    {
        private readonly IBaseWindow fBase;
        private readonly GEDCOMTree fTree;

        private GKListView ListPatriarchs;

        public TTPatSearchDlg(IBaseWindow baseWin)
        {
            InitializeComponent();
            btnClose.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            fBase = baseWin;
            fTree = fBase.Context.Tree;

            ListPatriarchs = new GKListView();
            ListPatriarchs.MouseDoubleClick += ListPatriarchs_DblClick;
            ListPatriarchs.AddColumn(LangMan.LS(LSID.LSID_Patriarch), 400, false);
            ListPatriarchs.AddColumn(LangMan.LS(LSID.LSID_Birth), 90, false);
            ListPatriarchs.AddColumn(LangMan.LS(LSID.LSID_Descendants), 90, false);
            ListPatriarchs.AddColumn(LangMan.LS(LSID.LSID_Generations), 90, false);
            panPatriarchsContainer.Content = ListPatriarchs;

            SetLang();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
            }
            base.Dispose(disposing);
        }

        public void SetLang()
        {
            Title = LangMan.LS(LSID.LSID_MITreeTools);
            pagePatSearch.Text = LangMan.LS(LSID.LSID_ToolOp_8);
            btnClose.Text = LangMan.LS(LSID.LSID_DlgClose);
            lblMinGenerations.Text = LangMan.LS(LSID.LSID_MinGenerations);
            btnSetPatriarch.Text = LangMan.LS(LSID.LSID_SetPatFlag);
            btnPatSearch.Text = LangMan.LS(LSID.LSID_Search);
            chkWithoutDates.Text = LangMan.LS(LSID.LSID_WithoutDates);
            btnPatriarchsDiagram.Text = LangMan.LS(LSID.LSID_PatriarchsDiagram);
        }

        private void ListPatriarchs_DblClick(object sender, EventArgs e)
        {
            GKListItem item = ListPatriarchs.GetSelectedItem();
            if (item == null) return;

            GEDCOMIndividualRecord iRec = item.Data as GEDCOMIndividualRecord;
            if (iRec == null) return;

            fBase.SelectRecordByXRef(iRec.XRef);
            Close();
        }

        private static int PatriarchsCompare(object item1, object item2)
        {
            return ((PatriarchObj)item1).BirthYear - ((PatriarchObj)item2).BirthYear;
        }

        private void btnPatSearch_Click(object sender, EventArgs e)
        {
            ListPatriarchs.BeginUpdate();
            ExtList<PatriarchObj> lst = null;
            try {
                ListPatriarchs.ClearItems();
                lst = PatriarchsMan.GetPatriarchsList(fBase.Context,
                                                      (int)edMinGens.Value, !chkWithoutDates.Checked.GetValueOrDefault());
                lst.QuickSort(PatriarchsCompare);

                int num = lst.Count;
                for (int i = 0; i < num; i++) {
                    PatriarchObj pObj = lst[i];
                    string pSign = ((pObj.IRec.Patriarch) ? "[*] " : "");

                    ListPatriarchs.AddItem(pObj.IRec, new object[] { pSign + GKUtils.GetNameString(pObj.IRec, true, false),
                                               pObj.BirthYear,
                                               pObj.DescendantsCount,
                                               pObj.DescGenerations });
                }
            } finally {
                if (lst != null) lst.Dispose();
                ListPatriarchs.EndUpdate();
            }
        }

        private void btnSetPatriarch_Click(object sender, EventArgs e)
        {
            try {
                GKListItem item = ListPatriarchs.GetSelectedItem();
                if (item == null) return;

                GEDCOMIndividualRecord iRec = item.Data as GEDCOMIndividualRecord;
                if (iRec != null) {
                    iRec.Patriarch = true;
                }

                fBase.RefreshLists(false);
            } finally {
                btnPatSearch_Click(null, null);
            }
        }

        private void btnPatriarchsDiagram_Click(object sender, EventArgs e)
        {
            PatriarchsViewerWin wnd = new PatriarchsViewerWin(fBase, (int)(edMinGens.Value));
            wnd.Show();
        }
    }
}
