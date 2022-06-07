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

using System.Collections.Generic;
using System.Text;
using BSLib.Design.MVP.Controls;
using GDModel;
using GKCore.Interfaces;
using GKCore.MVP;
using GKCore.MVP.Views;
using GKCore.Tools;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public class TreeCheckController : DialogController<ITreeCheckDlg>
    {
        private readonly List<TreeTools.CheckObj> fChecksList;

        public TreeCheckController(ITreeCheckDlg view) : base(view)
        {
            fChecksList = new List<TreeTools.CheckObj>();
        }

        public override void UpdateView()
        {
        }

        public void CheckBase()
        {
            AppHost.Instance.ExecuteWork((controller) => {
                TreeTools.CheckBase(fBase, fChecksList, controller);
            });

            fView.ChecksList.BeginUpdate();
            try {
                fView.ChecksList.ClearItems();

                foreach (TreeTools.CheckObj checkObj in fChecksList) {
                    fView.ChecksList.AddItem(checkObj, new object[] {
                        checkObj.GetRecordName(fBase.Context.Tree),
                        checkObj.Comment,
                        LangMan.LS(GKData.CheckSolveNames[(int)checkObj.Solve])
                    });
                }

                // TODO
                //fView.ChecksList.AutoResizeColumns(ColumnHeaderAutoResizeStyle.ColumnContent);
            } finally {
                fView.ChecksList.EndUpdate();
            }
        }

        public void Repair()
        {
            try {
                int num = fView.ChecksList.Items.Count;
                for (int i = 0; i < num; i++) {
                    IListItem item = fView.ChecksList.Items[i];
                    if (item.Checked) {
                        var checkObj = item.Data as TreeTools.CheckObj;
                        TreeTools.RepairProblem(fBase, checkObj);
                    }
                }
            } finally {
                fBase.RefreshLists(false);
                CheckBase();
            }
        }

        public void SelectRecord()
        {
            GDMRecord rec = GetSelectedRecord();
            if (rec == null) return;

            fView.Close();
            fBase.SelectRecordByXRef(rec.XRef);
        }

        public GDMRecord GetSelectedRecord()
        {
            return ((TreeTools.CheckObj)fView.ChecksList.GetSelectedData()).Rec;
        }

        public IList<GDMRecord> GetCheckedRecords()
        {
            var result = new List<GDMRecord>();

            int num = fView.ChecksList.Items.Count;
            for (int i = 0; i < num; i++) {
                IListItem item = fView.ChecksList.Items[i];
                if (item.Checked) {
                    var checkObj = item.Data as TreeTools.CheckObj;
                    result.Add(checkObj.Rec);
                }
            }

            return result;
        }

        public void ShowDetails()
        {
            GDMRecord rec = GetSelectedRecord();
            if (rec == null) return;

            BaseController.ViewRecordInfo(fBase, rec);
        }

        public void CopySelectedXRefs(IList<object> list)
        {
            var text = new StringBuilder();
            foreach (var item in list) {
                var checkObj = (TreeTools.CheckObj)item;
                text.Append(checkObj.Rec.XRef);
                text.Append("\r\n");
            }
            AppHost.Instance.SetClipboardText(text.ToString());
        }

        public void OpeningContextMenu()
        {
            var rec = GetSelectedRecord();
            GetControl<IMenuItem>("miDetails").Enabled = (rec != null);
            GetControl<IMenuItem>("miGoToRecord").Enabled = (rec != null);
            GetControl<IMenuItem>("miCopyXRef").Enabled = (rec != null);
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.LSID_ToolOp_7);

            GetControl<ITabPage>("pageTreeCheck").Text = LangMan.LS(LSID.LSID_ToolOp_7);
            GetControl<IButton>("btnClose").Text = LangMan.LS(LSID.LSID_DlgClose);
            GetControl<IButton>("btnAnalyseBase").Text = LangMan.LS(LSID.LSID_Analyze);
            GetControl<IButton>("btnBaseRepair").Text = LangMan.LS(LSID.LSID_Repair);
            GetControl<IMenuItem>("miDetails").Text = LangMan.LS(LSID.LSID_Details);
            GetControl<IMenuItem>("miGoToRecord").Text = LangMan.LS(LSID.LSID_GoToPersonRecord);
            GetControl<IMenuItem>("miCopyXRef").Text = LangMan.LS(LSID.LSID_CopyXRef);

            fView.ChecksList.AddColumn(LangMan.LS(LSID.LSID_Record), 400, false);
            fView.ChecksList.AddColumn(LangMan.LS(LSID.LSID_Problem), 200, false);
            fView.ChecksList.AddColumn(LangMan.LS(LSID.LSID_Solve), 200, false);
        }
    }
}
