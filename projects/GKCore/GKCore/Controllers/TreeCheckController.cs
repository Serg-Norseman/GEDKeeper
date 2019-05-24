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

using System.Collections.Generic;
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
            TreeTools.CheckBase(fBase, fChecksList);

            fView.ChecksList.BeginUpdate();
            try {
                fView.ChecksList.ClearItems();

                foreach (TreeTools.CheckObj checkObj in fChecksList) {
                    fView.ChecksList.AddItem(checkObj, new object[] { checkObj.GetRecordName(),
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
            GDMRecord rec = ((TreeTools.CheckObj)fView.ChecksList.GetSelectedData()).Rec;
            if (rec == null) return;

            fBase.SelectRecordByXRef(rec.XRef);
            fView.Close();
        }
    }
}
