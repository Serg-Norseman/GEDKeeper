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

using BSLib;
using GDModel;
using GKCore.MVP;
using GKCore.MVP.Views;
using GKCore.Types;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public class PatriarchsSearchController : DialogController<IPatriarchsSearchDlg>
    {
        public PatriarchsSearchController(IPatriarchsSearchDlg view) : base(view)
        {
        }

        public override void UpdateView()
        {
        }

        private static int PatriarchsCompare(object item1, object item2)
        {
            return ((PatriarchObj)item1).BirthYear - ((PatriarchObj)item2).BirthYear;
        }

        public void SelectPatriarch()
        {
            GDMIndividualRecord iRec = fView.PatriarchsList.GetSelectedData() as GDMIndividualRecord;
            if (iRec == null) return;

            fBase.SelectRecordByXRef(iRec.XRef);
            fView.Close();
        }

        public void Search()
        {
            fView.PatriarchsList.BeginUpdate();
            ExtList<PatriarchObj> lst = null;
            try {
                fView.PatriarchsList.ClearItems();
                lst = PatriarchsMan.GetPatriarchsList(fBase.Context, (int)fView.MinGensNum.Value, !fView.WithoutDatesCheck.Checked);
                lst.QuickSort(PatriarchsCompare);

                int num = lst.Count;
                for (int i = 0; i < num; i++) {
                    PatriarchObj pObj = lst[i];
                    string pSign = ((pObj.IRec.Patriarch) ? "[*] " : "");

                    fView.PatriarchsList.AddItem(pObj.IRec, new object[] { pSign + pObj.IRec.GetNameString(true, false),
                        pObj.BirthYear, pObj.DescendantsCount, pObj.DescGenerations
                    });
                }
            } finally {
                if (lst != null) lst.Dispose();
                fView.PatriarchsList.EndUpdate();
            }
        }

        public void SetPatriarch()
        {
            try {
                var iRec = fView.PatriarchsList.GetSelectedData() as GDMIndividualRecord;
                if (iRec != null) {
                    iRec.Patriarch = true;
                }

                fBase.RefreshLists(false);
            } finally {
                Search();
            }
        }

        public void ShowPatriarchsDiagram()
        {
            var wnd = AppHost.Container.Resolve<IPatriarchsViewer>(fBase, (int)fView.MinGensNum.Value);
            wnd.Show(false);
        }
    }
}
