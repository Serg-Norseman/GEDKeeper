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
using BSLib;
using BSLib.Design.MVP.Controls;
using GDModel;
using GKCore.MVP;
using GKCore.MVP.Views;
using GKCore.Tools;
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
            IList<PatriarchObj> lst = null;
            try {
                fView.PatriarchsList.ClearItems();

                var minGens = (int)fView.MinGensNum.Value;
                var withoutDates = !fView.WithoutDatesCheck.Checked;
                AppHost.Instance.ExecuteWork((controller) => {
                    lst = PatriarchsMan.GetPatriarchsList(fBase.Context, minGens, withoutDates, controller, true);
                });

                SortHelper.QuickSort(lst, PatriarchsCompare);

                int num = lst.Count;
                for (int i = 0; i < num; i++) {
                    PatriarchObj pObj = lst[i];
                    string pSign = ((pObj.IRec.Patriarch) ? "[*] " : "");

                    fView.PatriarchsList.AddItem(pObj.IRec, new object[] { pSign + GKUtils.GetNameString(pObj.IRec, true, false),
                        pObj.BirthYear, pObj.DescendantsCount, pObj.DescGenerations
                    });
                }
            } finally {
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

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.LSID_ToolOp_8);

            GetControl<ITabPage>("pagePatSearch").Text = LangMan.LS(LSID.LSID_ToolOp_8);
            GetControl<IButton>("btnClose").Text = LangMan.LS(LSID.LSID_DlgClose);
            GetControl<ILabel>("lblMinGenerations").Text = LangMan.LS(LSID.LSID_MinGenerations);
            GetControl<IButton>("btnSetPatriarch").Text = LangMan.LS(LSID.LSID_SetPatFlag);
            GetControl<IButton>("btnPatSearch").Text = LangMan.LS(LSID.LSID_Search);
            GetControl<ICheckBox>("chkWithoutDates").Text = LangMan.LS(LSID.LSID_WithoutDates);
            GetControl<IButton>("btnPatriarchsDiagram").Text = LangMan.LS(LSID.LSID_PatriarchsDiagram);

            fView.PatriarchsList.AddColumn(LangMan.LS(LSID.LSID_Patriarch), 400, false);
            fView.PatriarchsList.AddColumn(LangMan.LS(LSID.LSID_Birth), 90, false);
            fView.PatriarchsList.AddColumn(LangMan.LS(LSID.LSID_Descendants), 90, false);
            fView.PatriarchsList.AddColumn(LangMan.LS(LSID.LSID_Generations), 90, false);
        }
    }
}
