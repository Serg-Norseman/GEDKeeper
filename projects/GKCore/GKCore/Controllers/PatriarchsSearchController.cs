/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using GDModel;
using GKCore.Design.Controls;
using GKCore.Design;
using GKCore.Design.Views;
using GKCore.Tools;
using GKCore.Types;
using GKUI.Themes;

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
            fView.Title = LangMan.LS(LSID.PatriarchsSearch);

            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                GetControl<ITabPage>("pagePatSearch").Text = LangMan.LS(LSID.PatriarchsSearch);
                GetControl<IButton>("btnClose").Text = LangMan.LS(LSID.DlgClose);
            }

            GetControl<ILabel>("lblMinGenerations").Text = LangMan.LS(LSID.MinGenerations);
            GetControl<IButton>("btnSetPatriarch").Text = LangMan.LS(LSID.SetPatFlag);
            GetControl<IButton>("btnPatSearch").Text = LangMan.LS(LSID.Search);
            GetControl<ICheckBox>("chkWithoutDates").Text = LangMan.LS(LSID.WithoutDates);
            GetControl<IButton>("btnPatriarchsDiagram").Text = LangMan.LS(LSID.PatriarchsDiagram);

            fView.PatriarchsList.AddColumn(LangMan.LS(LSID.Patriarch), 400, false);
            fView.PatriarchsList.AddColumn(LangMan.LS(LSID.Birth), 90, false);
            fView.PatriarchsList.AddColumn(LangMan.LS(LSID.Descendants), 90, false);
            fView.PatriarchsList.AddColumn(LangMan.LS(LSID.Generations), 90, false);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnClose").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
