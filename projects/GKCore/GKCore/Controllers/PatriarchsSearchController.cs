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
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Options;
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
        private IList<PatriarchObj> fPatriarches;

        public PatriarchsSearchController(IPatriarchsSearchDlg view) : base(view)
        {
        }

        public override void Init(IBaseWindow baseWin)
        {
            base.Init(baseWin);

            var listModel = new PatriarchsListModel(fBase.Context);
            fView.PatriarchsList.ListMan = listModel;
            fView.PatriarchsList.UpdateContents();
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
            fPatriarches = null;
            try {
                var minGens = (int)fView.MinGensNum.Value;
                var withoutDates = !fView.WithoutDatesCheck.Checked;
                AppHost.Instance.ExecuteWork((controller) => {
                    fPatriarches = PatriarchsMan.GetPatriarchsList(fBase.Context, minGens, withoutDates, controller, true);
                });

                SortHelper.QuickSort(fPatriarches, PatriarchsCompare);

                ((PatriarchsListModel)fView.PatriarchsList.ListMan).DataSource = fPatriarches;
            } finally {
                fView.PatriarchsList.UpdateContents();
            }
        }

        public void SetPatriarch()
        {
            try {
                for (int i = 0; i < fPatriarches.Count; i++) {
                    var pObj = fPatriarches[i];
                    if (pObj.Mark) pObj.IRec.Patriarch = true;
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
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnClose").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }


        private sealed class PatriarchsListModel : SimpleListModel<PatriarchObj>
        {
            public PatriarchsListModel(IBaseContext baseContext) :
                base(baseContext, CreateListColumns())
            {
            }

            public static ListColumns CreateListColumns()
            {
                var result = new ListColumns(GKListType.ltNone);
                result.AddColumn(LangMan.LS(LSID.Enabled), DataType.dtBool, 40, false);
                result.AddColumn(LangMan.LS(LSID.Patriarch), 400, false);
                result.AddColumn(LangMan.LS(LSID.Birth), 90, false);
                result.AddColumn(LangMan.LS(LSID.Descendants), 90, false);
                result.AddColumn(LangMan.LS(LSID.Generations), 90, false);
                return result;
            }

            protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
            {
                object result = null;
                switch (colType) {
                    case 0:
                        result = fFetchedRec.Mark;
                        break;
                    case 1:
                        string pSign = ((fFetchedRec.IRec.Patriarch) ? "[*] " : "");
                        result = pSign + GKUtils.GetNameString(fFetchedRec.IRec, true, false);
                        break;
                    case 2:
                        result = fFetchedRec.BirthYear;
                        break;
                    case 3:
                        result = fFetchedRec.DescendantsCount;
                        break;
                    case 4:
                        result = fFetchedRec.DescGenerations;
                        break;
                }
                return result;
            }

            protected override void SetColumnValueEx(PatriarchObj item, int colIndex, object value)
            {
                if (item != null && colIndex == 0 && value is bool chk)
                    item.Mark = chk;
            }
        }
    }
}
