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
using System.Text;
using System.Threading.Tasks;
using GDModel;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;
using GKCore.Locales;
using GKCore.Options;
using GKCore.Tools;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public class TreeCheckController : DialogController<ITreeCheckDlg>
    {
        private readonly List<CheckObj> fChecksList;
        private readonly TreeInspectionOptions fOptions;

        public TreeCheckController(ITreeCheckDlg view) : base(view)
        {
            fChecksList = new List<CheckObj>();

            fOptions = new TreeInspectionOptions();
            fOptions.CheckIndividualPlaces = false;
        }

        public override void Init(IBaseWindow baseWin)
        {
            base.Init(baseWin);

            var listModel = new ProblemsListModel(fBase.Context);
            listModel.DataSource = fChecksList;
            fView.ChecksList.ListMan = listModel;
            fView.ChecksList.UpdateContents();
        }

        public void CheckBase()
        {
            fOptions.CheckIndividualPlaces = GetControl<ICheckBox>("chkCheckPersonPlaces").Checked;
            fOptions.CheckCensuses = GetControl<ICheckBox>("chkCheckCensuses").Checked;
            fOptions.CheckLinks = GetControl<ICheckBox>("chkCheckLinks").Checked;

            AppHost.Instance.ExecuteWork((controller) => {
                TreeInspector.CheckBase(fBase, fChecksList, controller, fOptions);
            });

            fView.ChecksList.UpdateContents();
        }

        public async Task Repair()
        {
            try {
                bool modified = false;
                for (int i = 0, num = fChecksList.Count; i < num; i++) {
                    var checkObj = fChecksList[i];
                    if (checkObj.Marked) {
                        await TreeInspector.RepairProblem(fView, fBase, checkObj);
                        modified = true;
                    }
                }

                if (modified) {
                    fBase.Context.SetModified();
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
            var selData = fView.ChecksList.GetSelectedData() as CheckObj;
            return (selData == null) ? null : selData.Rec;
        }

        public void ShowDetails()
        {
            GDMRecord rec = GetSelectedRecord();
            if (rec != null)
                BaseController.ViewRecordInfo(fView, fBase, rec);
        }

        public void CopySelectedXRefs()
        {
            var list = fView.ChecksList.GetSelectedItems();

            var text = new StringBuilder();
            foreach (var item in list) {
                var checkObj = (CheckObj)item;
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
            fView.SetTitle(LangMan.LS(LSID.TreeCheck));

            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                GetControl<IButton>("btnClose").Text = LangMan.LS(LSID.DlgClose);
                GetControl<IMenuItem>("miDetails").Text = LangMan.LS(LSID.Details);
                GetControl<IMenuItem>("miGoToRecord").Text = LangMan.LS(LSID.GoToPersonRecord);
                GetControl<IMenuItem>("miCopyXRef").Text = LangMan.LS(LSID.CopyXRef);
            }

            GetControl<ITabPage>("pageTreeCheck").Text = LangMan.LS(LSID.TreeCheck);
            GetControl<IButton>("btnAnalyseBase").Text = LangMan.LS(LSID.Analyze);
            GetControl<IButton>("btnBaseRepair").Text = LangMan.LS(LSID.Repair);

            GetControl<ITabPage>("pageOptions").Text = LangMan.LS(LSID.MIOptions);
            GetControl<ICheckBox>("chkCheckPersonPlaces").Text = LangMan.LS(LSID.CheckPersonPlaces);
            GetControl<ICheckBox>("chkCheckCensuses").Text = LangMan.LS(LSID.CensusAnalysis);
            GetControl<ICheckBox>("chkCheckLinks").Text = LangMan.LS(LSID.CheckLinks);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnClose").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }


        private sealed class ProblemsListModel : SimpleListModel<CheckObj>
        {
            public ProblemsListModel(BaseContext baseContext) :
                base(baseContext, CreateListColumns())
            {
            }

            public static ListColumns CreateListColumns()
            {
                var result = new ListColumns(GKListType.ltNone);
                result.AddColumn(LangMan.LS(LSID.Enabled), DataType.dtBool, 40, false);
                result.AddColumn(LangMan.LS(LSID.Record), DataType.dtString, 400, false);
                result.AddColumn(LangMan.LS(LSID.Problem), DataType.dtString, 200, false);
                result.AddColumn(LangMan.LS(LSID.Solve), DataType.dtString, 200, false);
                return result;
            }

            protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
            {
                object result = null;
                switch (colType) {
                    case 0:
                        result = fFetchedRec.Marked;
                        break;
                    case 1:
                        result = fFetchedRec.GetRecordName(fBaseContext.Tree);
                        break;
                    case 2:
                        result = fFetchedRec.Comment;
                        break;
                    case 3:
                        result = LangMan.LS(GKData.CheckSolveNames[(int)fFetchedRec.Solve]);
                        break;
                }
                return result;
            }

            protected override void SetColumnValueEx(CheckObj item, int colIndex, object value)
            {
                if (item != null && colIndex == 0 && value is bool chk)
                    item.Marked = chk;
            }
        }
    }
}
