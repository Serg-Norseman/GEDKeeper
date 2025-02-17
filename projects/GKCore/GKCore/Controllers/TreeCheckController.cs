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
using GKCore.Tools;
using GKCore.Types;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public class TreeCheckController : DialogController<ITreeCheckDlg>
    {
        private readonly List<TreeInspector.CheckObj> fChecksList;
        private readonly TreeInspectionOptions fOptions;

        public TreeCheckController(ITreeCheckDlg view) : base(view)
        {
            fChecksList = new List<TreeInspector.CheckObj>();

            fOptions = new TreeInspectionOptions();
            fOptions.CheckIndividualPlaces = false;
        }

        public override void UpdateView()
        {
        }

        public void CheckBase()
        {
            fOptions.CheckIndividualPlaces = GetControl<ICheckBox>("chkCheckPersonPlaces").Checked;
            fOptions.CheckCensuses = GetControl<ICheckBox>("chkCheckCensuses").Checked;
            fOptions.CheckLinks = GetControl<ICheckBox>("chkCheckLinks").Checked;

            AppHost.Instance.ExecuteWork((controller) => {
                TreeInspector.CheckBase(fBase, fChecksList, controller, fOptions);
            });

            fView.ChecksList.BeginUpdate();
            try {
                fView.ChecksList.ClearItems();

                foreach (TreeInspector.CheckObj checkObj in fChecksList) {
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

        public async Task Repair()
        {
            try {
                bool modified = false;
                int num = fView.ChecksList.Items.Count;
                for (int i = 0; i < num; i++) {
                    IListItem item = fView.ChecksList.Items[i];
                    if (item.Checked) {
                        var checkObj = item.Tag as TreeInspector.CheckObj;
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
            return ((TreeInspector.CheckObj)fView.ChecksList.GetSelectedData()).Rec;
        }

        public IList<GDMRecord> GetCheckedRecords()
        {
            var result = new List<GDMRecord>();

            int num = fView.ChecksList.Items.Count;
            for (int i = 0; i < num; i++) {
                IListItem item = fView.ChecksList.Items[i];
                if (item.Checked) {
                    var checkObj = item.Tag as TreeInspector.CheckObj;
                    result.Add(checkObj.Rec);
                }
            }

            return result;
        }

        public void ShowDetails()
        {
            GDMRecord rec = GetSelectedRecord();
            if (rec == null) return;

            BaseController.ViewRecordInfo(fView, fBase, rec);
        }

        public void CopySelectedXRefs(IList<object> list)
        {
            var text = new StringBuilder();
            foreach (var item in list) {
                var checkObj = (TreeInspector.CheckObj)item;
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
            fView.Title = LangMan.LS(LSID.TreeCheck);

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

            fView.ChecksList.AddColumn(LangMan.LS(LSID.Record), 400, false);
            fView.ChecksList.AddColumn(LangMan.LS(LSID.Problem), 200, false);
            fView.ChecksList.AddColumn(LangMan.LS(LSID.Solve), 200, false);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnClose").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
