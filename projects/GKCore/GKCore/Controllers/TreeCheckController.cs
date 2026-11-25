/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
        #region Design components
#pragma warning disable CS0649, IDE0044
        private IButton _btnClose;
        private ITabPage _pageTreeCheck;
        private IButton _btnAnalyseBase;
        private IButton _btnBaseRepair;
        private IMenuItem _miDetails;
        private IMenuItem _miGoToRecord;
        private IMenuItem _miCopyXRef;
        private IListView _ListChecks;
        private ITabPage _pageOptions;
        private ICheckBox _chkCheckPersonPlaces;
        private ICheckBox _chkCheckCensuses;
        private ICheckBox _chkCheckLinks;
#pragma warning restore CS0649, IDE0044
        #endregion

        private readonly List<CheckObj> fChecksList;
        private readonly TreeInspectionOptions fOptions;

        public TreeCheckController(ITreeCheckDlg view) : base(view)
        {
            fChecksList = new List<CheckObj>();

            fOptions = new TreeInspectionOptions();
            fOptions.CheckIndividualPlaces = false;
        }

        public override void InitView(IBaseWindow baseWin)
        {
            base.InitView(baseWin);

            var listModel = new ProblemsListModel(fBase.Context);
            listModel.DataSource = fChecksList;
            _ListChecks.ListMan = listModel;
            _ListChecks.UpdateContents();
        }

        public void CheckBase()
        {
            fOptions.CheckIndividualPlaces = _chkCheckPersonPlaces.Checked;
            fOptions.CheckCensuses = _chkCheckCensuses.Checked;
            fOptions.CheckLinks = _chkCheckLinks.Checked;

            AppHost.Instance.ExecuteWork((controller) => {
                TreeInspector.CheckBase(fBase, fChecksList, controller, fOptions);
            });

            _ListChecks.UpdateContents();
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
            var selData = _ListChecks.GetSelectedData() as CheckObj;
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
            var list = _ListChecks.GetSelectedItems();

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
            _miDetails.Enabled = (rec != null);
            _miGoToRecord.Enabled = (rec != null);
            _miCopyXRef.Enabled = (rec != null);
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.TreeCheck));

            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                if (AppHost.Instance.HasFeatureSupport(Feature.Graphics)) {
                    _btnClose.Text = LangMan.LS(LSID.DlgClose);
                }
                _miDetails.Text = LangMan.LS(LSID.Details);
                _miGoToRecord.Text = LangMan.LS(LSID.GoToPersonRecord);
                _miCopyXRef.Text = LangMan.LS(LSID.CopyXRef);
            }

            _pageTreeCheck.Text = LangMan.LS(LSID.TreeCheck);
            _btnAnalyseBase.Text = LangMan.LS(LSID.Analyze);
            _btnBaseRepair.Text = LangMan.LS(LSID.Repair);

            _pageOptions.Text = LangMan.LS(LSID.MIOptions);
            _chkCheckPersonPlaces.Text = LangMan.LS(LSID.CheckPersonPlaces);
            _chkCheckCensuses.Text = LangMan.LS(LSID.CensusAnalysis);
            _chkCheckLinks.Text = LangMan.LS(LSID.CheckLinks);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _btnClose.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
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
