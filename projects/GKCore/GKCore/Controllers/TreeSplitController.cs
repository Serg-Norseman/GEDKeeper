/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using System.IO;
using GDModel;
using GDModel.Providers.GEDCOM;
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
    public class TreeSplitController : DialogController<ITreeSplitDlg>
    {
        #region Design components
#pragma warning disable CS0649, IDE0044
        private ITabPage _pageTreeSplit;
        private IButton _btnClose;
        private IListView _ListSelected;
        private IListView _ListSkipped;
        private IButton _btnSelectAll;
        private IButton _btnSelectFamily;
        private IButton _btnSelectAncestors;
        private IButton _btnSelectDescendants;
        private IButton _btnSelectList;
        private IButton _btnDelete;
        private IButton _btnSave;
#pragma warning restore CS0649, IDE0044
        #endregion

        private bool fIndiMode;
        private readonly List<GDMRecord> fSplitList;

        private List<GDMRecord> fSelectedRecList;
        private List<GDMRecord> fSkippedRecList;


        public TreeSplitController(ITreeSplitDlg view) : base(view)
        {
            fSplitList = new List<GDMRecord>();
        }

        public override void InitView(IBaseWindow baseWin)
        {
            base.InitView(baseWin);

            fSelectedRecList = new List<GDMRecord>();
            var listModel = new RecsListModel(baseWin.Context, LangMan.LS(LSID.Record));
            listModel.DataSource = fSelectedRecList;
            _ListSelected.ListMan = listModel;
            _ListSelected.UpdateContents();

            fSkippedRecList = new List<GDMRecord>();
            listModel = new RecsListModel(baseWin.Context, LangMan.LS(LSID.Record));
            listModel.DataSource = fSkippedRecList;
            _ListSkipped.ListMan = listModel;
            _ListSkipped.UpdateContents();
        }

        public override void UpdateView()
        {
            try {
                fSelectedRecList.Clear();
                fSkippedRecList.Clear();

                var tree = fBase.Context.Tree;
                int cnt = 0;
                for (int i = 0, num = tree.RecordsCount; i < num; i++) {
                    GDMRecord rec = tree[i];

                    if (fIndiMode && rec is GDMIndividualRecord iRec) {
                        cnt++;
                        if (fSplitList.IndexOf(iRec) >= 0) {
                            fSelectedRecList.Add(iRec);
                        } else {
                            fSkippedRecList.Add(iRec);
                        }
                    } else {
                        cnt++;
                        if (fSplitList.IndexOf(rec) >= 0) {
                            fSelectedRecList.Add(rec);
                        }
                    }
                }

                fView.SetTitle($@"{fSelectedRecList.Count} / {cnt}");
            } finally {
                _ListSelected.UpdateContents();
                _ListSkipped.UpdateContents();
            }
        }

        public void Select(TreeWalkMode walkMode)
        {
            fIndiMode = true;
            Select(fBase.GetSelectedPerson(), walkMode);
        }

        public void Select(GDMIndividualRecord startPerson, TreeWalkMode walkMode)
        {
            fSplitList.Clear();

            if (startPerson == null) {
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.NotSelectedPerson));
            } else {
                TreeTools.WalkTree(fBase.Context.Tree, startPerson, walkMode, fSplitList);
            }

            UpdateView();
        }

        public void SelectList()
        {
            fIndiMode = false;

            fSplitList.Clear();
            var baseList = fBase.GetContentList(fBase.GetSelectedRecordType());
            fSplitList.AddRange(baseList);

            UpdateView();
        }

        public async void Delete()
        {
            int num = fSplitList.Count;
            if (num == 0) return;

            for (int i = 0; i < num; i++) {
                object obj = fSplitList[i];

                if (obj is GDMIndividualRecord iRec) {
                    await BaseController.DeleteRecord(fBase, iRec, false);
                }
            }

            fSplitList.Clear();
            UpdateView();
            fBase.RefreshLists(false);

            await AppHost.StdDialogs.ShowMessage(LangMan.LS(LSID.RecsDeleted));
        }

        public async void Save()
        {
            string fileName = await AppHost.StdDialogs.GetSaveFile("", "", LangMan.LS(LSID.GEDCOMFilter), 1, GKData.GEDCOM_EXT, "");
            if (string.IsNullOrEmpty(fileName)) return;

            TreeTools.CheckRelations(fBase.Context.Tree, fSplitList);

            var tree = fBase.Context.Tree;
            GKUtils.PrepareHeader(tree, fileName, GlobalOptions.Instance.DefCharacterSet, true);

            using (StreamWriter fs = new StreamWriter(fileName, false, GEDCOMUtils.GetEncodingByCharacterSet(tree.Header.CharacterSet.Value))) {
                var gedcomProvider = new GEDCOMProvider(tree);
                gedcomProvider.SaveToStream(fs, fSplitList);
            }
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.TreeSplit));

            if (AppHost.Instance.HasFeatureSupport(Feature.Graphics) && !AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                _pageTreeSplit.Text = LangMan.LS(LSID.TreeSplit);
                _btnClose.Text = LangMan.LS(LSID.DlgClose);
            }

            _btnSelectAll.Text = LangMan.LS(LSID.SelAll);
            _btnSelectFamily.Text = LangMan.LS(LSID.SelFamily);
            _btnSelectAncestors.Text = LangMan.LS(LSID.SelAncestors);
            _btnSelectDescendants.Text = LangMan.LS(LSID.SelDescendants);
            _btnSelectList.Text = LangMan.LS(LSID.SelList);
            _btnDelete.Text = LangMan.LS(LSID.DoDelete);
            _btnSave.Text = LangMan.LS(LSID.MIFileSaveAs);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _btnClose.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
