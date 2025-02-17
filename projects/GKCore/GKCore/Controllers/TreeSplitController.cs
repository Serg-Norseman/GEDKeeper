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
using System.IO;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Options;
using GKCore.Tools;
using GKCore.Types;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public class TreeSplitController : DialogController<ITreeSplitDlg>
    {
        private bool fIndiMode;
        private readonly List<GDMRecord> fSplitList;

        public TreeSplitController(ITreeSplitDlg view) : base(view)
        {
            fSplitList = new List<GDMRecord>();
        }

        public override void UpdateView()
        {
            fView.SelectedList.BeginUpdate();
            fView.SelectedList.ClearItems();
            fView.SkippedList.BeginUpdate();
            fView.SkippedList.ClearItems();
            try {
                var tree = fBase.Context.Tree;
                int cnt = 0;
                int num = tree.RecordsCount;
                for (int i = 0; i < num; i++) {
                    GDMRecord rec = tree[i];
                    if (fIndiMode && rec is GDMIndividualRecord) {
                        cnt++;
                        GDMIndividualRecord iRec = rec as GDMIndividualRecord;
                        string st = iRec.XRef + " / " + GKUtils.GetNameString(iRec, false);

                        if (fSplitList.IndexOf(iRec) < 0) {
                            fView.SkippedList.AddItem(null, st);
                        } else {
                            fView.SelectedList.AddItem(null, st);
                        }
                    } else {
                        string st = rec.XRef + " / " + GKUtils.GetRecordName(tree, rec, false);

                        if (fSplitList.IndexOf(rec) < 0) {
                            //fView.SkippedList.AddItem(null, st);
                        } else {
                            fView.SelectedList.AddItem(null, st);
                        }
                    }
                }
                fView.Title = fSplitList.Count.ToString() + @" / " + cnt.ToString();
            } finally {
                fView.SelectedList.EndUpdate();
                fView.SkippedList.EndUpdate();
            }
        }

        public void Select(TreeTools.TreeWalkMode walkMode)
        {
            fIndiMode = true;
            Select(fBase.GetSelectedPerson(), walkMode);
        }

        public void Select(GDMIndividualRecord startPerson, TreeTools.TreeWalkMode walkMode)
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

                if (obj is GDMIndividualRecord) {
                    await BaseController.DeleteRecord(fBase, obj as GDMIndividualRecord, false);
                }
            }

            fSplitList.Clear();
            UpdateView();
            fBase.RefreshLists(false);

            AppHost.StdDialogs.ShowMessage(LangMan.LS(LSID.RecsDeleted));
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
            fView.Title = LangMan.LS(LSID.TreeSplit);

            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                GetControl<ITabPage>("pageTreeSplit").Text = LangMan.LS(LSID.TreeSplit);
                GetControl<IButton>("btnClose").Text = LangMan.LS(LSID.DlgClose);
            }
            GetControl<IButton>("btnSelectAll").Text = LangMan.LS(LSID.SelAll);
            GetControl<IButton>("btnSelectFamily").Text = LangMan.LS(LSID.SelFamily);
            GetControl<IButton>("btnSelectAncestors").Text = LangMan.LS(LSID.SelAncestors);
            GetControl<IButton>("btnSelectDescendants").Text = LangMan.LS(LSID.SelDescendants);
            GetControl<IButton>("btnSelectList").Text = LangMan.LS(LSID.SelList);
            GetControl<IButton>("btnDelete").Text = LangMan.LS(LSID.DoDelete);
            GetControl<IButton>("btnSave").Text = LangMan.LS(LSID.MIFileSaveAs);

            fView.SelectedList.AddColumn(LangMan.LS(LSID.Record), 300, false);
            fView.SkippedList.AddColumn(LangMan.LS(LSID.Record), 300, false);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnClose").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
