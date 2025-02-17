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

using System;
using BSLib;
using GDModel;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Types;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class RepositoryCitDlgController : DialogController<IRepositoryCitEditDlg>
    {
        private readonly StringList fRepositoriesList;
        private GDMRepositoryCitation fRepositoryCitation;

        public GDMRepositoryCitation RepositoryCitation
        {
            get { return fRepositoryCitation; }
            set {
                if (fRepositoryCitation != value) {
                    fRepositoryCitation = value;
                    UpdateView();
                }
            }
        }


        public RepositoryCitDlgController(IRepositoryCitEditDlg view) : base(view)
        {
            fRepositoriesList = new StringList();
        }

        public override void Init(IBaseWindow baseWin)
        {
            base.Init(baseWin);

            fBase.Context.GetRepositoriesList(fRepositoriesList);
            RefreshRepositoriesList("");

            fView.RepositoryCombo.Activate();

            fView.CallNumbersList.ListModel = new CallNumbersListModel(fView, baseWin, fLocalUndoman);
        }

        public override void Done()
        {
            fView.CallNumbersList.ListModel.SaveSettings();
        }

        public override bool Accept()
        {
            try {
                int idx = fRepositoriesList.IndexOf(fView.RepositoryCombo.Text);
                GDMRepositoryRecord repoRec = ((idx < 0) ? null : (fRepositoriesList.GetObject(idx) as GDMRepositoryRecord));

                if (repoRec == null) {
                    AppHost.StdDialogs.ShowError(LangMan.LS(LSID.RepositoryIsNotSpecified));

                    return false;
                } else {
                    fRepositoryCitation.XRef = repoRec.XRef;

                    CommitChanges();

                    return true;
                }
            } catch (Exception ex) {
                Logger.WriteError("RepositoryCitDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            var repoRec = fBase.Context.Tree.GetPtrValue<GDMRepositoryRecord>(fRepositoryCitation);
            if (repoRec != null) fView.RepositoryCombo.Text = repoRec.RepositoryName;

            fView.CallNumbersList.ListModel.DataOwner = fRepositoryCitation;
        }

        public async void AddRepository()
        {
            GDMRepositoryRecord repo = await fBase.Context.SelectRecord(fView, GDMRecordType.rtRepository, null) as GDMRepositoryRecord;
            if (repo == null) return;

            fBase.Context.GetRepositoriesList(fRepositoriesList);
            RefreshRepositoriesList("");
            fView.RepositoryCombo.Text = repo.RepositoryName;
        }

        public void RefreshRepositoriesList(string filter)
        {
            fView.RepositoryCombo.BeginUpdate();
            try {
                fView.RepositoryCombo.Clear();

                string flt = "*" + filter + "*";

                int num = fRepositoriesList.Count;
                for (int i = 0; i < num; i++) {
                    string st = fRepositoriesList[i];

                    if (filter == "" || GKUtils.MatchesMask(st, flt)) {
                        fView.RepositoryCombo.AddItem(st, fRepositoriesList.GetObject(i));
                    }
                }
            } finally {
                fView.RepositoryCombo.EndUpdate();
            }
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.Repository);
            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.DlgCancel);
            GetControl<ILabel>("lblRepository").Text = LangMan.LS(LSID.Repository);
            GetControl<ITabPage>("pageCallNumbers").Text = LangMan.LS(LSID.CallNumbers);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnAccept").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            GetControl<IButton>("btnCancel").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);

            fView.CallNumbersList.ApplyTheme();
        }
    }
}
