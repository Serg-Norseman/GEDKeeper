/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using BSLib;
using GDModel;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;
using GKCore.Locales;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class RepositoryCitDlgController : DialogController<IRepositoryCitEditDlg>
    {
        #region Design components
#pragma warning disable CS0649, IDE0044
        private IButton _btnAccept;
        private IButton _btnCancel;
        private IButton _btnRepositoryAdd;
        private ILabel _lblRepository;
        private IComboBox _cmbRepository;
        private ITabPage _pageCallNumbers;
        private ISheetList _fCallNumbersList;
#pragma warning restore CS0649, IDE0044
        #endregion

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

        public override void InitView(IBaseWindow baseWin)
        {
            base.InitView(baseWin);

            fBase.Context.GetRepositoriesList(fRepositoriesList);
            RefreshRepositoriesList("");

            _cmbRepository.Activate();

            _fCallNumbersList.ListModel = new CallNumbersListModel(fView, baseWin, fLocalUndoman);
        }

        public override void DoneView()
        {
            _fCallNumbersList.ListModel.SaveSettings();
        }

        public override bool Accept()
        {
            try {
                int idx = fRepositoriesList.IndexOf(_cmbRepository.Text);
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
            if (repoRec != null) _cmbRepository.Text = repoRec.RepositoryName;

            _fCallNumbersList.ListModel.DataOwner = fRepositoryCitation;
        }

        public async void AddRepository()
        {
            GDMRepositoryRecord repo = await BaseController.SelectRecord(fView, fBase, GDMRecordType.rtRepository, null) as GDMRepositoryRecord;
            if (repo == null) return;

            fBase.Context.GetRepositoriesList(fRepositoriesList);
            RefreshRepositoriesList("");
            _cmbRepository.Text = repo.RepositoryName;
        }

        public void RefreshRepositoriesList(string filter)
        {
            _cmbRepository.BeginUpdate();
            try {
                _cmbRepository.Clear();

                string flt = "*" + filter + "*";

                int num = fRepositoriesList.Count;
                for (int i = 0; i < num; i++) {
                    string st = fRepositoriesList[i];

                    if (filter == "" || GKUtils.MatchesMask(st, flt)) {
                        _cmbRepository.AddItem(st, fRepositoriesList.GetObject(i));
                    }
                }
            } finally {
                _cmbRepository.EndUpdate();
            }
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.Repository));
            _btnAccept.Text = LangMan.LS(LSID.DlgAccept);
            _btnCancel.Text = LangMan.LS(LSID.DlgCancel);
            _lblRepository.Text = LangMan.LS(LSID.Repository);
            _pageCallNumbers.Text = LangMan.LS(LSID.CallNumbers);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _btnAccept.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            _btnCancel.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);

            _btnRepositoryAdd.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Attach);
        }
    }
}
