/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
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
    public sealed class RepositoryEditDlgController : DialogController<IRepositoryEditDlg>
    {
        #region Design components
#pragma warning disable CS0649, IDE0044
        private IButton _btnAccept;
        private IButton _btnCancel;
        private ITabControl _tabsData;
        private ILabel _lblName;
        private ITextBox _txtName;
        private ITabPage _pageNotes;
        private IButton _btnAddress;
        private ISheetList _fNotesList;
        private ISheetList _fUserRefList;
        private ITabPage _pageUserRefs;
#pragma warning restore CS0649, IDE0044
        #endregion

        private GDMRepositoryRecord fRepositoryRecord;

        public GDMRepositoryRecord RepositoryRecord
        {
            get { return fRepositoryRecord; }
            set {
                if (fRepositoryRecord != value) {
                    fRepositoryRecord = value;
                    UpdateView();
                }
            }
        }


        public RepositoryEditDlgController(IRepositoryEditDlg view) : base(view)
        {
        }

        public override void InitView(IBaseWindow baseWin)
        {
            base.InitView(baseWin);

            _txtName.Activate();

            _fNotesList.ListModel = new NoteLinksListModel(fView, baseWin, fLocalUndoman);
            _fUserRefList.ListModel = new URefsListModel(fView, baseWin, fLocalUndoman);
        }

        public override void DoneView()
        {
            _fNotesList.ListModel.SaveSettings();
            _fUserRefList.ListModel.SaveSettings();
        }

        public override bool Accept()
        {
            try {
                fRepositoryRecord.RepositoryName = _txtName.Text;

                if (!Validate(fRepositoryRecord)) return false;

                fBase.NotifyRecord(fRepositoryRecord, RecordAction.raEdit);

                CommitChanges();

                return true;
            } catch (Exception ex) {
                Logger.WriteError("RepositoryEditDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            _txtName.Text = fRepositoryRecord.RepositoryName;

            _fNotesList.ListModel.DataOwner = fRepositoryRecord;
            _fUserRefList.ListModel.DataOwner = fRepositoryRecord;
        }

        public async void ModifyAddress()
        {
            await BaseController.ModifyAddress(fView, fBase, fRepositoryRecord.Address);
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.Repository));
            _btnAccept.Text = LangMan.LS(LSID.DlgAccept);
            _btnCancel.Text = LangMan.LS(LSID.DlgCancel);
            _lblName.Text = LangMan.LS(LSID.Title);
            _pageNotes.Text = LangMan.LS(LSID.RPNotes);
            _pageUserRefs.Text = LangMan.LS(LSID.UserRefs);
            _btnAddress.Text = LangMan.LS(LSID.Address) + @"...";
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _btnAccept.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            _btnCancel.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
