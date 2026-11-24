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
using GKCore.Options;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class SourceEditDlgController : DialogController<ISourceEditDlg>
    {
        #region Design components
#pragma warning disable CS0649, IDE0044
        private IButton _btnAccept;
        private IButton _btnCancel;
        private ITabControl _tabsData;
        private ITabPage _pageNotes;
        private ITabPage _pageMultimedia;
        private ITabPage _pageRepositories;
        private ITabPage _pageText;
        private ITextBox _txtText;
        private ITabPage _pageCommon;
        private ILabel _lblShortTitle;
        private ITextBox _txtShortTitle;
        private ILabel _lblAuthor;
        private ITextBox _txtAuthor;
        private ILabel _lblTitle;
        private ITextBox _txtTitle;
        private ILabel _lblPublication;
        private ITextBox _txtPublication;
        private ISheetList _fNotesList;
        private ISheetList _fMediaList;
        private ISheetList _fRepositoriesList;
        private ILabel _lblDate;
        private IDateControl _dateCtl;
        private ITabPage _pageUserRefs;
        private ISheetList _fUserRefList;
#pragma warning restore CS0649, IDE0044
        #endregion

        private GDMSourceRecord fSourceRecord;

        public GDMSourceRecord SourceRecord
        {
            get { return fSourceRecord; }
            set {
                if (fSourceRecord != value) {
                    fSourceRecord = value;
                    UpdateView();
                }
            }
        }


        public SourceEditDlgController(ISourceEditDlg view) : base(view)
        {
        }

        private void ModifyReposSheet(object sender, ModifyEventArgs eArgs)
        {
            if (eArgs.Action == RecordAction.raJump) {
                JumpToRecord(eArgs.ItemData as GDMRepositoryCitation);
            }
        }

        public override void InitView(IBaseWindow baseWin)
        {
            base.InitView(baseWin);

            _fRepositoriesList.OnModify += ModifyReposSheet;

            _txtShortTitle.Activate();

            _fRepositoriesList.ListModel = new RepositoryCitationsListModel(fView, baseWin, fLocalUndoman);
            _fNotesList.ListModel = new NoteLinksListModel(fView, baseWin, fLocalUndoman);
            _fMediaList.ListModel = new MediaLinksListModel(fView, baseWin, fLocalUndoman);
            _fUserRefList.ListModel = new URefsListModel(fView, baseWin, fLocalUndoman);
        }

        public override void DoneView()
        {
            _fRepositoriesList.ListModel.SaveSettings();
            _fNotesList.ListModel.SaveSettings();
            _fMediaList.ListModel.SaveSettings();
            _fUserRefList.ListModel.SaveSettings();
        }

        public override bool Accept()
        {
            try {
                GDMCustomDate dt = _dateCtl.Date;
                if (dt == null) {
                    AppHost.StdDialogs.ShowError(LangMan.LS(LSID.DateInvalid));
                    return false;
                }
                fSourceRecord.Date.ParseString(dt.StringValue);

                fSourceRecord.ShortTitle = _txtShortTitle.Text;
                fSourceRecord.Originator.SetLines(_txtAuthor.Text);
                fSourceRecord.Title.SetLines(_txtTitle.Text);
                fSourceRecord.Publication.SetLines(_txtPublication.Text);
                fSourceRecord.Text.SetLines(_txtText.Text);

                if (!Validate(fSourceRecord)) return false;

                fLocalUndoman.Commit();

                fBase.NotifyRecord(fSourceRecord, RecordAction.raEdit);

                return true;
            } catch (Exception ex) {
                Logger.WriteError("SourceEditDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            _txtShortTitle.Text = fSourceRecord.ShortTitle;
            _txtAuthor.Text = fSourceRecord.Originator.Lines.Text.Trim();
            _txtTitle.Text = fSourceRecord.Title.Lines.Text.Trim();
            _txtPublication.Text = fSourceRecord.Publication.Lines.Text.Trim();
            _txtText.Text = fSourceRecord.Text.Lines.Text.Trim();

            _dateCtl.Date = fSourceRecord.Date.Value;
            _dateCtl.Enabled = !GlobalOptions.Instance.DisableNonStdFeatures;

            _fRepositoriesList.ListModel.DataOwner = fSourceRecord;
            _fNotesList.ListModel.DataOwner = fSourceRecord;
            _fMediaList.ListModel.DataOwner = fSourceRecord;
            _fUserRefList.ListModel.DataOwner = fSourceRecord;
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.Source));

            _btnAccept.Text = LangMan.LS(LSID.DlgAccept);
            _btnCancel.Text = LangMan.LS(LSID.DlgCancel);
            _lblShortTitle.Text = LangMan.LS(LSID.ShortTitle);
            _lblAuthor.Text = LangMan.LS(LSID.Author);
            _lblTitle.Text = LangMan.LS(LSID.Title);
            _lblPublication.Text = LangMan.LS(LSID.Publication);
            _lblDate.Text = LangMan.LS(LSID.Date);
            _pageCommon.Text = LangMan.LS(LSID.Common);
            _pageText.Text = LangMan.LS(LSID.Text);
            _pageRepositories.Text = LangMan.LS(LSID.RPRepositories);
            _pageNotes.Text = LangMan.LS(LSID.RPNotes);
            _pageMultimedia.Text = LangMan.LS(LSID.RPMultimedia);
            _pageUserRefs.Text = LangMan.LS(LSID.UserRefs);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _btnAccept.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            _btnCancel.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
