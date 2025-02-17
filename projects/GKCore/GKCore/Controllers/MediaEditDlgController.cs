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
using System.IO;
using BSLib;
using GDModel;
using GKCore.Design.Controls;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Design;
using GKCore.Design.Views;
using GKCore.Options;
using GKCore.Types;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class MediaEditDlgController : DialogController<IMediaEditDlg>
    {
        private GDMMultimediaRecord fMultimediaRecord;
        private bool fIsNew;

        public GDMMultimediaRecord MultimediaRecord
        {
            get { return fMultimediaRecord; }
            set {
                if (fMultimediaRecord != value) {
                    fMultimediaRecord = value;
                    UpdateView();
                }
            }
        }


        public MediaEditDlgController(IMediaEditDlg view) : base(view)
        {
            for (GDMMediaType mt = GDMMediaType.mtUnknown; mt <= GDMMediaType.mtLast; mt++) {
                fView.MediaType.Add(LangMan.LS(GKData.MediaTypes[(int)mt]));
            }

            fView.Name.Activate();
        }

        public override void Init(IBaseWindow baseWin)
        {
            base.Init(baseWin);

            fView.NotesList.ListModel = new NoteLinksListModel(fView, baseWin, fLocalUndoman);
            fView.SourcesList.ListModel = new SourceCitationsListModel(fView, baseWin, fLocalUndoman);
            fView.UserRefList.ListModel = new URefsListModel(fView, baseWin, fLocalUndoman);
        }

        public override void Done()
        {
            fView.NotesList.ListModel.SaveSettings();
            fView.SourcesList.ListModel.SaveSettings();
            fView.UserRefList.ListModel.SaveSettings();
        }

        public override bool Accept()
        {
            try {
                GDMFileReferenceWithTitle fileRef = fMultimediaRecord.FileReferences[0];

                if (fIsNew) {
                    MediaStoreType gst = fView.StoreType.GetSelectedTag<MediaStoreType>();

                    if ((gst == MediaStoreType.mstArchive || gst == MediaStoreType.mstStorage) && !fBase.Context.CheckBasePath()) {
                        return false;
                    }

                    string fileName = fView.File.Text;
                    if (string.IsNullOrEmpty(fileName) || (gst != MediaStoreType.mstURL && !File.Exists(fileName))) {
                        AppHost.StdDialogs.ShowError(LangMan.LS(LSID.InvalidFileName));
                        return false;
                    }

                    bool result = fBase.Context.MediaSave(fileRef, fileName, gst);

                    if (!result) {
                        return false;
                    }
                }

                fileRef.MediaType = (GDMMediaType)fView.MediaType.SelectedIndex;
                fileRef.Title = fView.Name.Text;

                UpdateControls();

                fLocalUndoman.Commit();
                fBase.NotifyRecord(fMultimediaRecord, RecordAction.raEdit);

                return true;
            } catch (Exception ex) {
                Logger.WriteError("MediaEditDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            fView.NotesList.ListModel.DataOwner = fMultimediaRecord;
            fView.SourcesList.ListModel.DataOwner = fMultimediaRecord;
            fView.UserRefList.ListModel.DataOwner = fMultimediaRecord;

            UpdateControls();
        }

        private void UpdateControls()
        {
            GDMFileReferenceWithTitle fileRef = fMultimediaRecord.FileReferences[0];

            fIsNew = (fileRef.StringValue == "");

            fView.Name.Text = fileRef.Title;
            fView.MediaType.SelectedIndex = (int)fileRef.MediaType;
            fView.File.Text = fileRef.StringValue;

            MediaStoreType storeType;

            if (fIsNew) {
                storeType = GlobalOptions.Instance.MediaStoreDefault;
                RefreshStoreTypes(GlobalOptions.Instance.AllowMediaStoreReferences, true,
                                  GlobalOptions.Instance.AllowMediaStoreRelativeReferences,
                                  storeType);
            } else {
                var mediaStore = fBase.Context.GetStoreType(fileRef);
                storeType = mediaStore.StoreType;
                RefreshStoreTypes((storeType == MediaStoreType.mstReference),
                                  (storeType == MediaStoreType.mstArchive),
                                  (storeType == MediaStoreType.mstRelativeReference),
                                  storeType);
            }

            UpdateFileStore(fIsNew, storeType);

            fView.NotesList.UpdateSheet();
            fView.SourcesList.UpdateSheet();
            fView.UserRefList.UpdateSheet();
        }

        private void UpdateFileStore(bool isNew, MediaStoreType storeType)
        {
            fView.FileSelectButton.Enabled = isNew && (storeType != MediaStoreType.mstURL);

            fView.File.Enabled = isNew && (storeType == MediaStoreType.mstURL);
            fView.File.ReadOnly = !fView.File.Enabled;

            fView.StoreType.Enabled = isNew;
        }

        private void RefreshStoreTypes(bool allowRef, bool allowArc, bool allowRel, MediaStoreType selectType)
        {
            fView.StoreType.Clear();

            // 0. Ref if allowed
            // 1. Stg
            // 2. Arc if allowed
            // 3. RelRef if allowed
            // 4. Url

            if (allowRef) {
                fView.StoreType.AddItem(LangMan.LS(GKData.GKStoreTypes[(int)MediaStoreType.mstReference].Name),
                    MediaStoreType.mstReference);
            }

            fView.StoreType.AddItem(LangMan.LS(GKData.GKStoreTypes[(int)MediaStoreType.mstStorage].Name),
                MediaStoreType.mstStorage);

            if (allowArc) {
                fView.StoreType.AddItem(LangMan.LS(GKData.GKStoreTypes[(int)MediaStoreType.mstArchive].Name),
                    MediaStoreType.mstArchive);
            }

            if (allowRel) {
                fView.StoreType.AddItem(LangMan.LS(GKData.GKStoreTypes[(int)MediaStoreType.mstRelativeReference].Name),
                    MediaStoreType.mstRelativeReference);
            }

            fView.StoreType.AddItem(LangMan.LS(GKData.GKStoreTypes[(int)MediaStoreType.mstURL].Name),
                MediaStoreType.mstURL);

            fView.StoreType.SetSelectedTag<MediaStoreType>(selectType);
        }

        public async void SelectFile()
        {
            string fileName = await AppHost.StdDialogs.GetOpenFile("", "", LangMan.LS(LSID.AllFilter), 1, "");
            if (string.IsNullOrEmpty(fileName)) return;

            if (GlobalOptions.Instance.RemovableMediaWarning && FileHelper.IsRemovableDrive(fileName)) {
                var res = await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.RemovableMediaWarningMessage));
                if (!res) {
                    return;
                }
            }

            var storeType = fView.StoreType.GetSelectedTag<MediaStoreType>();
            fView.File.Text = fileName;
            bool canArc = GKUtils.FileCanBeArchived(fileName);
            RefreshStoreTypes(GlobalOptions.Instance.AllowMediaStoreReferences, canArc,
                              GlobalOptions.Instance.AllowMediaStoreRelativeReferences,
                              storeType);
            fView.StoreType.Enabled = true;
        }

        public void ChangeStoreType()
        {
            MediaStoreType storeType = fView.StoreType.GetSelectedTag<MediaStoreType>();
            UpdateFileStore(true, storeType);
        }

        public void View()
        {
            if (fIsNew) {
                Accept();
            }

            fBase.ShowMedia(fMultimediaRecord, true);
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.RPMultimedia);

            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.DlgCancel);
            GetControl<ITabPage>("pageCommon").Text = LangMan.LS(LSID.Common);
            GetControl<ITabPage>("pageNotes").Text = LangMan.LS(LSID.RPNotes);
            GetControl<ITabPage>("pageSources").Text = LangMan.LS(LSID.RPSources);
            GetControl<ITabPage>("pageUserRefs").Text = LangMan.LS(LSID.UserRefs);
            GetControl<ILabel>("lblName").Text = LangMan.LS(LSID.Title);
            GetControl<ILabel>("lblType").Text = LangMan.LS(LSID.Type);
            GetControl<ILabel>("lblStoreType").Text = LangMan.LS(LSID.StoreType);
            GetControl<ILabel>("lblFile").Text = LangMan.LS(LSID.File);
            GetControl<IButton>("btnView").Text = LangMan.LS(LSID.View) + @"...";
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnAccept").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            GetControl<IButton>("btnCancel").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);

            fView.NotesList.ApplyTheme();
            fView.SourcesList.ApplyTheme();
            fView.UserRefList.ApplyTheme();
        }
    }
}
