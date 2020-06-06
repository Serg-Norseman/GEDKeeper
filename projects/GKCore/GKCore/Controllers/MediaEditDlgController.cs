/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2020 by Sergey V. Zhdanovskih.
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
using GKCore.MVP;
using GKCore.MVP.Views;
using GKCore.Options;
using GKCore.Types;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class MediaEditDlgController : DialogController<IMediaEditDlg>
    {
        private GDMMultimediaRecord fMediaRec;
        private bool fIsNew;

        public GDMMultimediaRecord MediaRec
        {
            get { return fMediaRec; }
            set {
                if (fMediaRec != value) {
                    fMediaRec = value;
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

        public override bool Accept()
        {
            try {
                GDMFileReferenceWithTitle fileRef = fMediaRec.FileReferences[0];

                if (fIsNew) {
                    MediaStoreType gst = fView.StoreType.GetSelectedTag<MediaStoreType>();

                    if ((gst == MediaStoreType.mstArchive || gst == MediaStoreType.mstStorage) && !fBase.Context.CheckBasePath()) {
                        return false;
                    }

                    bool result = fBase.Context.MediaSave(fileRef, fView.File.Text, gst);

                    if (!result) {
                        return false;
                    }
                }

                fileRef.MediaType = (GDMMediaType)fView.MediaType.SelectedIndex;
                fileRef.Title = fView.Name.Text;

                UpdateControls();

                fLocalUndoman.Commit();
                fBase.NotifyRecord(fMediaRec, RecordAction.raEdit);

                return true;
            } catch (Exception ex) {
                Logger.WriteError("MediaEditDlgController.Accept(): ", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            fView.NotesList.ListModel.DataOwner = fMediaRec;
            fView.SourcesList.ListModel.DataOwner = fMediaRec;

            UpdateControls();
        }

        private void UpdateControls()
        {
            GDMFileReferenceWithTitle fileRef = fMediaRec.FileReferences[0];

            fIsNew = (fileRef.StringValue == "");

            fView.Name.Text = fileRef.Title;
            fView.MediaType.SelectedIndex = (int)fileRef.MediaType;
            fView.File.Text = fileRef.StringValue;

            if (fIsNew) {
                RefreshStoreTypes(GlobalOptions.Instance.AllowMediaStoreReferences, true, GlobalOptions.Instance.AllowMediaStoreRelativeReferences, (MediaStoreType)GlobalOptions.Instance.MediaStoreDefault);
            } else {
                MediaStore mediaStore = fBase.Context.GetStoreType(fileRef);
                RefreshStoreTypes((mediaStore.StoreType == MediaStoreType.mstReference),
                                  (mediaStore.StoreType == MediaStoreType.mstArchive),
                                  (mediaStore.StoreType == MediaStoreType.mstRelativeReference),
                                  mediaStore.StoreType);
            }

            fView.FileSelectButton.Enabled = fIsNew;
            fView.StoreType.Enabled = fIsNew;

            fView.NotesList.UpdateSheet();
            fView.SourcesList.UpdateSheet();
        }

        private void RefreshStoreTypes(bool allowRef, bool allowArc, bool allowRel, MediaStoreType selectType)
        {
            fView.StoreType.Clear();

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

            fView.StoreType.SetSelectedTag<MediaStoreType>(selectType);
        }

        public void SelectFile()
        {
            string fileName = AppHost.StdDialogs.GetOpenFile("", "", LangMan.LS(LSID.LSID_AllFilter), 1, "");
            if (string.IsNullOrEmpty(fileName)) return;

            if (GlobalOptions.Instance.RemovableMediaWarning && FileHelper.IsRemovableDrive(fileName)) {
                if (!AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_RemovableMediaWarningMessage))) {
                    return;
                }
            }

            fView.File.Text = fileName;
            bool canArc = GKUtils.FileCanBeArchived(fileName);
            RefreshStoreTypes(GlobalOptions.Instance.AllowMediaStoreReferences, canArc, GlobalOptions.Instance.AllowMediaStoreRelativeReferences, MediaStoreType.mstReference);
            fView.StoreType.Enabled = true;
        }

        public void View()
        {
            if (fIsNew) {
                Accept();
            }

            fBase.ShowMedia(fMediaRec, true);
        }
    }
}
