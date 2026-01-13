/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.IO;
using System.Threading.Tasks;
using BSLib;
using GDModel;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Locales;
using GKCore.Media;
using GKCore.Options;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class MediaFileEditDlgController : DialogController<IMediaFileEditDlg>
    {
        private GDMFileReferenceWithTitle fFileRef;
        private bool fIsNew;

        public GDMFileReferenceWithTitle FileRef
        {
            get { return fFileRef; }
            set {
                if (fFileRef != value) {
                    fFileRef = value;
                    UpdateView();
                }
            }
        }


        public MediaFileEditDlgController(IMediaFileEditDlg view) : base(view)
        {
            for (GDMMediaType mt = GDMMediaType.mtUnknown; mt <= GDMMediaType.mtLast; mt++) {
                fView.MediaType.Add(LangMan.LS(GKData.MediaTypes[(int)mt]));
            }

            fView.Name.Activate();
        }

        public override bool Accept()
        {
            try {
                GDMFileReferenceWithTitle fileRef = fFileRef;

                if (fIsNew) {
                    MediaStoreType gst = fView.StoreType.GetSelectedTag<MediaStoreType>();

                    if ((gst == MediaStoreType.mstArchive) && !fBase.Context.CheckBasePath()) {
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
                //fBase.NotifyRecord(fMultimediaRecord, RecordAction.raEdit);

                return true;
            } catch (Exception ex) {
                Logger.WriteError("MediaEditDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            UpdateControls();
        }

        private void UpdateControls()
        {
            GDMFileReferenceWithTitle fileRef = fFileRef;

            fIsNew = (fileRef.StringValue == "");

            fView.Name.Text = fileRef.Title;
            fView.MediaType.SelectedIndex = (int)fileRef.MediaType;
            fView.File.Text = fileRef.StringValue;

            MediaStoreType storeType;

            if (fIsNew) {
                if (GlobalOptions.Instance.DisableNonStdFeatures) {
                    storeType = MediaStoreType.mstReference;
                    RefreshStoreTypes(true, false, false, storeType);
                } else {
                    storeType = GlobalOptions.Instance.MediaStoreDefault;
                    RefreshStoreTypes(GlobalOptions.Instance.AllowMediaStoreReferences, true,
                                      GlobalOptions.Instance.AllowMediaStoreRelativeReferences,
                                      storeType);
                }
            } else {
                storeType = MediaStore.GetStoreType(fileRef.StringValue);
                RefreshStoreTypes((storeType == MediaStoreType.mstReference),
                                  (storeType == MediaStoreType.mstArchive),
                                  (storeType == MediaStoreType.mstRelativeReference),
                                  storeType);
            }

            UpdateFileStore(fIsNew, storeType);
        }

        private void UpdateFileStore(bool isNew, MediaStoreType storeType)
        {
            fView.FileSelectButton.Enabled = isNew && (storeType != MediaStoreType.mstURL);

            fView.File.Enabled = isNew && (storeType == MediaStoreType.mstURL);
            fView.File.ReadOnly = !fView.File.Enabled;

            fView.StoreType.Enabled = isNew;
        }

        private void RefreshStoreTypes(bool allowAbsRef, bool allowArc, bool allowRelRef, MediaStoreType selectType)
        {
            bool disNoStd = GlobalOptions.Instance.DisableNonStdFeatures;

            fView.StoreType.Clear();

            if (allowAbsRef) {
                fView.StoreType.AddItem(LangMan.LS(GKData.GKStoreTypes[(int)MediaStoreType.mstReference].Name), MediaStoreType.mstReference);
            }

            if (allowRelRef) {
                fView.StoreType.AddItem(LangMan.LS(GKData.GKStoreTypes[(int)MediaStoreType.mstRelativeReference].Name), MediaStoreType.mstRelativeReference);
            }

            if (allowArc) {
                fView.StoreType.AddItem(LangMan.LS(GKData.GKStoreTypes[(int)MediaStoreType.mstArchive].Name), MediaStoreType.mstArchive);
            }

            if (!disNoStd) {
                fView.StoreType.AddItem(LangMan.LS(GKData.GKStoreTypes[(int)MediaStoreType.mstURL].Name), MediaStoreType.mstURL);
            }

            fView.StoreType.SetSelectedTag(selectType);
        }

        public async Task SelectFile()
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
            /*if (fIsNew) {
                Accept();
            }

            fBase.ShowMedia(fMultimediaRecord, true);*/
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.RPMultimedia));

            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.DlgCancel);
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
        }
    }
}
