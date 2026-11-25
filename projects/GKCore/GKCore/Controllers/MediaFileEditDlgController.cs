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
        #region Design components
#pragma warning disable CS0649, IDE0044
        private IButton _btnAccept;
        private IButton _btnCancel;
        private IButton _btnView;
        private ILabel _lblName;
        private ITextBox _txtName;
        private ILabel _lblType;
        private IComboBox _cmbMediaType;
        private ILabel _lblStoreType;
        private IComboBox _cmbStoreType;
        private ILabel _lblFile;
        private ITextBox _txtFile;
        private IButton _btnFileSelect;
#pragma warning restore CS0649, IDE0044
        #endregion

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
        }

        public override void InitView(IBaseWindow baseWin)
        {
            base.InitView(baseWin);

            _cmbMediaType.ReadOnly = true;
            _cmbStoreType.ReadOnly = true;

            for (GDMMediaType mt = GDMMediaType.mtUnknown; mt <= GDMMediaType.mtLast; mt++) {
                _cmbMediaType.Add(LangMan.LS(GKData.MediaTypes[(int)mt]));
            }

            _txtName.Activate();
        }

        public override bool Accept()
        {
            try {
                GDMFileReferenceWithTitle fileRef = fFileRef;

                if (fIsNew) {
                    MediaStoreType gst = _cmbStoreType.GetSelectedTag<MediaStoreType>();

                    if ((gst == MediaStoreType.mstArchive) && !fBase.Context.CheckBasePath()) {
                        return false;
                    }

                    string fileName = _txtFile.Text;
                    if (string.IsNullOrEmpty(fileName) || (gst != MediaStoreType.mstURL && !File.Exists(fileName))) {
                        AppHost.StdDialogs.ShowError(LangMan.LS(LSID.InvalidFileName));
                        return false;
                    }

                    bool result = fBase.Context.MediaSave(fileRef, fileName, gst);

                    if (!result) {
                        return false;
                    }
                }

                fileRef.MediaType = (GDMMediaType)_cmbMediaType.SelectedIndex;
                fileRef.Title = _txtName.Text;

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

            _txtName.Text = fileRef.Title;
            _cmbMediaType.SelectedIndex = (int)fileRef.MediaType;
            _txtFile.Text = fileRef.StringValue;

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
            _btnFileSelect.Enabled = isNew && (storeType != MediaStoreType.mstURL);

            _txtFile.Enabled = isNew && (storeType == MediaStoreType.mstURL);
            _txtFile.ReadOnly = !_txtFile.Enabled;

            _cmbStoreType.Enabled = isNew;
        }

        private void RefreshStoreTypes(bool allowAbsRef, bool allowArc, bool allowRelRef, MediaStoreType selectType)
        {
            bool disNoStd = GlobalOptions.Instance.DisableNonStdFeatures;

            _cmbStoreType.Clear();

            if (allowAbsRef) {
                _cmbStoreType.AddItem(LangMan.LS(GKData.GKStoreTypes[(int)MediaStoreType.mstReference].Name), MediaStoreType.mstReference);
            }

            if (allowRelRef) {
                _cmbStoreType.AddItem(LangMan.LS(GKData.GKStoreTypes[(int)MediaStoreType.mstRelativeReference].Name), MediaStoreType.mstRelativeReference);
            }

            if (allowArc) {
                _cmbStoreType.AddItem(LangMan.LS(GKData.GKStoreTypes[(int)MediaStoreType.mstArchive].Name), MediaStoreType.mstArchive);
            }

            if (!disNoStd) {
                _cmbStoreType.AddItem(LangMan.LS(GKData.GKStoreTypes[(int)MediaStoreType.mstURL].Name), MediaStoreType.mstURL);
            }

            _cmbStoreType.SetSelectedTag(selectType);
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

            var storeType = _cmbStoreType.GetSelectedTag<MediaStoreType>();
            _txtFile.Text = fileName;
            bool canArc = GKUtils.FileCanBeArchived(fileName);
            RefreshStoreTypes(GlobalOptions.Instance.AllowMediaStoreReferences, canArc,
                              GlobalOptions.Instance.AllowMediaStoreRelativeReferences,
                              storeType);
            _cmbStoreType.Enabled = true;
        }

        public void ChangeStoreType()
        {
            MediaStoreType storeType = _cmbStoreType.GetSelectedTag<MediaStoreType>();
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

            _btnAccept.Text = LangMan.LS(LSID.DlgAccept);
            _btnCancel.Text = LangMan.LS(LSID.DlgCancel);
            _lblName.Text = LangMan.LS(LSID.Title);
            _lblType.Text = LangMan.LS(LSID.Type);
            _lblStoreType.Text = LangMan.LS(LSID.StoreType);
            _lblFile.Text = LangMan.LS(LSID.File);
            _btnView.Text = LangMan.LS(LSID.View) + @"...";
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _btnAccept.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            _btnCancel.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
