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
using GDModel.Providers.GEDCOM;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;
using GKCore.Locales;
using GKCore.Media;
using GKCore.Options;
using GKUI.Themes;

namespace GKCore.Controllers
{
    public sealed class DNATestEditDlgController : DialogController<IDNATestEditDlg>
    {
        #region Design components
#pragma warning disable CS0649, IDE0044
        private IButton _btnAccept;
        private IButton _btnCancel;
        private ITabPage _pageNotes;
        private ITabPage _pageMultimedia;
        private ITabPage _pageCommon;
        private ILabel _lblTestName;
        private ITextBox _txtTestName;
        private ILabel _lblAgency;
        private IComboBox _cmbAgency;
        private ILabel _lblDate;
        private IDateBox _dateCtl;
        private ILabel _lblFileRef;
        private ITextBox _txtFileRef;
        private ILabel _lblStoreType;
        private IComboBox _cmbStoreType;
        private IButton _btnFileSelect;
        private ILabel _lblFileFormat;
        private IComboBox _cmbFileFormat;
        private ILabel _lblYHaplogroup;
        private IComboBox _cmbYHaplogroup;
        private ILabel _lblMHaplogroup;
        private IComboBox _cmbMHaplogroup;
        private ILabel _lblRestriction;
        private IComboBox _cmbRestriction;
        private ISheetList _fNotesList;
        private ISheetList _fMediaList;
#pragma warning restore CS0649, IDE0044
        #endregion

        private const string LabKey = "_DLAB";

        private GDMDNATest fDNATest;
        private bool fIsNew;


        public GDMDNATest DNATest
        {
            get { return fDNATest; }
            set {
                if (fDNATest != value) {
                    fDNATest = value;
                    UpdateView();
                }
            }
        }


        public DNATestEditDlgController(IDNATestEditDlg view) : base(view)
        {
        }

        public override void InitView(IBaseWindow baseWin)
        {
            base.InitView(baseWin);

            _cmbRestriction.ReadOnly = true;
            for (GDMRestriction res = GDMRestriction.rnNone; res <= GDMRestriction.rnLast; res++) {
                _cmbRestriction.Add(LangMan.LS(GKData.Restrictions[(int)res]));
            }

            _cmbStoreType.ReadOnly = true;
            for (var mst = MediaStoreType.mstReference; mst <= MediaStoreType.mstURL; mst++) {
                if (mst == MediaStoreType.mstArchive) continue;

                _cmbStoreType.AddItem(LangMan.LS(GKData.GKStoreTypes[(int)mst].Name), mst);
            }

            _cmbFileFormat.ReadOnly = true;
            for (var dff = GDMDNAFileFormat.None; dff <= GDMDNAFileFormat.STR; dff++) {
                _cmbFileFormat.AddItem(dff.ToString(), dff);
            }

            _txtTestName.Activate();

            _fNotesList.ListModel = new NoteLinksListModel(fView, baseWin, fLocalUndoman);
            _fMediaList.ListModel = new MediaLinksListModel(fView, baseWin, fLocalUndoman);
        }

        public override void DoneView()
        {
            _fNotesList.ListModel.SaveSettings();
            _fMediaList.ListModel.SaveSettings();
        }

        public override bool Accept()
        {
            try {
                fDNATest.TestName = _txtTestName.Text;
                fDNATest.Date.Assign(GDMDate.CreateByFormattedStr(_dateCtl.NormalizeDate, true));

                fDNATest.Agency = _cmbAgency.Text;
                fBase.Context.ValuesCollection.Add(LabKey, fDNATest.Agency);

                fDNATest.MHaplogroup = _cmbMHaplogroup.Text;
                fBase.Context.ValuesCollection.Add(GEDCOMTagName._MHAP, fDNATest.MHaplogroup);

                fDNATest.YHaplogroup = _cmbYHaplogroup.Text;
                fBase.Context.ValuesCollection.Add(GEDCOMTagName._YHAP, fDNATest.YHaplogroup);

                fDNATest.FileFormat = _cmbFileFormat.GetSelectedTag<GDMDNAFileFormat>();

                if (fIsNew) {
                    MediaStoreType gst = _cmbStoreType.GetSelectedTag<MediaStoreType>();
                    string fileName = _txtFileRef.Text;

                    if (!fBase.Context.CheckNewMedia(fileName, gst)) {
                        return false;
                    }

                    string fileRef;
                    bool result = fBase.Context.MediaSave(out fileRef, fileName, gst);
                    if (!result) {
                        return false;
                    } else {
                        fDNATest.FileReference = fileRef;
                    }
                }

                fDNATest.Restriction = (GDMRestriction)_cmbRestriction.SelectedIndex;

                CommitChanges();

                return true;
            } catch (Exception ex) {
                Logger.WriteError("EventEditController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            _fNotesList.ListModel.DataOwner = fDNATest;
            _fMediaList.ListModel.DataOwner = fDNATest;

            _txtTestName.Text = fDNATest.TestName;
            _dateCtl.NormalizeDate = fDNATest.Date.GetDisplayString(DateFormat.dfDD_MM_YYYY);

            BaseController.UpdateComboValues(fBase, _cmbAgency, true, false, LabKey, fDNATest.Agency);
            BaseController.UpdateComboValues(fBase, _cmbMHaplogroup, true, false, GEDCOMTagName._MHAP, fDNATest.MHaplogroup);
            BaseController.UpdateComboValues(fBase, _cmbYHaplogroup, true, false, GEDCOMTagName._YHAP, fDNATest.YHaplogroup);

            _cmbFileFormat.SetSelectedTag(fDNATest.FileFormat);

            string fileRef = fDNATest.FileReference;
            fIsNew = string.IsNullOrEmpty(fileRef);
            _txtFileRef.Text = fileRef;

            var storeType = fIsNew ? GlobalOptions.Instance.MediaStoreDefault : MediaStore.GetStoreType(fileRef);
            UpdateFileStore(fIsNew, storeType);

            _cmbRestriction.SelectedIndex = (int)fDNATest.Restriction;
        }

        private void UpdateFileStore(bool isNew, MediaStoreType storeType)
        {
            _btnFileSelect.Enabled = isNew && (storeType != MediaStoreType.mstURL);

            _txtFileRef.Enabled = isNew && (storeType == MediaStoreType.mstURL);
            _txtFileRef.ReadOnly = !_txtFileRef.Enabled;

            _cmbStoreType.Enabled = isNew;
            _cmbStoreType.SetSelectedTag(storeType);
        }

        public void LockEditor(bool locked)
        {
            _txtTestName.Enabled = !locked;
            _dateCtl.Enabled = !locked;
            _cmbAgency.Enabled = !locked;
            _cmbMHaplogroup.Enabled = !locked;
            _cmbYHaplogroup.Enabled = !locked;

            _fNotesList.ReadOnly = locked;
            _fMediaList.ReadOnly = locked;
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

            _txtFileRef.Text = fileName;
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.DNATest));

            _btnAccept.Text = LangMan.LS(LSID.DlgAccept);
            _btnCancel.Text = LangMan.LS(LSID.DlgCancel);

            _pageCommon.Text = LangMan.LS(LSID.Common);
            _pageNotes.Text = LangMan.LS(LSID.RPNotes);
            _pageMultimedia.Text = LangMan.LS(LSID.RPMultimedia);

            _lblTestName.Text = LangMan.LS(LSID.DNATestName);
            _lblDate.Text = LangMan.LS(LSID.Date);
            _lblAgency.Text = LangMan.LS(LSID.DNALaboratory);
            _lblFileRef.Text = LangMan.LS(LSID.File);
            _lblStoreType.Text = LangMan.LS(LSID.StoreType);
            _lblFileFormat.Text = LangMan.LS(LSID.DNAFileFormat);
            _lblMHaplogroup.Text = LangMan.LS(LSID.MDNAHaplogroup);
            _lblYHaplogroup.Text = LangMan.LS(LSID.YDNAHaplogroup);
            _lblRestriction.Text = LangMan.LS(LSID.Restriction);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _btnAccept.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            _btnCancel.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
