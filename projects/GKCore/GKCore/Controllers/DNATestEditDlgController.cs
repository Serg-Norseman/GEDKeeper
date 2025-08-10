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
            for (GDMRestriction res = GDMRestriction.rnNone; res <= GDMRestriction.rnLast; res++) {
                fView.Restriction.Add(LangMan.LS(GKData.Restrictions[(int)res]));
            }

            for (var mst = MediaStoreType.mstReference; mst <= MediaStoreType.mstURL; mst++) {
                if (mst == MediaStoreType.mstArchive) continue;
                fView.StoreType.AddItem(LangMan.LS(GKData.GKStoreTypes[(int)mst].Name), mst);
            }

            for (var dff = GDMDNAFileFormat.None; dff <= GDMDNAFileFormat.STR; dff++) {
                fView.FileFormat.AddItem(dff.ToString(), dff);
            }

            fView.TestName.Activate();
        }

        public override void Init(IBaseWindow baseWin)
        {
            base.Init(baseWin);

            fView.NotesList.ListModel = new NoteLinksListModel(fView, baseWin, fLocalUndoman);
            fView.MediaList.ListModel = new MediaLinksListModel(fView, baseWin, fLocalUndoman);
        }

        public override void Done()
        {
            fView.NotesList.ListModel.SaveSettings();
            fView.MediaList.ListModel.SaveSettings();
        }

        public override bool Accept()
        {
            try {
                fDNATest.TestName = fView.TestName.Text;
                fDNATest.Date.Assign(GDMDate.CreateByFormattedStr(fView.Date.NormalizeDate, true));

                fDNATest.Agency = fView.Agency.Text;
                fBase.Context.ValuesCollection.Add(LabKey, fDNATest.Agency);

                fDNATest.MHaplogroup = fView.MHaplogroup.Text;
                fBase.Context.ValuesCollection.Add(GEDCOMTagName._MHAP, fDNATest.MHaplogroup);

                fDNATest.YHaplogroup = fView.YHaplogroup.Text;
                fBase.Context.ValuesCollection.Add(GEDCOMTagName._YHAP, fDNATest.YHaplogroup);

                fDNATest.FileFormat = fView.FileFormat.GetSelectedTag<GDMDNAFileFormat>();

                if (fIsNew) {
                    MediaStoreType gst = fView.StoreType.GetSelectedTag<MediaStoreType>();
                    string fileName = fView.File.Text;

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

                fDNATest.Restriction = (GDMRestriction)fView.Restriction.SelectedIndex;

                CommitChanges();

                return true;
            } catch (Exception ex) {
                Logger.WriteError("EventEditController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            fView.NotesList.ListModel.DataOwner = fDNATest;
            fView.MediaList.ListModel.DataOwner = fDNATest;

            fView.TestName.Text = fDNATest.TestName;
            fView.Date.NormalizeDate = fDNATest.Date.GetDisplayString(DateFormat.dfDD_MM_YYYY);

            BaseController.UpdateComboValues(fBase, fView.Agency, true, false, LabKey, fDNATest.Agency);
            BaseController.UpdateComboValues(fBase, fView.MHaplogroup, true, false, GEDCOMTagName._MHAP, fDNATest.MHaplogroup);
            BaseController.UpdateComboValues(fBase, fView.YHaplogroup, true, false, GEDCOMTagName._YHAP, fDNATest.YHaplogroup);

            fView.FileFormat.SetSelectedTag(fDNATest.FileFormat);

            string fileRef = fDNATest.FileReference;
            fIsNew = string.IsNullOrEmpty(fileRef);
            fView.File.Text = fileRef;

            var storeType = fIsNew ? GlobalOptions.Instance.MediaStoreDefault : MediaStore.GetStoreType(fileRef);
            UpdateFileStore(fIsNew, storeType);

            fView.Restriction.SelectedIndex = (int)fDNATest.Restriction;
        }

        private void UpdateFileStore(bool isNew, MediaStoreType storeType)
        {
            fView.FileSelectButton.Enabled = isNew && (storeType != MediaStoreType.mstURL);

            fView.File.Enabled = isNew && (storeType == MediaStoreType.mstURL);
            fView.File.ReadOnly = !fView.File.Enabled;

            fView.StoreType.Enabled = isNew;
            fView.StoreType.SetSelectedTag(storeType);
        }

        public void LockEditor(bool locked)
        {
            fView.TestName.Enabled = !locked;
            fView.Date.Enabled = !locked;
            fView.Agency.Enabled = !locked;
            fView.MHaplogroup.Enabled = !locked;
            fView.YHaplogroup.Enabled = !locked;

            fView.NotesList.ReadOnly = locked;
            fView.MediaList.ReadOnly = locked;
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

            fView.File.Text = fileName;
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.DNATest));

            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.DlgCancel);

            GetControl<ITabPage>("pageCommon").Text = LangMan.LS(LSID.Common);
            GetControl<ITabPage>("pageNotes").Text = LangMan.LS(LSID.RPNotes);
            GetControl<ITabPage>("pageMultimedia").Text = LangMan.LS(LSID.RPMultimedia);

            GetControl<ILabel>("lblTestName").Text = LangMan.LS(LSID.DNATestName);
            GetControl<ILabel>("lblDate").Text = LangMan.LS(LSID.Date);
            GetControl<ILabel>("lblAgency").Text = LangMan.LS(LSID.DNALaboratory);
            GetControl<ILabel>("lblFileRef").Text = LangMan.LS(LSID.File);
            GetControl<ILabel>("lblStoreType").Text = LangMan.LS(LSID.StoreType);
            GetControl<ILabel>("lblFileFormat").Text = LangMan.LS(LSID.DNAFileFormat);
            GetControl<ILabel>("lblMHaplogroup").Text = LangMan.LS(LSID.MDNAHaplogroup);
            GetControl<ILabel>("lblYHaplogroup").Text = LangMan.LS(LSID.YDNAHaplogroup);
            GetControl<ILabel>("lblRestriction").Text = LangMan.LS(LSID.Restriction);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnAccept").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            GetControl<IButton>("btnCancel").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
