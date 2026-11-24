/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.IO;
using System.Text;
using GDModel;
using GDModel.Providers.GEDCOM;
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
    public class NoteEditDlgController : DialogController<INoteEdit>
    {
        #region Design components
#pragma warning disable CS0649, IDE0044
        private IButton _btnAccept;
        private IButton _btnCancel;
        private ITextBox _txtNote;
        private ITabControl _tabsData;
        private ISheetList _fSourcesList;
        private ISheetList _fUserRefList;
#pragma warning restore CS0649, IDE0044
        #endregion

        private GDMNoteRecord fNoteRecord;

        public GDMNoteRecord NoteRecord
        {
            get { return fNoteRecord; }
            set {
                if (fNoteRecord != value) {
                    fNoteRecord = value;
                    UpdateView();
                }
            }
        }


        public NoteEditDlgController(INoteEdit view) : base(view)
        {
        }

        public override void InitView(IBaseWindow baseWin)
        {
            base.InitView(baseWin);

            if (AppHost.Instance.HasFeatureSupport(Feature.DesktopV3)) {
                _fSourcesList.ListModel = new SourceCitationsListModel(fView, baseWin, fLocalUndoman);
                _fUserRefList.ListModel = new URefsListModel(fView, baseWin, fLocalUndoman);
            }
        }

        public override void DoneView()
        {
            if (AppHost.Instance.HasFeatureSupport(Feature.DesktopV3)) {
                _fSourcesList.ListModel.SaveSettings();
                _fUserRefList.ListModel.SaveSettings();
            }
        }

        public override bool Accept()
        {
            try {
                string noteText = _txtNote.Text.Trim();
                if (!string.IsNullOrEmpty(noteText)) {
                    int size = Encoding.UTF8.GetByteCount(noteText);
                    if (size > GEDCOMConsts.MaxNoteSize) {
                        AppHost.StdDialogs.ShowAlert(LangMan.LS(LSID.NoteMaxSizeExceeded));
                        return false;
                    }

                    fNoteRecord.SetNoteText(noteText);

                    fBase.NotifyRecord(fNoteRecord, RecordAction.raEdit);

                    return true;
                } else {
                    return false;
                }
            } catch (Exception ex) {
                Logger.WriteError("NoteEditDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            _txtNote.Text = fNoteRecord.Lines.Text.Trim();

            if (AppHost.Instance.HasFeatureSupport(Feature.DesktopV3)) {
                _fSourcesList.ListModel.DataOwner = fNoteRecord;
                _fUserRefList.ListModel.DataOwner = fNoteRecord;
            }
        }

        public void SetBold()
        {
            _txtNote.SelectedText = string.Format(" [b]{0}[/b] ", _txtNote.SelectedText);
        }

        public void SetItalic()
        {
            _txtNote.SelectedText = string.Format(" [i]{0}[/i] ", _txtNote.SelectedText);
        }

        public void SetUnderline()
        {
            _txtNote.SelectedText = string.Format(" [u]{0}[/u] ", _txtNote.SelectedText);
        }

        public void SetURL()
        {
            _txtNote.SelectedText = string.Format(" [url={0}]{0}[/url] ", _txtNote.SelectedText);
        }

        public void SelectAndCopy()
        {
            _txtNote.SelectAll();
            _txtNote.Copy();
        }

        public async void Import()
        {
            string fileName = await AppHost.StdDialogs.GetOpenFile("", "", "Text files (*.txt)|*.txt|All files (*.*)|*.*", 0, ".txt");
            if (string.IsNullOrEmpty(fileName))
                return;

            using (var sr = new StreamReader(fileName)) {
                _txtNote.Text = sr.ReadToEnd();
            }
        }

        public async void Export()
        {
            string fileName = await AppHost.StdDialogs.GetSaveFile("", "", "Text files (*.txt)|*.txt|All files (*.*)|*.*", 0, ".txt", "", true);
            if (string.IsNullOrEmpty(fileName))
                return;

            using (var sw = new StreamWriter(fileName)) {
                sw.Write(_txtNote.Text);
            }
        }

        public void Clear()
        {
            _txtNote.Text = string.Empty;
        }

        public void SetSize(string value)
        {
            _txtNote.SelectedText = string.Format(" [size=+{0}]{1}[/size] ", value, _txtNote.SelectedText);
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.Note));

            _btnAccept.Text = LangMan.LS(LSID.DlgAccept);
            _btnCancel.Text = LangMan.LS(LSID.DlgCancel);

            if (AppHost.Instance.HasFeatureSupport(Feature.DesktopV3)) {
                GetControl<ITabPage>("pageEditor").Text = LangMan.LS(LSID.Note);
                GetControl<ITabPage>("pageSources").Text = LangMan.LS(LSID.RPSources);
                GetControl<ITabPage>("pageUserRefs").Text = LangMan.LS(LSID.UserRefs);
            }
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _btnAccept.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            _btnCancel.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }

#if !MOBILE

    public class NoteEditDlgExController : NoteEditDlgController
    {
        public NoteEditDlgExController(INoteEdit view) : base(view)
        {
        }

        public override void SetLocale()
        {
            base.SetLocale();

#if NETCOREAPP
            // only for GKv3 (GKDropDownToolItem), not for GKv2 (ComboBox)
            GetControl<IToolItem>("cmbSizes").Text = "Sizes";
#endif

            GetControl<IToolItem>("ddbtnActions").Text = LangMan.LS(LSID.Actions);
            GetControl<IMenuItem>("miSelectAndCopy").Text = LangMan.LS(LSID.SelectAndCopy);
            GetControl<IMenuItem>("miImport").Text = LangMan.LS(LSID.Import);
            GetControl<IMenuItem>("miExport").Text = LangMan.LS(LSID.MIExport);
            GetControl<IMenuItem>("miClear").Text = LangMan.LS(LSID.Clear);
            GetControl<ITabPage>("pageEditor").Text = LangMan.LS(LSID.Note);
            GetControl<ITabPage>("pagePreview").Text = LangMan.LS(LSID.DocPreview);
        }
    }

#endif
}
