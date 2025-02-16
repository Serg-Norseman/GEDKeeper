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
using System.Text;
using GDModel;
using GKCore.Design.Controls;
using GKCore.Design;
using GKCore.Design.Views;
using GKCore.Types;
using GDModel.Providers.GEDCOM;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public class NoteEditDlgController : DialogController<INoteEdit>
    {
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

        public override bool Accept()
        {
            try {
                string noteText = fView.Note.Text.Trim();
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
            fView.Note.Text = fNoteRecord.Lines.Text.Trim();
        }

        public void SetBold()
        {
            fView.Note.SelectedText = string.Format(" [b]{0}[/b] ", fView.Note.SelectedText);
        }

        public void SetItalic()
        {
            fView.Note.SelectedText = string.Format(" [i]{0}[/i] ", fView.Note.SelectedText);
        }

        public void SetUnderline()
        {
            fView.Note.SelectedText = string.Format(" [u]{0}[/u] ", fView.Note.SelectedText);
        }

        public void SetURL()
        {
            fView.Note.SelectedText = string.Format(" [url={0}]{0}[/url] ", fView.Note.SelectedText);
        }

        public void SelectAndCopy()
        {
            fView.Note.SelectAll();
            fView.Note.Copy();
        }

        public async void Import()
        {
            string fileName = await AppHost.StdDialogs.GetOpenFile("", "", "Text files (*.txt)|*.txt|All files (*.*)|*.*", 0, ".txt");
            if (string.IsNullOrEmpty(fileName))
                return;

            using (var sr = new StreamReader(fileName)) {
                fView.Note.Text = sr.ReadToEnd();
            }
        }

        public async void Export()
        {
            string fileName = await AppHost.StdDialogs.GetSaveFile("", "", "Text files (*.txt)|*.txt|All files (*.*)|*.*", 0, ".txt", "", true);
            if (string.IsNullOrEmpty(fileName))
                return;

            using (var sw = new StreamWriter(fileName)) {
                sw.Write(fView.Note.Text);
            }
        }

        public void Clear()
        {
            fView.Note.Text = string.Empty;
        }

        public void SetSize(string value)
        {
            fView.Note.SelectedText = string.Format(" [size=+{0}]{1}[/size] ", value, fView.Note.SelectedText);
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.Note);

            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.DlgCancel);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnAccept").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            GetControl<IButton>("btnCancel").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
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

#if NETCORE
            // only for GKv3 (GKDropDownToolItem), not for GKv2 (ComboBox)
            GetControl<IButtonToolItem>("cmbSizes").Text = "Sizes";
#endif

            GetControl<IButtonToolItem>("ddbtnActions").Text = LangMan.LS(LSID.Actions);
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
