/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using BSLib.Design.MVP.Controls;
using GDModel;
using GKCore.MVP;
using GKCore.MVP.Views;
using GKCore.Types;

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
                    fNoteRecord.SetNotesArray(fView.Note.Lines);

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

        public void Import()
        {
            string fileName = AppHost.StdDialogs.GetOpenFile("", "", "Text files (*.txt)|*.txt|All files (*.*)|*.*", 0, ".txt");
            if (string.IsNullOrEmpty(fileName))
                return;

            using (var sr = new StreamReader(fileName)) {
                fView.Note.Text = sr.ReadToEnd();
            }
        }

        public void Export()
        {
            string fileName = AppHost.StdDialogs.GetSaveFile("", "", "Text files (*.txt)|*.txt|All files (*.*)|*.*", 0, ".txt", "", true);
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
            fView.Title = LangMan.LS(LSID.LSID_Note);

            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.LSID_DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.LSID_DlgCancel);
        }
    }

    public class NoteEditDlgExController : NoteEditDlgController
    {
        public NoteEditDlgExController(INoteEdit view) : base(view)
        {
        }

        public override void SetLocale()
        {
            base.SetLocale();

#if NETSTANDARD
            // only for GKv3 (DropDownToolItem), not for GKv2 (ComboBox)
            GetControl<IButtonToolItem>("cmbSizes").Text = "Sizes";
#endif

            GetControl<IButtonToolItem>("ddbtnActions").Text = LangMan.LS(LSID.LSID_Actions);
            GetControl<IMenuItem>("miSelectAndCopy").Text = LangMan.LS(LSID.LSID_SelectAndCopy);
            GetControl<IMenuItem>("miImport").Text = LangMan.LS(LSID.LSID_Import);
            GetControl<IMenuItem>("miExport").Text = LangMan.LS(LSID.LSID_MIExport);
            GetControl<IMenuItem>("miClear").Text = LangMan.LS(LSID.LSID_Clear);
            GetControl<ITabPage>("pageEditor").Text = LangMan.LS(LSID.LSID_Note);
            GetControl<ITabPage>("pagePreview").Text = LangMan.LS(LSID.LSID_DocPreview);
        }
    }
}
