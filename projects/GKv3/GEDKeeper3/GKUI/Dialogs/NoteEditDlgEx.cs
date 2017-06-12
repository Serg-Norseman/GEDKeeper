/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using Eto.Drawing;
using Eto.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;
using GKCore.UIContracts;
using GKUI.Components;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class NoteEditDlgEx : EditorDialog, INoteEditDlgEx
    {
        private GEDCOMNoteRecord fNoteRecord;

        public GEDCOMNoteRecord NoteRecord
        {
            get { return fNoteRecord; }
            set { SetNoteRecord(value); }
        }

        private void SetNoteRecord(GEDCOMNoteRecord value)
        {
            fNoteRecord = value;
            txtNote.Text = fNoteRecord.Note.Text.Trim();
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                int length = 0;
                for (int it = 0; txtNote.Lines.Length > it; ++it)
                {
                    length += txtNote.Lines[it].Trim().Length;
                }
                if (0 != length)
                {
                    fNoteRecord.SetNotesArray(txtNote.Lines);

                    fBase.NotifyRecord(fNoteRecord, RecordAction.raEdit);

                    DialogResult = DialogResult.Ok;
                }
                else
                {
                    DialogResult = DialogResult.Cancel;
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("NoteEditDlg.btnAccept_Click(): " + ex.Message);
                DialogResult = DialogResult.None;
            }
        }

        public NoteEditDlgEx()
        {
            InitializeComponent();
            FillSizes();

            btnAccept.Image = Bitmap.FromResource("Resources.btn_accept.gif");
            btnCancel.Image = Bitmap.FromResource("Resources.btn_cancel.gif");

            // SetLang()
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            Title = LangMan.LS(LSID.LSID_Note);

            ddbtnActions.Text = LangMan.LS(LSID.LSID_Actions);
            miSelectAndCopy.Text = LangMan.LS(LSID.LSID_SelectAndCopy);
            miImport.Text = LangMan.LS(LSID.LSID_Import);
            miExport.Text = LangMan.LS(LSID.LSID_MIExport);
            miClear.Text = LangMan.LS(LSID.LSID_Clear);
        }

        private void FillSizes()
        {
            cmbSizes.Items.Add(new GKComboItem("", 0));
            for (int i = 1; i <= 7; i++) {
                cmbSizes.Items.Add(new GKComboItem(i.ToString(), i));
            }
            cmbSizes.SelectedIndex = 0;
        }

        private void btnBold_Click(object sender, EventArgs e)
        {
            txtNote.SelectedText = string.Format(" [b]{0}[/b] ", txtNote.SelectedText);
        }

        private void btnItalic_Click(object sender, EventArgs e)
        {
            txtNote.SelectedText = string.Format(" [i]{0}[/i] ", txtNote.SelectedText);
        }

        private void btnUnderline_Click(object sender, EventArgs e)
        {
            txtNote.SelectedText = string.Format(" [u]{0}[/u] ", txtNote.SelectedText);
        }

        private void btnURL_Click(object sender, EventArgs e)
        {
            txtNote.SelectedText = string.Format(" [url={0}]{0}[/url] ", txtNote.SelectedText);
        }

        private void miSelectAndCopy_Click(object sender, EventArgs e)
        {
            txtNote.Select();
            txtNote.SelectAll();
            txtNote.Copy();
        }

        private void miImport_Click(object sender, EventArgs e)
        {
            string fileName = AppHost.StdDialogs.GetOpenFile("", "", "Text files (*.txt)|*.txt|All files (*.*)|*.*", 0, ".txt");
            if (string.IsNullOrEmpty(fileName)) return;

            using (var sr = new StreamReader(fileName)) {
                txtNote.Text = sr.ReadToEnd();
            }
        }

        private void miExport_Click(object sender, EventArgs e)
        {
            string fileName = AppHost.StdDialogs.GetSaveFile("", "", "Text files (*.txt)|*.txt|All files (*.*)|*.*", 0, ".txt", "", true);
            if (string.IsNullOrEmpty(fileName)) return;

            using (var sw = new StreamWriter(fileName)) {
                sw.Write(txtNote.Text);
            }
        }

        private void miClear_Click(object sender, EventArgs e)
        {
            txtNote.Text = string.Empty;
        }

        private void tabControl1_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (tabControl1.SelectedPage == pagePreview) {
                hyperView1.Lines.Text = txtNote.Text;
            }
        }

        private void cmbSizes_SelectedIndexChanged(object sender, EventArgs e)
        {
            var item = cmbSizes.SelectedItem as GKComboItem;
            if (item == null || item.Caption == "") return;

            string value = item.Tag.ToString();
            txtNote.SelectedText = string.Format(" [size=+{0}]{1}[/size] ", value, txtNote.SelectedText);
        }
    }
}
