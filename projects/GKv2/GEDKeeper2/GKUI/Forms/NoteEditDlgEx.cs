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
using GDModel;
using GKCore.Controllers;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;

namespace GKUI.Forms
{
    public sealed partial class NoteEditDlgEx : CommonDialog<INoteEdit, NoteEditDlgExController>, INoteEditDlgEx
    {
        public GDMNoteRecord NoteRecord
        {
            get { return fController.NoteRecord; }
            set { fController.NoteRecord = value; }
        }

        #region View Interface

        ITextBox INoteEdit.Note
        {
            get { return GetControlHandler<ITextBox>(txtNote); }
        }

        #endregion

        public NoteEditDlgEx(IBaseWindow baseWin)
        {
            InitializeComponent();
            FillSizes();

            txtNote.Enter += RichTextBox_Enter;
            txtNote.Leave += RichTextBox_Leave;

            fController = new NoteEditDlgExController(this);
            fController.Init(baseWin);
        }

        private void RichTextBox_Enter(object sender, EventArgs e)
        {
            AcceptButton = null;
        }

        private void RichTextBox_Leave(object sender, EventArgs e)
        {
            AcceptButton = btnAccept;
        }

        private void FillSizes()
        {
            cmbSizes.Items.Add(new ComboItem<int>("", 0));
            for (int i = 1; i <= 7; i++) {
                cmbSizes.Items.Add(new ComboItem<int>(i.ToString(), i));
            }
            cmbSizes.SelectedIndex = 0;
        }

        private void btnBold_Click(object sender, EventArgs e)
        {
            fController.SetBold();
        }

        private void btnItalic_Click(object sender, EventArgs e)
        {
            fController.SetItalic();
        }

        private void btnUnderline_Click(object sender, EventArgs e)
        {
            fController.SetUnderline();
        }

        private void btnURL_Click(object sender, EventArgs e)
        {
            fController.SetURL();
        }

        private void miSelectAndCopy_Click(object sender, EventArgs e)
        {
            fController.SelectAndCopy();
        }

        private void miImport_Click(object sender, EventArgs e)
        {
            fController.Import();
        }

        private void miExport_Click(object sender, EventArgs e)
        {
            fController.Export();
        }

        private void miClear_Click(object sender, EventArgs e)
        {
            fController.Clear();
        }

        private void tabControl1_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (tabControl1.SelectedTab == pagePreview) {
                hyperView1.Lines.Text = txtNote.Text;
            }
        }

        private void cmbSizes_SelectedIndexChanged(object sender, EventArgs e)
        {
            var item = cmbSizes.SelectedItem as ComboItem<int>;
            if (item == null || item.Text == "") return;

            string value = item.Tag.ToString();
            fController.SetSize(value);
        }
    }
}
