/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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
using Eto.Forms;

using GKCommon.GEDCOM;
using GKCore;
using GKCore.Controllers;
using GKCore.MVP.Controls;
using GKCore.MVP.Views;
using GKUI.Components;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class NoteEditDlgEx : EditorDialog, INoteEditDlgEx
    {
        private readonly NoteEditDlgController fController;

        public GEDCOMNoteRecord NoteRecord
        {
            get { return fController.NoteRecord; }
            set { fController.NoteRecord = value; }
        }

        #region View Interface

        ITextBoxHandler INoteEdit.Note
        {
            get { return fControlsManager.GetControlHandler<ITextBoxHandler>(txtNote); }
        }

        #endregion

        private void btnAccept_Click(object sender, EventArgs e)
        {
            DialogResult = fController.Accept() ? DialogResult.Ok : DialogResult.None;
        }

        public NoteEditDlgEx()
        {
            InitializeComponent();
            FillSizes();

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            // SetLang()
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            Title = LangMan.LS(LSID.LSID_Note);

            ddbtnActions.Text = LangMan.LS(LSID.LSID_Actions);
            miSelectAndCopy.Text = LangMan.LS(LSID.LSID_SelectAndCopy);
            miImport.Text = LangMan.LS(LSID.LSID_Import);
            miExport.Text = LangMan.LS(LSID.LSID_MIExport);
            miClear.Text = LangMan.LS(LSID.LSID_Clear);
            pageEditor.Text = LangMan.LS(LSID.LSID_Note);
            pagePreview.Text = LangMan.LS(LSID.LSID_DocPreview);

            fController = new NoteEditDlgController(this);
        }

        private void FillSizes()
        {
            for (int i = 1; i <= 7; i++) {
                var item = new GKToolStripMenuItem(i.ToString(), i);
                item.Click += cmbSizes_SelectedIndexChanged;
                menuSizes.Items.Add(item);
            }
            /*cmbSizes.Items.Add(new GKComboItem("", 0));
            for (int i = 1; i <= 7; i++) {
                cmbSizes.Items.Add(new GKComboItem(i.ToString(), i));
            }
            cmbSizes.SelectedIndex = 0;*/
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
            if (tabControl1.SelectedPage == pagePreview) {
                hyperView1.Lines.Text = txtNote.Text;
            }
        }

        private void cmbSizes_SelectedIndexChanged(object sender, EventArgs e)
        {
            var item = sender as GKToolStripMenuItem; //menuSizes.SelectedItem as GKComboItem;
            if (item == null || item.Text == "") return;

            string value = item.Tag.ToString();
            fController.SetSize(value);
        }
    }
}
