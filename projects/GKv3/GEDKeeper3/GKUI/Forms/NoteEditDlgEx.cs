/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKUI.Components;
using GKUI.Platform;

namespace GKUI.Forms
{
    public sealed partial class NoteEditDlgEx : CommonDialog<INoteEdit, NoteEditDlgExController>, INoteEditDlgEx
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Button btnAccept;
        private Button btnCancel;
        private RichTextArea txtNote;
        private HyperView hyperView1;
        private GKDropDownToolItem cmbSizes;
        private ContextMenu menuSizes;
        private ButtonMenuItem miClear;
        private ButtonMenuItem miExport;
        private ButtonMenuItem miImport;
        private ButtonMenuItem miSelectAndCopy;
        private GKDropDownToolItem ddbtnActions;
        private ContextMenu menuActions;
        private ButtonToolItem btnURL;
        private ButtonToolItem btnUnderline;
        private ButtonToolItem btnItalic;
        private ButtonToolItem btnBold;
        private TabPage pagePreview;
        private ToolBar toolStrip1;
        private TabPage pageEditor;
        private TabControl tabControl1;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

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

            fController = new NoteEditDlgExController(this);
            fController.Init(baseWin);
        }

        private void InitializeComponent()
        {
            XamlReader.Load(this);

            //btnBold.Font=new Font("Tahoma", 9F, FontStyle.Bold);
            //btnItalic.Font=new Font("Tahoma", 9F, FontStyle.Italic);
            //btnUnderline.Font=new Font("Tahoma", 9F, FontStyle.None, FontDecoration.Underline);
            //btnURL.Font=new Font("Tahoma", 9F, FontStyle.None, FontDecoration.Underline);
            //btnURL.TextColor=Colors.Blue;

            miSelectAndCopy = new ButtonMenuItem();
            miSelectAndCopy.Click += miSelectAndCopy_Click;

            miImport = new ButtonMenuItem();
            miImport.Click += miImport_Click;

            miExport = new ButtonMenuItem();
            miExport.Click += miExport_Click;

            miClear = new ButtonMenuItem();
            miClear.Click += miClear_Click;

            menuActions.Items.AddRange(new MenuItem[] {
                                           miSelectAndCopy,
                                           miImport,
                                           miExport,
                                           miClear});
        }

        private void FillSizes()
        {
            for (int i = 1; i <= 7; i++) {
                var item = new MenuItemEx(i.ToString(), i);
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
            var item = sender as MenuItemEx; //menuSizes.SelectedItem as GKComboItem;
            if (item == null || item.Text == "") return;

            string value = item.Tag.ToString();
            fController.SetSize(value);
        }
    }
}
