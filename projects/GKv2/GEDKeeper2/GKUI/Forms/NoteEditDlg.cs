/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;

namespace GKUI.Forms
{
    public sealed partial class NoteEditDlg : CommonDialog<INoteEdit, NoteEditDlgController>, INoteEditDlg
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

        public NoteEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            txtNote.Enter += RichTextBox_Enter;
            txtNote.Leave += RichTextBox_Leave;

            fController = new NoteEditDlgController(this);
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
    }
}
