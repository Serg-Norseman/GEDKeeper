/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using GKUI.Components;

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

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

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
