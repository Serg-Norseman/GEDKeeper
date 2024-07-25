/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using GKCore;
using GKCore.Controllers;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Types;

namespace GKUI.Forms
{
    public sealed partial class SourceEditDlg : CommonDialog<ISourceEditDlg, SourceEditDlgController>, ISourceEditDlg
    {
        public GDMSourceRecord SourceRecord
        {
            get { return fController.SourceRecord; }
            set { fController.SourceRecord = value; }
        }

        #region View Interface

        ISheetList ISourceEditDlg.NotesList
        {
            get { return fNotesList; }
        }

        ISheetList ISourceEditDlg.MediaList
        {
            get { return fMediaList; }
        }

        ISheetList ISourceEditDlg.RepositoriesList
        {
            get { return fRepositoriesList; }
        }

        ISheetList ISourceEditDlg.UserRefList
        {
            get { return fUserRefList; }
        }

        ITextBox ISourceEditDlg.ShortTitle
        {
            get { return GetControlHandler<ITextBox>(txtShortTitle); }
        }

        ITextBox ISourceEditDlg.Author
        {
            get { return GetControlHandler<ITextBox>(txtAuthor); }
        }

        ITextBox ISourceEditDlg.DescTitle
        {
            get { return GetControlHandler<ITextBox>(txtTitle); }
        }

        ITextBox ISourceEditDlg.Publication
        {
            get { return GetControlHandler<ITextBox>(txtPublication); }
        }

        ITextBox ISourceEditDlg.Text
        {
            get { return GetControlHandler<ITextBox>(txtText); }
        }

        IDateControl ISourceEditDlg.Date
        {
            get { return GetControlHandler<IDateControl>(dateCtl); }
        }

        #endregion

        public SourceEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            fController = new SourceEditDlgController(this);
            fController.Init(baseWin);
        }

        private void ModifyReposSheet(object sender, ModifyEventArgs eArgs)
        {
            GDMRepositoryCitation cit = eArgs.ItemData as GDMRepositoryCitation;
            if (eArgs.Action == RecordAction.raJump && cit != null) {
                fController.JumpToRecord(cit);
            }
        }

        private void EditShortTitle_TextChanged(object sender, EventArgs e)
        {
            Title = string.Format("{0} \"{1}\"", LangMan.LS(LSID.Source), txtShortTitle.Text);
        }
    }
}
