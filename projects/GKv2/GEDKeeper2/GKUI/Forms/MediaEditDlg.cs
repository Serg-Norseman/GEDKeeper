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
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class MediaEditDlg : CommonDialog<IMediaEditDlg, MediaEditDlgController>, IMediaEditDlg
    {
        private readonly GKSheetList fNotesList;
        private readonly GKSheetList fSourcesList;
        private readonly GKSheetList fUserRefList;

        public GDMMultimediaRecord MultimediaRecord
        {
            get { return fController.MultimediaRecord; }
            set { fController.MultimediaRecord = value; }
        }

        #region View Interface

        ISheetList IMediaEditDlg.NotesList
        {
            get { return fNotesList; }
        }

        ISheetList IMediaEditDlg.SourcesList
        {
            get { return fSourcesList; }
        }

        ISheetList IMediaEditDlg.UserRefList
        {
            get { return fUserRefList; }
        }

        IComboBox IMediaEditDlg.MediaType
        {
            get { return GetControlHandler<IComboBox>(cmbMediaType); }
        }

        IComboBox IMediaEditDlg.StoreType
        {
            get { return GetControlHandler<IComboBox>(cmbStoreType); }
        }

        ITextBox IMediaEditDlg.Name
        {
            get { return GetControlHandler<ITextBox>(txtName); }
        }

        ITextBox IMediaEditDlg.File
        {
            get { return GetControlHandler<ITextBox>(txtFile); }
        }

        IButton IMediaEditDlg.FileSelectButton
        {
            get { return GetControlHandler<IButton>(btnFileSelect); }
        }

        #endregion

        public MediaEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            tabsData.SelectedIndexChanged += tabControl_SelectedIndexChanged;

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            fNotesList = new GKSheetList(pageNotes);
            fSourcesList = new GKSheetList(pageSources);

            fUserRefList = new GKSheetList(pageUserRefs);
            fUserRefList.SetControlName("fUserRefList"); // for purpose of tests

            fController = new MediaEditDlgController(this);
            fController.Init(baseWin);
        }

        private void btnFileSelect_Click(object sender, EventArgs e)
        {
            fController.SelectFile();
        }

        private void btnView_Click(object sender, EventArgs e)
        {
            fController.View();
        }

        private void edName_TextChanged(object sender, EventArgs e)
        {
            Title = string.Format("{0} \"{1}\"", LangMan.LS(LSID.RPMultimedia), txtName.Text);
        }

        private void cmbStoreType_SelectedIndexChanged(object sender, EventArgs e)
        {
            fController.ChangeStoreType();
        }
    }
}
