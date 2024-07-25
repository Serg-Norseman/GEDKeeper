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
using Eto.Forms;
using Eto.Serialization.Xaml;
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
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private TabControl tabsData;
        private TabPage pageNotes;
        private TabPage pageSources;
        private Button btnAccept;
        private Button btnCancel;
        private Button btnView;
        private TabPage pageCommon;
        private Label lblName;
        private TextBox txtName;
        private Label lblType;
        private ComboBox cmbMediaType;
        private Label lblStoreType;
        private ComboBox cmbStoreType;
        private Label lblFile;
        private TextBox txtFile;
        private Button btnFileSelect;
        private GKSheetList fNotesList;
        private GKSheetList fSourcesList;
        private GKSheetList fUserRefList;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

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
            XamlReader.Load(this);

            tabsData.SelectedIndexChanged += tabControl_SelectedIndexChanged;

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
