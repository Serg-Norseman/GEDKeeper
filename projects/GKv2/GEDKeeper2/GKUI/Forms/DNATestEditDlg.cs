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
using GKCore.Lists;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class DNATestEditDlg : CommonDialog<IDNATestEditDlg, DNATestEditDlgController>, IDNATestEditDlg
    {
        private readonly GKSheetList fNotesList;
        private readonly GKSheetList fMediaList;

        public GDMDNATest DNATest
        {
            get { return fController.DNATest; }
            set { fController.DNATest = value; }
        }

        #region View Interface

        ISheetList IDNATestEditDlg.NotesList
        {
            get { return fNotesList; }
        }

        ISheetList IDNATestEditDlg.MediaList
        {
            get { return fMediaList; }
        }

        ITextBox IDNATestEditDlg.TestName
        {
            get { return GetControlHandler<ITextBox>(txtTestName); }
        }

        IDateBox IDNATestEditDlg.Date
        {
            get { return GetControlHandler<IDateBox>(dateCtl); }
        }

        IComboBox IDNATestEditDlg.Agency
        {
            get { return GetControlHandler<IComboBox>(cmbAgency); }
        }

        IComboBox IDNATestEditDlg.MHaplogroup
        {
            get { return GetControlHandler<IComboBox>(cmbMHaplogroup); }
        }

        IComboBox IDNATestEditDlg.YHaplogroup
        {
            get { return GetControlHandler<IComboBox>(cmbYHaplogroup); }
        }

        IComboBox IDNATestEditDlg.StoreType
        {
            get { return GetControlHandler<IComboBox>(cmbStoreType); }
        }

        ITextBox IDNATestEditDlg.File
        {
            get { return GetControlHandler<ITextBox>(txtFileRef); }
        }

        IButton IDNATestEditDlg.FileSelectButton
        {
            get { return GetControlHandler<IButton>(btnFileSelect); }
        }

        IComboBox IDNATestEditDlg.FileFormat
        {
            get { return GetControlHandler<IComboBox>(cmbFileFormat); }
        }

        IComboBox IDNATestEditDlg.Restriction
        {
            get { return GetControlHandler<IComboBox>(cmbRestriction); }
        }

        #endregion

        public DNATestEditDlg()
        {
            InitializeComponent();
        }

        public DNATestEditDlg(IBaseWindow baseWin) : this()
        {
            tabsData.SelectedIndexChanged += tabControl_SelectedIndexChanged;

            fNotesList = new GKSheetList(pageNotes);
            fMediaList = new GKSheetList(pageMultimedia);

            fController = new DNATestEditDlgController(this);
            fController.Init(baseWin);
        }

        private void btnFileSelect_Click(object sender, EventArgs e)
        {
            fController.SelectFile();
        }

        private void cbRestriction_SelectedIndexChanged(object sender, EventArgs e)
        {
            fController.LockEditor(cmbRestriction.SelectedIndex == (int)GDMRestriction.rnLocked);
        }
    }
}
