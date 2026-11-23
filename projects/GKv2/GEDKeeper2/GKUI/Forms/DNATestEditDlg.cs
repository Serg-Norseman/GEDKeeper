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
            get { return fController.GetControlHandler<ITextBox>(txtTestName); }
        }

        IDateBox IDNATestEditDlg.Date
        {
            get { return fController.GetControlHandler<IDateBox>(dateCtl); }
        }

        IComboBox IDNATestEditDlg.Agency
        {
            get { return fController.GetControlHandler<IComboBox>(cmbAgency); }
        }

        IComboBox IDNATestEditDlg.MHaplogroup
        {
            get { return fController.GetControlHandler<IComboBox>(cmbMHaplogroup); }
        }

        IComboBox IDNATestEditDlg.YHaplogroup
        {
            get { return fController.GetControlHandler<IComboBox>(cmbYHaplogroup); }
        }

        IComboBox IDNATestEditDlg.StoreType
        {
            get { return fController.GetControlHandler<IComboBox>(cmbStoreType); }
        }

        ITextBox IDNATestEditDlg.File
        {
            get { return fController.GetControlHandler<ITextBox>(txtFileRef); }
        }

        IButton IDNATestEditDlg.FileSelectButton
        {
            get { return fController.GetControlHandler<IButton>(btnFileSelect); }
        }

        IComboBox IDNATestEditDlg.FileFormat
        {
            get { return fController.GetControlHandler<IComboBox>(cmbFileFormat); }
        }

        IComboBox IDNATestEditDlg.Restriction
        {
            get { return fController.GetControlHandler<IComboBox>(cmbRestriction); }
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
            fController.InitView(baseWin);
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
