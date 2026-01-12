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
using GKCore.Lists;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class DNATestEditDlg : CommonDialog<IDNATestEditDlg, DNATestEditDlgController>, IDNATestEditDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Button btnAccept;
        private Button btnCancel;
        private TabControl tabsData;
        private TabPage pageNotes;
        private TabPage pageMultimedia;
        private TabPage pageCommon;
        private Label lblTestName;
        private TextBox txtTestName;
        private Label lblAgency;
        private ComboBox cmbAgency;
        private Label lblDate;
        private GKDateBox dateCtl;
        private Label lblFileRef;
        private TextBox txtFileRef;
        private Label lblStoreType;
        private ComboBox cmbStoreType;
        private Button btnFileSelect;
        private Label lblFileFormat;
        private ComboBox cmbFileFormat;
        private Label lblYHaplogroup;
        private ComboBox cmbYHaplogroup;
        private Label lblMHaplogroup;
        private ComboBox cmbMHaplogroup;
        private Label lblRestriction;
        private ComboBox cmbRestriction;

        private GKSheetList fNotesList;
        private GKSheetList fMediaList;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

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

        public DNATestEditDlg(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            tabsData.SelectedIndexChanged += tabControl_SelectedIndexChanged;

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
