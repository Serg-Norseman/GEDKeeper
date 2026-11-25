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
using GKCore.Design.Views;
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
