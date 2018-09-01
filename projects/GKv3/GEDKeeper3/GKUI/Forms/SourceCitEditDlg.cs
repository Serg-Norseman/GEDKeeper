/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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

using GKCommon.GEDCOM;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.UIContracts;
using GKUI.Components;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class SourceCitEditDlg : EditorDialog, ISourceCitEditDlg
    {
        private readonly SourceCitEditDlgController fController;

        public GEDCOMSourceCitation SourceCitation
        {
            get { return fController.SourceCitation; }
            set { fController.SourceCitation = value; }
        }

        #region View Interface

        ITextBoxHandler ISourceCitEditDlg.Page
        {
            get { return fControlsManager.GetControlHandler<ITextBoxHandler>(txtPage); }
        }

        IComboBoxHandler ISourceCitEditDlg.Certainty
        {
            get { return fControlsManager.GetControlHandler<IComboBoxHandler>(txtCertainty); }
        }

        IComboBoxHandler ISourceCitEditDlg.Source
        {
            get { return fControlsManager.GetControlHandler<IComboBoxHandler>(cmbSource); }
        }

        #endregion

        private void btnAccept_Click(object sender, EventArgs e)
        {
            DialogResult = fController.Accept() ? DialogResult.Ok : DialogResult.None;
        }

        private void btnSourceAdd_Click(object sender, EventArgs e)
        {
            fController.AddSource();
        }

        private void cbSource_KeyDown(object sender, KeyEventArgs e)
        {
            // dummy
        }

        private void cbSource_KeyUp(object sender, KeyEventArgs e)
        {
            fController.RefreshSourcesList(cmbSource.Text);
            //cmbSource.SelectionStart = cmbSource.Text.Length;
        }

        public SourceCitEditDlg()
        {
            InitializeComponent();

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");
            btnSourceAdd.Image = UIHelper.LoadResourceImage("Resources.btn_rec_new.gif");

            for (int i = 0; i < GKData.CertaintyAssessments.Length; i++) {
                txtCertainty.Items.Add(LangMan.LS(GKData.CertaintyAssessments[i]));
            }

            // SetLang()
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            Title = LangMan.LS(LSID.LSID_WinSourceCitEdit);
            lblSource.Text = LangMan.LS(LSID.LSID_Source);
            lblPage.Text = LangMan.LS(LSID.LSID_Page);
            lblCertainty.Text = LangMan.LS(LSID.LSID_Certainty);

            btnSourceAdd.ToolTip = LangMan.LS(LSID.LSID_SourceAddTip);

            fController = new SourceCitEditDlgController(this);
        }

        public override void InitDialog(IBaseWindow baseWin)
        {
            base.InitDialog(baseWin);
            fController.Init(baseWin);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
            }
            base.Dispose(disposing);
        }
    }
}
