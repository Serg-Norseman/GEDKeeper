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
using System.Windows.Forms;

using BSLib;
using GKCommon.GEDCOM;
using GKCore;
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
        private readonly StringList fSourcesList;

        private GEDCOMSourceCitation fSourceCitation;

        public GEDCOMSourceCitation SourceCitation
        {
            get { return fSourceCitation; }
            set { SetSourceCitation(value); }
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                int idx = fSourcesList.IndexOf(cmbSource.Text);
                GEDCOMSourceRecord src = ((idx < 0) ? null : (fSourcesList.GetObject(idx) as GEDCOMSourceRecord));

                if (src == null) {
                    AppHost.StdDialogs.ShowError(LangMan.LS(LSID.LSID_DoNotSetSource));
                    DialogResult = DialogResult.None;
                } else {
                    fSourceCitation.Value = src;
                    fSourceCitation.Page = txtPage.Text;
                    fSourceCitation.CertaintyAssessment = txtCertainty.SelectedIndex;
                    DialogResult = DialogResult.OK;
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("SourceCitEditDlg.btnAccept_Click(): " + ex.Message);
                DialogResult = DialogResult.None;
            }
        }

        private void btnSourceAdd_Click(object sender, EventArgs e)
        {
            object[] anArgs = new object[0];
            GEDCOMSourceRecord src = fBase.Context.SelectRecord(GEDCOMRecordType.rtSource, anArgs) as GEDCOMSourceRecord;
            if (src == null) return;
            
            fBase.Context.GetSourcesList(fSourcesList);
            RefreshSourcesList("");
            cmbSource.Text = src.FiledByEntry;
        }

        private void cbSource_KeyDown(object sender, KeyEventArgs e)
        {
            // dummy
        }

        private void cbSource_KeyUp(object sender, KeyEventArgs e)
        {
            RefreshSourcesList(cmbSource.Text);
            cmbSource.SelectionStart = cmbSource.Text.Length;
        }

        private void RefreshSourcesList(string filter)
        {
            cmbSource.BeginUpdate();
            try
            {
                cmbSource.Items.Clear();

                string flt = "*" + filter + "*";

                int num = fSourcesList.Count;
                for (int i = 0; i < num; i++) {
                    string st = fSourcesList[i];

                    if (filter == "" || GKUtils.MatchesMask(st, flt))
                    {
                        cmbSource.Items.Add(new GKComboItem(st, fSourcesList.GetObject(i)));
                    }
                }
            }
            finally
            {
                cmbSource.EndUpdate();
            }
        }

        private void SetSourceCitation(GEDCOMSourceCitation value)
        {
            fSourceCitation = value;

            GEDCOMSourceRecord src = (fSourceCitation.Value as GEDCOMSourceRecord);
            if (src != null) cmbSource.Text = src.FiledByEntry;

            txtPage.Text = fSourceCitation.Page;
            txtCertainty.SelectedIndex = fSourceCitation.CertaintyAssessment;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                fSourcesList.Dispose();
                if (components != null) components.Dispose();
            }
            base.Dispose(disposing);
        }

        public SourceCitEditDlg()
        {
            InitializeComponent();

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");
            btnSourceAdd.Image = UIHelper.LoadResourceImage("Resources.btn_rec_new.gif");

            for (int i = 0; i < GKData.CertaintyAssessments.Length; i++)
            {
                txtCertainty.Items.Add(LangMan.LS(GKData.CertaintyAssessments[i]));
            }

            fSourcesList = new StringList();

            // SetLang()
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            Text = LangMan.LS(LSID.LSID_WinSourceCitEdit);
            lblSource.Text = LangMan.LS(LSID.LSID_Source);
            lblPage.Text = LangMan.LS(LSID.LSID_Page);
            lblCertainty.Text = LangMan.LS(LSID.LSID_Certainty);

            toolTip1.SetToolTip(btnSourceAdd, LangMan.LS(LSID.LSID_SourceAddTip));
        }

        public override void InitDialog(IBaseWindow baseWin)
        {
            base.InitDialog(baseWin);

            fBase.Context.GetSourcesList(fSourcesList);
            RefreshSourcesList("");
        }
    }
}
