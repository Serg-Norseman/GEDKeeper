/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public partial class FilePropertiesDlg : Form, IBaseEditor
    {
        private readonly IBaseWindow fBase;

        public IBaseWindow Base
        {
            get { return this.fBase; }
        }

        public FilePropertiesDlg(IBaseWindow aBase)
        {
            this.InitializeComponent();

            this.fBase = aBase;

            GKResourceManager resourceManager = MainWin.ResourceManager;
            if (resourceManager != null) {
                this.btnAccept.Image = ((System.Drawing.Image)(resourceManager.GetObjectEx("iBtnAccept")));
                this.btnCancel.Image = ((System.Drawing.Image)(resourceManager.GetObjectEx("iBtnCancel")));
            }

            this.UpdateControls();

            // SetLang()
            this.Text = LangMan.LS(LSID.LSID_MIFileProperties);
            this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            this.pageAuthor.Text = LangMan.LS(LSID.LSID_Author);
            this.lblName.Text = LangMan.LS(LSID.LSID_Name);
            this.lblAddress.Text = LangMan.LS(LSID.LSID_Address);
            this.lblTelephone.Text = LangMan.LS(LSID.LSID_Telephone);
            this.pageOther.Text = LangMan.LS(LSID.LSID_Other);
            this.lvRecordStats.Columns[0].Text = LangMan.LS(LSID.LSID_RM_Records);
        }

        private void UpdateControls()
        {
            GEDCOMSubmitterRecord submitter = this.fBase.Tree.GetSubmitter();
            this.txtName.Text = submitter.Name.FullName;
            this.txtAddress.Text = submitter.Address.Address.Text;

            if (submitter.Address.PhoneNumbers.Count > 0) {
                this.txtTel.Text = submitter.Address.PhoneNumbers[0].StringValue;
            }

            this.UpdateStats();
        }

        private void UpdateStats()
        {
            int[] stats = this.fBase.Tree.GetRecordStats();

            lvRecordStats.Items.Clear();
            for (int i = 1; i < stats.Length; i++)
            {
                ListViewItem item = lvRecordStats.Items.Add(LangMan.LS(GKData.RecordTypes[i]));
                item.SubItems.Add(stats[i].ToString());
            }
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                GEDCOMSubmitterRecord submitter = this.fBase.Tree.GetSubmitter();
                submitter.Name.StringValue = this.txtName.Text;
                submitter.Address.SetAddressArray(this.txtAddress.Lines);

                if (submitter.Address.PhoneNumbers.Count > 0) {
                    submitter.Address.PhoneNumbers[0].StringValue = this.txtTel.Text;
                } else {
                    submitter.Address.AddPhoneNumber(this.txtTel.Text);
                }

                submitter.ChangeDate.ChangeDateTime = DateTime.Now;
                this.fBase.Modified = true;
                base.DialogResult = DialogResult.OK;
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("FilePropertiesDlg.btnAccept_Click(): " + ex.Message);
                base.DialogResult = DialogResult.None;
            }
        }
    }
}
