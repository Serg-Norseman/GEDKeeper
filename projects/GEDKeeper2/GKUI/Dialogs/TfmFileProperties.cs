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
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public partial class TfmFileProperties : Form, IBaseEditor
    {
        private readonly IBaseWindow fBase;

        public IBaseWindow Base
        {
            get { return this.fBase; }
        }

        public TfmFileProperties(IBaseWindow aBase)
        {
            this.InitializeComponent();

            this.fBase = aBase;

            this.UpdateControls();

            // SetLang()
            this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            this.SheetAuthor.Text = LangMan.LS(LSID.LSID_Author);
            this.Label1.Text = LangMan.LS(LSID.LSID_Name);
            this.Label2.Text = LangMan.LS(LSID.LSID_Address);
            this.Label3.Text = LangMan.LS(LSID.LSID_Telephone);
            this.SheetAdvanced.Text = LangMan.LS(LSID.LSID_Other);
        }

        private void UpdateControls()
        {
            GEDCOMSubmitterRecord submitter = this.fBase.Tree.GetSubmitter();
            this.EditName.Text = submitter.Name.FullName;
            this.MemoAddress.Text = submitter.Address.Address.Text;

            if (submitter.Address.PhoneNumbers.Count > 0) {
                this.EditTel.Text = submitter.Address.PhoneNumbers[0].StringValue;
            }

            this.UpdateStats();
        }

        private void UpdateStats()
        {
            int[] stats = new int[((int)GEDCOMRecordType.rtLast)];

            int num = this.fBase.Tree.RecordsCount;
            for (int i = 0; i < num; i++)
            {
                GEDCOMRecord rec = this.fBase.Tree[i];
                int index = (int)rec.RecordType;
                stats[index] += 1;
            }

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
                submitter.Name.StringValue = this.EditName.Text;
                submitter.Address.SetAddressArray(this.MemoAddress.Lines);

                if (submitter.Address.PhoneNumbers.Count > 0) {
                    submitter.Address.PhoneNumbers[0].StringValue = this.EditTel.Text;
                } else {
                    submitter.Address.AddPhoneNumber(this.EditTel.Text);
                }

                submitter.ChangeDate.ChangeDateTime = DateTime.Now;
                this.fBase.Modified = true;
                base.DialogResult = DialogResult.OK;
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("TfmFileProperties.btnAccept_Click(): " + ex.Message);
                base.DialogResult = DialogResult.None;
            }
        }
    }
}
