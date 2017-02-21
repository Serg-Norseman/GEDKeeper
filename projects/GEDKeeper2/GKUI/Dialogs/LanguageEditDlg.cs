/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using GKUI.Controls;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class LanguageEditDlg : Form
    {
        private GEDCOMLanguageID fLanguageID;

        public GEDCOMLanguageID LanguageID
        {
            get { return fLanguageID; }
            set { SetLanguageID(value); }
        }

        public LanguageEditDlg()
        {
            InitializeComponent();

            btnAccept.Image = GKResources.iBtnAccept;
            btnCancel.Image = GKResources.iBtnCancel;

            for (var lid = GEDCOMLanguageID.Unknown; lid < GEDCOMLanguageID.Yiddish; lid++)
            {
                cmbLanguage.Items.Add(new GKComboItem(GEDCOMConsts.LngEnumStr[(int)lid], lid));
            }

            // SetLang()
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            Text = LangMan.LS(LSID.LSID_Language);
            lblLanguage.Text = LangMan.LS(LSID.LSID_Language);
        }

        private void SetLanguageID(GEDCOMLanguageID value)
        {
            fLanguageID = value;
            cmbLanguage.Text = GEDCOMConsts.LngEnumStr[(int)fLanguageID];
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                var item = (GKComboItem)cmbLanguage.Items[cmbLanguage.SelectedIndex];
                fLanguageID = (GEDCOMLanguageID)item.Tag;
                DialogResult = DialogResult.OK;
            }
            catch (Exception ex)
            {
                Logger.LogWrite("LanguageEditDlg.btnAccept_Click(): " + ex.Message);
                DialogResult = DialogResult.None;
            }
        }
    }
}
