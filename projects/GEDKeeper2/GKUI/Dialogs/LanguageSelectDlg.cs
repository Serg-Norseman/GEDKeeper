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
using GKCore;
using GKCore.Options;
using GKUI.Controls;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public partial class LanguageSelectDlg : Form
    {
        public int SelectedLanguage;

        public LanguageSelectDlg()
        {
            InitializeComponent();

            this.btnAccept.Image = GKResources.iBtnAccept;
            this.btnCancel.Image = GKResources.iBtnCancel;
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                GKComboItem item = this.lstLanguages.Items[this.lstLanguages.SelectedIndex] as GKComboItem;
                if (item != null) {
                    SelectedLanguage = (int)item.Tag;
                }

                this.DialogResult = DialogResult.OK;
            }
            catch (Exception ex)
            {
                Logger.LogWrite("LanguageSelectDlg.btnAccept_Click(): " + ex.Message);
                base.DialogResult = DialogResult.None;
            }
        }

        private void LanguageSelectDlg_Load(object sender, EventArgs e)
        {
            this.lstLanguages.Items.Clear();
            this.lstLanguages.Items.Add(new GKComboItem(LangMan.LS_DEF_NAME, LangMan.LS_DEF_CODE));

            //int idx = 0;
            int num = GlobalOptions.Instance.GetLangsCount();
            for (int i = 0; i < num; i++)
            {
                LangRecord lngRec = GlobalOptions.Instance.GetLang(i);
                //if (GlobalOptions.Instance.InterfaceLang == lngRec.Code)
                //{
                //    idx = i + 1;
                //}
                this.lstLanguages.Items.Add(new GKComboItem(lngRec.Name, (int)lngRec.Code));
            }
            //this.lstLanguages.SelectedIndex = idx;
        }
    }
}
