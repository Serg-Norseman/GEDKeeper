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
    public sealed partial class FilePropertiesDlg : EditorDialog, IFilePropertiesDlg
    {
        private readonly FilePropertiesDlgController fController;

        #region View Interface

        IListView IFilePropertiesDlg.RecordStats
        {
            get { return lvRecordStats; }
        }

        ITextBoxHandler IFilePropertiesDlg.Language
        {
            get { return fControlsManager.GetControlHandler<ITextBoxHandler>(txtLanguage); }
        }

        ITextBoxHandler IFilePropertiesDlg.Name
        {
            get { return fControlsManager.GetControlHandler<ITextBoxHandler>(txtName); }
        }

        ITextBoxHandler IFilePropertiesDlg.Address
        {
            get { return  fControlsManager.GetControlHandler<ITextBoxHandler>(txtAddress); }
        }

        ITextBoxHandler IFilePropertiesDlg.Tel
        {
            get { return  fControlsManager.GetControlHandler<ITextBoxHandler>(txtTel); }
        }

        #endregion

        public FilePropertiesDlg()
        {
            InitializeComponent();

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");
            btnLangEdit.Image = UIHelper.LoadResourceImage("Resources.btn_rec_edit.gif");

            // SetLang()
            Text = LangMan.LS(LSID.LSID_MIFileProperties);
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            pageAuthor.Text = LangMan.LS(LSID.LSID_Author);
            lblName.Text = LangMan.LS(LSID.LSID_Name);
            lblAddress.Text = LangMan.LS(LSID.LSID_Address);
            lblTelephone.Text = LangMan.LS(LSID.LSID_Telephone);
            pageOther.Text = LangMan.LS(LSID.LSID_Other);
            lvRecordStats.Columns[0].Text = LangMan.LS(LSID.LSID_RM_Records);
            lblLanguage.Text = LangMan.LS(LSID.LSID_Language);

            fController = new FilePropertiesDlgController(this);
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            DialogResult = fController.Accept() ? DialogResult.OK : DialogResult.None;
        }

        private void btnLangEdit_Click(object sender, EventArgs e)
        {
            using (var dlg = new LanguageEditDlg()) {
                dlg.LanguageID = fBase.Context.Tree.Header.Language.Value;

                if (dlg.ShowDialog() == DialogResult.OK) {
                    // Assignment in control, instead of the header's property to work Cancel.
                    txtLanguage.Text = GEDCOMLanguageEnum.Instance.GetStrValue(dlg.LanguageID);
                }
            }
        }

        public override void InitDialog(IBaseWindow baseWin)
        {
            base.InitDialog(baseWin);
            fController.Init(baseWin);
            fController.UpdateView();
        }
    }
}
