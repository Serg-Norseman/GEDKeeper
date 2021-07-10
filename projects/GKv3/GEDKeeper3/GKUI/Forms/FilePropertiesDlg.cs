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
using System.ComponentModel;
using BSLib.Design.MVP.Controls;
using Eto.Forms;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Views;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class FilePropertiesDlg : CommonDialog, IFilePropertiesDlg
    {
        private readonly FilePropertiesDlgController fController;

        public IBaseWindow Base
        {
            get { return fController.Base; }
        }

        #region View Interface

        IListView IFilePropertiesDlg.RecordStats
        {
            get { return lvRecordStats; }
        }

        ITextBox IFilePropertiesDlg.Language
        {
            get { return GetControlHandler<ITextBox>(txtLanguage); }
        }

        ITextBox IFilePropertiesDlg.Name
        {
            get { return GetControlHandler<ITextBox>(txtName); }
        }

        ITextBox IFilePropertiesDlg.Address
        {
            get { return  GetControlHandler<ITextBox>(txtAddress); }
        }

        ITextBox IFilePropertiesDlg.Tel
        {
            get { return  GetControlHandler<ITextBox>(txtTel); }
        }

        #endregion

        public FilePropertiesDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");
            btnLangEdit.Image = UIHelper.LoadResourceImage("Resources.btn_rec_edit.gif");

            // SetLocale()
            Title = LangMan.LS(LSID.LSID_MIFileProperties);
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            pageAuthor.Text = LangMan.LS(LSID.LSID_Author);
            lblName.Text = LangMan.LS(LSID.LSID_Name);
            lblAddress.Text = LangMan.LS(LSID.LSID_Address);
            lblTelephone.Text = LangMan.LS(LSID.LSID_Telephone);
            pageOther.Text = LangMan.LS(LSID.LSID_Other);
            lvRecordStats.SetColumnCaption(0, LangMan.LS(LSID.LSID_RM_Records));
            lblLanguage.Text = LangMan.LS(LSID.LSID_Language);

            fController = new FilePropertiesDlgController(this);
            fController.Init(baseWin);
            fController.UpdateView();
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            DialogResult = fController.Accept() ? DialogResult.Ok : DialogResult.None;
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            DialogResult = fController.Cancel() ? DialogResult.Cancel : DialogResult.None;
        }

        protected override void OnClosing(CancelEventArgs e)
        {
            base.OnClosing(e);
            e.Cancel = fController.CheckChangesPersistence();
        }

        private void btnLangEdit_Click(object sender, EventArgs e)
        {
            fController.ChangeLanguage();
        }
    }
}
