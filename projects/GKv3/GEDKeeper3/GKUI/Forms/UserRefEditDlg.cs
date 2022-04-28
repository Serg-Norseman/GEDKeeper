/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using Eto.Serialization.Xaml;
using GDModel;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Views;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class UserRefEditDlg : EditorDialog, IUserRefEditDlg
    {
        #region Design components

        private Button btnAccept;
        private Button btnCancel;
        private Label lblReference;
        private ComboBox cmbRef;
        private Label lblRefType;
        private ComboBox cmbRefType;

        #endregion

        private readonly UserRefEditDlgController fController;

        public GDMUserReference UserRef
        {
            get { return fController.UserRef; }
            set { fController.UserRef = value; }
        }

        #region View Interface

        IComboBox IUserRefEditDlg.Ref
        {
            get { return GetControlHandler<IComboBox>(cmbRef); }
        }

        IComboBox IUserRefEditDlg.RefType
        {
            get { return GetControlHandler<IComboBox>(cmbRefType); }
        }

        #endregion

        public UserRefEditDlg(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            // in the root element of the xeto file cause an error that Label cannot be converted to Button!
            DefaultButton = btnAccept;
            AbortButton = btnCancel;

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            // SetLocale()
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            Title = LangMan.LS(LSID.LSID_WinUserRefEdit);
            lblReference.Text = LangMan.LS(LSID.LSID_Reference);
            lblRefType.Text = LangMan.LS(LSID.LSID_Type);

            fController = new UserRefEditDlgController(this);
            fController.Init(baseWin);
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
    }
}
