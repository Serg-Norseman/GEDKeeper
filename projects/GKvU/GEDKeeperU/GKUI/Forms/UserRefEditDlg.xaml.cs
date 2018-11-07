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

using GKCommon.GEDCOM;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Controls;
using GKCore.MVP.Views;
using GKUI.Components;
using Windows.UI.Xaml;

namespace GKUI.Forms
{
    public sealed partial class UserRefEditDlg : EditorDialog, IUserRefEditDlg
    {
        private readonly UserRefEditDlgController fController;

        public GEDCOMUserReference UserRef
        {
            get { return fController.UserRef; }
            set { fController.UserRef = value; }
        }

        #region View Interface

        IComboBoxHandler IUserRefEditDlg.Ref
        {
            get { return fControlsManager.GetControlHandler<IComboBoxHandler>(cmbRef); }
        }

        IComboBoxHandler IUserRefEditDlg.RefType
        {
            get { return fControlsManager.GetControlHandler<IComboBoxHandler>(cmbRefType); }
        }

        #endregion

        private void btnAccept_Click(object sender, RoutedEventArgs e)
        {
            DialogResult = fController.Accept() ? DialogResult.Ok : DialogResult.None;
        }

        public UserRefEditDlg() : this(null)
        {
        }

        public UserRefEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            // SetLang()
            btnAccept.Content = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Content = LangMan.LS(LSID.LSID_DlgCancel);
            Caption = LangMan.LS(LSID.LSID_WinUserRefEdit);
            lblReference.Text = LangMan.LS(LSID.LSID_Reference);
            lblRefType.Text = LangMan.LS(LSID.LSID_Type);

            fController = new UserRefEditDlgController(this);
            fController.Init(baseWin);
        }
    }
}
