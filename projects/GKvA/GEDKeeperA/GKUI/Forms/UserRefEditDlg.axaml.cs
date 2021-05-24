/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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

using Avalonia.Controls;
using Avalonia.Interactivity;
using Avalonia.Markup.Xaml;
using BSLib.Design.MVP.Controls;
using GDModel;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Views;

namespace GKUI.Forms
{
    public sealed partial class UserRefEditDlg : EditorDialog, IUserRefEditDlg
    {
        private readonly UserRefEditDlgController fController;

        public GDMUserReference UserRef
        {
            get { return fController.UserRef; }
            set { fController.UserRef = value; }
        }

        #region Design

        private TextBlock lblReference;
        private AutoCompleteBox cmbRef;
        private TextBlock lblRefType;
        private AutoCompleteBox cmbRefType;
        private Button btnAccept;
        private Button btnCancel;

        private void InitializeComponent()
        {
            AvaloniaXamlLoader.Load(this);

            lblReference = this.FindControl<TextBlock>("lblReference");
            cmbRef = this.FindControl<AutoCompleteBox>("cmbRef");
            lblRefType = this.FindControl<TextBlock>("lblRefType");
            cmbRefType = this.FindControl<AutoCompleteBox>("cmbRefType");
            btnAccept = this.FindControl<Button>("btnAccept");
            btnCancel = this.FindControl<Button>("btnCancel");
        }

        #endregion

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

        public UserRefEditDlg() : this(null)
        {
        }

        public UserRefEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            //btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            //btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            // SetLang()
            btnAccept.Content = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Content = LangMan.LS(LSID.LSID_DlgCancel);
            Title = LangMan.LS(LSID.LSID_WinUserRefEdit);
            lblReference.Text = LangMan.LS(LSID.LSID_Reference);
            lblRefType.Text = LangMan.LS(LSID.LSID_Type);

            fController = new UserRefEditDlgController(this);
            fController.Init(baseWin);
        }

        private void btnAccept_Click(object sender, RoutedEventArgs e)
        {
            DialogResult = fController.Accept() ? DialogResult.Ok : DialogResult.None;
        }
    }
}
