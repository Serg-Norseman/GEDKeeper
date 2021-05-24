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
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Views;
using GKUI.Components;

namespace GKUI.Forms
{
    public partial class LanguageSelectDlg : CommonDialog, ILanguageSelectDlg
    {
        private readonly LanguageSelectDlgController fController;

        public int SelectedLanguage
        {
            get { return fController.SelectedLanguage; }
            set { fController.SelectedLanguage = value; }
        }

        #region Design

        private GKListView lstLanguages;
        private Button btnAccept;
        private Button btnCancel;

        private void InitializeComponent()
        {
            AvaloniaXamlLoader.Load(this);

            lstLanguages = this.FindControl<GKListView>("lstLanguages");
            btnAccept = this.FindControl<Button>("btnAccept");
            btnCancel = this.FindControl<Button>("btnCancel");
        }

        #endregion

        #region View Interface

        IListViewEx ILanguageSelectDlg.LanguagesList
        {
            get { return lstLanguages; }
        }

        #endregion

        public LanguageSelectDlg()
        {
            InitializeComponent();

            //btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            //btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            fController = new LanguageSelectDlgController(this);
        }

        private void btnAccept_Click(object sender, RoutedEventArgs e)
        {
            DialogResult = fController.Accept() ? DialogResult.Ok : DialogResult.None;
        }
    }
}
