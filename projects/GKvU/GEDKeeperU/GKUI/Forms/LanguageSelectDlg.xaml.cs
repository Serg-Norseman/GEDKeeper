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

using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Views;
using Windows.UI.Xaml;

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

        #region View Interface

        IListView ILanguageSelectDlg.LanguagesList
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
