/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2018-2023 by Sergey V. Zhdanovskih.
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
using GKCore.Design.Controls;
using GKCore.Design.Views;
using Xamarin.Forms.Xaml;

namespace GKUI.Forms
{
    [XamlCompilation(XamlCompilationOptions.Compile)]
    public partial class LanguageSelectDlg : CommonDialog<ILanguageSelectDlg, LanguageSelectDlgController>, ILanguageSelectDlg
    {
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

            lstLanguages.AddColumn(@"Language", 300);

            fController = new LanguageSelectDlgController(this);
        }
    }
}
