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
using Avalonia.Input;
using Avalonia.Interactivity;
using Avalonia.Markup.Xaml;
using BSLib.Design.MVP.Controls;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Views;

namespace GKUI.Forms
{
    public sealed partial class QuickSearchDlg : CommonForm, IQuickSearchDlg
    {
        private readonly QuickSearchDlgController fController;

        #region Design

        private TextBox txtSearchPattern;
        private Button btnPrev;
        private Button btnNext;

        private void InitializeComponent()
        {
            AvaloniaXamlLoader.Load(this);

            base.CanResize = false;

            txtSearchPattern = this.FindControl<TextBox>("txtSearchPattern");
            btnPrev = this.FindControl<Button>("btnPrev");
            btnNext = this.FindControl<Button>("btnNext");
        }

        #endregion

        #region View Interface

        ITextBox IQuickSearchDlg.SearchPattern
        {
            get { return GetControlHandler<ITextBox>(txtSearchPattern); }
        }

        #endregion

        public QuickSearchDlg() : this(null)
        {
        }

        public QuickSearchDlg(IWorkWindow workWindow)
        {
            InitializeComponent();

            //KeyDown += SearchPanel_KeyDown;
            //btnPrev.Image = UIHelper.LoadResourceImage("Resources.btn_left.gif");
            //btnNext.Image = UIHelper.LoadResourceImage("Resources.btn_right.gif");
            // TextChanged="SearchPattern_TextChanged" - FIXME: Avalonia not supported

            fController = new QuickSearchDlgController(this, workWindow);

            SetLang();
        }

        private void SearchPattern_TextChanged(object sender, RoutedEventArgs e)
        {
            fController.ChangeText();
        }

        private void FindNext_Click(object sender, RoutedEventArgs e)
        {
            fController.FindNext();
        }

        private void FindPrev_Click(object sender, RoutedEventArgs e)
        {
            fController.FindPrev();
        }

        private void SearchPanel_KeyDown(object sender, KeyEventArgs e)
        {
            switch (e.Key) {
                case Key.Enter:
                    e.Handled = true;
                    /*if (e.Shift)
                        fController.FindPrev();
                    else
                        fController.FindNext();*/
                    break;

                case Key.Escape:
                    e.Handled = true;
                    Close();
                    break;
            }
        }

        public void SetLang()
        {
            Title = LangMan.LS(LSID.LSID_Search);
            //txtSearchPattern.Text = LangMan.LS(LSID.LSID_NoMatchesFound);
            SetToolTip(btnPrev, LangMan.LS(LSID.LSID_FindPrevious));
            SetToolTip(btnNext, LangMan.LS(LSID.LSID_FindNext));
        }
    }
}
