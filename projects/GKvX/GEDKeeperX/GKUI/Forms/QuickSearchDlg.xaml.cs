/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using GKCore;
using GKCore.Interfaces;
using GKCore.Search;
using Xamarin.Forms;

namespace GKUI.Forms
{
    public sealed partial class QuickSearchDlg : ContentView
    {
        private readonly IWorkWindow fWorkWindow;
        private ISearchStrategy fStrategy;


        public QuickSearchDlg(IWorkWindow workWindow)
        {
            InitializeComponent();
            fWorkWindow = workWindow;
        }

        private void SearchPattern_TextChanged(object sender, EventArgs e)
        {
            ChangeText();
        }

        private void FindNext_Click(object sender, EventArgs e)
        {
            FindNext();
        }

        private void FindPrev_Click(object sender, EventArgs e)
        {
            FindPrev();
        }

        #region Controller

        private void ChangeText()
        {
            fStrategy = new SearchStrategy(fWorkWindow, txtSearchPattern.Text);
        }

        private void SelectResult(SearchResult result)
        {
            if (result != null && result.Record != null) {
                fWorkWindow.SelectByRec(result.Record);
            }
        }

        private void FindNext()
        {
            if (fStrategy == null) return;

            if (!fStrategy.HasResults()) {
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.NoMatchesFound));
                return;
            }

            ISearchResult result = fStrategy.FindNext();
            if (result != null) {
                SelectResult(result as SearchResult);
            }
        }

        private void FindPrev()
        {
            if (fStrategy == null) return;

            if (!fStrategy.HasResults()) {
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.NoMatchesFound));
                return;
            }

            ISearchResult result = fStrategy.FindPrev();
            if (result != null) {
                SelectResult(result as SearchResult);
            }
        }

        #endregion
    }
}
