/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GKCore;
using GKCore.Design;
using GKCore.Locales;
using GKCore.Search;
using Xamarin.Forms;

namespace GKUI.Forms
{
    public sealed partial class QuickSearchDlg : ContentView
    {
        private readonly IWorkWindow fWorkWindow;
        private SearchStrategy fStrategy;


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
            fStrategy = new WorkSearchStrategy(fWorkWindow, txtSearchPattern.Text);
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
