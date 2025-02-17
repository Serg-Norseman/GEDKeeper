/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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

using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Search;
using GKCore.Types;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class QuickSearchDlgController : FormController<IQuickSearchDlg>
    {
        private readonly IWorkWindow fWorkWindow;
        private ISearchStrategy fStrategy;

        public IWorkWindow WorkWindow
        {
            get { return fWorkWindow; }
        }


        public QuickSearchDlgController(IQuickSearchDlg view, IWorkWindow workWindow) : base(view)
        {
            fWorkWindow = workWindow;
        }

        public override void UpdateView()
        {
        }

        public void ChangeText()
        {
            fStrategy = new SearchStrategy(fWorkWindow, fView.SearchPattern.Text);
        }

        private void SelectResult(SearchResult result)
        {
            if (result != null && result.Record != null) {
                fWorkWindow.SelectByRec(result.Record);
            }
        }

        public void FindNext()
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

        public void FindPrev()
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

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.Search);
            //txtSearchPattern.Text = LangMan.LS(LSID.NoMatchesFound);
            SetToolTip("btnPrev", LangMan.LS(LSID.FindPrevious));
            SetToolTip("btnNext", LangMan.LS(LSID.FindNext));
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnPrev").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Prev, true);
            GetControl<IButton>("btnNext").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Next, true);
        }
    }
}
