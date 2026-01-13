/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Locales;
using GKCore.Search;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class QuickSearchDlgController : FormController<IQuickSearchDlg>
    {
        private readonly IWorkWindow fWorkWindow;
        private SearchStrategy fStrategy;

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
            fStrategy = new WorkSearchStrategy(fWorkWindow, fView.SearchPattern.Text);
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
            fView.SetTitle(LangMan.LS(LSID.Search));
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
