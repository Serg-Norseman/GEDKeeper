﻿/*
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

using System;
using BSLib.Design.MVP;
using GDModel;
using GKCore.Interfaces;
using GKCore.MVP.Views;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class QuickSearchDlgController : Controller<IQuickSearchDlg>
    {
        private readonly IWorkWindow fWorkWindow;
        private ISearchStrategy fStrategy;

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
            if (result == null || result.Result == null) return;

            fWorkWindow.SelectByRec(result.Result as GDMIndividualRecord);
        }

        public void FindNext()
        {
            if (fStrategy == null) return;

            if (!fStrategy.HasResults()) {
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.LSID_NoMatchesFound));
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
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.LSID_NoMatchesFound));
                return;
            }

            ISearchResult result = fStrategy.FindPrev();
            if (result != null) {
                SelectResult(result as SearchResult);
            }
        }
    }
}
