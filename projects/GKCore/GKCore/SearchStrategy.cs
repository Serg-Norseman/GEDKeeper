/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using System.Collections.Generic;
using System.Linq;
using GKCommon.GEDCOM;
using GKCore.Interfaces;

namespace GKCore
{
    public class SearchResult : ISearchResult
    {
        public readonly GDMObject Result;

        public SearchResult(GDMObject result)
        {
            Result = result;
        }
    }

    public class SearchStrategy : ISearchStrategy
    {
        private readonly string fSearchPattern;
        private readonly IWorkWindow fWorkWindow;

        private IList<ISearchResult> fCurrentResults;
        private ISearchResult fCurResult;

        public SearchStrategy(IWorkWindow workWindow, string searchPattern)
        {
            if (searchPattern == null)
                throw new ArgumentNullException("searchPattern");

            fSearchPattern = searchPattern;
            fWorkWindow = workWindow;
            fCurrentResults = FindAll();
        }

        public IList<ISearchResult> FindAll()
        {
            return fWorkWindow.FindAll(fSearchPattern);
        }

        public bool HasResults()
        {
            return (fCurrentResults != null && fCurrentResults.Count > 0);
        }

        public ISearchResult FindNext()
        {
            if (fCurResult == null) {
                if (fCurrentResults == null) fCurrentResults = FindAll();

                fCurResult = fCurrentResults.FirstOrDefault();
            } else {
                int idx = fCurrentResults.IndexOf(fCurResult) + 1;

                fCurResult = (idx < fCurrentResults.Count) ? fCurrentResults[idx] : null;
            }

            return fCurResult;
        }

        public ISearchResult FindPrev()
        {
            if (fCurResult == null) {
                if (fCurrentResults == null) fCurrentResults = FindAll();

                fCurResult = fCurrentResults.LastOrDefault();
            } else {
                int idx = fCurrentResults.IndexOf(fCurResult) - 1;

                fCurResult = (idx >= 0) ? fCurrentResults[idx] : null;
            }

            return fCurResult;
        }
    }
}
