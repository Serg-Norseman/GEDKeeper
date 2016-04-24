/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using GKCommon.GEDCOM;
using GKCore.Interfaces;

namespace GKCore
{
    public class BaseSearchStrategy : ISearchStrategy
    {
        private readonly String searchPattern;
        private readonly IWorkWindow workWindow;

        private IList<ISearchResult> currentResults;
        private ISearchResult curResult;
        
        public BaseSearchStrategy(IWorkWindow workWindow, String searchPattern)
        {
            if (searchPattern == null)
                throw new ArgumentNullException("searchPattern");

            this.searchPattern = searchPattern;
            this.workWindow = workWindow;
            this.currentResults = this.FindAll();
        }
        
        public IList<ISearchResult> FindAll()
        {
            return this.workWindow.FindAll(this.searchPattern);
            
            /*foreach (Match result in searchPattern.Matches(document.Text)) {
				int resultEndOffset = result.Length + result.Index;
				if (offset > result.Index || endOffset < resultEndOffset)
					continue;
				if (matchWholeWords && (!IsWordBorder(document, result.Index) || !IsWordBorder(document, resultEndOffset)))
					continue;
				yield return new SearchResult { StartOffset = result.Index, Length = result.Length, Data = result };
			}*/
        }
        
        public bool HasResults()
        {
            return (currentResults != null && currentResults.Count > 0);
        }
        
        public ISearchResult FindNext()
        {
            if (curResult == null) {
                if (currentResults == null) currentResults = this.FindAll();
                
                curResult = GKUtils.FirstOrDefault(currentResults);
            } else
            {
                int idx = currentResults.IndexOf(curResult) + 1;

                curResult = (idx < currentResults.Count) ? currentResults[idx] : null;
            }

            return curResult;
        }
        
        public ISearchResult FindPrev()
        {
            if (curResult == null) {
                if (currentResults == null) currentResults = this.FindAll();
                
                curResult = GKUtils.LastOrDefault(currentResults);
            } else
            {
                int idx = currentResults.IndexOf(curResult) - 1;

                curResult = (idx >= 0) ? currentResults[idx] : null;
            }

            return curResult;
        }
    }

    public class SearchResult : ISearchResult
    {
        public GEDCOMObject Result { get; private set; }
        
        public SearchResult(GEDCOMObject result)
        {
            this.Result = result;
        }
    }
}
