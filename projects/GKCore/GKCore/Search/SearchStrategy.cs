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

using System.Collections.Generic;
using System.Linq;

namespace GKCore.Search
{
    public interface ISearchResult
    {
    }


    public abstract class SearchStrategy
    {
        protected IList<ISearchResult> fCurrentResults;
        private ISearchResult fCurResult;


        public ISearchResult CurResult
        {
            get { return fCurResult; }
        }


        public abstract IList<ISearchResult> FindAll();

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

        public bool HasResults()
        {
            return (fCurrentResults != null && fCurrentResults.Count > 0);
        }
    }
}
