/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
