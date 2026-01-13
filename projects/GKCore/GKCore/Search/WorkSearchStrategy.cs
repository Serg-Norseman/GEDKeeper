/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using GKCore.Design;

namespace GKCore.Search
{
    public class WorkSearchStrategy : SearchStrategy
    {
        private readonly string fSearchPattern;
        private readonly IWorkWindow fWorkWindow;

        public WorkSearchStrategy(IWorkWindow workWindow, string searchPattern)
        {
            if (searchPattern == null)
                throw new ArgumentNullException(nameof(searchPattern));

            fSearchPattern = searchPattern;
            fWorkWindow = workWindow;
            fCurrentResults = FindAll();
        }

        public override IList<ISearchResult> FindAll()
        {
            return fWorkWindow.FindAll(fSearchPattern);
        }
    }
}
