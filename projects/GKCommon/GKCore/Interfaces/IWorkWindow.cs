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

using System.Collections.Generic;
using GKCommon.GEDCOM;

namespace GKCore.Interfaces
{
    public interface IWorkWindow
    {
        string GetStatusString();

        /// <summary>
        /// Is called after changing the settings of the user interface.
        /// </summary>
        void UpdateView();

        bool NavCanBackward();
        bool NavCanForward();
        void NavNext();
        void NavPrev();

        bool AllowQuickSearch();
        IList<ISearchResult> FindAll(string searchPattern);
        void QuickSearch();
        void SelectByRec(GEDCOMIndividualRecord iRec);

        bool AllowFilter();
        void SetFilter();
    }
}
