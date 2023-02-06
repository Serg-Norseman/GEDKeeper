/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using GDModel;
using GKCore.Design.MVP;

namespace GKCore.Interfaces
{
    public interface IWindow : IView, ILocalizable
    {
        void Show(bool showInTaskbar);
    }

    public interface IWorkWindow : IWindow
    {
        void UpdateControls();

        /// <summary>
        /// Is called after changing the settings of the user interface.
        /// </summary>
        void UpdateSettings();

        bool NavCanBackward();
        bool NavCanForward();
        void NavNext();
        void NavPrev();

        bool AllowQuickSearch();
        IList<ISearchResult> FindAll(string searchPattern);
        void QuickSearch();
        void SelectByRec(GDMRecord record);

        bool AllowFilter();
        void SetFilter();
    }
}
