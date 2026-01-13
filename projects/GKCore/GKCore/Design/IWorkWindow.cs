/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using GDModel;
using GKCore.Locales;
using GKCore.Search;

namespace GKCore.Design
{
    public interface IWindow : IForm, ILocalizable
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
