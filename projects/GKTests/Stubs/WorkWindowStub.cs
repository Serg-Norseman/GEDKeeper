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
using BSLib;
using GDModel;
using GKCore.Design;
using GKCore.Search;

namespace GKTests.Stubs
{
    internal class WorkWindowStub : BaseObject, IWorkWindow
    {
        public string Title { get; set; }
        public bool Enabled { get; set; }
        public bool Visible { get; set; }

        public void Activate() {}
        public void Close() {}

        public void UpdateControls() {}
        public void UpdateSettings() {}
        public bool NavCanBackward() { return false; }
        public bool NavCanForward() { return false; }
        public void NavNext() {}
        public void NavPrev() {}
        public bool AllowQuickSearch() { return false; }
        public IList<ISearchResult> FindAll(string searchPattern) { return new List<ISearchResult>(); }
        public void QuickSearch() {}
        public void SelectByRec(GDMRecord record) {}
        public bool AllowFilter() { return false; }
        public void SetFilter() {}
        public void SetLocale() {}
        public void Show(bool showInTaskbar) {}
        public object GetControl(string controlName) { return null; }
        public void SetToolTip(object component, string toolTip) { }
    }
}
