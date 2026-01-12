/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
        T IView.GetCoreControl<T>(string controlName) { return default(T); }
        public void SetToolTip(object component, string toolTip) { }
        public void SetTitle(string value) { }
    }
}
