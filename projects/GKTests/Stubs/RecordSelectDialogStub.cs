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

using GDModel;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;

namespace GKTests.Stubs
{
    internal class RecordSelectDialogStub : IRecordSelectDialog
    {
        private static GDMRecord TestResult;

        public static void SetTestResult(GDMRecord value)
        {
            TestResult = value;
        }

        public GDMRecord ResultRecord { get { return TestResult; } set { } }

        public IComboBox FilterCombo { get { return null; } }

        public ITextBox FilterText { get { return null; } }

        public IFilterControl FilterCtl { get { return null; } }

        public IListView RecordsList { get { return null; } }

        public bool Enabled { get { return false; } set { } }
        public bool Visible { get { return false; } set { } }

        public void Activate() { }
        public void Close() { }
        public void Dispose() { }
        public object GetControl(string controlName) { return null; }
        public void SetTarget(TargetMode mode, GDMIndividualRecord target, GDMSex needSex, string defFilter = "*") { }
        public void SetTitle(string value) { }
        public void SetToolTip(object component, string toolTip) { }
        T IView.GetCoreControl<T>(string controlName) { return default(T); }

        public RecordSelectDialogStub(IBaseWindow baseWin, GDMRecordType recType)
        {
        }
    }
}
