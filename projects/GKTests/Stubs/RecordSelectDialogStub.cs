/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
