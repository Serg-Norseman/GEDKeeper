﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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
using BSLib.Design.Graphics;
using GDModel;
using GKCore;
using GKCore.Interfaces;
using GKCore.Plugins;
using GKCore.Types;
using NSubstitute;

namespace GKTests.Stubs
{
    internal class WorkWindowStub : BaseObject, IWorkWindow
    {
        public string Title { get; set; }
        public bool Enabled { get; set; }

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
        public void SetLang() {}
        public void Show(bool showInTaskbar) {}
    }

    internal class ProgressStub : IProgressController
    {
        public void ProgressInit(string title, int max, bool cancelable = false) {}
        public void ProgressDone() {}
        public void ProgressStep() {}
        public void ProgressStep(int value) {}
        public bool IsCanceled { get { return false; } }
    }

    internal class BaseWindowStub : WorkWindowStub, IBaseWindow
    {
        private static IHost fHost = Substitute.For<IHost>();

        private readonly IBaseContext fContext;
        private readonly GDMTree fTree;

        public BaseWindowStub(bool fill = true)
        {
            fContext = TestUtils.CreateContext(this);
            if (fill) {
                TestUtils.FillContext(fContext);
            }
            fTree = fContext.Tree;
        }

        public BaseWindowStub(IBaseContext context)
        {
            fContext = context;
            fTree = fContext.Tree;
        }

        public IBaseContext Context { get { return fContext; } }
        public IHost Host { get { return fHost; } }
        public bool Modified { get { return false; } set {} }
        public GDMTree Tree { get { return fTree; } }
        public ValuesCollection ValuesCollection { get { return null; } }

        public void AddRecord() { }
        public void ApplyFilter(GDMRecordType recType = GDMRecordType.rtNone) { }
        public void ChangeRecord(GDMRecord record) { }
        public void CheckAutosave() { }
        public void CreateNewFile() { }
        public void CriticalSave() { }
        public void DeleteRecord() { }
        public void DuplicateRecord() { }
        public void EditRecord() { }
        public bool IsUnknown() { return false; }
        public void LoadFile(string fileName) { }
        public void NotifyRecord(GDMRecord record, RecordAction action) { }
        public bool RecordIsFiltered(GDMRecord record) { return false; }
        public void SaveFile(string fileName) { }
        public void SaveFileEx(bool saveAs) { }
        public void SelectRecordByXRef(string xref, bool delayedTransition = false) { }
        public void Show() { }
        public void ShowMedia(GDMMultimediaRecord mediaRec, bool modal) { }

        public List<GDMRecord> GetContentList(GDMRecordType recType) { return null; }
        public StringList GetRecordContent(GDMRecord record) { return null; }
        public string GetRecordName(GDMRecord record, bool signed) { return string.Empty; }
        public IListManager GetRecordsListManByType(GDMRecordType recType) { return null; }
        public GDMIndividualRecord GetSelectedPerson() { return null; }
        public GDMRecordType GetSelectedRecordType() { return GDMRecordType.rtIndividual; }
        public void RefreshLists(bool columnsChanged) { }
        public void RefreshRecordsView(GDMRecordType recType) { }
        public void ShowRecordsTab(GDMRecordType recType) { }
        public void UpdateControls(bool forceDeactivate, bool blockDependent = false) { }
        public void SetExternalFilter(ExternalFilterHandler filterHandler, 
                                      GDMRecordType recType = GDMRecordType.rtNone) { }
        public GDMRecord GetSelectedRecordEx() { return null; }
    }

    public class TestPlugin : OrdinaryPlugin
    {
        private ILangMan fLangMan;

        public override string DisplayName { get { return "TestPlugin"; } }
        public override ILangMan LangMan { get { return fLangMan; } }
        public override IImage Icon { get { return null; } }
        public override PluginCategory Category { get { return PluginCategory.Common; } }

        public TestPlugin()
        {
            fLangMan = null;
        }

        public TestPlugin(ILangMan langMan)
        {
            fLangMan = (langMan != null) ? langMan : Substitute.For<ILangMan>();
        }

        public override void Execute()
        {
        }
    }
}
