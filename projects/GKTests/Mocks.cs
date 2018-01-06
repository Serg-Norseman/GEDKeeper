/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using GKCommon;
using GKCommon.GEDCOM;
using GKCore.Export;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKTests.Mocks
{
    internal class WorkWindowMock : IWorkWindow
    {
        public string GetStatusString() { return ""; }
        public void UpdateView() {}
        public bool NavCanBackward() { return false; }
        public bool NavCanForward() { return false; }
        public void NavNext() {}
        public void NavPrev() {}
        public bool AllowQuickSearch() { return false; }
        public IList<ISearchResult> FindAll(string searchPattern) { return new List<ISearchResult>(); }
        public void QuickSearch() {}
        public void SelectByRec(GEDCOMIndividualRecord iRec) {}
        public bool AllowFilter() { return false; }
        public void SetFilter() {}
        public void SetLang() {}
    }

    internal class ProgressMock : IProgressController
    {
        public void ProgressInit(string title, int max) {}
        public void ProgressDone() {}
        public void ProgressStep() {}
        public void ProgressStep(int value) {}
    }

    internal class BaseWindowMock : WorkWindowMock, IBaseWindow
    {
        private static IHost fHost = new HostMock();

        private readonly IBaseContext fContext;
        private readonly GEDCOMTree fTree;

        public BaseWindowMock()
        {
            fContext = TestStubs.CreateContext(/*this*/);
            TestStubs.FillContext(fContext);
            fTree = fContext.Tree;
        }

        public new void SetLang() {}

        public IHost Host { get { return fHost; } }
        public IBaseContext Context { get { return fContext; } }

        public bool Modified { get { return false; } set {} }
        public ShieldState ShieldState { get { return ShieldState.None; } set {} }
        public GEDCOMTree Tree { get { return fTree; } }
        public ValuesCollection ValuesCollection { get { return null; } }

        public void Activate() { }
        public void ApplyFilter(GEDCOMRecordType recType = GEDCOMRecordType.rtNone) { }
        public void ChangeRecord(GEDCOMRecord record) { }
        public void Close() { }

        public bool IsUnknown() { return false; }
        public void CreateNewFile() { }
        public void LoadFile(string fileName) { }
        public void SaveFile(string fileName) { }
        public void CriticalSave() { }
        public void SaveFileEx(bool saveAs) { }
        public void CheckAutosave() { }

        public List<GEDCOMRecord> GetContentList(GEDCOMRecordType recType) { return null; }
        public StringList GetRecordContent(GEDCOMRecord record) { return null; }
        public string GetRecordName(GEDCOMRecord record, bool signed) { return string.Empty; }
        public IListManager GetRecordsListManByType(GEDCOMRecordType recType) { return null; }
        public GEDCOMIndividualRecord GetSelectedPerson() { return null; }
        public GEDCOMRecordType GetSelectedRecordType() { return GEDCOMRecordType.rtIndividual; }
        public void RefreshLists(bool columnsChanged) { }
        public void RefreshRecordsView(GEDCOMRecordType recType) { }
        public void ShowRecordsTab(GEDCOMRecordType recType) { }
        public void UpdateControls(bool forceDeactivate) { }

        public void AddRecord() { }
        public void DeleteRecord() { }
        public void EditRecord() { }
        public void DuplicateRecord() { }
        public bool RecordIsFiltered(GEDCOMRecord record) { return false; }
        public void NotifyRecord(GEDCOMRecord record, RecordAction action) { }

        public void SelectRecordByXRef(string xref) { }
        public void Show() { }
        public void ShowMedia(GEDCOMMultimediaRecord mediaRec, bool modal) { }
    }

    public class HostMock : IHost
    {
        public INamesTable NamesTable { get { return null; } }

        public IBaseWindow GetCurrentFile(bool extMode = false) { return null; }
        public IWorkWindow GetWorkWindow() { return null; }

        public string GetUserFilesPath(string filePath) { return string.Empty; }
        public IBaseWindow CreateBase(string fileName) { return null; }
        public void LoadBase(IBaseWindow baseWin, string fileName) { }
        public IBaseWindow FindBase(string fileName) { return null; }
        public void BaseChanged(IBaseWindow baseWin) {}
        public void BaseClosed(IBaseWindow baseWin) {}
        public void BaseRenamed(IBaseWindow baseWin, string oldName, string newName) {}
        public void NotifyRecord(IBaseWindow baseWin, object record, RecordAction action) {}

        public void ApplyOptions() { }
        public string GetAppDataPath() { return string.Empty; }

        public bool IsWidgetActive(IWidget widget) { return false; }
        public void WidgetShow(IWidget widget) {}
        public void WidgetClose(IWidget widget) {}

        public void ShowWindow(IWindow window) {}

        public ILangMan CreateLangMan(object sender) { return null; }
        public void LoadLanguage(int langCode) {}
        public void UpdateNavControls() {}
        public void UpdateControls(bool forceDeactivate) {}
        public void ShowHelpTopic(string topic) {}
        public void EnableWindow(IWidgetForm form, bool value) {}
        public void Restore() {}

        public bool ShowModalX(ICommonDialog form, bool keepModeless = false) { return false; }

        public void SetLang() {}
    }

    public class MockWriter : CustomWriter
    {
        public MockWriter() { }
        public override void beginWrite() { }
        public override void endWrite() { }
        public override void addParagraph(string text, IFont font, TextAlignment alignment) { }
        public override void addParagraph(string text, IFont font) { }
        public override void addParagraphAnchor(string text, IFont font, string anchor) { }
        public override void addParagraphLink(string text, IFont font, string link, IFont linkFont) { }
        public override IFont CreateFont(string name, float size, bool bold, bool underline, IColor color) { return null; }
        public override void beginList() { }
        public override void endList() { }
        public override void addListItem(string text, IFont font) { }
        public override void addListItemLink(string text, IFont font, string link, IFont linkFont) { }
        public override void beginParagraph(TextAlignment alignment, float spacingBefore, float spacingAfter) { }
        public override void endParagraph() { }
        public override void addParagraphChunk(string text, IFont font) { }
        public override void addParagraphChunkAnchor(string text, IFont font, string anchor) { }
        public override void addParagraphChunkLink(string text, IFont font, string link, IFont linkFont, bool sup) { }
        public override void addNote(string text, IFont font) { }
    }
}
