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

using System;
using System.Collections.Generic;
using System.Drawing;
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
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
        public bool AllowQuickFind() { return false; }
        public IList<ISearchResult> FindAll(string searchPattern) { return new List<ISearchResult>(); }
        public void QuickFind() {}
        public void SelectByRec(GEDCOMIndividualRecord iRec) {}
        public bool AllowFilter() { return false; }
        public void SetFilter() {}
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

        private IBaseContext fContext;
        private GEDCOMTree fTree;

        public BaseWindowMock()
        {
            this.fContext = TestStubs.CreateContext(/*this*/);
            TestStubs.FillContext(this.fContext as BaseContext);
            this.fTree = fContext.Tree;
        }

        public void ProgressInit(string title, int max) { }
        public void ProgressDone() { }
        public void ProgressStep() { }
        public void ProgressStep(int value) { }

        public void SetLang() {}


        public IHost Host { get { return fHost; } }
        public IBaseContext Context { get { return this.fContext; } }

        public bool Modified { get { return false; } set {} }
        public ShieldState ShieldState { get { return ShieldState.None; } set {} }
        public GEDCOMTree Tree { get { return this.fTree; } }
        public ValuesCollection ValuesCollection { get { return null; } }

        public void Activate() { }
        public void ApplyFilter() { }
        public void ApplyFilter(GEDCOMRecordType recType) { }
        public void ChangeRecord(GEDCOMRecord record) { }
        public void Close() { }

        public string DefinePatronymic(string name, GEDCOMSex sex, bool confirm) { return null; }
        public GEDCOMSex DefineSex(string iName, string iPatr) { return GEDCOMSex.svNone; }
        public void CheckPersonSex(GEDCOMIndividualRecord iRec) { }
        public void CollectEventValues(GEDCOMCustomEvent evt) { }

        public bool IsUnknown() { return false; }
        public void FileNew() { }
        public void FileLoad(string fileName) { }
        public void FileSave(string fileName) { }
        public void CriticalSave() { }
        public void CollectTips(StringList tipsList) { }

        public GEDCOMIndividualRecord AddChildForParent(GEDCOMIndividualRecord parent, GEDCOMSex needSex) { return null; }
        public GEDCOMFamilyRecord AddFamilyForSpouse(GEDCOMIndividualRecord spouse) { return null; }
        public GEDCOMFamilyRecord GetChildFamily(GEDCOMIndividualRecord iChild, bool canCreate, GEDCOMIndividualRecord newParent) { return null; }
        public List<GEDCOMRecord> GetContentList(GEDCOMRecordType recType) { return null; }
        public StringList GetRecordContent(GEDCOMRecord record) { return null; }
        public string GetRecordName(GEDCOMRecord record, bool signed) { return string.Empty; }
        public IListManager GetRecordsListManByType(GEDCOMRecordType recType) { return null; }
        public GEDCOMIndividualRecord GetSelectedPerson() { return null; }
        public GEDCOMRecordType GetSelectedRecordType() { return GEDCOMRecordType.rtIndividual; }
        public void RefreshLists(bool titles) { }
        public void ShowRecordsTab(GEDCOMRecordType recType) { }

        public GEDCOMIndividualRecord CreatePersonDialog(GEDCOMIndividualRecord target, TargetMode targetMode, GEDCOMSex needSex) { return null; }

        public void LockRecord(GEDCOMRecord record) { }
        public void UnlockRecord(GEDCOMRecord record) { }
        public bool IsAvailableRecord(GEDCOMRecord record) { return true; }

        public bool ModifyMedia(ref GEDCOMMultimediaRecord mediaRec) { mediaRec = null; return false; }
        public bool ModifyNote(ref GEDCOMNoteRecord noteRec) { noteRec = null; return false; }
        public bool ModifySource(ref GEDCOMSourceRecord sourceRec) { sourceRec = null; return false; }
        public bool ModifyRepository(ref GEDCOMRepositoryRecord repRec) { repRec = null; return false; }
        public bool ModifyGroup(ref GEDCOMGroupRecord groupRec) { groupRec = null; return false; }
        public bool ModifyResearch(ref GEDCOMResearchRecord researchRec) { researchRec = null; return false; }
        public bool ModifyTask(ref GEDCOMTaskRecord taskRec) { taskRec = null; return false; }
        public bool ModifyCommunication(ref GEDCOMCommunicationRecord commRec) { commRec = null; return false; }
        public bool ModifyLocation(ref GEDCOMLocationRecord locRec) { locRec = null; return false; }
        public bool ModifyPerson(ref GEDCOMIndividualRecord indivRec,
                                 GEDCOMIndividualRecord target, TargetMode targetMode, GEDCOMSex needSex) { indivRec = null; return false; }
        public bool ModifyFamily(ref GEDCOMFamilyRecord familyRec, FamilyTarget target, GEDCOMIndividualRecord person) { familyRec = null; return false; }
        public bool ModifyAddress(GEDCOMAddress address) { return false; }
        public bool ModifySourceCitation(IGEDCOMStructWithLists _struct, ref GEDCOMSourceCitation cit) { cit = null; return false; }

        public void RecordAdd() { }
        public void RecordDelete() { }
        public bool RecordDelete(GEDCOMRecord record, bool confirm) { return false; }
        public void RecordEdit(object sender, EventArgs e) { }
        public bool RecordIsFiltered(GEDCOMRecord record) { return false; }

        public GEDCOMIndividualRecord SelectSpouseFor(GEDCOMIndividualRecord iRec) { return null; }
        public GEDCOMFamilyRecord SelectFamily(GEDCOMIndividualRecord target) { return null; }
        public GEDCOMIndividualRecord SelectPerson(GEDCOMIndividualRecord target, TargetMode targetMode, GEDCOMSex needSex) { return null; }
        public GEDCOMRecord SelectRecord(GEDCOMRecordType mode, params object[] args) { return null; }
        public void SelectRecordByXRef(string xref) { }
        public void Show() { }
        public void ShowMedia(GEDCOMMultimediaRecord mediaRec, bool modal) { }
    }

    public class HostMock : IHost
    {
        public INamesTable NamesTable { get { return null; } }

        public IBaseWindow GetCurrentFile(bool extMode = false) { return null; }
        public IWorkWindow GetWorkWindow() { return null; }

        public IBaseWindow CreateBase(string fileName) { return null; }
        public IBaseWindow FindBase(string fileName) { return null; }
        public void BaseChanged(IBaseWindow baseWin) {}
        public void BaseClosed(IBaseWindow baseWin) {}
        public void BaseRenamed(IBaseWindow baseWin, string oldName, string newName) {}
        public void NotifyRecord(IBaseWindow baseWin, object record, RecordAction action) {}

        public string GetAppDataPath() { return string.Empty; }

        public void LogWrite(string msg) {}

        public bool IsWidgetActive(IWidget widget) { return false; }
        public void WidgetShow(IWidget widget) {}
        public void WidgetClose(IWidget widget) {}

        public void ShowMDI(Form form) {}

        public ILangMan CreateLangMan(object sender) { return null; }
        public void LoadLanguage(int langCode) {}
        public void UpdateNavControls() {}
        public void UpdateControls(bool forceDeactivate) {}
        public void ShowHelpTopic(string topic) {}
        public void EnableWindow(Form form, bool value) {}

        public bool IsUnix() { return false; }
        public void ShowWarning(string msg) {}

        public void SetLang() {}
    }

    internal class ValItem
    {
        public double Value;

        public ValItem(double value)
        {
            this.Value = value;
        }
    }

    public class MockWriter : CustomWriter
    {
        public MockWriter() { }
        public override void beginWrite() { }
        public override void endWrite() { }
        public override void addParagraph(string text, object font, TextAlignment alignment) { }
        public override void addParagraph(string text, object font) { }
        public override void addParagraphAnchor(string text, object font, string anchor) { }
        public override void addParagraphLink(string text, object font, string link, object linkFont) { }
        public override object CreateFont(string name, float size, bool bold, bool underline, Color color) { return null; }
        public override void beginList() { }
        public override void endList() { }
        public override void addListItem(string text, object font) { }
        public override void addListItemLink(string text, object font, string link, object linkFont) { }
        public override void beginParagraph(TextAlignment alignment, float spacingBefore, float spacingAfter) { }
        public override void endParagraph() { }
        public override void addParagraphChunk(string text, object font) { }
        public override void addParagraphChunkAnchor(string text, object font, string anchor) { }
        public override void addParagraphChunkLink(string text, object font, string link, object linkFont, bool sup) { }
    }
}
