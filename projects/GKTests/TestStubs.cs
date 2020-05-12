﻿/*
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

using System;
using System.Collections.Generic;
using BSLib;
using BSLib.Design.Graphics;
using BSLib.Design.MVP.Controls;
using GDModel;
using GKCore;
using GKCore.Export;
using GKCore.Interfaces;
using GKCore.Maps;
using GKCore.MVP.Controls;
using GKCore.Plugins;
using GKCore.Types;
using GKUI.Providers;

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
        public void ProgressInit(string title, int max) {}
        public void ProgressDone() {}
        public void ProgressStep() {}
        public void ProgressStep(int value) {}
    }

    internal class BaseWindowStub : WorkWindowStub, IBaseWindow
    {
        private static IHost fHost = new HostStub();

        private readonly IBaseContext fContext;
        private readonly GDMTree fTree;

        public BaseWindowStub(bool fill = true)
        {
            fContext = TestUtils.CreateContext(/*this*/);
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

    public class HostStub : IHost
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
        public void SelectedIndexChanged(IBaseWindow baseWin) {}
        public void TabChanged(IBaseWindow baseWin) {}

        public void ApplyOptions() { }
        public string GetAppDataPath() { return string.Empty; }

        public bool IsWidgetActive(IWidget widget) { return false; }
        public void WidgetShow(IWidget widget) {}
        public void WidgetClose(IWidget widget) {}

        public void ShowWindow(IWindow window) {}

        public ILangMan CreateLangMan(object sender) { return null; }
        public void LoadLanguage(int langCode) {}
        public void UpdateNavControls() {}
        public void UpdateControls(bool forceDeactivate, bool blockDependent = false) {}
        public void ShowHelpTopic(string topic) {}
        public void EnableWindow(IWidgetForm form, bool value) {}
        public void Restore() {}

        public bool ShowModalX(ICommonDialog form, bool keepModeless = false) { return false; }

        public void SetLang() {}
    }

    public class WriterStub : CustomWriter
    {
        public WriterStub() { }
        public override void BeginWrite() { }
        public override void EndWrite() { }
        public override void EnablePageNumbers() { }
        public override void NewPage() { }
        public override void NewLine(float spacingBefore = 0.0f, float spacingAfter = 0.0f) { }
        public override void AddParagraph(string text, IFont font, GKCore.Export.TextAlignment alignment) { }
        public override void AddParagraph(string text, IFont font) { }
        public override void AddParagraphAnchor(string text, IFont font, string anchor) { }
        public override void AddParagraphLink(string text, IFont font, string link) { }
        public override void AddParagraphLink(string text, IFont font, string link, IFont linkFont) { }
        public override IFont CreateFont(string name, float size, bool bold, bool underline, IColor color) { return null; }
        public override void BeginList() { }
        public override void EndList() { }
        public override void AddListItem(string text, IFont font) { }
        public override void AddListItemLink(string text, IFont font, string link, IFont linkFont) { }
        public override void BeginParagraph(GKCore.Export.TextAlignment alignment,
                                            float spacingBefore, float spacingAfter,
                                            float indent = 0.0f, bool keepTogether = false) { }
        public override void EndParagraph() { }
        public override void AddParagraphChunk(string text, IFont font) { }
        public override void AddParagraphChunkAnchor(string text, IFont font, string anchor) { }
        public override void AddParagraphChunkLink(string text, IFont font, string link, bool sup = false) { }
        public override void AddNote(string text, IFont font) { }
        public override void BeginMulticolumns(int columnCount, float columnSpacing) { }
        public override void EndMulticolumns() { }
        public override void AddImage(IImage image) { }
    }

    public class MapBrowserStub : IMapBrowser
    {
        public bool ShowPoints { get; set; }
        public bool ShowLines { get; set; }
        public ExtList<GeoPoint> MapPoints { get { return null; } }
        public bool Enabled { get { return true; } set { } }

        public int AddPoint(double latitude, double longitude, string hint) { return -1; }
        public void ClearPoints() { }
        public void DeletePoint(int index) { }
        public void BeginUpdate() { }
        public void EndUpdate() { }
        public void Activate() {}
        public void InitMap() { }
        public void RefreshPoints() { }
        public void SaveSnapshot(string fileName) { }
        public void SetCenter(double latitude, double longitude, int scale) { }
        public void ZoomToBounds() { }
    }

    public class TestLangMan : ILangMan
    {
        public string LS(Enum lsid)
        {
            return "test";
        }

        public bool LoadFromFile(string fileName, int offset = 0)
        {
            return true;
        }
    }

    public class TestPlugin : OrdinaryPlugin, IPlugin
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
            fLangMan = (langMan != null) ? langMan : new TestLangMan();
        }

        public override void Execute()
        {
        }
    }

    public sealed class TextBoxStub : BaseControlHandler<System.Windows.Forms.TextBox, TextBoxStub>, ITextBox
    {
        private StringList fStrings;

        public TextBoxStub(System.Windows.Forms.TextBox control) : base(control)
        {
            fStrings = new StringList();
        }

        public string[] Lines
        {
            get { return fStrings.ToArray(); }
            set {
                fStrings.Clear();
                fStrings.AddStrings(value);
            }
        }

        public bool ReadOnly
        {
            get { return false; }
            set {  }
        }

        public string SelectedText
        {
            get { return string.Empty; }
            set {  }
        }

        public string Text
        {
            get { return string.Empty; }
            set {  }
        }

        public void AppendText(string text)
        {
            fStrings.Add(text);
        }

        public void Clear()
        {
            fStrings.Clear();
        }

        public void Copy()
        {
        }

        public void SelectAll()
        {
        }
    }
}
