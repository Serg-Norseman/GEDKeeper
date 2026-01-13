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
using GKCore.Filters;
using GKCore.Lists;

namespace GKCore.Design
{
    public enum RecordContentType { Full, Quick }


    public interface IBaseWindow : IWorkWindow
    {
        BaseContext Context { get; }

        NavigationStack<GDMRecord> Navman { get; }

        void AddRecord();
        void CheckAutosave();
        bool CheckModified();
        void CreateNewFile();
        void DeleteRecord();
        void DuplicateRecord();
        void EditRecord();
        void LoadFile(string fileName);
        void NotifyRecord(GDMRecord record, RecordAction action);
        bool RecordIsFiltered(GDMRecord record);
        void RefreshLists(bool columnsChanged);
        void RefreshRecordsView(GDMRecordType recType);
        void SaveFile(string fileName);
        void SaveFileEx(bool saveAs);
        void SelectRecordByXRef(string xref, bool delayedTransition = false);
        void ShowMedia(string link, bool modal);
        void ShowMedia(GDMMultimediaRecord mediaRec, int fileNum, bool modal);
        void ShowRecordsTab(GDMRecordType recType);
        void UpdateChangedRecords(GDMRecord select = null);
        void UpdateControls(bool forceDeactivate, bool blockDependent = false);
        void UpdateMRU();

        void ApplyFilter(GDMRecordType recType = GDMRecordType.rtNone);
        List<GDMRecord> GetContentList(GDMRecordType recType);
        StringList GetRecordContent(GDMRecord record, RecordContentType contentType);
        IRecordsListModel GetRecordsListManByType(GDMRecordType recType);
        GDMIndividualRecord GetSelectedPerson();
        GDMRecordType GetSelectedRecordType();
        GDMRecord GetSelectedRecordEx();
        void SetExternalFilter(ExternalFilterHandler filterHandler, GDMRecordType recType = GDMRecordType.rtNone);
    }


    /// <summary>
    /// Interface for all windows dependent on IWindow (IBaseWindow or IChartWindow).
    /// </summary>
    public interface IWindowDependent
    {
        //IBaseWindow Base { get; }
        IWindow OwnerWindow { get; }
    }
}
