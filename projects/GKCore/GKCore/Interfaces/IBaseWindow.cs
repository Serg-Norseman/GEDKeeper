/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using GKCore.Types;

namespace GKCore.Interfaces
{
    public enum RecordContentType { Full, Quick }


    public interface IBaseWindow : IWorkWindow
    {
        IBaseContext Context { get; }

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
        void ShowMedia(GDMMultimediaRecord mediaRec, bool modal);
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
