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
using GKCommon.GEDCOM;
using GKCore.Types;

namespace GKCore.Interfaces
{
    public interface IBaseWindow : IWorkWindow
    {
        IBaseContext Context { get; }

        void AddRecord();
        void CheckAutosave();
        void CreateNewFile();
        void DeleteRecord();
        void DuplicateRecord();
        void EditRecord();
        void LoadFile(string fileName);
        void NotifyRecord(GEDCOMRecord record, RecordAction action);
        bool RecordIsFiltered(GEDCOMRecord record);
        void RefreshLists(bool columnsChanged);
        void RefreshRecordsView(GEDCOMRecordType recType);
        void SaveFile(string fileName);
        void SaveFileEx(bool saveAs);
        void SelectRecordByXRef(string xref);
        void ShowMedia(GEDCOMMultimediaRecord mediaRec, bool modal);
        void ShowRecordsTab(GEDCOMRecordType recType);
        void UpdateControls(bool forceDeactivate, bool blockDependent = false);

        void ApplyFilter(GEDCOMRecordType recType = GEDCOMRecordType.rtNone);
        List<GEDCOMRecord> GetContentList(GEDCOMRecordType recType);
        StringList GetRecordContent(GEDCOMRecord record);
        IListManager GetRecordsListManByType(GEDCOMRecordType recType);
        GEDCOMIndividualRecord GetSelectedPerson();
        GEDCOMRecordType GetSelectedRecordType();
        GEDCOMRecord GetSelectedRecordEx();
        void SetExternalFilter(ExternalFilterHandler filterHandler, GEDCOMRecordType recType = GEDCOMRecordType.rtNone);
    }
}
