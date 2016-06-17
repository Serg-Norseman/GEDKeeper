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

using GKCommon;
using GKCommon.GEDCOM;
using GKCore.Types;

namespace GKCore.Interfaces
{
    public interface IBaseWindow : IWorkWindow, IProgressController, ILocalization
    {
        IHost Host { get; }
        IBaseContext Context { get; }
        
        bool Modified { get; set; }
        ShieldState ShieldState { get; set; }
        GEDCOMTree Tree { get; }
        ValuesCollection ValuesCollection { get; }

        void Activate();
        void ApplyFilter();
        void ApplyFilter(GEDCOMRecordType recType);
        void ChangeRecord(GEDCOMRecord record);
        void Close();

        string DefinePatronymic(string name, GEDCOMSex sex, bool confirm);
        GEDCOMSex DefineSex(string iName, string iPatr);
        void CheckPersonSex(GEDCOMIndividualRecord iRec);
        void CollectEventValues(GEDCOMCustomEvent evt);

        void FileNew();
        void FileLoad(string fileName);
        void FileSave(string fileName);

        GEDCOMFamilyRecord GetChildFamily(GEDCOMIndividualRecord iChild, bool canCreate, GEDCOMIndividualRecord newParent);
        List<GEDCOMRecord> GetContentList(GEDCOMRecordType recType);
        StringList GetRecordContent(GEDCOMRecord record);
        IListManager GetRecordsListManByType(GEDCOMRecordType recType);
        GEDCOMIndividualRecord GetSelectedPerson();
        GEDCOMRecordType GetSelectedRecordType();
        void RefreshLists(bool titles);
        //void RefreshRecordsView(GEDCOMRecordType recType);
        
        GEDCOMIndividualRecord CreatePersonDialog(GEDCOMIndividualRecord target, TargetMode targetMode, GEDCOMSex needSex);
        
        bool ModifyMedia(ref GEDCOMMultimediaRecord mediaRec);
        bool ModifyNote(ref GEDCOMNoteRecord noteRec);
        bool ModifySource(ref GEDCOMSourceRecord sourceRec);
        bool ModifyRepository(ref GEDCOMRepositoryRecord repRec);
        bool ModifyGroup(ref GEDCOMGroupRecord groupRec);
        bool ModifyResearch(ref GEDCOMResearchRecord researchRec);
        bool ModifyTask(ref GEDCOMTaskRecord taskRec);
        bool ModifyCommunication(ref GEDCOMCommunicationRecord commRec);
        bool ModifyLocation(ref GEDCOMLocationRecord locRec);
        bool ModifyPerson(ref GEDCOMIndividualRecord indivRec);
        bool ModifyFamily(ref GEDCOMFamilyRecord familyRec, FamilyTarget target, GEDCOMIndividualRecord person);
        bool ModifyAddress(GEDCOMAddress address);

        void RecordAdd();
        void RecordDelete();
        bool RecordDelete(GEDCOMRecord record, bool confirm);
        void RecordEdit(object sender, EventArgs e);
        bool RecordIsFiltered(GEDCOMRecord record);

        GEDCOMFamilyRecord SelectFamily(GEDCOMIndividualRecord target);
        GEDCOMIndividualRecord SelectPerson(GEDCOMIndividualRecord target, TargetMode targetMode, GEDCOMSex needSex);
        GEDCOMRecord SelectRecord(GEDCOMRecordType mode, params object[] args);
        void SelectRecordByXRef(string xref);
        void Show();
        void ShowMedia(GEDCOMMultimediaRecord mediaRec, bool modal);
    }
}
