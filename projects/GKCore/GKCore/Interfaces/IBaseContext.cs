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

using System;
using System.Collections.Generic;
using System.IO;

using GKCommon;
using GKCommon.GEDCOM;
using GKCore.Types;

namespace GKCore.Interfaces
{
    public interface IBaseContext : IDisposable
    {
        ICulture Culture { get; }
        string FileName { get; }
        GEDCOMTree Tree { get; }
        ValuesCollection ValuesCollection { get; }
        ShieldState ShieldState { get; set; }
        bool Modified { get; set; }
        IBaseWindow Viewer { get; }

        bool IsUnknown();
        void Clear();
        bool FileLoad(string fileName);
        bool FileSave(string fileName);
        void SetFileName(string fileName);
        void CriticalSave();

        // Data manipulation
        GEDCOMCustomEvent CreateEventEx(GEDCOMRecordWithEvents aRec, string evSign, string evDate, string evPlace);
        GEDCOMCustomEvent CreateEventEx(GEDCOMRecordWithEvents aRec, string evSign, GEDCOMCustomDate evDate, string evPlace);
        GEDCOMIndividualRecord CreatePersonEx(string iName, string iPatronymic, string iSurname, GEDCOMSex iSex, bool birthEvent);
        bool DeleteRecord(GEDCOMRecord record);
        bool IsRecordAccess(GEDCOMRestriction restriction);

        // Individual utils
        bool IsChildless(GEDCOMIndividualRecord iRec);
        int FindBirthYear(GEDCOMIndividualRecord iRec);
        int FindDeathYear(GEDCOMIndividualRecord iRec);
        void CollectEventValues(GEDCOMCustomEvent evt);
        void CollectTips(StringList tipsList);
        IList<ISearchResult> FindAll(GEDCOMRecordType recordType, string searchPattern);

        // Multimedia support
        bool CheckBasePath();
        MediaStore GetStoreType(GEDCOMFileReference fileReference);
        Stream MediaLoad(GEDCOMFileReference fileReference, bool throwException);
        string MediaLoad(GEDCOMFileReference fileReference);
        bool MediaSave(GEDCOMFileReference fileReference, string fileName, MediaStoreType storeType);

        // Used only in MediaViewer and Slideshow
        IImage LoadMediaImage(GEDCOMFileReference fileReference, bool throwException);
        IImage LoadMediaImage(GEDCOMFileReference fileReference, int thumbWidth, int thumbHeight, ExtRect cutoutArea, bool throwException);
        // Used in FamilyBookExporter, TreeChart and PersonEdit
        IImage GetPrimaryBitmap(GEDCOMIndividualRecord iRec, int thumbWidth, int thumbHeight, bool throwException);
        string GetPrimaryBitmapUID(GEDCOMIndividualRecord iRec);

        bool IsUpdated();
        void BeginUpdate();
        void EndUpdate();
        void SwitchShieldState();

        void DoUndo();
        void DoRedo();
        void DoCommit();
        void DoRollback();

        void LockRecord(GEDCOMRecord record);
        void UnlockRecord(GEDCOMRecord record);
        bool IsAvailableRecord(GEDCOMRecord record);

        GEDCOMSourceRecord FindSource(string sourceName);
        void GetSourcesList(StringList sources);

        string DefinePatronymic(string name, GEDCOMSex sex, bool confirm);
        GEDCOMSex DefineSex(string iName, string iPatr);
        void CheckPersonSex(GEDCOMIndividualRecord iRec);

        GEDCOMFamilyRecord SelectFamily(GEDCOMIndividualRecord target);
        GEDCOMIndividualRecord SelectPerson(GEDCOMIndividualRecord target,
                                            TargetMode targetMode, GEDCOMSex needSex);
        GEDCOMRecord SelectRecord(GEDCOMRecordType mode, params object[] args);
        GEDCOMFamilyRecord GetChildFamily(GEDCOMIndividualRecord iChild,
                                                 bool canCreate,
                                                 GEDCOMIndividualRecord newParent);
        GEDCOMFamilyRecord AddFamilyForSpouse(GEDCOMIndividualRecord spouse);
        GEDCOMIndividualRecord AddChildForParent(GEDCOMIndividualRecord parent, GEDCOMSex needSex);
        GEDCOMIndividualRecord SelectSpouseFor(GEDCOMIndividualRecord iRec);
    }
}
