/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using GDModel;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKNavigatorPlugin
{
    public enum DataCategory
    {
        Root,
        RecentActivity,
        JumpHistory,
        PotencialProblems,
        Filters,
        Bookmarks,
        Records,
        Languages,
    }


    public sealed class RecordInfo
    {
        public readonly GDMRecordType Type;
        public readonly RecordAction Action;
        public readonly string XRef;
        public readonly string Name;
        public readonly GDMRecord Record; // null for deleted records
        public readonly DateTime Time;

        public RecordInfo(RecordAction action, string xref, string name, GDMRecordType type, GDMRecord record)
        {
            Action = action;
            XRef = xref;
            Name = name;
            Type = type;
            Record = record;
            Time = DateTime.Now;
        }
    }


    public sealed class BaseData
    {
        private readonly List<RecordInfo> fChangedRecords;

        public List<RecordInfo> ChangedRecords
        {
            get { return fChangedRecords; }
        }

        public BaseData()
        {
            fChangedRecords = new List<RecordInfo>();
        }

        public void NotifyRecord(IBaseWindow baseWin, object record, RecordAction action)
        {
            GDMRecord gRecord = record as GDMRecord;
            if (gRecord == null) return;

            try {
                string recName = GKUtils.GetRecordName(baseWin.Context.Tree, gRecord, false);

                int index = Find(gRecord);
                if (index >= 0) {
                    fChangedRecords.RemoveAt(index);
                }

                switch (action) {
                    case RecordAction.raAdd:
                    case RecordAction.raEdit:
                        fChangedRecords.Add(new RecordInfo(action, gRecord.XRef, recName, gRecord.RecordType, gRecord));
                        break;

                    case RecordAction.raDelete:
                        fChangedRecords.Add(new RecordInfo(action, gRecord.XRef, recName, gRecord.RecordType, null));
                        break;
                }
            } catch (Exception ex) {
            }
        }

        public int Find(GDMRecord record)
        {
            for (int i = 0; i < fChangedRecords.Count; i++) {
                RecordInfo recInfo = fChangedRecords[i];
                if (recInfo.Record == record) {
                    return i;
                }
            }
            return -1;
        }
    }


    public sealed class NavigatorData
    {
        private readonly Dictionary<string, BaseData> fBases;

        public BaseData this[string name]
        {
            get {
                BaseData result;
                if (!fBases.TryGetValue(name, out result)) {
                    result = new BaseData();
                    fBases.Add(name, result);
                }
                return result;
            }
        }

        public NavigatorData()
        {
            fBases = new Dictionary<string, BaseData>();
        }

        public void CloseBase(string name)
        {
            BaseData b_data;
            if (!fBases.TryGetValue(name, out b_data)) {
                return; // base not exists
            }

            fBases.Remove(name);
        }

        public void RenameBase(string oldName, string newName)
        {
            BaseData b_data;
            if (!fBases.TryGetValue(oldName, out b_data)) {
                return; // base not exists
            }

            fBases.Remove(oldName);
            fBases.Add(newName, b_data);
        }
    }
}
