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

using GDModel;
using GKCore.Interfaces;

namespace GKCore.Lists
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class TaskListMan : ListManager<GDMTaskRecord>
    {
        public enum ColumnType
        {
            ctXRefNum,
            ctGoal,
            ctPriority,
            ctStartDate,
            ctStopDate,
            ctChangeDate
        }


        private GDMTaskRecord fRec;


        public TaskListMan(IBaseContext baseContext) :
            base(baseContext, CreateTaskListColumns(), GDMRecordType.rtTask)
        {
        }

        public static ListColumns<GDMTaskRecord> CreateTaskListColumns()
        {
            var result = new ListColumns<GDMTaskRecord>();

            result.AddColumn(LSID.LSID_NumberSym, DataType.dtInteger, 50, true);
            result.AddColumn(LSID.LSID_Goal, DataType.dtString, 300, true, true);
            result.AddColumn(LSID.LSID_Priority, DataType.dtString, 90, true);
            result.AddColumn(LSID.LSID_StartDate, DataType.dtString, 90, true);
            result.AddColumn(LSID.LSID_StopDate, DataType.dtString, 90, true);
            result.AddColumn(LSID.LSID_Changed, DataType.dtDateTime, 150, true);

            result.ResetDefaults();
            return result;
        }

        public override bool CheckFilter()
        {
            bool res = IsMatchesMask(GKUtils.GetTaskGoalStr(fBaseContext.Tree, fRec), QuickFilter);

            res = res && CheckCommonFilter() && CheckExternalFilter(fRec);

            return res;
        }

        public override void Fetch(GDMRecord aRec)
        {
            fRec = (GDMTaskRecord)aRec;
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch ((ColumnType)colType) {
                case ColumnType.ctXRefNum:
                    result = fRec.GetId();
                    break;

                case ColumnType.ctGoal:
                    result = GKUtils.GetTaskGoalStr(fBaseContext.Tree, fRec);
                    break;

                case ColumnType.ctPriority:
                    result = LangMan.LS(GKData.PriorityNames[(int)fRec.Priority]);
                    break;

                case ColumnType.ctStartDate:
                    result = GetDateValue(fRec.StartDate, isVisible);
                    break;

                case ColumnType.ctStopDate:
                    result = GetDateValue(fRec.StopDate, isVisible);
                    break;

                case ColumnType.ctChangeDate:
                    result = fRec.ChangeDate.ChangeDateTime;
                    break;
            }
            return result;
        }
    }
}
