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

using GKCommon.GEDCOM;
using GKCore.Interfaces;

namespace GKCore.Lists
{
    /// <summary>
    /// 
    /// </summary>
    public enum TaskColumnType
    {
        ctGoal,
        ctPriority,
        ctStartDate,
        ctStopDate,
        ctChangeDate
    }

    /// <summary>
    /// 
    /// </summary>
    public sealed class TaskListColumns : ListColumns
    {
        protected override void InitColumnStatics()
        {
            AddColumn(LSID.LSID_Goal, DataType.dtString, 300, true);
            AddColumn(LSID.LSID_Priority, DataType.dtString, 90, true);
            AddColumn(LSID.LSID_StartDate, DataType.dtString, 90, true);
            AddColumn(LSID.LSID_StopDate, DataType.dtString, 90, true);
            AddColumn(LSID.LSID_Changed, DataType.dtDateTime, 150, true);
        }
    }

    /// <summary>
    /// 
    /// </summary>
    public sealed class TaskListMan : ListManager
    {
        private GEDCOMTaskRecord fRec;

        public TaskListMan(IBaseContext baseContext) : base(baseContext, new TaskListColumns())
        {
        }

        public override bool CheckFilter()
        {
            bool res = (QuickFilter == "*" || IsMatchesMask(GKUtils.GetTaskGoalStr(fRec), QuickFilter));

            res = res && CheckCommonFilter();

            return res;
        }

        public override void Fetch(GEDCOMRecord aRec)
        {
            fRec = (aRec as GEDCOMTaskRecord);
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch ((TaskColumnType)colType)
            {
                case TaskColumnType.ctGoal:
                    result = GKUtils.GetTaskGoalStr(fRec);
                    break;

                case TaskColumnType.ctPriority:
                    result = LangMan.LS(GKData.PriorityNames[(int)fRec.Priority]);
                    break;

                case TaskColumnType.ctStartDate:
                    result = GetDateValue(fRec.StartDate, isVisible);
                    break;

                case TaskColumnType.ctStopDate:
                    result = GetDateValue(fRec.StopDate, isVisible);
                    break;

                case TaskColumnType.ctChangeDate:
                    result = fRec.ChangeDate.ChangeDateTime;
                    break;
            }
            return result;
        }
    }
}
