/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GDModel;
using GKCore.Locales;
using GKCore.Options;

namespace GKCore.Lists
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class TaskListModel : RecordsListModel<GDMTaskRecord>
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


        public TaskListModel(BaseContext baseContext) :
            base(baseContext, CreateListColumns(), GDMRecordType.rtTask)
        {
        }

        public static ListColumns CreateListColumns()
        {
            var result = new ListColumns(GKListType.rtTask);
            result.AddColumn(LSID.NumberSym, DataType.dtInteger, 50, true);
            result.AddColumn(LSID.Goal, DataType.dtString, 300, true);
            result.AddColumn(LSID.Priority, DataType.dtString, 90, true);
            result.AddColumn(LSID.StartDate, DataType.dtString, 90, true);
            result.AddColumn(LSID.StopDate, DataType.dtString, 90, true);
            result.AddColumn(LSID.Changed, DataType.dtDateTime, 150, true);
            return result;
        }

        public override void Fetch(GDMTaskRecord aRec)
        {
            base.Fetch(aRec);
            fQuickFilterBuffer = GKUtils.GetTaskGoalStr(fBaseContext.Tree, fFetchedRec);
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch ((ColumnType)colType) {
                case ColumnType.ctXRefNum:
                    result = fFetchedRec.GetId();
                    break;

                case ColumnType.ctGoal:
                    result = fQuickFilterBuffer;
                    break;

                case ColumnType.ctPriority:
                    result = LangMan.LS(GKData.PriorityNames[(int)fFetchedRec.Priority]);
                    break;

                case ColumnType.ctStartDate:
                    result = GetDateValue(fFetchedRec.StartDate, isVisible);
                    break;

                case ColumnType.ctStopDate:
                    result = GetDateValue(fFetchedRec.StopDate, isVisible);
                    break;

                case ColumnType.ctChangeDate:
                    result = fFetchedRec.ChangeDate.ChangeDateTime;
                    break;
            }
            return result;
        }
    }
}
