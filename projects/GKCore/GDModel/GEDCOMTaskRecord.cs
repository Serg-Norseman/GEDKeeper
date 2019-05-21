/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2019 by Sergey V. Zhdanovskih.
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

namespace GKCommon.GEDCOM
{
    /// <summary>
    /// This type of Genealogical Data Model (GDM) defines the goal of the task.
    /// </summary>
    public enum GDMGoalType
    {
        gtIndividual,
        gtFamily,
        gtSource,
        gtOther
    }


    public sealed class GEDCOMTaskRecord : GEDCOMRecord
    {
        public string Goal
        {
            get { return GetTagStringValue(GEDCOMTagType._GOAL); }
            set { SetTagStringValue(GEDCOMTagType._GOAL, value); }
        }

        public GDMResearchPriority Priority
        {
            get { return GEDCOMUtils.GetPriorityVal(GetTagStringValue(GEDCOMTagType._PRIORITY)); }
            set { SetTagStringValue(GEDCOMTagType._PRIORITY, GEDCOMUtils.GetPriorityStr(value)); }
        }

        public GEDCOMDate StartDate
        {
            get { return GetTag(GEDCOMTagType._STARTDATE, GEDCOMDate.Create) as GEDCOMDate; }
        }

        public GEDCOMDate StopDate
        {
            get { return GetTag(GEDCOMTagType._STOPDATE, GEDCOMDate.Create) as GEDCOMDate; }
        }


        public GEDCOMTaskRecord(GEDCOMObject owner) : base(owner)
        {
            SetRecordType(GEDCOMRecordType.rtTask);
            SetName(GEDCOMTagType._TASK);
        }

        #region Auxiliary

        public sealed class TaskGoalRet
        {
            public readonly GDMGoalType GoalType;
            public readonly GEDCOMRecord GoalRec;

            public TaskGoalRet(GDMGoalType goalType, GEDCOMRecord goalRec)
            {
                GoalType = goalType;
                GoalRec = goalRec;
            }
        }

        public TaskGoalRet GetTaskGoal()
        {
            GEDCOMTree tree = GetTree();
            GEDCOMRecord goalRec = tree.XRefIndex_Find(GEDCOMUtils.CleanXRef(Goal));

            GDMGoalType goalType;
            if (goalRec is GEDCOMIndividualRecord) {
                goalType = GDMGoalType.gtIndividual;
            } else if (goalRec is GEDCOMFamilyRecord) {
                goalType = GDMGoalType.gtFamily;
            } else if (goalRec is GEDCOMSourceRecord) {
                goalType = GDMGoalType.gtSource;
            } else {
                goalType = GDMGoalType.gtOther;
            }

            return new TaskGoalRet(goalType, goalRec);
        }

        #endregion
    }
}
