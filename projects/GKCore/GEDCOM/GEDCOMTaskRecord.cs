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
    public sealed class GEDCOMTaskRecord : GEDCOMRecord
    {
        public string Goal
        {
            get { return GetTagStringValue("_GOAL"); }
            set { SetTagStringValue("_GOAL", value); }
        }

        public GKResearchPriority Priority
        {
            get { return GEDCOMUtils.GetPriorityVal(GetTagStringValue("_PRIORITY")); }
            set { SetTagStringValue("_PRIORITY", GEDCOMUtils.GetPriorityStr(value)); }
        }

        public GEDCOMDate StartDate
        {
            get { return TagClass("_STARTDATE", GEDCOMDate.Create) as GEDCOMDate; }
        }

        public GEDCOMDate StopDate
        {
            get { return TagClass("_STOPDATE", GEDCOMDate.Create) as GEDCOMDate; }
        }


        public GEDCOMTaskRecord(GEDCOMTree owner, GEDCOMObject parent) : base(owner, parent)
        {
            SetRecordType(GEDCOMRecordType.rtTask);
            SetName(GEDCOMTagType._TASK);
        }

        public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
        {
            GEDCOMTag result;

            if (tagName == "_STARTDATE" || tagName == "_STOPDATE") {
                result = base.AddTag(tagName, tagValue, GEDCOMDate.Create);
            } else {
                result = base.AddTag(tagName, tagValue, tagConstructor);
            }

            return result;
        }

        #region Auxiliary

        public sealed class TaskGoalRet
        {
            public readonly GKGoalType GoalType;
            public readonly GEDCOMRecord GoalRec;

            public TaskGoalRet(GKGoalType goalType, GEDCOMRecord goalRec)
            {
                GoalType = goalType;
                GoalRec = goalRec;
            }
        }

        public TaskGoalRet GetTaskGoal()
        {
            GEDCOMTree tree = Owner;
            GEDCOMRecord goalRec = tree.XRefIndex_Find(GEDCOMUtils.CleanXRef(Goal));

            GKGoalType goalType;
            if (goalRec is GEDCOMIndividualRecord) {
                goalType = GKGoalType.gtIndividual;
            } else if (goalRec is GEDCOMFamilyRecord) {
                goalType = GKGoalType.gtFamily;
            } else if (goalRec is GEDCOMSourceRecord) {
                goalType = GKGoalType.gtSource;
            } else {
                goalType = GKGoalType.gtOther;
            }

            return new TaskGoalRet(goalType, goalRec);
        }

        #endregion
    }
}
