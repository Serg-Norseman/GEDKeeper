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

using GDModel.Providers.GEDCOM;

namespace GDModel
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


    public sealed class GDMTaskRecord : GDMRecord
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

        public GDMDate StartDate
        {
            get { return GetTag<GDMDate>(GEDCOMTagType._STARTDATE, GDMDate.Create); }
        }

        public GDMDate StopDate
        {
            get { return GetTag<GDMDate>(GEDCOMTagType._STOPDATE, GDMDate.Create); }
        }


        public GDMTaskRecord(GDMObject owner) : base(owner)
        {
            SetRecordType(GDMRecordType.rtTask);
            SetName(GEDCOMTagType._TASK);
        }

        public sealed class TaskGoalRet
        {
            public readonly GDMGoalType GoalType;
            public readonly string GoalXRef;
            public readonly GDMRecord GoalRec;

            public TaskGoalRet(GDMGoalType goalType, string goalXRef, GDMRecord goalRec)
            {
                GoalType = goalType;
                GoalXRef = goalXRef;
                GoalRec = goalRec;
            }
        }

        public TaskGoalRet GetTaskGoal()
        {
            GDMTree tree = GetTree();
            string goalXRef = string.Empty;
            GDMRecord goalRec = tree.XRefIndex_Find(GEDCOMUtils.CleanXRef(Goal));

            GDMGoalType goalType;
            if (goalRec is GDMIndividualRecord) {
                goalType = GDMGoalType.gtIndividual;
            } else if (goalRec is GDMFamilyRecord) {
                goalType = GDMGoalType.gtFamily;
            } else if (goalRec is GDMSourceRecord) {
                goalType = GDMGoalType.gtSource;
            } else {
                goalType = GDMGoalType.gtOther;
            }

            return new TaskGoalRet(goalType, goalXRef, goalRec);
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);

            TaskGoalRet goalRet = GetTaskGoal();
            if (goalRet.GoalType != GDMGoalType.gtOther) {
                Goal = GEDCOMUtils.EncloseXRef(map.FindNewXRef(GEDCOMUtils.CleanXRef(Goal)));
            }
        }
    }
}
