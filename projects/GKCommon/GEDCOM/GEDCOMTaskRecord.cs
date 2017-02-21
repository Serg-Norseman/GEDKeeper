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

        public GEDCOMDateExact StartDate
        {
            get { return TagClass("_STARTDATE", GEDCOMDateExact.Create) as GEDCOMDateExact; }
        }

        public GEDCOMDateExact StopDate
        {
            get { return TagClass("_STOPDATE", GEDCOMDateExact.Create) as GEDCOMDateExact; }
        }

        protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
        {
            base.CreateObj(owner, parent);
            SetRecordType(GEDCOMRecordType.rtTask);
            SetName("_TASK");
        }

        public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
        {
            GEDCOMTag result;

            if (tagName == "_STARTDATE" || tagName == "_STOPDATE")
            {
                result = base.AddTag(tagName, tagValue, GEDCOMDateExact.Create);
            }
            else
            {
                result = base.AddTag(tagName, tagValue, tagConstructor);
            }

            return result;
        }

        public GEDCOMTaskRecord(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
        }

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
        {
            return new GEDCOMTaskRecord(owner, parent, tagName, tagValue);
        }

        #region Auxiliary

        public void GetTaskGoal(out GKGoalType goalType, out GEDCOMRecord goalRec)
        {
            GEDCOMTree tree = Owner;
            goalRec = tree.XRefIndex_Find(GEDCOMUtils.CleanXRef(Goal));

            if (goalRec is GEDCOMIndividualRecord)
            {
                goalType = GKGoalType.gtIndividual;
            }
            else if (goalRec is GEDCOMFamilyRecord)
            {
                goalType = GKGoalType.gtFamily;
            }
            else if (goalRec is GEDCOMSourceRecord)
            {
                goalType = GKGoalType.gtSource;
            }
            else
            {
                goalType = GKGoalType.gtOther;
            }
        }

        #endregion
    }
}
