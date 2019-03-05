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

using System.IO;

namespace GKCommon.GEDCOM
{
    public sealed class GEDCOMResearchRecord : GEDCOMRecord
    {
        private GEDCOMList<GEDCOMPointer> fTasks;
        private GEDCOMList<GEDCOMPointer> fCommunications;
        private GEDCOMList<GEDCOMPointer> fGroups;

        public string ResearchName
        {
            get { return GetTagStringValue("NAME"); }
            set { SetTagStringValue("NAME", value); }
        }

        public GKResearchPriority Priority
        {
            get { return GEDCOMUtils.GetPriorityVal(GetTagStringValue("_PRIORITY")); }
            set { SetTagStringValue("_PRIORITY", GEDCOMUtils.GetPriorityStr(value)); }
        }

        public GKResearchStatus Status
        {
            get { return GEDCOMUtils.GetStatusVal(GetTagStringValue("_STATUS")); }
            set { SetTagStringValue("_STATUS", GEDCOMUtils.GetStatusStr(value)); }
        }

        public GEDCOMDate StartDate
        {
            get { return TagClass("_STARTDATE", GEDCOMDate.Create) as GEDCOMDate; }
        }

        public GEDCOMDate StopDate
        {
            get { return TagClass("_STOPDATE", GEDCOMDate.Create) as GEDCOMDate; }
        }

        public int Percent
        {
            get { return GetTagIntegerValue("_PERCENT", 0); }
            set { SetTagIntegerValue("_PERCENT", value); }
        }

        public GEDCOMList<GEDCOMPointer> Tasks
        {
            get { return fTasks; }
        }

        public GEDCOMList<GEDCOMPointer> Communications
        {
            get { return fCommunications; }
        }

        public GEDCOMList<GEDCOMPointer> Groups
        {
            get { return fGroups; }
        }


        public GEDCOMResearchRecord(GEDCOMTree owner, GEDCOMObject parent) : base(owner, parent)
        {
            SetRecordType(GEDCOMRecordType.rtResearch);
            SetName(GEDCOMTagType._RESEARCH);

            fTasks = new GEDCOMList<GEDCOMPointer>(this);
            fCommunications = new GEDCOMList<GEDCOMPointer>(this);
            fGroups = new GEDCOMList<GEDCOMPointer>(this);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fTasks.Dispose();
                fCommunications.Dispose();
                fGroups.Dispose();
            }
            base.Dispose(disposing);
        }

        public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
        {
            GEDCOMTag result;

            if (tagName == "NAME")
            {
                result = base.AddTag(tagName, tagValue, null);
            }
            else if (tagName == "_STARTDATE" || tagName == "_STOPDATE")
            {
                result = base.AddTag(tagName, tagValue, GEDCOMDate.Create);
            }
            else if (tagName == GEDCOMTagType._TASK)
            {
                result = fTasks.Add(new GEDCOMPointer(Owner, this, tagName, tagValue));
            }
            else if (tagName == GEDCOMTagType._COMM)
            {
                result = fCommunications.Add(new GEDCOMPointer(Owner, this, tagName, tagValue));
            }
            else if (tagName == GEDCOMTagType._GROUP)
            {
                result = fGroups.Add(new GEDCOMPointer(Owner, this, tagName, tagValue));
            }
            else
            {
                result = base.AddTag(tagName, tagValue, tagConstructor);
            }

            return result;
        }

        public override void Clear()
        {
            base.Clear();

            fTasks.Clear();
            fCommunications.Clear();
            fGroups.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && fTasks.Count == 0 && fCommunications.Count == 0 && fGroups.Count == 0;
        }

        public override void ReplaceXRefs(XRefReplacer map)
        {
            base.ReplaceXRefs(map);

            fTasks.ReplaceXRefs(map);
            fCommunications.ReplaceXRefs(map);
            fGroups.ReplaceXRefs(map);
        }

        public override void ResetOwner(GEDCOMTree newOwner)
        {
            base.ResetOwner(newOwner);

            fTasks.ResetOwner(newOwner);
            fCommunications.ResetOwner(newOwner);
            fGroups.ResetOwner(newOwner);
        }

        public override void SaveToStream(StreamWriter stream)
        {
            base.SaveToStream(stream);

            fTasks.SaveToStream(stream);
            fCommunications.SaveToStream(stream);
            fGroups.SaveToStream(stream);
        }

        #region Auxiliary

        public bool AddTask(GEDCOMTaskRecord taskRecord)
        {
            bool result = false;

            if (taskRecord != null) {
                GEDCOMPointer ptr = new GEDCOMPointer(Owner, this);
                ptr.SetNamedValue(GEDCOMTagType._TASK, taskRecord);
                fTasks.Add(ptr);
                result = true;
            }

            return result;
        }

        public void RemoveTask(GEDCOMTaskRecord taskRecord)
        {
            if (taskRecord == null) return;

            fTasks.DeleteAt(IndexOfTask(taskRecord));
        }

        public int IndexOfTask(GEDCOMTaskRecord taskRec)
        {
            int result = -1;

            if (taskRec != null) {
                int num = fTasks.Count;
                for (int i = 0; i < num; i++) {
                    if (fTasks[i].XRef == taskRec.XRef) {
                        result = i;
                        break;
                    }
                }
            }

            return result;
        }

        public bool AddGroup(GEDCOMGroupRecord groupRecord)
        {
            bool result = false;

            if (groupRecord != null) {
                GEDCOMPointer ptr = new GEDCOMPointer(Owner, this);
                ptr.SetNamedValue(GEDCOMTagType._GROUP, groupRecord);
                fGroups.Add(ptr);
                result = true;
            }

            return result;
        }

        public void RemoveGroup(GEDCOMGroupRecord groupRecord)
        {
            if (groupRecord == null) return;

            fGroups.DeleteAt(IndexOfGroup(groupRecord));
        }

        public int IndexOfGroup(GEDCOMGroupRecord groupRec)
        {
            int result = -1;

            if (groupRec != null) {
                int num = fGroups.Count;
                for (int i = 0; i < num; i++) {
                    if (fGroups[i].XRef == groupRec.XRef) {
                        result = i;
                        break;
                    }
                }
            }

            return result;
        }

        public bool AddCommunication(GEDCOMCommunicationRecord commRecord)
        {
            bool result = false;

            if (commRecord != null) {
                GEDCOMPointer ptr = new GEDCOMPointer(Owner, this);
                ptr.SetNamedValue(GEDCOMTagType._COMM, commRecord);
                fCommunications.Add(ptr);
                result = true;
            }

            return result;
        }

        public void RemoveCommunication(GEDCOMCommunicationRecord commRecord)
        {
            if (commRecord == null) return;

            fCommunications.DeleteAt(IndexOfCommunication(commRecord));
        }

        public int IndexOfCommunication(GEDCOMCommunicationRecord commRec)
        {
            int result = -1;

            if (commRec != null) {
                int num = fCommunications.Count;
                for (int i = 0; i < num; i++) {
                    if (fCommunications[i].XRef == commRec.XRef) {
                        result = i;
                        break;
                    }
                }
            }

            return result;
        }

        #endregion
    }
}
