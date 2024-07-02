/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using GDModel.Providers.GEDCOM;

namespace GDModel
{
    /// <summary>
    /// This type of Genealogical Data Model (GDM) defines the priority of the research.
    /// </summary>
    public enum GDMResearchPriority
    {
        rpNone,
        rpLow,
        rpNormal,
        rpHigh,
        rpTop
    }


    /// <summary>
    /// This type of Genealogical Data Model (GDM) defines the status of the research.
    /// </summary>
    public enum GDMResearchStatus
    {
        rsDefined,
        rsInProgress,
        rsOnHold,
        rsProblems,
        rsCompleted,
        rsWithdrawn
    }


    public sealed class GDMResearchRecord : GDMRecord
    {
        private string fResearchName;
        private GDMResearchPriority fPriority;
        private GDMResearchStatus fStatus;
        private readonly GDMDate fStartDate;
        private readonly GDMDate fStopDate;
        private int fPercent;
        private readonly GDMList<GDMPointer> fTasks;
        private readonly GDMList<GDMPointer> fCommunications;
        private readonly GDMList<GDMPointer> fGroups;


        public string ResearchName
        {
            get { return fResearchName; }
            set { fResearchName = value; }
        }

        public GDMResearchPriority Priority
        {
            get { return fPriority; }
            set { fPriority = value; }
        }

        public GDMResearchStatus Status
        {
            get { return fStatus; }
            set { fStatus = value; }
        }

        public GDMDate StartDate
        {
            get { return fStartDate; }
        }

        public GDMDate StopDate
        {
            get { return fStopDate; }
        }

        public int Percent
        {
            get { return fPercent; }
            set { fPercent = value; }
        }

        public GDMList<GDMPointer> Tasks
        {
            get { return fTasks; }
        }

        public GDMList<GDMPointer> Communications
        {
            get { return fCommunications; }
        }

        public GDMList<GDMPointer> Groups
        {
            get { return fGroups; }
        }


        public GDMResearchRecord(GDMTree tree) : base(tree)
        {
            SetName(GEDCOMTagType._RESEARCH);

            fStartDate = new GDMDate((int)GEDCOMTagType._STARTDATE);
            fStopDate = new GDMDate((int)GEDCOMTagType._STOPDATE);

            fTasks = new GDMList<GDMPointer>();
            fCommunications = new GDMList<GDMPointer>();
            fGroups = new GDMList<GDMPointer>();
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

        internal override void TrimExcess()
        {
            base.TrimExcess();

            fStartDate.TrimExcess();
            fStopDate.TrimExcess();
            fTasks.TrimExcess();
            fCommunications.TrimExcess();
            fGroups.TrimExcess();
        }

        public override void Assign(GDMTag source)
        {
            GDMResearchRecord sourceObj = (source as GDMResearchRecord);
            if (sourceObj == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(sourceObj);

            fResearchName = sourceObj.fResearchName;
            fPriority = sourceObj.fPriority;
            fStatus = sourceObj.fStatus;
            fStartDate.Assign(sourceObj.fStartDate);
            fStopDate.Assign(sourceObj.fStopDate);
            fPercent = sourceObj.fPercent;

            AssignList(sourceObj.fTasks, fTasks);
            AssignList(sourceObj.fCommunications, fCommunications);
            AssignList(sourceObj.fGroups, fGroups);
        }

        public override void Clear()
        {
            base.Clear();

            fResearchName = string.Empty;
            fPriority = GDMResearchPriority.rpNone;
            fStatus = GDMResearchStatus.rsDefined;
            fStartDate.Clear();
            fStopDate.Clear();
            fPercent = 0;

            fTasks.Clear();
            fCommunications.Clear();
            fGroups.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && (fTasks.Count == 0) && (fCommunications.Count == 0) && (fGroups.Count == 0) &&
                string.IsNullOrEmpty(fResearchName) && (fPriority == GDMResearchPriority.rpNone) &&
                (fStatus == GDMResearchStatus.rsDefined) && fStartDate.IsEmpty() && (fStopDate.IsEmpty()) && (fPercent == 0);
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);

            fTasks.ReplaceXRefs(map);
            fCommunications.ReplaceXRefs(map);
            fGroups.ReplaceXRefs(map);
        }

        public bool AddTask(GDMTaskRecord taskRecord)
        {
            bool result = false;

            if (taskRecord != null) {
                GDMPointer ptr = new GDMPointer((int)GEDCOMTagType._TASK);
                ptr.XRef = taskRecord.XRef;
                fTasks.Add(ptr);
                result = true;
            }

            return result;
        }

        public void RemoveTask(GDMTaskRecord taskRecord)
        {
            if (taskRecord == null) return;

            fTasks.RemoveAt(IndexOfTask(taskRecord));
        }

        public int IndexOfTask(GDMTaskRecord taskRec)
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

        public bool AddGroup(GDMGroupRecord groupRecord)
        {
            bool result = false;

            if (groupRecord != null) {
                GDMPointer ptr = new GDMPointer((int)GEDCOMTagType._GROUP);
                ptr.XRef = groupRecord.XRef;
                fGroups.Add(ptr);
                result = true;
            }

            return result;
        }

        public void RemoveGroup(GDMGroupRecord groupRecord)
        {
            if (groupRecord == null) return;

            fGroups.RemoveAt(IndexOfGroup(groupRecord));
        }

        public int IndexOfGroup(GDMGroupRecord groupRec)
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

        public bool AddCommunication(GDMCommunicationRecord commRecord)
        {
            bool result = false;

            if (commRecord != null) {
                GDMPointer ptr = new GDMPointer((int)GEDCOMTagType._COMM);
                ptr.XRef = commRecord.XRef;
                fCommunications.Add(ptr);
                result = true;
            }

            return result;
        }

        public void RemoveCommunication(GDMCommunicationRecord commRecord)
        {
            if (commRecord == null) return;

            fCommunications.RemoveAt(IndexOfCommunication(commRecord));
        }

        public int IndexOfCommunication(GDMCommunicationRecord commRec)
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

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fResearchName);
            hashCode.Add(fPriority);
            hashCode.Add(fStatus);
            hashCode.Add(fStartDate);
            hashCode.Add(fStopDate);
            hashCode.Add(fPercent);
            ProcessHashes(ref hashCode, fTasks);
            ProcessHashes(ref hashCode, fCommunications);
            ProcessHashes(ref hashCode, fGroups);
        }
    }
}
