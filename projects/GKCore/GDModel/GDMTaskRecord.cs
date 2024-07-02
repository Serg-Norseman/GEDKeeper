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
        private string fGoal;
        private GDMResearchPriority fPriority;
        private readonly GDMDate fStartDate;
        private readonly GDMDate fStopDate;


        public string Goal
        {
            get { return fGoal; }
            set { fGoal = value; }
        }

        public GDMResearchPriority Priority
        {
            get { return fPriority; }
            set { fPriority = value; }
        }

        public GDMDate StartDate
        {
            get { return fStartDate; }
        }

        public GDMDate StopDate
        {
            get { return fStopDate; }
        }


        public GDMTaskRecord(GDMTree tree) : base(tree)
        {
            SetName(GEDCOMTagType._TASK);

            fStartDate = new GDMDate((int)GEDCOMTagType._STARTDATE);
            fStopDate = new GDMDate((int)GEDCOMTagType._STOPDATE);
        }

        public override void Assign(GDMTag source)
        {
            GDMTaskRecord sourceObj = (source as GDMTaskRecord);
            if (sourceObj == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(sourceObj);

            fGoal = sourceObj.fGoal;
            fPriority = sourceObj.fPriority;
            fStartDate.Assign(sourceObj.fStartDate);
            fStopDate.Assign(sourceObj.fStopDate);
        }

        internal override void TrimExcess()
        {
            base.TrimExcess();

            fStartDate.TrimExcess();
            fStopDate.TrimExcess();
        }

        public override void Clear()
        {
            base.Clear();

            fGoal = string.Empty;
            fPriority = GDMResearchPriority.rpNone;
            fStartDate.Clear();
            fStopDate.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && string.IsNullOrEmpty(fGoal) && (fPriority == GDMResearchPriority.rpNone) &&
                fStartDate.IsEmpty() && (fStopDate.IsEmpty());
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);

            if (GEDCOMUtils.IsXRef(fGoal)) {
                fGoal = GEDCOMUtils.EncloseXRef(map.FindNewXRef(GEDCOMUtils.CleanXRef(fGoal)));
            }
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fGoal);
            hashCode.Add(fPriority);
            hashCode.Add(fStartDate);
            hashCode.Add(fStopDate);
        }
    }
}
