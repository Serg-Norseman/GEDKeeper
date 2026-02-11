/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
                throw new ArgumentException(@"Argument is null or wrong type", nameof(source));

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
