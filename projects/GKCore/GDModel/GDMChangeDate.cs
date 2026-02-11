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
    public sealed class GDMChangeDate : GDMTag
    {
        internal static readonly DateTime ZeroDateTime = new DateTime(0);

        private DateTime fChangeDateTime;


        public DateTime ChangeDateTime
        {
            get { return fChangeDateTime; }
            set { fChangeDateTime = value; }
        }


        public GDMChangeDate()
        {
            SetName(GEDCOMTagType.CHAN);
        }

        public override string ToString()
        {
            DateTime cdt = fChangeDateTime;
            string result = ((cdt.Ticks == 0) ? "" : cdt.ToString("yyyy.MM.dd HH:mm:ss", null));
            return result;
        }

        public override void Assign(GDMTag source)
        {
            GDMChangeDate otherChnDate = (source as GDMChangeDate);
            if (otherChnDate == null)
                throw new ArgumentException(@"Argument is null or wrong type", nameof(source));

            base.Assign(otherChnDate);

            fChangeDateTime = otherChnDate.fChangeDateTime;
        }

        public override void Clear()
        {
            base.Clear();

            fChangeDateTime = new DateTime(0);
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && (fChangeDateTime.Equals(ZeroDateTime));
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fChangeDateTime);
        }

        internal void SetRawDate(DateTime date)
        {
            fChangeDateTime = date;
        }

        internal void SetRawTime(TimeSpan time)
        {
            fChangeDateTime = fChangeDateTime.AddTicks(time.Ticks);
        }
    }
}
