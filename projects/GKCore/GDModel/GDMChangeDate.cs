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
                throw new ArgumentException(@"Argument is null or wrong type", "source");

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
    }
}
