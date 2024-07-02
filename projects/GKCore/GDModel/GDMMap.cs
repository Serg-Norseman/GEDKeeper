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
    public sealed class GDMMap : GDMTag
    {
        private double fLati;
        private double fLong;


        public double Lati
        {
            get { return fLati; }
            set { fLati = value; }
        }

        public double Long
        {
            get { return fLong; }
            set { fLong = value; }
        }


        public GDMMap()
        {
            SetName(GEDCOMTagType.MAP);
        }

        public override void Assign(GDMTag source)
        {
            GDMMap otherMap = (source as GDMMap);
            if (otherMap == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(otherMap);

            fLati = otherMap.fLati;
            fLong = otherMap.fLong;
        }

        public override void Clear()
        {
            base.Clear();

            fLati = 0.0d;
            fLong = 0.0d;
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && (fLati == 0.0d && fLong == 0.0d);
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fLati);
            hashCode.Add(fLong);
        }
    }
}
