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
                throw new ArgumentException(@"Argument is null or wrong type", nameof(source));

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
