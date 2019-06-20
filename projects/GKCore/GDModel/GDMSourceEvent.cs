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
    public sealed class GDMSourceEvent : GDMTag
    {
        private GDMDatePeriod fDate;
        private GDMPlace fPlace;


        public GDMDatePeriod Date
        {
            get { return fDate; }
        }

        public GDMPlace Place
        {
            get { return fPlace; }
        }


        public GDMSourceEvent(GDMObject owner) : base(owner)
        {
            SetName(GEDCOMTagType.EVEN);

            fDate = new GDMDatePeriod(this);
            fPlace = new GDMPlace(this);
        }

        public override void Clear()
        {
            base.Clear();

            fDate.Clear();
            fPlace.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && fDate.IsEmpty() && fPlace.IsEmpty();
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);
            fPlace.ReplaceXRefs(map);
        }
    }
}
