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

using System;
using GDModel.Providers.GEDCOM;

namespace GDModel
{
    public sealed class GDMPlace : GDMTagWithLists
    {
        private string fForm;
        private GDMPointer fLocation;
        private GDMMap fMap;


        public string Form
        {
            get { return fForm; }
            set { fForm = value; }
        }

        public GDMPointer Location
        {
            get { return fLocation; }
        }

        public GDMMap Map
        {
            get { return fMap; }
        }


        public GDMPlace(GDMObject owner) : base(owner)
        {
            SetName(GEDCOMTagType.PLAC);

            fForm = string.Empty;
            fLocation = new GDMPointer(this, (int)GEDCOMTagType._LOC, string.Empty);
            fMap = new GDMMap(this);
        }

        public GDMPlace(GDMObject owner, int tagId, string tagValue) : this(owner)
        {
            SetNameValue(tagId, tagValue);
        }

        public new static GDMTag Create(GDMObject owner, int tagId, string tagValue)
        {
            return new GDMPlace(owner, tagId, tagValue);
        }

        public override void Assign(GDMTag source)
        {
            GDMPlace otherPlace = (source as GDMPlace);
            if (otherPlace == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(otherPlace);

            fForm = otherPlace.fForm;
            fLocation.Assign(otherPlace.fLocation);
            fMap.Assign(otherPlace.fMap);
        }

        public override void Clear()
        {
            base.Clear();

            fForm = string.Empty;
            fLocation.Clear();
            fMap.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && fLocation.IsEmpty() && fMap.IsEmpty() && string.IsNullOrEmpty(fForm);
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);
            fLocation.ReplaceXRefs(map);
            fMap.ReplaceXRefs(map);
        }
    }
}
