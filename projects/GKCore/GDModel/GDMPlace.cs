﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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
    public sealed class GDMPlace : GDMValueTag, IGDMStructWithNotes
    {
        private string fForm;
        private GDMPointer fLocation;
        private GDMMap fMap;
        private GDMList<GDMNotes> fNotes;


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

        public bool HasNotes
        {
            get { return fNotes != null && fNotes.Count != 0; }
        }

        public GDMList<GDMNotes> Notes
        {
            get {
                if (fNotes == null) {
                    fNotes = new GDMList<GDMNotes>();
                }

                return fNotes;
            }
        }


        public GDMPlace()
        {
            SetName(GEDCOMTagType.PLAC);

            fForm = string.Empty;
            fLocation = new GDMPointer((int)GEDCOMTagType._LOC, string.Empty);
            fMap = new GDMMap();
        }

        public GDMPlace(int tagId, string tagValue) : this()
        {
            SetNameValue(tagId, tagValue);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fNotes != null) fNotes.Dispose();
            }
            base.Dispose(disposing);
        }

        internal override void TrimExcess()
        {
            base.TrimExcess();

            fLocation.TrimExcess();
            fMap.TrimExcess();
            if (fNotes != null) fNotes.TrimExcess();
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
            if (otherPlace.fNotes != null) AssignList(otherPlace.fNotes, Notes);
        }

        public override void Clear()
        {
            base.Clear();

            fForm = string.Empty;
            fLocation.Clear();
            fMap.Clear();
            if (fNotes != null) fNotes.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && fLocation.IsEmpty() && fMap.IsEmpty() && string.IsNullOrEmpty(fForm) && (fNotes == null || fNotes.Count == 0);
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);
            fLocation.ReplaceXRefs(map);
            fMap.ReplaceXRefs(map);
            if (fNotes != null) fNotes.ReplaceXRefs(map);
        }
    }
}
