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
            get {
                if (fMap == null) {
                    fMap = new GDMMap();
                }
                return fMap;
            }
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
            fLocation = new GDMPointer((int)GEDCOMTagType._LOC);
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
            if (fMap != null) fMap.TrimExcess();
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
            if (otherPlace.fMap != null) Map.Assign(otherPlace.fMap);
            if (otherPlace.fNotes != null) AssignList(otherPlace.fNotes, Notes);
        }

        public override void Clear()
        {
            base.Clear();

            fForm = string.Empty;
            fLocation.Clear();
            if (fMap != null) fMap.Clear();
            if (fNotes != null) fNotes.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && string.IsNullOrEmpty(fForm) && fLocation.IsEmpty() && 
                (fMap == null || fMap.IsEmpty()) && fNotes.IsEmpty();
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);
            fLocation.ReplaceXRefs(map);
            if (fMap != null) fMap.ReplaceXRefs(map);
            if (fNotes != null) fNotes.ReplaceXRefs(map);
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fForm);
            hashCode.Add(fLocation);
            hashCode.Add(fMap);
            ProcessHashes(ref hashCode, fNotes);
        }
    }
}
