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

namespace GDModel
{
    public class GDMPointerWithNotes : GDMPointer, IGDMStructWithNotes
    {
        private GDMList<GDMNotes> fNotes;

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

            if (fNotes != null) fNotes.TrimExcess();
        }

        public override void Assign(GDMTag source)
        {
            GDMPointerWithNotes sourceObj = source as GDMPointerWithNotes;
            if (sourceObj == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(source);

            if (sourceObj.fNotes != null) AssignList(sourceObj.fNotes, Notes);
        }

        public override void Clear()
        {
            base.Clear();
            if (fNotes != null) fNotes.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && (fNotes == null || fNotes.Count == 0);
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);
            if (fNotes != null) fNotes.ReplaceXRefs(map);
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            ProcessHashes(ref hashCode, fNotes);
        }
    }
}
