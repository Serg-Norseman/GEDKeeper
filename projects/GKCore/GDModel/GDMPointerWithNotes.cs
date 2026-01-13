/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
            return base.IsEmpty() && fNotes.IsEmpty();
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
