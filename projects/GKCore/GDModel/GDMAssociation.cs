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
    public sealed class GDMAssociation : GDMPointerWithNotes, IGDMStructWithSourceCitations
    {
        private string fRelation;
        private GDMList<GDMSourceCitation> fSourceCitations;


        public string Relation
        {
            get { return fRelation; }
            set { fRelation = value; }
        }

        public bool HasSourceCitations
        {
            get { return fSourceCitations != null && fSourceCitations.Count != 0; }
        }

        public GDMList<GDMSourceCitation> SourceCitations
        {
            get {
                if (fSourceCitations == null) {
                    fSourceCitations = new GDMList<GDMSourceCitation>();
                }

                return fSourceCitations;
            }
        }


        public GDMAssociation()
        {
            SetName(GEDCOMTagType.ASSO);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fSourceCitations != null) fSourceCitations.Dispose();
            }
            base.Dispose(disposing);
        }

        internal override void TrimExcess()
        {
            base.TrimExcess();

            if (fSourceCitations != null) fSourceCitations.TrimExcess();
        }

        public override void Clear()
        {
            base.Clear();
            if (fSourceCitations != null) fSourceCitations.Clear();
            fRelation = string.Empty;
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && string.IsNullOrEmpty(fRelation) && fSourceCitations.IsEmpty();
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);
            if (fSourceCitations != null) fSourceCitations.ReplaceXRefs(map);
        }

        public override void Assign(GDMTag source)
        {
            GDMAssociation sourceObj = (source as GDMAssociation);
            if (sourceObj == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(sourceObj);

            fRelation = sourceObj.fRelation;
            if (sourceObj.fSourceCitations != null) AssignList(sourceObj.fSourceCitations, SourceCitations);
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fRelation);

            ProcessHashes(ref hashCode, fSourceCitations);
        }
    }
}
