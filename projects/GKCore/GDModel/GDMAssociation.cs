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
    public sealed class GDMAssociation : GDMPointerWithNotes
    {
        private string fRelation;
        private GDMList<GDMSourceCitation> fSourceCitations;


        public GDMIndividualRecord Individual
        {
            get { return (Value as GDMIndividualRecord); }
            set { Value = value; }
        }

        public string Relation
        {
            get { return fRelation; }
            set { fRelation = value; }
        }

        public GDMList<GDMSourceCitation> SourceCitations
        {
            get { return fSourceCitations; }
        }


        public GDMAssociation(GDMObject owner) : base(owner)
        {
            SetName(GEDCOMTagType.ASSO);
            fSourceCitations = new GDMList<GDMSourceCitation>(this);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fSourceCitations.Dispose();
            }
            base.Dispose(disposing);
        }

        public override void Clear()
        {
            base.Clear();
            fSourceCitations.Clear();
            fRelation = string.Empty;
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && string.IsNullOrEmpty(fRelation) && (fSourceCitations.Count == 0);
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);
            fSourceCitations.ReplaceXRefs(map);
        }

        public override void Assign(GDMTag source)
        {
            GDMAssociation srcAsso = (source as GDMAssociation);
            if (srcAsso == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(source);

            fRelation = srcAsso.fRelation;
        }
    }
}
