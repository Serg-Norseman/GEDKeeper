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
    public sealed class GDMMultimediaRecord : GDMRecord
    {
        private readonly GDMList<GDMFileReferenceWithTitle> fFileReferences;

        public GDMList<GDMFileReferenceWithTitle> FileReferences
        {
            get { return fFileReferences; }
        }


        public GDMMultimediaRecord(GDMTree tree) : base(tree)
        {
            SetName(GEDCOMTagType.OBJE);

            fFileReferences = new GDMList<GDMFileReferenceWithTitle>();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fFileReferences.Dispose();
            }
            base.Dispose(disposing);
        }

        internal override void TrimExcess()
        {
            base.TrimExcess();

            fFileReferences.TrimExcess();
        }

        public override void Assign(GDMTag source)
        {
            GDMMultimediaRecord sourceObj = source as GDMMultimediaRecord;
            if (sourceObj == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(sourceObj);

            AssignList(sourceObj.fFileReferences, fFileReferences);
        }

        public override void Clear()
        {
            base.Clear();
            fFileReferences.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && fFileReferences.Count == 0;
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);
            fFileReferences.ReplaceXRefs(map);
        }

        public string GetFileTitle()
        {
            if (fFileReferences.Count <= 0) return string.Empty;

            GDMFileReferenceWithTitle fileRef = fFileReferences[0];
            return fileRef.Title;
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            ProcessHashes(ref hashCode, fFileReferences);
        }
    }
}
