/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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

using System.IO;

namespace GKCommon.GEDCOM
{
    public sealed class GEDCOMMultimediaRecord : GEDCOMRecord
    {
        private GEDCOMList<GEDCOMFileReferenceWithTitle> fFileReferences;

        public GEDCOMList<GEDCOMFileReferenceWithTitle> FileReferences
        {
            get { return this.fFileReferences; }
        }

        protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
        {
            base.CreateObj(owner, parent);
            base.SetRecordType(GEDCOMRecordType.rtMultimedia);
            base.SetName("OBJE");

            this.fFileReferences = new GEDCOMList<GEDCOMFileReferenceWithTitle>(this);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                this.fFileReferences.Dispose();
            }
            base.Dispose(disposing);
        }

        public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
        {
            GEDCOMTag result;

            if (tagName == "FILE")
            {
                result = this.fFileReferences.Add(new GEDCOMFileReferenceWithTitle(base.Owner, this, tagName, tagValue));
            }
            else
            {
                result = base.AddTag(tagName, tagValue, tagConstructor);
            }

            return result;
        }

        public override void Clear()
        {
            base.Clear();
            this.fFileReferences.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && this.fFileReferences.Count == 0;
        }

        public override void Pack()
        {
            base.Pack();
            this.fFileReferences.Pack();
        }

        public override void ReplaceXRefs(XRefReplacer map)
        {
            base.ReplaceXRefs(map);
            this.fFileReferences.ReplaceXRefs(map);
        }

        public override void ResetOwner(GEDCOMTree newOwner)
        {
            base.ResetOwner(newOwner);
            this.fFileReferences.ResetOwner(newOwner);
        }

        public override void SaveToStream(StreamWriter stream)
        {
            base.SaveToStream(stream);
            this.fFileReferences.SaveToStream(stream);
        }

        public GEDCOMMultimediaRecord(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
        }

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
        {
            return new GEDCOMMultimediaRecord(owner, parent, tagName, tagValue);
        }

        #region Auxiliary

        public string GetFileTitle()
        {
            if (this.fFileReferences.Count > 0) {
                GEDCOMFileReferenceWithTitle fileRef = this.fFileReferences[0];
                return fileRef.Title;
            }
            
            return string.Empty;
        }
        
        #endregion
    }
}
