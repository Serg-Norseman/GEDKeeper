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
using System.IO;

namespace GKCommon.GEDCOM
{
    public sealed class GEDCOMUserReference : GEDCOMTag
    {
        private string fReferenceType;

        public string ReferenceType
        {
            get { return fReferenceType; }
            set { fReferenceType = value; }
        }


        public GEDCOMUserReference(GEDCOMTree owner, GEDCOMObject parent) : base(owner, parent)
        {
            SetName(GEDCOMTagType.REFN);
        }

        public GEDCOMUserReference(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : this(owner, parent)
        {
            SetNameValue(tagName, tagValue);
        }

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
        {
            return new GEDCOMUserReference(owner, parent, tagName, tagValue);
        }

        public override void Clear()
        {
            base.Clear();
            fReferenceType = string.Empty;
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && string.IsNullOrEmpty(fReferenceType);
        }

        public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
        {
            GEDCOMTag result;

            if (tagName == GEDCOMTagType.TYPE) {
                fReferenceType = tagValue;
                result = null;
            } else {
                result = base.AddTag(tagName, tagValue, tagConstructor);
            }

            return result;
        }

        public override void SaveToStream(StreamWriter stream)
        {
            base.SaveToStream(stream);
            WriteTagLine(stream, Level + 1, GEDCOMTagType.TYPE, fReferenceType, true);
        }

        public override void Assign(GEDCOMTag source)
        {
            GEDCOMUserReference srcUserRef = (source as GEDCOMUserReference);
            if (srcUserRef == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(source);

            fReferenceType = srcUserRef.fReferenceType;
        }
    }
}
