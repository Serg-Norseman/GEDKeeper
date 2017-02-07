/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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

using System.Collections.Generic;

namespace GKCommon.GEDCOM
{
    public sealed class XRefReplacer : GEDCOMObject
    {
        public sealed class XRefEntry
        {
            public readonly GEDCOMRecord Rec;
            public readonly string OldXRef;
            public readonly string NewXRef;

            public XRefEntry(GEDCOMRecord rec, string oldXRef, string newXRef) {
                this.Rec = rec;
                this.OldXRef = oldXRef;
                this.NewXRef = newXRef;
            }
        }

        private readonly List<XRefEntry> fList;

        public int Count
        {
            get	{ return this.fList.Count; }
        }

        public XRefEntry this[int index]
        {
            get { return this.fList[index]; }
        }

        public XRefReplacer()
        {
            this.fList = new List<XRefEntry>();
        }

        public void AddXRef(GEDCOMRecord rec, string oldXRef, string newXRef)
        {
            this.fList.Add(new XRefEntry(rec, oldXRef, newXRef));
        }

        public string FindNewXRef(string oldXRef)
        {
            string result = oldXRef;

            foreach (XRefEntry entry in this.fList)
            {
                if (GEDCOMUtils.CleanXRef(entry.OldXRef) == GEDCOMUtils.CleanXRef(oldXRef))
                {
                    result = entry.NewXRef;
                    break;
                }
            }

            return result;
        }
    }
}
