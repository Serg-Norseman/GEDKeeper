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

using System.Collections.Generic;
using GDModel.Providers.GEDCOM;

namespace GDModel
{
    public sealed class GDMXRefReplacer : GDMObject
    {
        public sealed class XRefEntry
        {
            public readonly GDMRecord Rec;
            public readonly string OldXRef;
            public readonly string NewXRef;

            public XRefEntry(GDMRecord rec, string oldXRef, string newXRef) {
                Rec = rec;
                OldXRef = oldXRef;
                NewXRef = newXRef;
            }

            public override string ToString()
            {
                return string.Format("{0} -> {1}", OldXRef, NewXRef);
            }
        }

        private readonly List<XRefEntry> fList;


        public int Count
        {
            get	{ return fList.Count; }
        }

        public XRefEntry this[int index]
        {
            get { return fList[index]; }
        }


        public GDMXRefReplacer()
        {
            fList = new List<XRefEntry>();
        }

        public void AddXRef(GDMRecord rec, string oldXRef, string newXRef)
        {
            // protection
            oldXRef = GEDCOMUtils.CleanXRef(oldXRef);
            newXRef = GEDCOMUtils.CleanXRef(newXRef);

            fList.Add(new XRefEntry(rec, oldXRef, newXRef));
        }

        public string FindNewXRef(string oldXRef)
        {
            // protection
            oldXRef = GEDCOMUtils.CleanXRef(oldXRef);

            string result = oldXRef;

            foreach (XRefEntry entry in fList) {
                if (entry.OldXRef == oldXRef) {
                    result = entry.NewXRef;
                    break;
                }
            }

            return result;
        }
    }
}
