/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
