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
    public class GDMPointer : GDMTag, IGDMPointerHost
    {
        private string fXRef;


        public bool IsPointer
        {
            get { return (!string.IsNullOrEmpty(fXRef)); }
        }

        public string XRef
        {
            get { return fXRef; }
            set { fXRef = value; }
        }


        public GDMPointer()
        {
            fXRef = string.Empty;
        }

        public GDMPointer(int tagId) : base(tagId)
        {
            fXRef = string.Empty;
        }

        public GDMPointer(int tagId, StringSpan tagValue)
        {
            SetNameValue(tagId, tagValue);
        }

        public override void Clear()
        {
            base.Clear();
            fXRef = string.Empty;
        }

        public override bool IsEmpty()
        {
            return (base.IsEmpty() && string.IsNullOrEmpty(fXRef));
        }

        protected override string GetStringValue()
        {
            return GEDCOMUtils.EncloseXRef(fXRef);
        }

        public override string ParseString(string strValue)
        {
            // here XRef is a pure value without delimiters
            return GEDCOMUtils.ParseXRefPointer(strValue, out fXRef);
        }

        public override string ParseString(StringSpan strValue)
        {
            // here XRef is a pure value without delimiters
            return GEDCOMUtils.ParseXRefPointer(strValue, out fXRef);
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);
            if (map != null) {
                XRef = map.FindNewXRef(XRef);
            }
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fXRef);
        }
    }
}
