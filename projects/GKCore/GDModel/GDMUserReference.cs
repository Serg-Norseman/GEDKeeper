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
    public sealed class GDMUserReference : GDMValueTag
    {
        private string fReferenceType;

        public string ReferenceType
        {
            get { return fReferenceType; }
            set { fReferenceType = value; }
        }


        public GDMUserReference()
        {
            SetName(GEDCOMTagType.REFN);
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

        public override void Assign(GDMTag source)
        {
            GDMUserReference srcUserRef = (source as GDMUserReference);
            if (srcUserRef == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(source);

            fReferenceType = srcUserRef.fReferenceType;
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fReferenceType);
        }
    }
}
