/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;

namespace GDModel
{
    public sealed class GDMFileReferenceWithTitle : GDMFileReference
    {
        private string fTitle;

        public string Title
        {
            get { return fTitle; }
            set { fTitle = value; }
        }


        public override void Assign(GDMTag source)
        {
            GDMFileReferenceWithTitle srcObj = (source as GDMFileReferenceWithTitle);
            if (srcObj == null)
                throw new ArgumentException(@"Argument is null or wrong type", nameof(source));

            base.Assign(source);

            fTitle = srcObj.fTitle;
        }

        public override void Clear()
        {
            base.Clear();
            fTitle = string.Empty;
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && string.IsNullOrEmpty(fTitle);
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fTitle);
        }
    }
}
