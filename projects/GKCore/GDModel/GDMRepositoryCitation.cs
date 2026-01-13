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
    /// <summary>
    /// 
    /// </summary>
    public class GDMSourceCallNumber : GDMValueTag
    {
        private GDMMediaType fMediaType;


        public GDMMediaType MediaType
        {
            get { return fMediaType; }
            set { fMediaType = value; }
        }


        public GDMSourceCallNumber()
        {
            SetName(GEDCOMTagType.CALN);
            fMediaType = GDMMediaType.mtUnknown;
        }

        public override void Assign(GDMTag source)
        {
            GDMSourceCallNumber sourceObj = (source as GDMSourceCallNumber);
            if (sourceObj == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(sourceObj);
            fMediaType = sourceObj.fMediaType;
        }

        public override void Clear()
        {
            base.Clear();
            fMediaType = GDMMediaType.mtUnknown;
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && (fMediaType == GDMMediaType.mtUnknown);
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);
            hashCode.Add(fMediaType);
        }
    }


    public sealed class GDMRepositoryCitation : GDMPointerWithNotes
    {
        private GDMList<GDMSourceCallNumber> fCallNumbers;


        public bool HasCallNumbers
        {
            get { return fCallNumbers != null && fCallNumbers.Count != 0; }
        }

        public GDMList<GDMSourceCallNumber> CallNumbers
        {
            get {
                if (fCallNumbers == null) {
                    fCallNumbers = new GDMList<GDMSourceCallNumber>();
                }
                return fCallNumbers;
            }
        }


        public GDMRepositoryCitation()
        {
            SetName(GEDCOMTagType.REPO);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fCallNumbers != null) fCallNumbers.Dispose();
            }
            base.Dispose(disposing);
        }

        internal override void TrimExcess()
        {
            base.TrimExcess();
            if (fCallNumbers != null) fCallNumbers.TrimExcess();
        }

        public override void Assign(GDMTag source)
        {
            GDMRepositoryCitation sourceObj = source as GDMRepositoryCitation;
            if (sourceObj == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(source);
            if (sourceObj.fCallNumbers != null) AssignList(sourceObj.fCallNumbers, CallNumbers);
        }

        public override void Clear()
        {
            base.Clear();
            if (fCallNumbers != null) fCallNumbers.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && fCallNumbers.IsEmpty();
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);
            if (fCallNumbers != null) fCallNumbers.ReplaceXRefs(map);
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);
            ProcessHashes(ref hashCode, fCallNumbers);
        }
    }
}
