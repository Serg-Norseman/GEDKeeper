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
    public enum GDMOrdinanceProcessFlag
    {
        opNone,
        opNo,
        opYes
    }


    public sealed class GDMSubmissionRecord : GDMRecord
    {
        private string fFamilyFileName;
        private string fTempleCode;
        private int fGenerationsOfAncestors;
        private int fGenerationsOfDescendants;
        private GDMOrdinanceProcessFlag fOrdinanceProcessFlag;
        private GDMPointer fSubmitter;

        public string FamilyFileName
        {
            get { return fFamilyFileName; }
            set { fFamilyFileName = value; }
        }

        public string TempleCode
        {
            get { return fTempleCode; }
            set { fTempleCode = value; }
        }

        public int GenerationsOfAncestors
        {
            get { return fGenerationsOfAncestors; }
            set { fGenerationsOfAncestors = value; }
        }

        public int GenerationsOfDescendants
        {
            get { return fGenerationsOfDescendants; }
            set { fGenerationsOfDescendants = value; }
        }

        public GDMOrdinanceProcessFlag OrdinanceProcessFlag
        {
            get { return fOrdinanceProcessFlag; }
            set { fOrdinanceProcessFlag = value; }
        }

        public GDMPointer Submitter
        {
            get { return fSubmitter; }
        }


        public GDMSubmissionRecord(GDMTree tree) : base(tree)
        {
            SetName(GEDCOMTagType.SUBN);

            fFamilyFileName = string.Empty;
            fTempleCode = string.Empty;
            fGenerationsOfAncestors = -1;
            fGenerationsOfDescendants = -1;
            fOrdinanceProcessFlag = GDMOrdinanceProcessFlag.opNone;
            fSubmitter = new GDMPointer((int)GEDCOMTagType.SUBM);
        }

        internal override void TrimExcess()
        {
            base.TrimExcess();

            fSubmitter.TrimExcess();
        }

        public override void Clear()
        {
            base.Clear();

            fFamilyFileName = string.Empty;
            fTempleCode = string.Empty;
            fGenerationsOfAncestors = -1;
            fGenerationsOfDescendants = -1;
            fOrdinanceProcessFlag = GDMOrdinanceProcessFlag.opNone;
            fSubmitter.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && string.IsNullOrEmpty(fFamilyFileName) && string.IsNullOrEmpty(fTempleCode) &&
                (fGenerationsOfAncestors == -1) && (fGenerationsOfDescendants == -1) &&
                (fOrdinanceProcessFlag == GDMOrdinanceProcessFlag.opNone) && (fSubmitter.IsEmpty());
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);

            fSubmitter.ReplaceXRefs(map);
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fFamilyFileName);
            hashCode.Add(fTempleCode);
            hashCode.Add(fGenerationsOfAncestors);
            hashCode.Add(fGenerationsOfDescendants);
            hashCode.Add(fOrdinanceProcessFlag);
            hashCode.Add(fSubmitter);
        }
    }
}
