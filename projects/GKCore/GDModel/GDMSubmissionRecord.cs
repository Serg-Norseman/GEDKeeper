/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
