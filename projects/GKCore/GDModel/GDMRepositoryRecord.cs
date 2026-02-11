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
    public sealed class GDMRepositoryRecord : GDMRecord, IGDMStructWithAddress
    {
        private GDMAddress fAddress;
        private string fRepositoryName;


        public bool HasAddress
        {
            get { return fAddress != null && !fAddress.IsEmpty(); }
        }

        public GDMAddress Address
        {
            get {
                if (fAddress == null) {
                    fAddress = new GDMAddress();
                }
                return fAddress;
            }
        }

        public string RepositoryName
        {
            get { return fRepositoryName; }
            set { fRepositoryName = value; }
        }


        public GDMRepositoryRecord(GDMTree tree) : base(tree)
        {
            SetName(GEDCOMTagType.REPO);

            fRepositoryName = string.Empty;
        }

        internal override void TrimExcess()
        {
            base.TrimExcess();

            if (fAddress != null) fAddress.TrimExcess();
        }

        public override void Assign(GDMTag source)
        {
            GDMRepositoryRecord otherRepo = (source as GDMRepositoryRecord);
            if (otherRepo == null)
                throw new ArgumentException(@"Argument is null or wrong type", nameof(source));

            base.Assign(otherRepo);

            if (otherRepo.fAddress != null) Address.Assign(otherRepo.fAddress);
            fRepositoryName = otherRepo.fRepositoryName;
        }

        public override void Clear()
        {
            base.Clear();

            if (fAddress != null) fAddress.Clear();
            fRepositoryName = string.Empty;
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && string.IsNullOrEmpty(fRepositoryName)
                && (fAddress == null || fAddress.IsEmpty());
        }

        public override float IsMatch(GDMTag tag, MatchParams matchParams)
        {
            GDMRepositoryRecord otherRep = tag as GDMRepositoryRecord;
            if (otherRep == null) return 0.0f;

            float match = GetStrMatch(RepositoryName, otherRep.RepositoryName, matchParams);
            return match;
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);
            if (fAddress != null) fAddress.ReplaceXRefs(map);
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fAddress);
            hashCode.Add(fRepositoryName);
        }
    }
}
