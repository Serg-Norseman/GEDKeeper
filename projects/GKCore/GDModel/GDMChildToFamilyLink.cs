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
    public enum GDMChildLinkageStatus
    {
        clNone,
        clChallenged,
        clDisproven,
        clProven
    }


    public enum GDMPedigreeLinkageType
    {
        plNone,
        plAdopted,
        plBirth,
        plFoster,
    }


    public sealed class GDMChildToFamilyLink : GDMPointerWithNotes
    {
        private GDMChildLinkageStatus fChildLinkageStatus;
        private GDMPedigreeLinkageType fPedigreeLinkageType;


        public GDMChildLinkageStatus ChildLinkageStatus
        {
            get { return fChildLinkageStatus; }
            set { fChildLinkageStatus = value; }
        }

        public GDMPedigreeLinkageType PedigreeLinkageType
        {
            get { return fPedigreeLinkageType; }
            set { fPedigreeLinkageType = value; }
        }


        public GDMChildToFamilyLink()
        {
            SetName(GEDCOMTagType.FAMC);
        }

        public GDMChildToFamilyLink(string familyXRef)
        {
            SetName(GEDCOMTagType.FAMC);
            XRef = familyXRef;
        }

        public override void Assign(GDMTag source)
        {
            GDMChildToFamilyLink srcCFL = source as GDMChildToFamilyLink;
            if (srcCFL == null)
                throw new ArgumentException(@"Argument is null or wrong type", nameof(source));

            base.Assign(source);

            fChildLinkageStatus = srcCFL.fChildLinkageStatus;
            fPedigreeLinkageType = srcCFL.fPedigreeLinkageType;
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fChildLinkageStatus);
            hashCode.Add(fPedigreeLinkageType);
        }
    }
}
