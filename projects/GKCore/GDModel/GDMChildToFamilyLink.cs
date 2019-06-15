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

        public GDMFamilyRecord Family
        {
            get { return (GDMFamilyRecord)base.Value; }
            set { base.Value = value; }
        }


        public GDMChildToFamilyLink(GDMObject owner) : base(owner)
        {
            SetName(GEDCOMTagType.FAMC);
        }

        public GDMChildToFamilyLink(GDMObject owner, string tagName, string tagValue) : this(owner)
        {
            SetNameValue(tagName, tagValue);
        }

        public new static GDMTag Create(GDMObject owner, string tagName, string tagValue)
        {
            return new GDMChildToFamilyLink(owner, tagName, tagValue);
        }

        public override void Assign(GDMTag source)
        {
            GDMChildToFamilyLink srcCFL = source as GDMChildToFamilyLink;
            if (srcCFL == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(source);

            fChildLinkageStatus = srcCFL.fChildLinkageStatus;
            fPedigreeLinkageType = srcCFL.fPedigreeLinkageType;
        }
    }
}
