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
    /// <summary>
    /// This type of Genealogical Data Model (GDM) defines the direction of the communication.
    /// </summary>
    public enum GDMCommunicationDir
    {
        cdFrom,
        cdTo
    }


    /// <summary>
    /// This type of Genealogical Data Model (GDM) defines the kinds of the communication.
    /// </summary>
    public enum GDMCommunicationType
    {
        ctCall,
        ctEMail,
        ctFax,
        ctLetter,
        ctTape,
        ctVisit,

        ctLast = ctVisit
    }


    public sealed class GDMCommunicationRecord : GDMRecord
    {
        public static readonly string[] CommunicationTags = new string[] { GEDCOMTagType.FROM, GEDCOMTagType.TO };

        private string fCommName;
        private GDMCommunicationType fCommunicationType;
        private GDMDate fDate;


        public GDMDate Date
        {
            get { return fDate; }
        }

        public string CommName
        {
            get { return fCommName; }
            set { fCommName = value; }
        }

        public GDMCommunicationType CommunicationType
        {
            get { return fCommunicationType; }
            set { fCommunicationType = value; }
        }


        public GDMCommunicationRecord(GDMObject owner) : base(owner)
        {
            SetRecordType(GDMRecordType.rtCommunication);
            SetName(GEDCOMTagType._COMM);

            fDate = new GDMDate(this);
        }

        public override void Assign(GDMTag source)
        {
            GDMCommunicationRecord otherComm = (source as GDMCommunicationRecord);
            if (otherComm == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(otherComm);

            fCommName = otherComm.fCommName;
            fCommunicationType = otherComm.fCommunicationType;
            fDate.Assign(otherComm.fDate);
        }

        public override void Clear()
        {
            base.Clear();

            fCommName = string.Empty;
            fCommunicationType = GDMCommunicationType.ctCall;
            fDate.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && string.IsNullOrEmpty(fCommName) && fDate.IsEmpty();
        }


        public sealed class CorresponderRet
        {
            public readonly GDMCommunicationDir CommDir;
            public readonly GDMIndividualRecord Corresponder;

            public CorresponderRet(GDMCommunicationDir commDir, GDMIndividualRecord corresponder)
            {
                CommDir = commDir;
                Corresponder = corresponder;
            }
        }

        public CorresponderRet GetCorresponder()
        {
            GDMCommunicationDir commDir = GDMCommunicationDir.cdFrom;
            GDMIndividualRecord corresponder = null;

            GDMTag corrTag = FindTag(GEDCOMTagType.FROM, 0);
            if (corrTag == null) {
                corrTag = FindTag(GEDCOMTagType.TO, 0);
            }

            if (corrTag != null) {
                corresponder = (GetTree().XRefIndex_Find(GEDCOMUtils.CleanXRef(corrTag.StringValue)) as GDMIndividualRecord);

                if (corrTag.Name == GEDCOMTagType.FROM) {
                    commDir = GDMCommunicationDir.cdFrom;
                } else if (corrTag.Name == GEDCOMTagType.TO) {
                    commDir = GDMCommunicationDir.cdTo;
                }
            }

            return new CorresponderRet(commDir, corresponder);
        }

        public void SetCorresponder(GDMCommunicationDir commDir, GDMIndividualRecord corresponder)
        {
            DeleteTag(GEDCOMTagType.FROM);
            DeleteTag(GEDCOMTagType.TO);

            if (corresponder != null) {
                AddTag(CommunicationTags[(int)commDir], GEDCOMUtils.EncloseXRef(corresponder.XRef), null);
            }
        }
    }
}
