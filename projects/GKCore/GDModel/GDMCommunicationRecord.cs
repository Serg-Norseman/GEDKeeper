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
        private string fCommName;
        private GDMCommunicationType fCommunicationType;
        private readonly GDMDate fDate;
        private GDMCommunicationDir fCommDirection;
        private readonly GDMIndividualLink fCorresponder;


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

        public GDMCommunicationDir CommDirection
        {
            get { return fCommDirection; }
            set {
                fCommDirection = value;

                GEDCOMTagType tagName = GEDCOMTagType.Unknown;
                if (fCommDirection == GDMCommunicationDir.cdFrom) {
                    tagName = GEDCOMTagType.FROM;
                } else if (fCommDirection == GDMCommunicationDir.cdTo) {
                    tagName = GEDCOMTagType.TO;
                }
                fCorresponder.SetName(tagName);
            }
        }

        public GDMIndividualLink Corresponder
        {
            get { return fCorresponder; }
        }


        public GDMCommunicationRecord(GDMTree tree) : base(tree)
        {
            SetName(GEDCOMTagType._COMM);

            fDate = new GDMDate();
            fCorresponder = new GDMIndividualLink();
        }

        internal override void TrimExcess()
        {
            base.TrimExcess();

            fDate.TrimExcess();
            fCorresponder.TrimExcess();
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
            fCorresponder.Assign(otherComm.fCorresponder);
        }

        public override void Clear()
        {
            base.Clear();

            fCommName = string.Empty;
            fCommunicationType = GDMCommunicationType.ctCall;
            fDate.Clear();
            fCorresponder.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && string.IsNullOrEmpty(fCommName) && fDate.IsEmpty() && fCorresponder.IsEmpty();
        }

        public void SetCorresponder(GDMCommunicationDir direction, GDMIndividualRecord corresponder)
        {
            if (corresponder != null) {
                CommDirection = direction;
                fCorresponder.XRef = corresponder.XRef;
            }
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);
            fCorresponder.ReplaceXRefs(map);
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fCommName);
            hashCode.Add(fCommunicationType);
            hashCode.Add(fDate);
            hashCode.Add(fCommDirection);
            hashCode.Add(fCorresponder);
        }
    }
}
