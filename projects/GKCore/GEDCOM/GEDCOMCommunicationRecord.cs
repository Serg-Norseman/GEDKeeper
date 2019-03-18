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

namespace GKCommon.GEDCOM
{
    public sealed class GEDCOMCommunicationRecord : GEDCOMRecord
    {
        public static readonly string[] CommunicationTags = new string[] { GEDCOMTagType.FROM, GEDCOMTagType.TO };

        public GEDCOMDate Date
        {
            get { return TagClass(GEDCOMTagType.DATE, GEDCOMDate.Create) as GEDCOMDate; }
        }

        public string CommName
        {
            get { return GetTagStringValue(GEDCOMTagType.NAME); }
            set { SetTagStringValue(GEDCOMTagType.NAME, value); }
        }

        public GKCommunicationType CommunicationType
        {
            get { return GEDCOMUtils.GetCommunicationTypeVal(GetTagStringValue(GEDCOMTagType.TYPE)); }
            set { SetTagStringValue(GEDCOMTagType.TYPE, GEDCOMUtils.GetCommunicationTypeStr(value)); }
        }


        public GEDCOMCommunicationRecord(GEDCOMTree owner, GEDCOMObject parent) : base(owner, parent)
        {
            SetRecordType(GEDCOMRecordType.rtCommunication);
            SetName(GEDCOMTagType._COMM);
        }

        public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
        {
            GEDCOMTag result;

            if (tagName == GEDCOMTagType.NAME) {
                result = base.AddTag(tagName, tagValue, null);
            } else if (tagName == GEDCOMTagType.DATE) {
                result = base.AddTag(tagName, tagValue, GEDCOMDate.Create);
            } else {
                result = base.AddTag(tagName, tagValue, tagConstructor);
            }

            return result;
        }
        
        #region Auxiliary

        public sealed class CorresponderRet
        {
            public readonly GKCommunicationDir CommDir;
            public readonly GEDCOMIndividualRecord Corresponder;

            public CorresponderRet(GKCommunicationDir commDir, GEDCOMIndividualRecord corresponder)
            {
                CommDir = commDir;
                Corresponder = corresponder;
            }
        }

        public CorresponderRet GetCorresponder()
        {
            GKCommunicationDir commDir = GKCommunicationDir.cdFrom;
            GEDCOMIndividualRecord corresponder = null;

            GEDCOMTag corrTag = FindTag(GEDCOMTagType.FROM, 0);
            if (corrTag == null) {
                corrTag = FindTag(GEDCOMTagType.TO, 0);
            }

            if (corrTag != null) {
                corresponder = (Owner.XRefIndex_Find(GEDCOMUtils.CleanXRef(corrTag.StringValue)) as GEDCOMIndividualRecord);

                if (corrTag.Name == GEDCOMTagType.FROM) {
                    commDir = GKCommunicationDir.cdFrom;
                } else if (corrTag.Name == GEDCOMTagType.TO) {
                    commDir = GKCommunicationDir.cdTo;
                }
            }

            return new CorresponderRet(commDir, corresponder);
        }

        public void SetCorresponder(GKCommunicationDir commDir, GEDCOMIndividualRecord corresponder)
        {
            DeleteTag(GEDCOMTagType.FROM);
            DeleteTag(GEDCOMTagType.TO);

            if (corresponder != null) {
                AddTag(CommunicationTags[(int)commDir], GEDCOMUtils.EncloseXRef(corresponder.XRef), null);
            }
        }

        #endregion
    }
}
