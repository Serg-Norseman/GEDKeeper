/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
        public static readonly string[] CommunicationTags = new string[] { "FROM", "TO" };

        public GEDCOMDateExact Date
        {
            get { return base.TagClass("DATE", GEDCOMDateExact.Create) as GEDCOMDateExact; }
        }

        public string CommName
        {
            get { return base.GetTagStringValue("NAME"); }
            set { base.SetTagStringValue("NAME", value); }
        }

        public GKCommunicationType CommunicationType
        {
            get { return GEDCOMUtils.GetCommunicationTypeVal(base.GetTagStringValue("TYPE")); }
            set { base.SetTagStringValue("TYPE", GEDCOMUtils.GetCommunicationTypeStr(value)); }
        }

        protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
        {
            base.CreateObj(owner, parent);
            base.SetRecordType(GEDCOMRecordType.rtCommunication);
            base.SetName("_COMM");
        }

        public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
        {
            GEDCOMTag result;

            if (tagName == "NAME")
            {
                result = base.AddTag(tagName, tagValue, null);
            }
            else if (tagName == "DATE")
            {
                result = base.AddTag(tagName, tagValue, GEDCOMDateExact.Create);
            }
            else
            {
                result = base.AddTag(tagName, tagValue, tagConstructor);
            }

            return result;
        }

        public GEDCOMCommunicationRecord(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
        }

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
        {
            return new GEDCOMCommunicationRecord(owner, parent, tagName, tagValue);
        }
        
        #region Auxiliary

        public void GetCorresponder(out GKCommunicationDir commDir, out GEDCOMIndividualRecord corresponder)
        {
            commDir = GKCommunicationDir.cdFrom;
            corresponder = null;

            GEDCOMTag corrTag = base.FindTag("FROM", 0);
            if (corrTag == null) {
                corrTag = base.FindTag("TO", 0);
            }

            if (corrTag != null) {
                corresponder = (this.Owner.XRefIndex_Find(GEDCOMUtils.CleanXRef(corrTag.StringValue)) as GEDCOMIndividualRecord);

                if (corrTag.Name == "FROM") {
                    commDir = GKCommunicationDir.cdFrom;
                } else if (corrTag.Name == "TO") {
                    commDir = GKCommunicationDir.cdTo;
                }
            }
        }

        public void SetCorresponder(GKCommunicationDir commDir, GEDCOMIndividualRecord corresponder)
        {
            base.DeleteTag("FROM");
            base.DeleteTag("TO");

            if (corresponder != null) {
                this.AddTag(CommunicationTags[(int)commDir], GEDCOMUtils.EncloseXRef(corresponder.XRef), null);
            }
        }

        #endregion
    }
}
