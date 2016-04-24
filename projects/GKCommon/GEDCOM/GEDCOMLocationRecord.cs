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
    public sealed class GEDCOMLocationRecord : GEDCOMRecord
    {
        public GEDCOMMap Map
        {
            get { return base.TagClass("MAP", GEDCOMMap.Create) as GEDCOMMap; }
        }

        public string LocationName
        {
            get { return base.GetTagStringValue("NAME"); }
            set { base.SetTagStringValue("NAME", value); }
        }

        protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
        {
            base.CreateObj(owner, parent);
            base.SetRecordType(GEDCOMRecordType.rtLocation);
            base.SetName("_LOC");
        }

        /*public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			// "MAP" defines by default
			return base.AddTag(tagName, tagValue, tagConstructor);
		}*/

        public GEDCOMLocationRecord(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
        }

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
        {
            return new GEDCOMLocationRecord(owner, parent, tagName, tagValue);
        }
    }
}
