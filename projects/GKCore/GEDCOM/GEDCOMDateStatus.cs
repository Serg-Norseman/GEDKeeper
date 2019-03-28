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
    public sealed class GEDCOMDateStatus : GEDCOMTag
    {
        public GEDCOMDate ChangeDate
        {
            get { return TagClass(GEDCOMTagType.DATE, GEDCOMDate.Create) as GEDCOMDate; }
        }


        public new static GEDCOMTag Create(GEDCOMObject owner, string tagName, string tagValue)
        {
            return new GEDCOMDateStatus(owner, tagName, tagValue);
        }

        public GEDCOMDateStatus(GEDCOMObject owner) : base(owner)
        {
            SetName(GEDCOMTagType.STAT);
        }

        public GEDCOMDateStatus(GEDCOMObject owner, string tagName, string tagValue) : this(owner)
        {
            SetNameValue(tagName, tagValue);
        }

        public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
        {
            GEDCOMTag result;

            if (tagName == GEDCOMTagType.DATE) {
                result = base.AddTag(tagName, tagValue, GEDCOMDate.Create);
            } else {
                result = base.AddTag(tagName, tagValue, tagConstructor);
            }

            return result;
        }
    }
}
