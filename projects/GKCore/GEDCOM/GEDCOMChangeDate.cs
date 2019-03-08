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

namespace GKCommon.GEDCOM
{
    public sealed class GEDCOMChangeDate : GEDCOMTag
    {
        public GEDCOMDate ChangeDate
        {
            get { return TagClass(GEDCOMTagType.DATE, GEDCOMDate.Create) as GEDCOMDate; }
        }

        public GEDCOMTime ChangeTime
        {
            get {
                GEDCOMTag dateTag = ChangeDate;
                return dateTag.TagClass(GEDCOMTagType.TIME, GEDCOMTime.Create) as GEDCOMTime;
            }
        }

        public DateTime ChangeDateTime
        {
            get {
                return ChangeDate.Date + ChangeTime.Value;
            }
            set {
                ChangeDate.Date = value.Date;
                ChangeTime.Value = value.TimeOfDay;
            }
        }

        public GEDCOMNotes Notes
        {
            get { return TagClass(GEDCOMTagType.NOTE, GEDCOMNotes.Create) as GEDCOMNotes; }
        }


        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
        {
            return new GEDCOMChangeDate(owner, parent, tagName, tagValue);
        }

        public GEDCOMChangeDate(GEDCOMTree owner, GEDCOMObject parent) : base(owner, parent)
        {
            SetName(GEDCOMTagType.CHAN);
        }

        public GEDCOMChangeDate(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : this(owner, parent)
        {
            SetNameValue(tagName, tagValue);
        }

        public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
        {
            GEDCOMTag result;

            if (tagName == GEDCOMTagType.DATE) {
                result = base.AddTag(tagName, tagValue, GEDCOMDate.Create);
            } else if (tagName == GEDCOMTagType.NOTE) {
                result = base.AddTag(tagName, tagValue, GEDCOMNotes.Create);
            } else {
                result = base.AddTag(tagName, tagValue, tagConstructor);
            }

            return result;
        }

        public override string ToString()
        {
            DateTime cdt = ChangeDateTime;
            string result = ((cdt.Ticks == 0) ? "" : cdt.ToString("yyyy.MM.dd HH:mm:ss", null));
            return result;
        }
    }
}
