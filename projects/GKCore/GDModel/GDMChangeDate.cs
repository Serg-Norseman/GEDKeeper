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
    public sealed class GDMChangeDate : GDMTag
    {
        public GDMDate ChangeDate
        {
            get { return GetTag<GDMDate>(GEDCOMTagType.DATE, GDMDate.Create); }
        }

        public GDMTime ChangeTime
        {
            get {
                GDMTag dateTag = ChangeDate;
                return dateTag.GetTag<GDMTime>(GEDCOMTagType.TIME, GDMTime.Create);
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

        public GDMNotes Notes
        {
            get { return GetTag<GDMNotes>(GEDCOMTagType.NOTE, GDMNotes.Create); }
        }


        public new static GDMTag Create(GDMObject owner, string tagName, string tagValue)
        {
            return new GDMChangeDate(owner, tagName, tagValue);
        }

        public GDMChangeDate(GDMObject owner) : base(owner)
        {
            SetName(GEDCOMTagType.CHAN);
        }

        public GDMChangeDate(GDMObject owner, string tagName, string tagValue) : this(owner)
        {
            SetNameValue(tagName, tagValue);
        }

        public override string ToString()
        {
            DateTime cdt = ChangeDateTime;
            string result = ((cdt.Ticks == 0) ? "" : cdt.ToString("yyyy.MM.dd HH:mm:ss", null));
            return result;
        }
    }
}
