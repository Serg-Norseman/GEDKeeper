/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
    public sealed class GEDCOMData : GEDCOMTagWithLists
    {
        private GEDCOMList<GEDCOMEvent> fEvents;

        public GEDCOMList<GEDCOMEvent> Events
        {
            get { return fEvents; }
        }

        public string Agency
        {
            get { return GetTagStringValue("AGNC"); }
            set { SetTagStringValue("AGNC", value); }
        }

        protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
        {
            base.CreateObj(owner, parent);
            SetName("DATA");

            fEvents = new GEDCOMList<GEDCOMEvent>(this);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                fEvents.Dispose();
            }
            base.Dispose(disposing);
        }

        public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
        {
            GEDCOMTag result;

            if (tagName == "EVEN")
            {
                result = fEvents.Add(new GEDCOMEvent(Owner, this, tagName, tagValue));
            }
            else
            {
                result = base.AddTag(tagName, tagValue, tagConstructor);
            }

            return result;
        }

        public override void Clear()
        {
            base.Clear();
            fEvents.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && (fEvents.Count == 0);
        }

        public override void ResetOwner(GEDCOMTree newOwner)
        {
            base.ResetOwner(newOwner);
            fEvents.ResetOwner(newOwner);
        }

        public GEDCOMData(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
        }

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
        {
            return new GEDCOMData(owner, parent, tagName, tagValue);
        }
    }
}
