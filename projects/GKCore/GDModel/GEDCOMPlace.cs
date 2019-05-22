﻿/*
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
    public sealed class GEDCOMPlace : GEDCOMTagWithLists
    {
        public string Form
        {
            get { return GetTagStringValue(GEDCOMTagType.FORM); }
            set { SetTagStringValue(GEDCOMTagType.FORM, value); }
        }

        public GEDCOMPointer Location
        {
            get { return GetTag(GEDCOMTagType._LOC, GEDCOMPointer.Create) as GEDCOMPointer; }
        }

        public GEDCOMMap Map
        {
            get { return GetTag(GEDCOMTagType.MAP, GEDCOMMap.Create) as GEDCOMMap; }
        }


        public GEDCOMPlace(GEDCOMObject owner) : base(owner)
        {
            SetName(GEDCOMTagType.PLAC);
        }

        public GEDCOMPlace(GEDCOMObject owner, string tagName, string tagValue) : this(owner)
        {
            SetNameValue(tagName, tagValue);
        }

        public new static GEDCOMTag Create(GEDCOMObject owner, string tagName, string tagValue)
        {
            return new GEDCOMPlace(owner, tagName, tagValue);
        }
    }
}