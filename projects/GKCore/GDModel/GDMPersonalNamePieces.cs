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

using System.IO;
using GDModel.Providers.GEDCOM;

namespace GDModel
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class GDMPersonalNamePieces : GDMTagWithLists
    {
        public string Prefix
        {
            get { return GetTagStringValue(GEDCOMTagType.NPFX); }
            set { SetTagStringValue(GEDCOMTagType.NPFX, value); }
        }

        public string Given
        {
            get { return GetTagStringValue(GEDCOMTagType.GIVN); }
            set { SetTagStringValue(GEDCOMTagType.GIVN, value); }
        }

        public string Nickname
        {
            get { return GetTagStringValue(GEDCOMTagType.NICK); }
            set { SetTagStringValue(GEDCOMTagType.NICK, value); }
        }

        public string SurnamePrefix
        {
            get { return GetTagStringValue(GEDCOMTagType.SPFX); }
            set { SetTagStringValue(GEDCOMTagType.SPFX, value); }
        }

        public string Surname
        {
            get { return GetTagStringValue(GEDCOMTagType.SURN); }
            set { SetTagStringValue(GEDCOMTagType.SURN, value); }
        }

        public string Suffix
        {
            get { return GetTagStringValue(GEDCOMTagType.NSFX); }
            set { SetTagStringValue(GEDCOMTagType.NSFX, value); }
        }


        public string PatronymicName
        {
            get { return GetTagStringValue(GEDCOMTagType._PATN); }
            set { SetTagStringValue(GEDCOMTagType._PATN, value); }
        }

        public string MarriedName
        {
            get { return GetTagStringValue(GEDCOMTagType._MARN); }
            set { SetTagStringValue(GEDCOMTagType._MARN, value); }
        }

        public string ReligiousName
        {
            get { return GetTagStringValue(GEDCOMTagType._RELN); }
            set { SetTagStringValue(GEDCOMTagType._RELN, value); }
        }

        public string CensusName
        {
            get { return GetTagStringValue(GEDCOMTagType._CENN); }
            set { SetTagStringValue(GEDCOMTagType._CENN, value); }
        }


        public override void SaveToStream(StreamWriter stream, int level)
        {
            // without NameValue
            int lev = level + 1;
            SaveTagsToStream(stream, lev);

            fNotes.SaveToStream(stream, level);
            fSourceCitations.SaveToStream(stream, level);
        }

        public GDMPersonalNamePieces(GDMObject owner) : base(owner)
        {
        }

        public GDMPersonalNamePieces(GDMObject owner, string tagName, string tagValue) : this(owner)
        {
            SetNameValue(tagName, tagValue);
        }
    }
}
