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

using System.IO;

namespace GKCommon.GEDCOM
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class GEDCOMPersonalNamePieces : GEDCOMTagWithLists
    {
        public string Prefix
        {
            get { return GetTagStringValue("NPFX"); }
            set { SetTagStringValue("NPFX", value); }
        }

        public string Given
        {
            get { return GetTagStringValue("GIVN"); }
            set { SetTagStringValue("GIVN", value); }
        }

        public string Nickname
        {
            get { return GetTagStringValue("NICK"); }
            set { SetTagStringValue("NICK", value); }
        }

        public string SurnamePrefix
        {
            get { return GetTagStringValue("SPFX"); }
            set { SetTagStringValue("SPFX", value); }
        }

        public string Surname
        {
            get { return GetTagStringValue("SURN"); }
            set { SetTagStringValue("SURN", value); }
        }

        public string Suffix
        {
            get { return GetTagStringValue("NSFX"); }
            set { SetTagStringValue("NSFX", value); }
        }



        public string PatronymicName
        {
            get { return GetTagStringValue("_PATN"); }
            set { SetTagStringValue("_PATN", value); }
        }

        // as BKW6
        public string MarriedName
        {
            get { return GetTagStringValue("_MARN"); }
            set { SetTagStringValue("_MARN", value); }
        }

        // as BKW6
        public string ReligiousName
        {
            get { return GetTagStringValue("_RELN"); }
            set { SetTagStringValue("_RELN", value); }
        }

        // as BKW6
        public string CensusName
        {
            get { return GetTagStringValue("_CENN"); }
            set { SetTagStringValue("_CENN", value); }
        }


        public override void SaveToStream(StreamWriter stream)
        {
            SaveTagsToStream(stream);

            fNotes.SaveToStream(stream);
            fSourceCitations.SaveToStream(stream);
        }

        public GEDCOMPersonalNamePieces(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
        }
    }
}
