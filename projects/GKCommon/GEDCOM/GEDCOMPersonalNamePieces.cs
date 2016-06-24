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
            get { return base.GetTagStringValue("NPFX"); }
            set { base.SetTagStringValue("NPFX", value); }
        }

        public string Given
        {
            get { return base.GetTagStringValue("GIVN"); }
            set { base.SetTagStringValue("GIVN", value); }
        }

        public string Nickname
        {
            get { return base.GetTagStringValue("NICK"); }
            set { base.SetTagStringValue("NICK", value); }
        }

        public string SurnamePrefix
        {
            get { return base.GetTagStringValue("SPFX"); }
            set { base.SetTagStringValue("SPFX", value); }
        }

        public string Surname
        {
            get { return base.GetTagStringValue("SURN"); }
            set { base.SetTagStringValue("SURN", value); }
        }

        public string Suffix
        {
            get { return base.GetTagStringValue("NSFX"); }
            set { base.SetTagStringValue("NSFX", value); }
        }



        public string PatronymicName
        {
            get { return base.GetTagStringValue("_PATN"); }
            set { base.SetTagStringValue("_PATN", value); }
        }

        // as BKW6
        public string MarriedName
        {
            get { return base.GetTagStringValue("_MARN"); }
            set { base.SetTagStringValue("_MARN", value); }
        }

        // as BKW6
        public string ReligiousName
        {
            get { return base.GetTagStringValue("_RELN"); }
            set { base.SetTagStringValue("_RELN", value); }
        }

        // as BKW6
        public string CensusName
        {
            get { return base.GetTagStringValue("_CENN"); }
            set { base.SetTagStringValue("_CENN", value); }
        }


        public override void SaveToStream(StreamWriter stream)
        {
            this.SaveTagsToStream(stream);

            this.fNotes.SaveToStream(stream);
            this.fSourceCitations.SaveToStream(stream);
        }

        public GEDCOMPersonalNamePieces(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
        }
    }
}
