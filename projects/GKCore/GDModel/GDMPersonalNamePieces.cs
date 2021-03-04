/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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

namespace GDModel
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class GDMPersonalNamePieces : GDMTag
    {
        private string fPrefix;
        private string fGiven;
        private string fNickname;
        private string fSurnamePrefix;
        private string fSurname;
        private string fSuffix;
        private string fPatronymicName;
        private string fMarriedName;
        private string fReligiousName;
        private string fCensusName;


        public string Prefix
        {
            get { return fPrefix; }
            set { fPrefix = value; }
        }

        public string Given
        {
            get { return fGiven; }
            set { fGiven = value; }
        }

        public string Nickname
        {
            get { return fNickname; }
            set { fNickname = value; }
        }

        public string SurnamePrefix
        {
            get { return fSurnamePrefix; }
            set { fSurnamePrefix = value; }
        }

        public string Surname
        {
            get { return fSurname; }
            set { fSurname = value; }
        }

        public string Suffix
        {
            get { return fSuffix; }
            set { fSuffix = value; }
        }

        public string PatronymicName
        {
            get { return fPatronymicName; }
            set { fPatronymicName = value; }
        }

        public string MarriedName
        {
            get { return fMarriedName; }
            set { fMarriedName = value; }
        }

        public string ReligiousName
        {
            get { return fReligiousName; }
            set { fReligiousName = value; }
        }

        public string CensusName
        {
            get { return fCensusName; }
            set { fCensusName = value; }
        }


        public GDMPersonalNamePieces(GDMObject owner) : base(owner)
        {
            fPrefix = string.Empty;
            fGiven = string.Empty;
            fNickname = string.Empty;
            fSurnamePrefix = string.Empty;
            fSurname = string.Empty;
            fSuffix = string.Empty;
            fPatronymicName = string.Empty;
            fMarriedName = string.Empty;
            fReligiousName = string.Empty;
            fCensusName = string.Empty;
        }

        public override void Assign(GDMTag source)
        {
            GDMPersonalNamePieces otherPNP = (source as GDMPersonalNamePieces);
            if (otherPNP == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(otherPNP);

            fPrefix = otherPNP.fPrefix;
            fGiven = otherPNP.fGiven;
            fNickname = otherPNP.fNickname;
            fSurnamePrefix = otherPNP.fSurnamePrefix;
            fSurname = otherPNP.fSurname;
            fSuffix = otherPNP.fSuffix;
            fPatronymicName = otherPNP.fPatronymicName;
            fMarriedName = otherPNP.fMarriedName;
            fReligiousName = otherPNP.fReligiousName;
            fCensusName = otherPNP.fCensusName;
        }

        public override void Clear()
        {
            base.Clear();

            fPrefix = string.Empty;
            fGiven = string.Empty;
            fNickname = string.Empty;
            fSurnamePrefix = string.Empty;
            fSurname = string.Empty;
            fSuffix = string.Empty;
            fPatronymicName = string.Empty;
            fMarriedName = string.Empty;
            fReligiousName = string.Empty;
            fCensusName = string.Empty;
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && 
                string.IsNullOrEmpty(fPrefix) && string.IsNullOrEmpty(fGiven) && string.IsNullOrEmpty(fNickname) && 
                string.IsNullOrEmpty(fSurnamePrefix) && string.IsNullOrEmpty(fSurname) && string.IsNullOrEmpty(fSuffix) && 
                string.IsNullOrEmpty(fPatronymicName) && string.IsNullOrEmpty(fMarriedName) && string.IsNullOrEmpty(fReligiousName) && 
                string.IsNullOrEmpty(fCensusName);
        }
    }
}
