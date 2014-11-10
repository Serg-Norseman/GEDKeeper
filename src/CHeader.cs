/* CHeader.cs
 * 
 * Copyright 2009 Alexander Curtis <alex@logicmill.com>
 * This file is part of GEDmill - A family history website creator
 * 
 * GEDmill is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * GEDmill is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GEDmill.  If not, see <http://www.gnu.org/licenses/>.
 *
 *
 * History:  
 * 10Dec08 AlexC          Migrated from GEDmill 1.10
 *
 */

using System;
using System.Collections;

namespace GEDmill
{
    // Structure used to store the header info from the GEDCOM file.
    // Note : This info not used at the moment (10Dec08)
    public class CHeader
    {       
        public string m_sApprovedSystemId;
        public string m_sVersionNumber;
        public string m_sNameOfBusiness;
        public string m_sNameOfProduct;
        public string m_sAddressLine;
        public string m_sAddressLine1;
        public string m_sAddressLine2;
        public string m_sAddressLine3;
        public string m_sAddressCity;
        public string m_sAddressState;
        public string m_sAddressPostalCode;
        public string m_sAddressCountry ;
        public string m_sReceivingSystemName;
        public string m_xrefSubM;
        public string m_xrefSubN;
        public string m_sFilename;
        public string m_sCopyright2;
        public string m_sLanguage;
        public string m_sGedcomContentDescription; // NOTE: This to be email-obfuscated if ever used in html
        public string m_sTransmissionDate;
        public string m_sTransmissionTime;
        public string m_sGedcomVersionNumber;
        public string m_sGedcomForm;
        public string m_sCharacterSet;
        public string m_sCharacterSetVersion;
        public string m_sPlaceHierarchy;
        public ArrayList m_alPhone;
        public ArrayList m_alEmail;
        public ArrayList m_alFax;
        public ArrayList m_alWww;
        public string m_sNameOfSourceData;
        public string m_sPublicationDate;
        public string m_sCopyrightSourceData;

        // Constructor
        public CHeader()
        {
            m_alPhone = new ArrayList();
            m_alEmail = new ArrayList();
            m_alFax = new ArrayList();
            m_alWww = new ArrayList();
        }
    }
}
