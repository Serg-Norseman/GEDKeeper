/* CAddressStructure.cs
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

namespace GEDmill.LLClasses
{
    // GEDCOM 'ADDR'. See GEDCOM standard for details on GEDCOM data.
    public class CAddressStructure : GEDmill.LLClasses.CParserObject
    {
        // GEDCOM data
        public string m_sAddressLine;
        public string m_sAddressLine1;
        public string m_sAddressLine2;
        public string m_sAddressLine3;
        public string m_sAddressCity;
        public string m_sAddressState;
        public string m_sAddressPostalCode;
        public string m_sAddressCountry;
        public ArrayList m_alPhoneNumbers;
        public ArrayList m_alAddressEmails;
        public ArrayList m_alAddressFaxes;
        public ArrayList m_alAddressWebPages;

        // Constructor
        public CAddressStructure( CGedcom gedcom ) : base( gedcom )
        {           
        }

        // Copy Constructor
        public CAddressStructure( CAddressStructure ads ) : base( ads.Gedcom )
        {
            m_sAddressLine = ads.m_sAddressLine;
            m_sAddressLine1 = ads.m_sAddressLine1;
            m_sAddressLine2 = ads.m_sAddressLine2;
            m_sAddressLine3 = ads.m_sAddressLine3;
            m_sAddressCity = ads.m_sAddressCity;
            m_sAddressState = ads.m_sAddressState;
            m_sAddressPostalCode = ads.m_sAddressPostalCode;
            m_sAddressCountry = ads.m_sAddressCountry;
            m_alPhoneNumbers = new ArrayList();
            foreach( string pn in ads.m_alPhoneNumbers )
            {
                m_alPhoneNumbers.Add( pn );
            }
            m_alAddressEmails = new ArrayList();
            foreach( string ae in ads.m_alAddressEmails )
            {
                m_alAddressEmails.Add( ae );
            }
            m_alAddressFaxes = new ArrayList();
            foreach( string af in ads.m_alAddressFaxes )
            {
                m_alAddressFaxes.Add( af );
            }
            m_alAddressWebPages = new ArrayList();
            foreach( string awp in ads.m_alAddressWebPages )
            {
                m_alAddressWebPages.Add( awp );
            }
        }

        // Parser
        public static CAddressStructure Parse( CGedcom gedcom, int nLevel )
        {
            CGedcomLine gedcomLine;
            string sAddressLine;

            // There must be one of these, it defines the object.
            if ((gedcomLine = gedcom.GetLine(nLevel, "ADDR")) == null)
            {
                // Not one of us
                return null;
            }
            sAddressLine = gedcomLine.LineItem;
            gedcom.IncrementLineIndex(1);

            // Temporary holders for class members. Saves constructing a class early.
            string sAddressLine1 = "";
            string sAddressLine2 = "";
            string sAddressLine3 = "";
            string sAddressCity = "";
            string sAddressState = "";
            string sAddressPostalCode = "";
            string sAddressCountry = "";
            ArrayList alPhoneNumbers = new ArrayList();
            ArrayList alAddressEmails = new ArrayList();
            ArrayList alAddressFaxes = new ArrayList();
            ArrayList alAddressWebPages = new ArrayList();

            bool bParsingFinished;
            do
            {
                bParsingFinished = true;

                if( (gedcomLine = gedcom.GetLine(nLevel+1, "CONT")) != null )
                {
                    sAddressLine += "\n" + gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }               
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "ADR1")) != null )
                {
                    sAddressLine1 = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }   
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "ADR2")) != null )
                {
                    sAddressLine2 = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }   
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "ADR3")) != null )
                {
                    sAddressLine3 = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }   
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "CITY")) != null )
                {
                    sAddressCity = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }   
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "STAE")) != null )
                {
                    sAddressState = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }   
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "POST")) != null )
                {
                    sAddressPostalCode = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }   
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "CTRY")) != null )
                {
                    sAddressCountry = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }   
                else if( (gedcomLine = gedcom.GetLine(nLevel, "PHON")) != null )
                {
                    alPhoneNumbers.Add( gedcomLine.LineItem );
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }   
                else if( (gedcomLine = gedcom.GetLine(nLevel, "EMAIL")) != null )
                {
                    alAddressEmails.Add( gedcomLine.LineItem );
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }   
                else if( (gedcomLine = gedcom.GetLine(nLevel, "FAX")) != null )
                {
                    alAddressFaxes.Add( gedcomLine.LineItem );
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }   
                else if( (gedcomLine = gedcom.GetLine(nLevel, "WWW")) != null )
                {
                    alAddressWebPages.Add( gedcomLine.LineItem );
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }
                else if( ( gedcomLine = gedcom.GetLine()).Level > nLevel )
                {
                    LogFile.TheLogFile.WriteLine( LogFile.DT_GEDCOM, LogFile.EDebugLevel.Warning, "Unknown tag :" );
                    LogFile.TheLogFile.WriteLine( LogFile.DT_GEDCOM, LogFile.EDebugLevel.Warning, gedcomLine.ToString() );
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }
            }
            while( !bParsingFinished );
            
            // Parsing went ok. Construct a new object and return it.
            CAddressStructure ads = new CAddressStructure( gedcom );
            ads.m_sAddressLine = sAddressLine;
            ads.m_sAddressLine1 = sAddressLine1;
            ads.m_sAddressLine2 = sAddressLine2;
            ads.m_sAddressLine3 = sAddressLine3;
            ads.m_sAddressCity = sAddressCity;
            ads.m_sAddressState = sAddressState;
            ads.m_sAddressPostalCode = sAddressPostalCode;
            ads.m_sAddressCountry = sAddressCountry;
            ads.m_alPhoneNumbers = alPhoneNumbers;
            ads.m_alAddressEmails = alAddressEmails;
            ads.m_alAddressFaxes = alAddressFaxes;
            ads.m_alAddressWebPages = alAddressWebPages;
            return ads;
        }

        // Accessor
        public string GetUrl()
        {
            if( m_alAddressWebPages.Count > 0 )
            {
                return (string)m_alAddressWebPages[0];
            }
            return "";
        }

        // For display
        public override string ToString()
        {
            string sResult = "";
            if( m_sAddressLine.Length > 0 )
            {
                sResult += m_sAddressLine;
            }
            if( m_sAddressLine1.Length > 0 )
            {
                if( sResult.Length > 0 )
                {
                    sResult += ", ";
                }
                sResult += m_sAddressLine1;
            }
            if( m_sAddressLine2.Length > 0 )
            {
                if( sResult.Length > 0 )
                {
                    sResult += ", ";
                }
                sResult += m_sAddressLine2;
            }
            if( m_sAddressLine3.Length > 0 )
            {
                if( sResult.Length > 0 )
                {
                    sResult += ", ";
                }
                sResult += m_sAddressLine3;
            }
            if( m_sAddressCity.Length > 0 )
            {
                if( sResult.Length > 0 )
                {
                    sResult += ", ";
                }
                sResult += m_sAddressCity;
            }
            if( m_sAddressState.Length > 0 )
            {
                if( sResult.Length > 0 )
                {
                    sResult += ", ";
                }
                sResult += m_sAddressState;
            }
            if( m_sAddressPostalCode.Length > 0 )
            {
                if( sResult.Length > 0 )
                {
                    sResult += ", ";
                }
                sResult += m_sAddressPostalCode;
            }
            if( m_sAddressCountry.Length > 0 )
            {
                if( sResult.Length > 0 )
                {
                    sResult += ", ";
                }
                sResult += m_sAddressCountry;
            }
            
            return( sResult );
        }
    }
}
