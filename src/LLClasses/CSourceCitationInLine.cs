/* CSourceCitationInLine.cs
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
    // GEDCOM 'SOUR'. See GEDCOM standard for details on GEDCOM data.
    public class CSourceCitationInLine : GEDmill.LLClasses.CSourceCitation
    {
        // True if user wants to pretend this source (record or inline) doesn't exist when creating the webpages.
        private bool m_bRestricted; 

        // The text to use for this citation
        private string m_sSourceDescription;

        // False if this source is never referenced by anything on the webpages. (Set during webpage generation)
        private bool m_bRequiredInHTML;

        // List of references from this back to referring records
        private ArrayList m_alBackreferences; 

        // Constructor
        public CSourceCitationInLine( CGedcom gedcom ) : base( gedcom )
        {
            m_bRestricted = false;
            m_alBackreferences = new ArrayList();
        }

        // Copy Constructor
        public override CSourceCitation CopyConstructor()
        {
            CSourceCitationInLine scil = new CSourceCitationInLine( Gedcom );
            CopyFieldsInto( scil );
            scil.m_bRestricted = m_bRestricted;
            scil.m_sSourceDescription = m_sSourceDescription;
            scil.m_alBackreferences = new ArrayList();
            foreach( CBackReference br in m_alBackreferences )
            {
                scil.m_alBackreferences.Add( new CBackReference( br ) );
            }
            return scil;
        }

        // Parser
        public static new CSourceCitationInLine Parse( CGedcom gedcom, int nLevel )
        {
            CGedcomLine gedcomLine;

            // Temporary holders for class members. Saves constructing a class early.
            string sSourceDescription;
            ArrayList alTextsFromSource = new ArrayList();
            CMultimediaLink multimediaLink;
            ArrayList alMultimediaLinks = new ArrayList();
            CNoteStructure noteStructure;
            ArrayList alNoteStructures = new ArrayList();
            string sCertaintyAssessment = "";

            // There must be one of these, it defines the object.
            if ((gedcomLine = gedcom.GetLine(nLevel, "SOUR")) == null)
            {
                // Not one of us
                return null; 
            }
            sSourceDescription = gedcomLine.LineItem;

            // Parsing is going well enough to say that we definitely have one of these, 
            // so we can adjust the gedcom now.
            gedcom.IncrementLineIndex(1);

            bool bParsingFinished;
            do
            {
                bParsingFinished = true;

                if( (gedcomLine = gedcom.GetLine(nLevel+1, "CONC")) != null )
                {
                    sSourceDescription += gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "CONT")) != null )
                {
                    sSourceDescription += "\n" + gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }               
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "TEXT")) != null )
                {
                    string textFromSource = gedcomLine.LineItem;

                    gedcom.IncrementLineIndex(1);
                    bool bParsingFinished3;
                    do
                    {
                        bParsingFinished3 = true;
                        if( (gedcomLine = gedcom.GetLine(nLevel+2, "CONC")) != null )
                        {
                            textFromSource += gedcomLine.LineItem;
                            gedcom.IncrementLineIndex(1);
                            bParsingFinished3 = false;
                        }
                        else if( (gedcomLine = gedcom.GetLine(nLevel+2, "CONT")) != null )
                        {
                            textFromSource += "\n" + gedcomLine.LineItem;
                            gedcom.IncrementLineIndex(1);
                            bParsingFinished3 = false;
                        }
                    }
                    while( !bParsingFinished3 );

                    alTextsFromSource.Add( textFromSource );
                    bParsingFinished = false;
                }
                else if( (multimediaLink = CMultimediaLink.Parse( gedcom, nLevel+1 )) != null )
                {
                    alMultimediaLinks.Add( multimediaLink );
                    bParsingFinished = false;
                }
                else if( (noteStructure = CNoteStructure.Parse( gedcom, nLevel+1 )) != null )
                {
                    alNoteStructures.Add( noteStructure );
                    bParsingFinished = false;
                }
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "QUAY")) != null )
                {
                    sCertaintyAssessment = gedcomLine.LineItem;
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
            CSourceCitationInLine sc = new CSourceCitationInLine( gedcom );
            sc.m_sSourceDescription = sSourceDescription;
            sc.m_alTextsFromSource = alTextsFromSource;
            sc.m_alMultimediaLinks = alMultimediaLinks;
            sc.m_alNoteStructures = alNoteStructures;
            sc.m_sCertaintyAssessment = sCertaintyAssessment;
            return sc;
        }
        
        // Returns a unique number for this source citation, in HTML suitable to use in the page
        public override string MakeLinkNumber( uint uSourceCount, bool bComma )
        {
            string sComma = bComma?",":"";
            return String.Concat("<span class=\"reference\">",sComma, uSourceCount.ToString(), "</span>");
        }
         
        // Returns a string to use in the list of references at the bottom of the page
        public override string MakeLinkText( uint uSourceCount )
        {
            return String.Concat(uSourceCount.ToString(), ". ", m_sSourceDescription );
        }

        // Accessor
        public override string GetXref()
        {
            return "";
        }

        // Returns the GEDCOM "Where withing Source" field
        public override string GetWhereWithinSource()
        {
            return "";
        }

        // Tests to see if this source citation displays the same as the xref citation given.
        // Used to ensure the each source is added to the list at the bottom of the web page only once.
        public override bool SameAs(CSourceCitationXref sc)
        {
            return false;
        }

        // Tests to see if this source citation displays the same as the inline citation given.
        // Used to ensure the each source is added to the list at the bottom of the web page only once.
        public override bool SameAs(CSourceCitationInLine sc)
        {
            return( this.m_bRestricted == sc.m_bRestricted 
                && this.m_sSourceDescription == sc.m_sSourceDescription
                && m_sCertaintyAssessment == sc.m_sCertaintyAssessment );
        }

        // Returns false if this source is never referenced by anything on the webpages. (Set during webpage generation)
        public override bool RequiredInHTML
        {
            get
            {
                return m_bRequiredInHTML;
            }

            set
            {
                m_bRequiredInHTML = value;
            }
        }

        // Returns true if user wants to pretend this source (record or inline) doesn't exist when creating the webpages.
        public override bool Restricted
        {
            get
            {
                return m_bRestricted;
            }

            set
            {
                m_bRestricted = value;
            }
        }

        // Records a page that references this source
        public override void AddBackreference( CBackReference br )
        {
            m_alBackreferences.Add( br );
        }

        // In GEDCOM, citations can have multimedia attached. GEDmill only supports multimedia attached to records.
        public override void AddPicFromCitationToRecord()
        {
            // TODO: Should really create a new source record to hold the picture.
        }
    }
}
