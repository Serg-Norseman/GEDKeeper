/* CSourceCitationXref.cs
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
using GEDmill.HTMLClasses;

namespace GEDmill.LLClasses
{
    // GEDCOM 'SOUR'. See GEDCOM standard for details on GEDCOM data.
    public class CSourceCitationXref : GEDmill.LLClasses.CSourceCitation
    {
        // GEDCOM data
        private string m_sWhereWithinSource;
        private string m_sEventTypeCitedFrom;
        private string m_sRoleInEvent;
        private string m_sEntryRecordingDate;
        public string m_xref;

        // Constructor
        public CSourceCitationXref( CGedcom gedcom ) : base( gedcom )
        {
        }

        // Copy constructor
        public override CSourceCitation CopyConstructor()
        {
            CSourceCitationXref sc = new CSourceCitationXref( Gedcom );
            CopyFieldsInto( sc );
            sc.m_sWhereWithinSource = m_sWhereWithinSource;
            sc.m_sEventTypeCitedFrom = m_sEventTypeCitedFrom;
            sc.m_sRoleInEvent = m_sRoleInEvent;
            sc.m_sEntryRecordingDate = m_sEntryRecordingDate;
            sc.m_xref = m_xref;
            return sc;
        }

        // Parser
        public static new CSourceCitationXref Parse( CGedcom gedcom, int nLevel )
        {
            CGedcomLine gedcomLine;

            // Temporary holders for class members. Saves constructing a class early.
            string xref;
            string sWhereWithinSource = "";
            string sEventTypeCitedFrom = "";
            string sRoleInEvent = "";
            string sEntryRecordingDate = "";
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
            xref = gedcomLine.LinePointer;

            // Parsing is going well enough to say that we definitely have one of these, 
            // so we can adjust the gedcom now.
            gedcom.IncrementLineIndex(1);

            bool bParsingFinished;
            do
            {
                bParsingFinished = true;

                // There may be one of these, standard specifies {0:1}
                if( (gedcomLine = gedcom.GetLine(nLevel+1, "PAGE")) != null )
                {
                    sWhereWithinSource = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "EVEN")) != null )
                {
                    sEventTypeCitedFrom = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    if( (gedcomLine = gedcom.GetLine(nLevel+2, "ROLE")) != null )
                    {
                        sRoleInEvent = gedcomLine.LineItem;
                        gedcom.IncrementLineIndex(1);
                    }
                    bParsingFinished = false;
                }
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "DATA")) != null )
                {
                    gedcom.IncrementLineIndex(1);

                    bool bParsingFinished2;
                    do
                    {
                        bParsingFinished2 = true;
                        if( (gedcomLine = gedcom.GetLine(nLevel+2, "DATE")) != null )
                        {
                            sEntryRecordingDate = gedcomLine.LineItem;
                            gedcom.IncrementLineIndex(1);
                            bParsingFinished2 = false;
                        }
                        else if( (gedcomLine = gedcom.GetLine(nLevel+2, "TEXT")) != null )
                        {
                            string textFromSource = gedcomLine.LineItem;
                            gedcom.IncrementLineIndex(1);

                            bool bParsingFinished3;
                            do
                            {
                                bParsingFinished3 = true;
                                if( (gedcomLine = gedcom.GetLine(nLevel+3, "CONC")) != null )
                                {
                                    textFromSource += gedcomLine.LineItem;
                                    gedcom.IncrementLineIndex(1);
                                    bParsingFinished3 = false;
                                }
                                else if( (gedcomLine = gedcom.GetLine(nLevel+3, "CONT")) != null )
                                {
                                    textFromSource += "\n" + gedcomLine.LineItem;
                                    gedcom.IncrementLineIndex(1);
                                    bParsingFinished3 = false;
                                }
                            }
                            while( !bParsingFinished3 );

                            alTextsFromSource.Add( textFromSource );
                            bParsingFinished2 = false;
                        }
                    }
                    while( !bParsingFinished2 );
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
            CSourceCitationXref sc = new CSourceCitationXref( gedcom );
            sc.m_xref = xref;
            sc.m_sWhereWithinSource = sWhereWithinSource;
            sc.m_sEventTypeCitedFrom = sEventTypeCitedFrom;
            sc.m_sRoleInEvent = sRoleInEvent;
            sc.m_sEntryRecordingDate = sEntryRecordingDate;
            sc.m_alTextsFromSource = alTextsFromSource;
            sc.m_alMultimediaLinks = alMultimediaLinks;
            sc.m_alNoteStructures = alNoteStructures;
            sc.m_sCertaintyAssessment = sCertaintyAssessment;
            return sc;
        }

        // Returns a unique number for this source citation, in HTML suitable to use in the page
        public override string MakeLinkNumber(uint uSourceCount, bool bComma)
        {
            CSourceRecord sr = Gedcom.GetSourceRecord( m_xref );
            
            string sComma = bComma?",":"";
            
            if( sr == null || sr.ContainsAnyInformation() == false )
            {
                return String.Concat("<span class=\"reference\">",sComma, uSourceCount.ToString(), "</span>");
            }

            return String.Concat("<span class=\"reference\">", sComma, "<a href=\"sour", m_xref, ".", MainForm.s_config.m_sHtmlExtension, "\">", uSourceCount.ToString(), "</a></span>");
        }

        // Returns a string to use in the list of references at the bottom of the page
        public override string MakeLinkText(uint uSourceCount)
        {
            CSourceRecord sr = Gedcom.GetSourceRecord( m_xref );

            if (sr == null)
            {
                return String.Concat(uSourceCount.ToString(), ". unknown source");
            }

            if( sr.ContainsAnyInformation() == false )
            {
                return String.Concat( uSourceCount.ToString(), ". ", sr.DescriptiveTitle );
            }

            return String.Concat("<a href=\"sour", m_xref, ".", MainForm.s_config.m_sHtmlExtension, "\">", uSourceCount.ToString(), ". ", sr.DescriptiveTitle, "</a>");
        }

        // Accessor
        public override string GetXref()
        {
            return m_xref;
        }

        // Returns the GEDCOM "Where withing Source" field
        public override string GetWhereWithinSource()
        {
            return m_sWhereWithinSource;
        }

        // Tests to see if this source citation displays the same as the xref citation given.
        // Used to ensure the each source is added to the list at the bottom of the web page only once.
        public override bool SameAs(CSourceCitationXref sc)
        {
            return( this.m_sWhereWithinSource == sc.m_sWhereWithinSource 
                //not displayed atm so ignore: && this.m_eventTypeCitedFrom == sc.m_eventTypeCitedFrom
                //not displayed atm so ignore: && this.m_roleInEvent == sc.m_roleInEvent
                //not displayed atm so ignore: && this.m_entryRecordingDate == sc.m_entryRecordingDate
                && this.m_xref == sc.m_xref
                && m_sCertaintyAssessment == sc.m_sCertaintyAssessment );           
        }

        // Tests to see if this source citation displays the same as the inline citation given.
        // Used to ensure the each source is added to the list at the bottom of the web page only once.
        public override bool SameAs(CSourceCitationInLine sc)
        {
            return false;
        }

        // Returns false if this source is never referenced by anything on the webpages. (Set during webpage generation)
        public override bool RequiredInHTML
        {
            get
            {
                bool result = false;
                CSourceRecord sr = Gedcom.GetSourceRecord( m_xref );
                if( sr != null )
                {
                    result = sr.m_bRequiredInHTML;
                }
                return result;
            }

            set
            {
                CSourceRecord sr = Gedcom.GetSourceRecord( m_xref );
                if( sr != null )
                {
                    sr.m_bRequiredInHTML = value;
                }           
            }
        }

        // Returns true if user wants to pretend this source (record or inline) doesn't exist when creating the webpages.
        public override bool Restricted
        {
            get
            {
                bool result = false;
                CSourceRecord sr = Gedcom.GetSourceRecord( m_xref );
                if( sr != null )
                {
                    result = sr.Restricted;
                }
                return result;
            }

            set
            {
                CSourceRecord sr = Gedcom.GetSourceRecord( m_xref );
                if( sr != null )
                {
                    sr.Restricted = value;
                }           
            }
        }

        // Records a page that references this source
        public override void AddBackreference(CBackReference br)
        {
            CSourceRecord sr = Gedcom.GetSourceRecord( m_xref );
            if( sr != null )
            {
                sr.AddBackreference( br );
            }   
        }

        // In GEDCOM, citations can have multimedia attached. GEDmill only supports multimedia attached to records.
        public override void AddPicFromCitationToRecord()
        {
            CSourceRecord sr = Gedcom.GetSourceRecord( m_xref );
            if( sr != null )
            {
                sr.m_alMultimediaLinks.AddRange( m_alMultimediaLinks );
                m_alMultimediaLinks = null;
            }
        }

    }
}
