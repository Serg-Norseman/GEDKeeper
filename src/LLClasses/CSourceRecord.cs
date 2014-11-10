/* CSourceRecord.cs
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
    public class CSourceRecord : GEDmill.LLClasses.CISRecord
    {
        // GEDCOM data
        public ArrayList m_alEventsRecorded;
        public string m_sResponsibleAgency;
        // Notes contained in the DATA section of the source record. See also CRecord::m_noteStructures.
        public ArrayList m_alDataNoteStructures;
        // NOTE: This is to be email-obfuscated if ever used in html
        public string m_sSourceOriginator; 
        public string m_sSourceDescriptiveTitle;
        public string m_sSourceFiledByEntry;
        public string m_sSourcePublicationFacts;
        public string m_sTextFromSource;
        public ArrayList m_alSourceRepositoryCitations;
        // False if this source is never referenced by anything on the webpages. (Set through source citation, during webpage generation)
        public bool m_bRequiredInHTML; 
        // List of references from this back to referring records
        public ArrayList m_alBackreferences; 

        // Constructor
        public CSourceRecord( CGedcom gedcom ) : base( gedcom )
        {
            m_alEventsRecorded = new ArrayList();
            m_alSourceRepositoryCitations = new ArrayList();
            m_alDataNoteStructures = new ArrayList();
            m_bRequiredInHTML = false;
            m_alBackreferences = new ArrayList();
        }

        // Parser
        public static CSourceRecord Parse( CGedcom gedcom, int nLevel )
        {
            CGedcomLine gedcomLine;
            bool bParsingFinished;

            // Temporary holders for class members.
            CEventRecorded er;
            CSourceRepositoryCitation src;
            CNoteStructure ns;
            CMultimediaLink ml;

            // Without an xref header, we can't continue
            if ((gedcomLine = gedcom.GetLine(nLevel, "SOUR")) == null)
            {
                // Not one of us
                return null;
            }
            
            CSourceRecord sr = new CSourceRecord( gedcom );

            System.Text.StringBuilder sbTextFromSource = new System.Text.StringBuilder( 64 );

            sr.m_xref = gedcomLine.XrefID;
            gedcom.IncrementLineIndex(1);

            do
            {
                bParsingFinished = true;

                // Let Record have a go at parsing the rest
                if( sr.ParseRecord( gedcom, nLevel ) )
                {
                    bParsingFinished = false;
                    continue;
                }
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "DATA")) != null )
                {
                    gedcom.IncrementLineIndex(1);
                    bool bParsingFinished2;
                    do
                    {
                        bParsingFinished2 = true;
                        if( (er = CEventRecorded.Parse( gedcom, nLevel+2)) != null )
                        {
                            sr.m_alEventsRecorded.Add( er );
                            bParsingFinished2 = false;
                        }               
                        else if( (gedcomLine = gedcom.GetLine(nLevel+2, "AGNC")) != null )
                        {
                            sr.m_sResponsibleAgency = gedcomLine.LineItem;
                            gedcom.IncrementLineIndex(1);
                            bParsingFinished2 = false;
                        }
                        else if( (ns = CNoteStructure.Parse( gedcom, nLevel+2 )) != null )
                        {
                            sr.m_alDataNoteStructures.Add( ns );
                            bParsingFinished2 = false;
                        }               
                    } 
                    while( !bParsingFinished2 );
                    bParsingFinished = false;
                }
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "AUTH")) != null )
                {
                    sr.m_sSourceOriginator = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bool bParsingFinished2;
                    do
                    {
                        bParsingFinished2 = true;
                        if( (gedcomLine = gedcom.GetLine(nLevel+2, "CONC")) != null )
                        {
                            sr.m_sSourceOriginator += gedcomLine.LineItem;
                            gedcom.IncrementLineIndex(1);
                            bParsingFinished2 = false;
                        }
                        else if( (gedcomLine = gedcom.GetLine(nLevel+2, "CONT")) != null )
                        {
                            sr.m_sSourceOriginator += "\n" + gedcomLine.LineItem;
                            gedcom.IncrementLineIndex(1);
                            bParsingFinished2 = false;
                        }
                    } 
                    while( !bParsingFinished2 );
                    bParsingFinished = false;
                }   
                else if( ((gedcomLine = gedcom.GetLine(nLevel+1, "TITL")) != null) || ((gedcomLine = gedcom.GetLine(nLevel+1, "NAME")) != null) )
                {
                    sr.m_sSourceDescriptiveTitle = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bool bParsingFinished2;
                    do
                    {
                        bParsingFinished2 = true;
                        if( (gedcomLine = gedcom.GetLine(nLevel+2, "CONC")) != null )
                        {
                            sr.m_sSourceDescriptiveTitle += gedcomLine.LineItem;
                            gedcom.IncrementLineIndex(1);
                            bParsingFinished2 = false;
                        }
                        else if( (gedcomLine = gedcom.GetLine(nLevel+2, "CONT")) != null )
                        {
                            sr.m_sSourceDescriptiveTitle += "\n" + gedcomLine.LineItem;
                            gedcom.IncrementLineIndex(1);
                            bParsingFinished2 = false;
                        }
                    } 
                    while( !bParsingFinished2 );
                    bParsingFinished = false;
                }   
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "ABBR")) != null )
                {
                    sr.m_sSourceFiledByEntry = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }       
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "PUBL")) != null )
                {
                    sr.m_sSourcePublicationFacts = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bool bParsingFinished2;
                    do
                    {
                        bParsingFinished2 = true;
                        if( (gedcomLine = gedcom.GetLine(nLevel+2, "CONC")) != null )
                        {
                            sr.m_sSourcePublicationFacts += gedcomLine.LineItem;
                            gedcom.IncrementLineIndex(1);
                            bParsingFinished2 = false;
                        }
                        else if( (gedcomLine = gedcom.GetLine(nLevel+2, "CONT")) != null )
                        {
                            sr.m_sSourcePublicationFacts += "\n" + gedcomLine.LineItem;
                            gedcom.IncrementLineIndex(1);
                            bParsingFinished2 = false;
                        }
                    } 
                    while( !bParsingFinished2 );
                    bParsingFinished = false;
                }   
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "TEXT")) != null )
                {
                    // Actually want strcpy equiv here.
                    sbTextFromSource.Append( gedcomLine.LineItem ); 
                    gedcom.IncrementLineIndex(1);
                    bool bParsingFinished2;
                    do
                    {
                        bParsingFinished2 = true;
                        if( (gedcomLine = gedcom.GetLine(nLevel+2, "CONC")) != null )
                        {
                            sbTextFromSource.Append( gedcomLine.LineItem );
                            gedcom.IncrementLineIndex(1);
                            bParsingFinished2 = false;
                        }
                        else if( (gedcomLine = gedcom.GetLine(nLevel+2, "CONT")) != null )
                        {
                            sbTextFromSource.Append( "\n" + gedcomLine.LineItem );
                            gedcom.IncrementLineIndex(1);
                            bParsingFinished2 = false;
                        }
                    } 
                    while( !bParsingFinished2 );
                    bParsingFinished = false;
                }   
                else if( (src = CSourceRepositoryCitation.Parse( gedcom, nLevel+1 )) != null )
                {
                    sr.m_alSourceRepositoryCitations.Add( src );
                    bParsingFinished = false;
                }   
                else if( (ml = CMultimediaLink.Parse( gedcom, nLevel+1 )) != null )
                {
                    sr.m_alMultimediaLinks.Add( ml );
                    bParsingFinished = false;
                }
                else if( (ns = CNoteStructure.Parse( gedcom, nLevel+1 )) != null )
                {
                    sr.m_alNoteStructures.Add( ns );
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

            sr.m_sTextFromSource = sbTextFromSource.ToString();

            return sr;
        }

        // Returns a suitable string to describe the source to the user
        public string DescriptiveTitle
        {
            get
            {
                string sTitle = "";
                if (m_sSourceDescriptiveTitle == null || m_sSourceDescriptiveTitle == "")
                {
                    if (m_sSourceFiledByEntry == null || m_sSourceFiledByEntry == "")
                    {
                        return "Source " + m_xref;
                    }
                    else
                    {
                        sTitle = m_sSourceFiledByEntry;
                    }
                }
                else
                {
                    sTitle = m_sSourceDescriptiveTitle;
                }

                string sReplacement1 = sTitle.Replace( "\\n", " " );
                string sReplacement2 = sReplacement1.Replace( "\n", " " );

                return sReplacement2;
            }
        } 
        
        // Used to decide whether it's worth creating a web page for it.
        public bool ContainsAnyInformation()
        {
            if (Restricted)
            {
                return false;
            }

            if (m_alUserReferenceNumbers.Count > 0)
            {
                return true;
            }

            if (m_sAutomatedRecordId != null && m_sAutomatedRecordId != "")
            {
                return true;
            }

            foreach( CSourceRepositoryCitation src in m_alSourceRepositoryCitations )
            {
                if (src.m_xrefRepo != null)
                {
                    return true;
                }
                if (src.m_alNoteStructures != null && src.m_alNoteStructures.Count > 0)
                {
                    return true;
                }
            }

            if (m_alUniqueFileRefs.Count > 0)
            {
                return true;
            }

            if (m_sTextFromSource != null && m_sTextFromSource != "")
            {
                return true;
            }

            if (m_alNoteStructures.Count > 0)
            {
                return true;
            }

            return false;
        }

        // So far the only use of this function is to populate the sources exclusion list, in which we only want to display the first line of the title.
        public override string ToString()
        {
            string sTitle = m_sSourceDescriptiveTitle;
            if( sTitle == null || sTitle == "" )
            {
                sTitle = m_sSourceFiledByEntry;
            }

            if( sTitle == null || sTitle == "" )
            {
                return "<no title>";
            }

            string sTitleCleaned = sTitle;
            int nTitleChars = sTitleCleaned.Length;
            int nCrPos = sTitleCleaned.IndexOf( '\n' );
            if( nCrPos<0 ) nCrPos = nTitleChars;
            int nLfPos = sTitleCleaned.IndexOf( '\r' );
            if( nLfPos<0 ) nLfPos = nTitleChars;
            int nBnPos = sTitleCleaned.IndexOf( "\\n" );
            if( nBnPos<0 ) nBnPos = nTitleChars;
            int nBreakPos = nCrPos<nLfPos?nCrPos<nBnPos?nCrPos:nBnPos:nLfPos<nBnPos?nLfPos:nBnPos;
            if( nBreakPos < nTitleChars )
            {
                sTitleCleaned = sTitleCleaned.Substring(0, nBreakPos);
            }

            return sTitleCleaned;
        }

        // Returns a string containing CRLFs describing this record for use in a message box
        public override string Details()
        {
            string sDetails = String.Concat( "Record: ", m_xref, "\r\n\r\n" );
            
            sDetails += "Title: " + DescriptiveTitle + "\r\n\r\n";
            
            if( m_sTextFromSource != null && m_sTextFromSource != "" )
            {
                string sReplacement1 = m_sTextFromSource.Replace( "\\n", "\n" );
                string sReplacement2 = sReplacement1.Replace( "\n", "\r\n" );
                sDetails += "Text:\r\n" + sReplacement2 + "\r\n";
            }
            
            if( m_alNoteStructures.Count > 0 )
            {
                sDetails += "Notes:\r\n";
                foreach( CNoteStructure ns in m_alNoteStructures )
                {
                    string sNoteText = ns.Text;
                    string sReplacement1 = sNoteText.Replace( "\\n", "\n" );
                    string sReplacement2 = sReplacement1.Replace( "\n", "\r\n" );
                    sDetails += sReplacement2 + "\r\n";
                }
            }

            return sDetails;
        }

        // Call this to add a record that references this source, so that a list of all referring records can be built.
        public void AddBackreference( CBackReference br )
        {
            m_alBackreferences.Add( br );
        }

        // Returns a hashtable keyed on individual names, based on the list of backreferences, including all individuals in family records.
        public Hashtable MakeBackRefList()
        {
            Hashtable htBackrefs = new Hashtable( m_alBackreferences.Count );
            
            foreach( CBackReference br in m_alBackreferences )
            {
                ERecordType recordType = br.m_ertRecordType;
                string xref = br.m_xref;

                switch( recordType )
                {
                    case ERecordType.Individual:
                        CIndividualRecord ir = Gedcom.GetIndividualRecord( xref );
                        if( ir != null )
                        {
                            htBackrefs[ ir.Name ] = ir;
                        }
                        break;
                    case ERecordType.Family:
                        CFamilyRecord fr = Gedcom.GetFamilyRecord( xref );
                        if( fr != null )
                        {
                            string xrefHusband = fr.m_xrefHusband;
                            string xrefWife = fr.m_xrefWife;
                            if( xrefHusband != "" )
                            {
                                CIndividualRecord irHusband = Gedcom.GetIndividualRecord( xrefHusband );
                                if( irHusband != null )
                                {
                                    htBackrefs[ irHusband.Name ] = irHusband;
                                }
                            }
                            if( xrefWife != "" )
                            {
                                CIndividualRecord irWife = Gedcom.GetIndividualRecord( xrefWife );
                                if( irWife != null )
                                {
                                    htBackrefs[ irWife.Name ] = irWife;
                                }
                            }
                        }
                        break;
                    case ERecordType.Note:
                        // Don't do sources attached to notes: typeString = "note";
                        break;
                }
            }
            return htBackrefs;
        }
    }
}
