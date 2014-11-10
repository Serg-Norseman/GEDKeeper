/* CSourceCitation.cs
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
    // Base class for inline and xref types.
    public abstract class CSourceCitation : GEDmill.LLClasses.CParserObject
    {
        // GEDCOM data. See GEDCOM standard for details.
        public ArrayList m_alMultimediaLinks;
        public ArrayList m_alNoteStructures;
        public string m_sCertaintyAssessment;
        public ArrayList m_alTextsFromSource;

        // Constructor
        public CSourceCitation( CGedcom gedcom ) : base( gedcom )
        {
        }

        // Copy Constructor
        public abstract CSourceCitation CopyConstructor();

        // Parser
        public static CSourceCitation Parse( CGedcom gedcom, int nLevel )
        {
            CGedcomLine gedcomLine;

            // There must be one of these, it defines the object.
            if ((gedcomLine = gedcom.GetLine(nLevel, "SOUR")) == null)
            {
                // Not one of us
                return null;
            }

            // If bit after tag contains a pointer, it must be a reference sType of note
            if( gedcomLine.LinePointer != null ) //TODO
            {
                return CSourceCitationXref.Parse( gedcom, nLevel );
            }

            // Must be the inline version
            return CSourceCitationInLine.Parse( gedcom, nLevel );
        }


        // Returns false if this source is never referenced by anything on the webpages. (Set during webpage generation)
        public abstract bool RequiredInHTML { get; set; }

        // Returns true if user wants to pretend this source (record or inline) doesn't exist when creating the webpages.
        public abstract bool Restricted { get; set; } 

        // Used by copy constructors to copy base class fields.
        protected void CopyFieldsInto(CSourceCitation sc)
        {
            sc.m_alNoteStructures = new ArrayList();
            foreach (CNoteStructure ns in m_alNoteStructures)
            {
                sc.m_alNoteStructures.Add(ns.CopyConstructor());
            }
            sc.m_alMultimediaLinks = new ArrayList();
            foreach (CMultimediaLink ml in m_alMultimediaLinks)
            {
                sc.m_alMultimediaLinks.Add(ml.CopyConstructor());
            }
            sc.m_sCertaintyAssessment = m_sCertaintyAssessment;
            sc.m_alTextsFromSource = new ArrayList();
            foreach (string text in m_alTextsFromSource)
            {
                sc.m_alTextsFromSource.Add(text);
            }
        }

        // Generates the HTML for the little number that identifies the source reference in the web page
        public abstract string MakeLinkNumber( uint sourceCount, bool bComma );

        // Generates the HTML for the link to the source record
        public abstract string MakeLinkText( uint sourceCount );
        
        // Returns the xref if there is one
        public abstract string GetXref();

        // Returns the GEDCOM "Where withing Source" field if there is one
        public abstract string GetWhereWithinSource();
        
        // Returns a string for the GEDCOM certainty assesment
        public string GetCertaintyAssessment()
        {
            string certainty = "";
            switch( m_sCertaintyAssessment )
            {
                case "0":
                    certainty = "(unreliable evidence)";
                    break;
                case "1":
                    certainty = "(questionable evidence)";
                    break;
                case "2":
                    certainty = "(secondary evidence)";
                    break;
                case "3":
                    certainty = "(primary evidence)";
                    break;
                default:
                    certainty = "";
                    break;
            }
            return certainty;
        }

        // Used to ensure the each source is added to the list at the bottom of the web page only once.
        public bool SameAs( CSourceCitation sc )
        {
            if( sc is CSourceCitationXref )
            {
                return SameAs( (CSourceCitationXref)sc );
            }
            else if( sc is CSourceCitationInLine )
            {
                return SameAs( (CSourceCitationInLine)sc );
            }
            // Should really compare texts from source but this will only help if user has typed in exactly the same text twice, so for now just return false.
            return false;
        }

        // Used to ensure the each source is added to the list at the bottom of the web page only once.
        public abstract bool SameAs( CSourceCitationXref sc );

        // Used to ensure the each source is added to the list at the bottom of the web page only once.
        public abstract bool SameAs( CSourceCitationInLine sc );

        // Enables the source record to refer back to the pages that reference it.
        public abstract void AddBackreference( CBackReference br );

        // In GEDCOM the picture might be stored against the citation, but 
        // for GEDmill we can only show the picture against the source record.
        public abstract void AddPicFromCitationToRecord();
    }


}
