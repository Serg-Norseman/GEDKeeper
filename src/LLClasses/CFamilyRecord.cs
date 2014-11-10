/* CFamilyRecord.cs
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
    // GEDCOM 'FAM'. See GEDCOM standard for details on GEDCOM data.
    public class CFamilyRecord : GEDmill.LLClasses.CRecord
    {
        // GEDCOM data
        public string m_sRestrictionNotice;
        public ArrayList m_alFamilyEventStructures;
        public string m_xrefHusband;
        public string m_xrefWife;
        public ArrayList m_alXrefsChildren;
        public ArrayList m_alChildren;
        public string m_sCountOfChildren;
        public ArrayList m_alXrefSubms;
        public ArrayList m_alLdsSpouseSealings;
        public ArrayList m_alNoteStructures;
        public ArrayList m_alSourceCitations;
        public ArrayList m_alMultimediaLinks;
        public string m_sStatus; // Family Historian specific: _STAT

        // Constructor
        public CFamilyRecord( CGedcom gedcom ) : base( gedcom )
        {           
            m_alFamilyEventStructures = new ArrayList();
            m_alXrefsChildren = new ArrayList();
            m_alXrefSubms = new ArrayList();
            m_alLdsSpouseSealings = new ArrayList();
            m_alNoteStructures = new ArrayList();
            m_alSourceCitations = new ArrayList();
            m_alMultimediaLinks = new ArrayList();
        }

        // Parser
        public static CFamilyRecord Parse( CGedcom gedcom, int nLevel )
        {
            CGedcomLine gedcomLine;
            bool bParsingFinished;

            // Temporary holders for class members.
            CFamilyEventStructure fes;
            CLdsOrdinance lss;
            CNoteStructure ns;
            CSourceCitation sc;
            CMultimediaLink ml;
            int nChildPositionInFile = 0;

            // Without an xref header, we can't continue
            if ((gedcomLine = gedcom.GetLine(nLevel, "FAM")) == null)
            {
                // Not one of us
                return null;
            }
            
            CFamilyRecord fr = new CFamilyRecord( gedcom );

            fr.m_xref = gedcomLine.XrefID;
            gedcom.IncrementLineIndex(1);

            do
            {
                bParsingFinished = true;

                
                // Test for underscore items first so that parser doesn't skip them later
                if( (gedcomLine = gedcom.GetLine(nLevel+1, "_STAT")) != null || (gedcomLine = gedcom.GetLine(nLevel+1, "_MSTAT")) != null ) // Family tree maker (export as "FTW" format)
                {
                    fr.m_sStatus = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }           
                // Let Record have a go at parsing the rest
                else if( fr.ParseRecord( gedcom, nLevel ) )
                {
                    bParsingFinished = false;
                    continue;
                }
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "RESN")) != null )
                {
                    fr.m_sRestrictionNotice = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }
                else if( (fes = CFamilyEventStructure.Parse( gedcom, nLevel+1)) != null )
                {
                    fr.m_alFamilyEventStructures.Add( fes );
                    bParsingFinished = false;
                }               
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "HUSB")) != null )
                {
                    fr.m_xrefHusband = gedcomLine.LinePointer;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "WIFE")) != null )
                {
                    fr.m_xrefWife = gedcomLine.LinePointer;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }       
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "CHIL")) != null )
                {
                    fr.m_alXrefsChildren.Add( new CChildXref( nChildPositionInFile++, gedcomLine.LinePointer ) );
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }       
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "NCHI")) != null )
                {
                    fr.m_sCountOfChildren = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }               
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "SUBM")) != null )
                {
                    fr.m_alXrefSubms.Add( gedcomLine.LinePointer );
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }               
                else if( (lss = CLdsOrdinance.Parse( gedcom, nLevel+1 )) != null )
                {
                    fr.m_alLdsSpouseSealings.Add( lss );
                    bParsingFinished = false;
                }   
                else if( (sc = CSourceCitation.Parse( gedcom, nLevel+1 )) != null )
                {
                    fr.m_alSourceCitations.Add( sc );
                    bParsingFinished = false;
                }               
                else if( (ml = CMultimediaLink.Parse( gedcom, nLevel+1 )) != null )
                {
                    fr.m_alMultimediaLinks.Add( ml );
                    bParsingFinished = false;
                }
                else if( (ns = CNoteStructure.Parse( gedcom, nLevel+1 )) != null )
                {
                    fr.m_alNoteStructures.Add( ns );
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

            return fr;
        }


        // This is a bit nasty. It's to overcome the way that Family Historian
        // puts a MARR event, though sets the fr status to a string like
        // "Never Married", so they weren't actually married at all!
        public bool WereTheyReallyMarried()
        {
            if (m_sStatus == "Never Married")
            {
                return false;
            }
            if (m_sStatus == "Unmarried Couple")
            {
                return false;
            }
            if (m_sStatus == "Unknown")
            {
                return false;
            }
            // Note that Legacy 6.0 uses _STAT for the same purpose, but with the value "Unmarried"
            if (m_sStatus == "Unmarried")
            {
                return false;
            }

            // Family tree maker. Export GEDCOM in FTW format
            if (m_sStatus == "Friends")
            {
                return false;
            }
            if (m_sStatus == "Other")
            {
                return false;
            }
            if (m_sStatus == "Partners")
            {
                return false;
            }
            if (m_sStatus == "Private")
            {
                return false;
            }
            if (m_sStatus == "Single")
            {
                return false;
            }
            if (m_sStatus == "Unknown")
            {
                return false;
            }
            if (m_sStatus == "Married/Partner")
            {
                return false;
            }
            return true;
        }

        // Gets the first event of the given type
        public CEventDetail GetEvent( string sType )
        {
            foreach( CFamilyEventStructure fes in m_alFamilyEventStructures )
            {
                if( fes.Type == sType )
                {
                    return fes.m_eventDetail;
                }
            }
            return null;
        }

        // Constructs the m_alChildren array.
        public void AddChildren( CGedcom gedcom )
        {
            m_alChildren = new ArrayList();

            foreach( CChildXref cxr in m_alXrefsChildren )
            {
                if( cxr != null && cxr.m_xref != "" )
                {
                    CIndividualRecord ir = gedcom.GetIndividualRecord( cxr.m_xref );
                    if( ir != null )
                    {
                        m_alChildren.Add( new CChild( cxr.m_nPositionInFile, ir ) );
                    }
                }
            }
            m_alChildren.Sort( new CChild.Comparer() );
        }

        // Gets the n'th child in this family, in order of birth date.
        // Need to call AddChildren before calling this method.
        public CIndividualRecord GetChildByBirthDate( int n )
        {
            if( n < 0 || m_alChildren == null || m_alChildren.Count <= n )
            {
                return null;
            }
            return ((CChild)m_alChildren[ n ]).m_ir;
        }

        // Returns the n'th child of the family, according to the order in which they appear in the GEDCOM.
        // Need to call AddChildren before calling this method.
        public CIndividualRecord GetChildByPositionInFile(int n)
        {
            foreach( CChild ch in m_alChildren )
            {
                if( ch.m_nPositionInFile == n )
                {
                    return ch.m_ir;
                }
            }
            return null;
        }

        // Compares two family records by date of marriage, (or event most like marriage)
        public class FamilyComparer : IComparer
        {
            public int Compare(object x, object y) 
            {
                // Nulls go at the end
                if( x == null && y == null )
                {
                    return 0;
                }
                else if( x == null )
                {
                    return 1;
                }
                else if( y == null )
                {
                    return -1;
                }


                CPGDate date1;
                CPGDate date2;
                // Go through all events for this fr, pick the date with the best quality
                date1 = ((CFamilyRecord)x).GetBestDate();
                date2 = ((CFamilyRecord)y).GetBestDate();

                if( date1 == null && date2 == null )
                {
                    return 0;
                }
                else if( date1 == null )
                {
                    return 1;
                }
                else if( date2 == null )
                {
                    return -1;
                }

                return date1.CompareTo( date2 );
            }
        }   

        // Tries to find the best date for the marriage of the parents in this family.
        protected CPGDate GetBestDate()
        {
            // First find marriage event, failing that contract, failing that licence, 
            // failing that banns, failing that settlement, failing that engagement, failing that divorce,
            // failing that divorce filing, failing that anulment, failing that any other event, failing 
            // that birth of first irSibling, failing that original order

            CPGDate date = null;
            uint uBestQuality = 0;

            if( m_alFamilyEventStructures != null )
            {
                foreach( CFamilyEventStructure fes in m_alFamilyEventStructures )
                {
                    if( fes != null && fes.Type != null && fes.Type.Length > 0 && fes.m_eventDetail != null)
                    {
                        uint uQuality = 0;
                        switch( fes.Type )
                        {
                            case "ANUL":
                                uQuality = 2;
                                break;
                            case "ENGA":
                                uQuality = 5;
                                break;
                            case "MARB":
                                uQuality = 7;
                                break;
                            case "MARC":
                                uQuality = 9;
                                break;
                            case "MARR":
                                uQuality = 10;
                                break;
                            case "MARL":
                                uQuality = 8;
                                break;
                            case "MARS":
                                uQuality = 6;
                                break;
                            case "DIV":
                                uQuality = 4;
                                break;
                            case "DIVF":
                                uQuality = 3;
                                break;
                            default:
                                uQuality = 1;
                                break;
                        }
                        if( uQuality > uBestQuality )
                        {
                            uBestQuality = uQuality;
                            date = fes.m_eventDetail.m_dateValue;
                        }
                    }
                }

                if( date == null )
                {
                    // Find birth of first irSibling
                    // Assumes Sort has already been called on this object to ensure children are in correct order
                    foreach( CChildXref cxr in m_alXrefsChildren )
                    {
                        CIndividualRecord ir = Gedcom.GetIndividualRecord( cxr.m_xref );
                        if( ir != null )
                        {
                            CPGQualifiedDate birthDate = ir.BirthDate;
                            if( birthDate != null )
                            {
                                date = birthDate.m_date;
                                break;
                            }
                        }
                    }
                }
            }

            return date;
        }

        // Gets the parent who isn't the given individual
        public CIndividualRecord GetSpouse(CIndividualRecord ir)
        {
            CIndividualRecord irSpouse = null;
            if (ir != null)
            {
                if (m_xrefHusband == ir.m_xref)
                {
                    irSpouse = Gedcom.GetIndividualRecord(m_xrefWife);
                }
                else
                {
                    irSpouse = Gedcom.GetIndividualRecord(m_xrefHusband);
                }
            }
            return irSpouse;
        }

        // Returns true if the given individual is the male parent in this family
        public bool IsHusband(CIndividualRecord ir)
        {
            return (ir != null && m_xrefHusband == ir.m_xref);
        }

        // Returns true if the given individual is the female parent in this family
        public bool IsWife(CIndividualRecord ir)
        {
            return (ir != null && m_xrefWife == ir.m_xref);
        }

    }
}
