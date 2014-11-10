/* CIndividualRecord.cs
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
    // GEDCOM 'INDI'. See GEDCOM standard for details on GEDCOM data.
    public class CIndividualRecord : GEDmill.LLClasses.CISRecord
    {
        public enum EVisibility
        {
            Invisible,
            Restricted,
            Visible
        };

        // GEDCOM data
        public string m_sRestrictionNotice;
        public ArrayList m_alPersonalNameStructures;
        public string m_sSexValue;
        // Also includes AttributeStructures:
        public ArrayList m_alIndividualEventStructures;
        public ArrayList m_alLdsIndividualOrdinances;
        public ArrayList m_alChildToFamilyLinks;
        public ArrayList m_alSpouseToFamilyLinks;
        public ArrayList m_alXrefSubms;
        public ArrayList m_alAssociationStructures;
        public ArrayList m_alXrefAlias;
        public ArrayList m_alXrefAncis;
        public ArrayList m_alXrefDesis;
        public string m_sPermanentRecordFileNumber;
        public string m_sAncestralFileNumber;
        public ArrayList m_alSourceCitations;
        public string m_sStillLiving;

        // Constructor
        public CIndividualRecord( CGedcom gedcom ) : base( gedcom )
        {

            m_alPersonalNameStructures = new ArrayList();
            m_alIndividualEventStructures = new ArrayList();
            m_alLdsIndividualOrdinances = new ArrayList();
            m_alChildToFamilyLinks = new ArrayList();
            m_alSpouseToFamilyLinks = new ArrayList();
            m_alXrefSubms = new ArrayList();
            m_alAssociationStructures = new ArrayList();
            m_alXrefAlias = new ArrayList();
            m_alXrefAncis = new ArrayList();
            m_alXrefDesis = new ArrayList();
            m_alSourceCitations = new ArrayList();

        }
        
        // Parser
        public static CIndividualRecord Parse( CGedcom gedcom, int nLevel )
        {
            CGedcomLine gedcomLine;
            bool bParsingFinished;

            // Temporary holders for class members.
            CPersonalNameStructure personalNameStructure;
            CIndividualEventStructure individualEventStructure;
            CLdsOrdinance ldsIndividualOrdinance;
            CChildToFamilyLink childToFamilyLink;
            CSpouseToFamilyLink spouseToFamilyLink;
            CAssociationStructure associationStructure;
            CNoteStructure noteStructure;
            CSourceCitation sourceCitation;
            CMultimediaLink multimediaLink;
            ArrayList alAliases = new ArrayList();

            // Without an xref header, we can't continue
            if ((gedcomLine = gedcom.GetLine(nLevel, "INDI")) == null)
            {
                // Not one of us
                return null;
            }
            
            CIndividualRecord ir = new CIndividualRecord( gedcom );

            ir.m_xref = gedcomLine.XrefID;
            gedcom.IncrementLineIndex(1);

            do
            {
                bParsingFinished = true;

                // Family Historian tag _FLGS
                if( (gedcomLine = gedcom.GetLine(nLevel+1, "_FLGS")) != null ) 
                {
                    gedcom.IncrementLineIndex(1);
                    if( (gedcomLine = gedcom.GetLine(nLevel+2, "__LIVING")) != null )
                    {
                        ir.m_sStillLiving = gedcomLine.LineItem;
                        gedcom.IncrementLineIndex(1);
                    }
                    bParsingFinished = false;
                }
                // Let Record have a go at parsing the rest
                else if( ir.ParseRecord( gedcom, nLevel ) )
                {
                    bParsingFinished = false;
                    continue;
                }
                else if( (gedcomLine = gedcom.GetLine( nLevel+1, "RESN" )) != null )
                {
                    ir.m_sRestrictionNotice = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }
                else if( (personalNameStructure = CPersonalNameStructure.Parse( gedcom, nLevel+1 )) != null )
                {
                    ir.m_alPersonalNameStructures.Add( personalNameStructure );
                    bParsingFinished = false;
                }               
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "SEX")) != null )
                {
                    ir.m_sSexValue = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }
                else if( (individualEventStructure = CIndividualEventStructure.Parse( gedcom, nLevel+1 )) != null )
                {
                    ir.m_alIndividualEventStructures.Add( individualEventStructure );
                    bParsingFinished = false;

                    if( individualEventStructure.Type == "ADOP" )
                    {
                        gedcom.m_alAdoptedIndividuals.Add( ir );
                    }
                }               
                else if( (ldsIndividualOrdinance = CLdsOrdinance.Parse( gedcom, nLevel+1 )) != null )
                {
                    ir.m_alLdsIndividualOrdinances.Add( ldsIndividualOrdinance );
                    bParsingFinished = false;
                }               
                else if( (childToFamilyLink = CChildToFamilyLink.Parse( gedcom, nLevel+1 )) != null )
                {
                    ir.m_alChildToFamilyLinks.Add( childToFamilyLink );
                    bParsingFinished = false;
                }               
                else if( (spouseToFamilyLink = CSpouseToFamilyLink.Parse( gedcom, nLevel+1 )) != null )
                {
                    ir.m_alSpouseToFamilyLinks.Add( spouseToFamilyLink );
                    bParsingFinished = false;
                }           
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "SUBM")) != null )
                {
                    ir.m_alXrefSubms.Add( gedcomLine.LinePointer );
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }   
                else if( (associationStructure = CAssociationStructure.Parse( gedcom, nLevel+1 )) != null )
                {
                    ir.m_alAssociationStructures.Add( associationStructure );
                    bParsingFinished = false;
                }               
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "ALIA")) != null )
                {
                    if( gedcomLine.LinePointer != null )
                    {
                        ir.m_alXrefAlias.Add( gedcomLine.LinePointer );
                        gedcom.IncrementLineIndex(1);
                        bParsingFinished = false;
                    }
                    else if( gedcomLine.LineItem != null && gedcomLine.LineItem.Length > 0 )
                    {
                        alAliases.Add( gedcomLine.LineItem );
                        gedcom.IncrementLineIndex(1);
                        bParsingFinished = false;
                    }
                }   
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "ANCI")) != null )
                {
                    ir.m_alXrefAncis.Add( gedcomLine.LinePointer );
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }   
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "DESI")) != null )
                {
                    ir.m_alXrefDesis.Add( gedcomLine.LinePointer );
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }   
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "RFN")) != null )
                {
                    ir.m_sPermanentRecordFileNumber = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "AFN")) != null )
                {
                    ir.m_sAncestralFileNumber = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }               
                else if( (sourceCitation = CSourceCitation.Parse( gedcom, nLevel+1 )) != null )
                {
                    ir.m_alSourceCitations.Add( sourceCitation );
                    bParsingFinished = false;
                }               
                else if( (multimediaLink = CMultimediaLink.Parse( gedcom, nLevel+1 )) != null )
                {
                    ir.m_alMultimediaLinks.Add( multimediaLink );
                    bParsingFinished = false;
                }
                else if( (noteStructure = CNoteStructure.Parse( gedcom, nLevel+1 )) != null )
                {
                    ir.m_alNoteStructures.Add( noteStructure );
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

            // Workaround for GEDCOM that incorrectly contains this: ALIA Fred /Bloggs/ (instead of ALIA @I12@)
            if( alAliases.Count > 0 )
            {
                foreach( string sAlias in alAliases )
                {
                    sAlias.Trim();
                    if( sAlias.Length > 0 )
                    {
                        CPersonalNameStructure pns = new CPersonalNameStructure(gedcom);
                        pns.m_sNamePersonal = sAlias;
                        ir.m_alPersonalNameStructures.Add( pns );
                    }
                }
            }

            return ir;
        }

        // Get the individual's main name
        public string Name
        {
            get
            {
                if( m_alPersonalNameStructures.Count == 0 )
                {
                    return "";
                }
                return ((CPersonalNameStructure)m_alPersonalNameStructures[0]).m_sNamePersonal;
            }
        }

        // Get the n'th name (where an individual has more than one name)
        public string GetName(int n)
        {
            if (m_alPersonalNameStructures == null || m_alPersonalNameStructures.Count <= n || n < 0)
            {
                return "";
            }
            CPersonalNameStructure pns = ((CPersonalNameStructure)m_alPersonalNameStructures[n]);
            return pns.m_sNamePersonal;
        }

        // Returns the Name Suffix as per GEDCOM, or blank if no name recorded
        public string NameSuffix
        {
            get
            {
                if (m_alPersonalNameStructures.Count == 0)
                {
                    return "";
                }
                if(((CPersonalNameStructure)m_alPersonalNameStructures[0]).m_personalNamePieces != null )
                {
                    return ((CPersonalNameStructure)m_alPersonalNameStructures[0]).m_personalNamePieces.m_sNamePieceSuffix;
                }
                return "";
            }
        }

        // Returns the n'th name and associated sources
        public CNameAndSource GetNameAndSource( int n )
        {
            if(  m_alPersonalNameStructures == null || m_alPersonalNameStructures.Count <= n || n < 0 )
            {
                return null;
            }
            CPersonalNameStructure pns = ((CPersonalNameStructure)m_alPersonalNameStructures[n]);
            CNameAndSource nas = new CNameAndSource( pns.m_sNamePersonal );
            if( pns.m_personalNamePieces != null )
            {
                nas.m_alSources.AddRange( pns.m_personalNamePieces.m_alSourceCitations );
            }
            return nas;
        }

        // Adds the source records pertinent to the individual's name
        public void AddMainNameSources( ref ArrayList alSources )
        {
            if( m_alPersonalNameStructures != null && m_alPersonalNameStructures.Count > 0)
            {
                if( ((CPersonalNameStructure)m_alPersonalNameStructures[0]).m_personalNamePieces != null )
                {
                    alSources.AddRange( ((CPersonalNameStructure)m_alPersonalNameStructures[0]).m_personalNamePieces.m_alSourceCitations );
                }
            }
        }

        // Constructs and returns the full name, or returns blank if no name recorded
        public string FullName
        {
            get
            {
                if( m_alPersonalNameStructures.Count == 0 )
                {
                    return "";
                }
                CPersonalNameStructure pns = ((CPersonalNameStructure)m_alPersonalNameStructures[0]);
                CPersonalNamePieces pnp = pns.m_personalNamePieces;
                if (pnp == null)
                {
                    return ((CPersonalNameStructure)m_alPersonalNameStructures[0]).m_sNamePersonal;
                }
                string sFullName = pnp.m_sNamePiecePrefix;
                if (sFullName != "")
                {
                    sFullName += " ";
                }
                sFullName += pns.m_sNamePersonal;
                if (pnp.m_sNamePieceSuffix != "")
                {
                    sFullName += " " + pnp.m_sNamePieceSuffix;
                }
                return sFullName;
            }
        }

        // Returns the Nick Name as per GEDCOM, or blank if no name recorded
        public string NickName
        {
            get
            {
                if( m_alPersonalNameStructures.Count == 0 )
                {
                    return "";
                }
                CPersonalNameStructure pns = ((CPersonalNameStructure)m_alPersonalNameStructures[0]);
                CPersonalNamePieces pnp = pns.m_personalNamePieces;
                if (pnp == null)
                {
                    return "";
                }
                return pnp.m_sNamePieceNickname;
            }
        }

        // Returns the Used Name as per GEDCOM, or blank if no name recorded
        public string UsedName
        {
            get
            {
                if( m_alPersonalNameStructures.Count == 0 )
                {
                    return "";
                }
                CPersonalNameStructure pns = ((CPersonalNameStructure)m_alPersonalNameStructures[0]);
                return pns.m_sUsedName;
            }
        }

        // Returns the first event recorded of the given type
        public CEventDetail GetEvent( string sEventType )
        {
            foreach( CIndividualEventStructure ies in m_alIndividualEventStructures )
            {
                if( ies.Type == sEventType )
                {
                    return ies.m_eventDetail;
                }
            }
            return null;
        }

        // Returns the sex of the individual, or '-' if no sex value exists.
        public char Sex
        {
            get
            {
                if(  m_sSexValue != null && m_sSexValue.Length > 0 )
                {
                    return char.ToUpper( m_sSexValue[0] );
                }
                return '-';
            }
        }

        // Returns whether this record should be hidden, or shown anonymously, or fully visible in the website
        public EVisibility Visibility()
        {
            EVisibility visibility = EVisibility.Invisible;
            if( MainForm.s_config.m_bOnlyConceal)
            {
                visibility = EVisibility.Restricted;
            }

            if (Restricted)
            {
                return visibility;
            }

            if ((m_sRestrictionNotice != null && m_sRestrictionNotice.ToLower() == "confidential") && (MainForm.s_config.m_bRestrictConfidential == true))
            {
                return visibility;
            }
            if ((m_sRestrictionNotice != null && m_sRestrictionNotice.ToLower() == "privacy") && (MainForm.s_config.m_bRestrictPrivacy == true))
            {
                return visibility;
            }

            return EVisibility.Visible;
        }

        // User selected restriction (for prune screen)
        public bool Living
        {
            get
            {
                return (m_sStillLiving != null && m_sStillLiving.ToUpper() == "LIVING");
            }
        }

        // Returns year of birth as a string. If year is not precise, enclose it in brackets.
        public string BirthYearWithBrackets
        {
            get
            {
                string sResult = "";
                CEventDetail edBirth = GetEvent( "BIRT" );
                if( edBirth == null )
                {
                    edBirth = GetEvent( "CHR" );
                }
                if( edBirth == null )
                {
                    edBirth = GetEvent( "BAPM" );
                }
                if( edBirth != null )
                {
                    if( edBirth.m_dateValue != null )
                    {
                        sResult = edBirth.m_dateValue.QuotedYearString();
                    }
                }
                return sResult;
            }
        }

        // Returns year of death as a string. If year is not precise, enclose it in brackets.
        public string DeathYearWithBrackets
        {
            get
            {
                string sResult = "";
                CEventDetail edDeath = GetEvent( "DEAT" );
                if( edDeath == null )
                {
                    edDeath = GetEvent( "BURI" );
                }
                if( edDeath == null )
                {
                    edDeath = GetEvent( "CREM" );
                }
                if( edDeath != null )
                {
                    if( edDeath.m_dateValue != null )
                    {
                        sResult = edDeath.m_dateValue.QuotedYearString();
                    }
                }
                return sResult;
            }
        }

        // Returns e.g. "1812-1892". Returns " " rather than blank
        public string LifeYears
        {
            get
            {
                string sBirthDate = BirthYearWithBrackets;
                string sDeathDate = DeathYearWithBrackets;
                if( sBirthDate != "" || sDeathDate != "" )
                {
                    sBirthDate += " -";
                }
                sBirthDate += " " + sDeathDate;
                return sBirthDate;
            }
        }

        // Gets date of birth, or closest related date if actual birth date not known.
        public CPGQualifiedDate BirthDate
        {
            get
            {
                CPGQualifiedDate.EQualification qual = CPGQualifiedDate.EQualification.Birth;
                CEventDetail edBirth = GetEvent( "BIRT" );
                if( edBirth == null )
                {
                    edBirth = GetEvent( "CHR" );
                    qual = CPGQualifiedDate.EQualification.Christening;
                }
                if( edBirth == null )
                {
                    edBirth = GetEvent( "BAPM" );
                    qual = CPGQualifiedDate.EQualification.Baptism;
                }
                if( edBirth != null )
                {
                    if( edBirth.m_dateValue != null )
                    {
                        return new CPGQualifiedDate( edBirth.m_dateValue, qual );
                    }
                }
                return null;
            }
        }

        // Gets date of death, or closest related date if actual death date not known.
        public CPGQualifiedDate DeathDate
        {
            get
            {
                CPGQualifiedDate.EQualification qual = CPGQualifiedDate.EQualification.Death;

                CEventDetail edDeath = GetEvent( "DEAT" );
                if( edDeath == null )
                {
                    edDeath = GetEvent( "BURI" );
                    qual = CPGQualifiedDate.EQualification.Burial;
                }
                if( edDeath == null )
                {
                    edDeath = GetEvent( "CREM" );
                    qual = CPGQualifiedDate.EQualification.Cremation;
                }
                if( edDeath != null )
                {
                    if( edDeath.m_dateValue != null )
                    {
                        return new CPGQualifiedDate( edDeath.m_dateValue, qual );
                    }
                }
                return null;
            }
        }
        
        // Returns first GEDCOM user-defined reference number, if any.
        // Note argument n is not used at the moment.
        public string UserReferenceNumber( int n )
        {
            if( m_alUserReferenceNumbers.Count >= 1 )
            {
                CUserReferenceNumber urn = (CUserReferenceNumber)m_alUserReferenceNumbers[0];
                if( urn != null )
                {
                    return urn.m_sUserReferenceNumber;
                }
            }
            return "";
        }

        // Returns a string containing CRLFs describing this record for use in a message box
        public override string Details()
        {
            string sDetails = String.Concat( "Record: ", m_xref, "\r\n" );
            string sName = Name;
            string sDummy = "";
            if( sName == null || sName == "" )
            {
                sName = MainForm.s_config.m_sUnknownName;
            }
            else
            {
                sName = MainForm.s_config.CapitaliseName( sName, ref sDummy, ref sDummy );
            }
            string sNickName = NickName;
            sDetails += String.Format("Name: {0}\r\n", sName);
            if( sNickName != null && sNickName != "" )
            {
                sDetails += String.Format("Nickname: {0}\r\n", sNickName);
            }

            CPGQualifiedDate qdateBorn = BirthDate;
            if( qdateBorn != null )
            {
                string sLabel;
                switch( qdateBorn.m_eqQualification )
                {
                    case CPGQualifiedDate.EQualification.Baptism:
                        sLabel = "Baptised";
                        break;
                    case CPGQualifiedDate.EQualification.Christening:
                        sLabel = "Christened";
                        break;
                    case CPGQualifiedDate.EQualification.Birth:
                    default:
                        sLabel = "Born";
                        break;
                }

                sDetails += String.Concat( sLabel, ": ", qdateBorn.ToString(), "\r\n" );
            }
            CPGQualifiedDate qdateDied = DeathDate;
            if( qdateDied != null )
            {
                string sLabel;
                switch( qdateDied.m_eqQualification )
                {
                    case CPGQualifiedDate.EQualification.Burial:
                        sLabel = "Buried";
                        break;
                    case CPGQualifiedDate.EQualification.Cremation:
                        sLabel = "Cremated";
                        break;
                    case CPGQualifiedDate.EQualification.Death:
                    default:
                        sLabel = "Died";
                        break;
                }

                sDetails += String.Concat( sLabel, ": ", qdateDied.ToString(), "\r\n" );
            }

            // Find sex
            char cSex = Sex;
            string sChildOf;
            if( cSex == 'M' )
            {
                sChildOf = "Son of";
            }
            else if( cSex == 'F' )
            {
                sChildOf = "Daughter of";
            }
            else
            {
                sChildOf = "Child of";
            }

            // Find son of, or daughter of
            int nParents = 0;
            CFamilyRecord fr;
            while( (fr = Gedcom.GetFamilyByChild( this, nParents++ )) != null )
            {
                if (fr == null)
                {
                    continue;
                }

                String sParents = sChildOf;
                CIndividualRecord irFather = Gedcom.GetIndividualRecord( fr.m_xrefHusband );
                if( irFather != null )
                {
                    sName = MainForm.s_config.CapitaliseName( irFather.Name, ref sDummy, ref sDummy );
                    sParents += " " + sName;
                }
                CIndividualRecord irMother = Gedcom.GetIndividualRecord( fr.m_xrefWife );
                if( irMother != null )
                {
                    sName = MainForm.s_config.CapitaliseName( irMother.Name, ref sDummy, ref sDummy );
                    if( irFather != null )
                    {
                        sParents += " and";
                    }
                    sParents += " " + sName;
                }
                if( irFather != null || irMother != null )
                {
                    sDetails += sParents + "\r\n";
                }
            }

            // Find irSubject
            int nSpouses = 0;
            while( (fr = Gedcom.GetFamilyBySpouse( this, nSpouses++ )) != null )
            {
                // Did they definitely get married?
                bool bMarried = false;
                String sMarriedDate = "";
                if( fr.m_alFamilyEventStructures != null )
                {
                    foreach( CFamilyEventStructure fes in fr.m_alFamilyEventStructures )
                    {
                        if( fes.Type == "MARR" )
                        {
                            if( fr.WereTheyReallyMarried() )
                            {
                                bMarried = true;
                            }
                            break;
                        }
                    }
                }

                bool bEffectivelyUselessInformation = false;
                String sMarried = "";
                CIndividualRecord irHusband = Gedcom.GetIndividualRecord( fr.m_xrefHusband );
                CIndividualRecord irWife = Gedcom.GetIndividualRecord( fr.m_xrefWife );
                if( irHusband == this )
                {
                    if( bMarried )
                    {           
                        sMarried = "Husband of ";
                    }
                    else
                    {
                        sMarried = "Partner(m) of ";
                    }
                    if( irWife != null )
                    {
                        sName = MainForm.s_config.CapitaliseName( irWife.Name, ref sDummy, ref sDummy );
                    }
                    else
                    {
                        sName = MainForm.s_config.m_sUnknownName;
                        bEffectivelyUselessInformation = true;
                    }

                }
                else if( irWife == this )
                {
                    if( bMarried )
                    {
                        sMarried = "Wife of ";
                    }
                    else
                    {
                        sMarried = "Partner(f) of ";
                    }   
                    if( irHusband != null )
                    {
                        sName = MainForm.s_config.CapitaliseName( irHusband.Name, ref sDummy, ref sDummy );
                    }
                    else
                    {
                        sName = MainForm.s_config.m_sUnknownName;
                        bEffectivelyUselessInformation = true;
                    }
                }
                else
                {
                    if( sMarried == "" )
                    {
                        sMarried = "Partner of ";
                    }                   
                    sName = MainForm.s_config.m_sUnknownName;
                    bEffectivelyUselessInformation = true;

                }
                if( !bEffectivelyUselessInformation )
                {
                    sDetails += String.Concat( sMarried, sName, sMarriedDate, "\r\n" );
                }
            }

            return sDetails;            
        }
    }


}
