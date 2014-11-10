/* CCreatorRecordIndividual.cs
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
using GEDmill.LLClasses;
using System.Collections;
using GEDmill.ImageClasses;
using System.Drawing;
using System.IO;

namespace GEDmill.HTMLClasses
{
  public class CCreatorRecordIndividual : CCreatorRecord
  {
    // The individual record that we are creating the page for.
    private CIndividualRecord m_ir;

    // Indicates that this individual should have most of the record data excluded from the website for privacy.
    private bool m_bConcealed;

    // List of the events in the individual's life history.
    private ArrayList m_alEventList;

    // List of other facts known about the individual.
    private ArrayList m_alAttributeList;

    // List of sources referenced by this page.
    private ArrayList m_alReferenceList; 

    // List of occupations known for the individual
    private ArrayList m_alOccupations;

    // An HTML link to the previous sibling in this fr.
    private string m_sPreviousChildLink;

    // An HTML link to the next sibling in this fr.
    private string m_sNextChildLink;

    // List of aliases and other names for the individual
    private ArrayList m_alOtherNames;

    // A date inferred for the individual's birthday, with associated quality so that it can be rejected in favour of more certain information.
    private CPGQualifiedDate m_qdateInferredBirthday;

    // The individual's date of birth
    private CPGDate m_dateActualBirthday;

    // A date inferred for the individual's date of death, with associated quality so that it can be rejected in favour of more certain information.
    private CPGQualifiedDate m_qdateInferredDeathday;

    // The individual's date of death
    private CPGDate m_dateActualDeathday;

    // Records first occurrence of one-off events, so that they may be marked as "preferred"
    private Hashtable m_htFirstFoundEvent;

    // The sources giving the individual's birth date
    private string m_sBirthdaySourceRefs;

    // The sources giving the individual's death date
    private string m_sDeathdaySourceRefs;

    // The individual's title (GEDCOM:TITL)
    private string m_sNameTitle;

    // Indicates that a name for this individual is not available
    private bool m_bUnknownName;

    // The individual's main name
    private string m_sName;

    // The suffix on the individual's name (e.g. "Snr")
    private string m_sNameSuffix;

    // The individual's first name
    private string m_sFirstName;
  
    // The individual's surname
    private string m_sSurname;

    // The individual's fully expanded name
    private string m_sFullName;

    // The individual's nickname
    private string m_sNickName;

    // The individual's commonly used name
    private string m_sUsedName;

    // The sources giving the individual's name
    private string m_sNameSources;

    // The individual's occupation, for display at the page head
    private string m_sOccupation;

    // All the frParents of the individual. May be more than one pair if individual was associated with more than one fr.
    private ArrayList m_alParents; 

    // A reference to the index creator, so that individual pages can be added to the index as they are created.
    private CCreatorIndexIndividuals m_indiIndexCreator;

    // The paintbox with which to draw the mini tree.
    private CPaintbox m_paintbox;

    // Constructor
    public CCreatorRecordIndividual( CGedcom gedcom, IProgressCallback progress, string sW3cfile, CIndividualRecord ir, CCreatorIndexIndividuals indiIndexCreator, CPaintbox paintbox ) : base( gedcom, progress, sW3cfile )
    {
      m_ir = ir;
      m_indiIndexCreator = indiIndexCreator;
      m_paintbox = paintbox;
      m_htFirstFoundEvent = new Hashtable();
      m_sBirthdaySourceRefs = "";
      m_sDeathdaySourceRefs = "";
      m_sNameTitle = "";
      m_bUnknownName = false;
      m_sName = m_ir.Name;
      m_sNameSuffix = m_ir.NameSuffix;
      m_sFirstName = "";
      m_sSurname = "";
      m_sOccupation = "";
      m_bConcealed = m_ir.Visibility() == CIndividualRecord.EVisibility.Restricted;
      m_alEventList = new ArrayList();
      m_alAttributeList = new ArrayList();
      m_alReferenceList = new ArrayList();
      m_alOccupations = new ArrayList();
      m_sPreviousChildLink = "";
      m_sNextChildLink = "";
      m_alOtherNames = new ArrayList();
      m_qdateInferredBirthday = null;
      m_dateActualBirthday = null;
      m_qdateInferredDeathday = null;
      m_dateActualDeathday = null;
      m_alParents = new ArrayList();
    }

    // The main method that causes the page to be created.
    public bool Create( CStats stats )
    {
      LogFile.TheLogFile.WriteLine(LogFile.DT_HTML, LogFile.EDebugLevel.Note, "CCreatorRecordIndividual.Create()");

      if (m_ir == null)
      {
        return false;
      }

      if (m_ir.Visibility() == CIndividualRecord.EVisibility.Invisible)
      {
        return false;
      }
      
      // Collect together multimedia links
      if (MainForm.s_config.m_bAllowMultimedia && (m_ir.m_alUniqueFileRefs != null) && !m_bConcealed)
      {
        m_ir.m_alUniqueFileRefs.Sort(new CMultimediaFileReference.OrderComparer());
        AddMultimedia( null, m_ir.m_alUniqueFileRefs, String.Concat(m_ir.m_xref, "mm"), String.Concat(m_ir.m_xref, "mo"), MainForm.s_config.m_uMaxImageWidth, MainForm.s_config.m_uMaxImageHeight, stats );
      }

      AddEvents();

      RemoveLoneOccupation();

      if (m_qdateInferredBirthday != null && m_qdateInferredBirthday.m_eqQualification == CPGQualifiedDate.EQualification.Birth)
      {
        m_dateActualBirthday = m_qdateInferredBirthday.m_date;
      }

      if (m_qdateInferredDeathday != null && m_qdateInferredDeathday.m_eqQualification == CPGQualifiedDate.EQualification.Death)
      {
        m_dateActualDeathday = m_qdateInferredDeathday.m_date;
      }

      ConstructName();

      CPGDate age30;
      if (m_qdateInferredBirthday != null && CPGDate.IsValid(m_qdateInferredBirthday.m_date))
      {
        age30 = new CPGDate(m_qdateInferredBirthday.m_date);
      }
      else
      {
        age30 = new CPGDate(DateTime.Now);
      }
      age30.m_year.m_nYear += MainForm.s_config.m_nAgeForOccupation;

      // We should have birthday and deathday by now, so find longest occupation
      if (!m_bConcealed)
      {
        m_sOccupation = BestOccupation(m_alOccupations, age30, (m_qdateInferredBirthday != null) ? m_qdateInferredBirthday.m_date : null, (m_qdateInferredDeathday != null) ? m_qdateInferredDeathday.m_date : null);// Picks occupation with longest time span
      }

      // Go through all families this person was a irSubject to
      if (!m_bConcealed)
      {
        foreach (CSpouseToFamilyLink sfl in m_ir.m_alSpouseToFamilyLinks)
        {
          CFamilyRecord fr = m_gedcom.GetFamilyRecord(sfl.m_xrefFam);
          if (fr != null)
          {

            // Find the irSubject's name
            CIndividualRecord spouse = null;
            string spouseLink = "";
            if (fr.m_xrefHusband != m_ir.m_xref)
            {
              spouse = m_gedcom.GetIndividualRecord(fr.m_xrefHusband);
            }
            else
            {
              spouse = m_gedcom.GetIndividualRecord(fr.m_xrefWife);
            }
            if (spouse != null && spouse.Visibility() != CIndividualRecord.EVisibility.Invisible)
            {
              spouseLink = MakeLink(spouse);
            }

            // Add fr events as events connected to this individual
            foreach (CFamilyEventStructure fes in fr.m_alFamilyEventStructures)
            {
              ProcessEvent(fes, spouseLink );
            }

            AddChildrensEvents(fr);

            AddMarriage( spouse, spouseLink, fr );
          }
        }
        AddParentsAndSiblings();
      } // end if !concealed

      string birthyear = "";
      string deathyear = "";
      if (!m_bConcealed)
      {
        if (m_qdateInferredBirthday != null && m_qdateInferredBirthday.m_date != null)
        {
          birthyear = m_qdateInferredBirthday.m_date.Year;
        }
        if (m_qdateInferredDeathday != null && m_qdateInferredDeathday.m_date != null)
        {
          deathyear = m_qdateInferredDeathday.m_date.Year;
        }
      }


      string title = m_sName; //"Fred Bloggs 1871-1921"
      string lifeDates = "";
      if (!m_bConcealed)
      {
        if (birthyear != "" || deathyear != "")
        {
          lifeDates = String.Concat(birthyear, "-", deathyear);
          title = String.Concat(m_sName, " ", lifeDates);
        }
      }

      AddIndividualIndexEntry( lifeDates );

      OutputHTML( title );

      return true;
    }

    // Adds the marriage associated with the fr record to the list of events. Also adds irSubject death if within this person's lifetime.
    private void AddMarriage( CIndividualRecord spouse, string spouseLink, CFamilyRecord fr )
    {
      // Find wedding date
      if (spouse != null)
      {
        string sourceRefs = AddSpouseDeath(spouse, spouseLink);

        CPGDate marriageDate;
        string marriageNote;
        string marriagePlace;
        sourceRefs = AddMarriageEvent(fr, sourceRefs, out marriageDate, out marriageNote, out marriagePlace);

        marriageNote = BuildMaritalStatusNote(fr, marriageNote);

        // Add fr record notes to marriage event
        if (fr.m_alNoteStructures != null)
        {
          foreach (CNoteStructure ns in fr.m_alNoteStructures)
          {
            if (ns.Text != null && ns.Text.Length > 0)
            {
              if (marriageNote != "")
              {
                marriageNote += "\n";
              }
              if (MainForm.s_config.m_bObfuscateEmails)
              {
                marriageNote += ObfuscateEmail(ns.Text);
              }
              else
              {
                marriageNote += ns.Text;
              }
            }
          }
        }
        string marriedString = "married ";
        if (fr.WereTheyReallyMarried() == false)
        {
          marriedString = "partner of ";
        }
        if (marriageDate != null)
        {
          CIEvent iEvent = new CIEvent(marriageDate, "_MARRIAGE", String.Concat(marriedString, spouseLink, marriagePlace, ".", sourceRefs), "", marriageNote, true, MainForm.s_config.m_bCapitaliseEventDescriptions);
          m_alEventList.Add(iEvent);
        }
        // else its an attribute.
        else
        {
          CIEvent iEvent = new CIEvent(marriageDate, "_MARRIAGE", String.Concat(marriedString, spouseLink, marriagePlace, ".", sourceRefs), "", marriageNote, true, MainForm.s_config.m_bCapitaliseEventDescriptions);
          // Marriages go at the front of the list so that they appear first in "Other facts"
          m_alAttributeList.Insert(0, iEvent); 
        }

      } // end if (irSubject != null )
    }

    // Goes through all families this person was a irSibling in and finds their frParents and siblings.
    private void AddParentsAndSiblings()
    {
      // Set a limit for date comparisons
      DateTime dtNow = DateTime.Now;

      // Go through all families this person was a irSibling in
      foreach (CChildToFamilyLink cfl in m_ir.m_alChildToFamilyLinks)
      {
        CFamilyRecord fr = m_gedcom.GetFamilyRecord(cfl.m_xrefFam);
        if (fr != null)
        {
          CIndividualRecord husband = m_gedcom.GetIndividualRecord(fr.m_xrefHusband);
          CIndividualRecord wife = m_gedcom.GetIndividualRecord(fr.m_xrefWife);

          if (husband != null || wife != null)
          {
            CHusbandAndWife parents = new CHusbandAndWife();
            parents.m_irHusband = husband;
            parents.m_irWife = wife;
            m_alParents.Add(parents);
          }

          // Get all the children in order, to find previous and next irSibling
          CPGDate testBirthday = (m_qdateInferredBirthday != null) ? m_qdateInferredBirthday.m_date : null;

          if (testBirthday == null)
          {
            testBirthday = new CPGDate(dtNow);
          }

          int previousDifference = -100 * 365; // 100 years should be enough
          int nextDifference = 100 * 365;
          int previousXrefDifference = -100 * 365; // Arbitrary big value
          int nextXrefDifference = 100 * 365; // Arbitrary big value
          int xrefNumber = CGedcom.MakeXrefNumber(m_ir.m_xref);

          foreach (CChild ch in fr.m_alChildren)
          {
            if (ch.m_ir.m_xref == m_ir.m_xref)
              continue;

            CIndividualRecord child = ch.m_ir;
            if (child != null)
            {
              if (child.Visibility() == CIndividualRecord.EVisibility.Invisible)
                continue;

              CEventDetail childBirthday = child.GetEvent("BIRT");
              if (childBirthday == null)
              {
                childBirthday = child.GetEvent("CHR");
              }
              if (childBirthday == null)
              {
                childBirthday = child.GetEvent("BAPM");
              }

              CPGDate childBirthdate = null;
              if (childBirthday != null)
                childBirthdate = childBirthday.m_dateValue;
              if (childBirthdate == null)
              {
                childBirthdate = new CPGDate(dtNow);
              }

              int difference = CPGDate.Difference(testBirthday, childBirthdate);
              if (difference < 0)
              {
                if (difference > previousDifference)
                {
                  previousDifference = difference;
                  m_sPreviousChildLink = MakeLink(child,"previous child");
                }
              }
              else if (difference > 0)
              {
                if (difference < nextDifference)
                {
                  nextDifference = difference;
                  m_sNextChildLink = MakeLink(child,"next child");
                }
              }
              else
              {
                // Twins or no birthday. Sort according to xref number
                int childXrefNumber = CGedcom.MakeXrefNumber(ch.m_ir.m_xref);
                int xrefDifference = xrefNumber - childXrefNumber;
                if (xrefDifference < 0)
                {
                  if (xrefDifference > previousXrefDifference)
                  {
                    previousXrefDifference = xrefDifference;
                    m_sPreviousChildLink = MakeLink(child,"previous child");
                  }
                }
                else
                {
                  if (xrefDifference < nextXrefDifference)
                  {
                    nextXrefDifference = xrefDifference;
                    m_sNextChildLink = MakeLink(child,"next child");
                  }
                }
              } // end 3 way if
            } // end if(irSibling != null )
          } // end foreach irSibling
        } // end if( fr != null )
      } // end foreach fr
    }

    // Adds this individual page to the index of pages.
    private void AddIndividualIndexEntry( string lifeDates )
    {
      string relativeFilename = GetIndividualHTMLFilename(m_ir);
      // Create some strings to use in index entry
      string sUserRef = "";
      if (m_ir.m_alUserReferenceNumbers.Count > 0)
      {
        CUserReferenceNumber urn = (CUserReferenceNumber)m_ir.m_alUserReferenceNumbers[0];
        sUserRef = EscapeHTML(urn.m_sUserReferenceNumber, false);
        if (sUserRef.Length > 0)
        {
          sUserRef = String.Concat(" [", sUserRef, "]");
        }
      }
      string alterEgo = "";
      if (MainForm.s_config.m_bIncludeNickNamesInIndex)
      {
        if (m_sNickName != "")
        {
          alterEgo = String.Concat("(", m_sNickName, ") ");
        }
        else if (m_sUsedName != "")
        {
          alterEgo = String.Concat("(", m_sUsedName, ") ");
        }
      }

      if( null != m_indiIndexCreator )
      {
        // Add index entry for this individuals main name (or hidden/unknown string)
        string sFirstName = m_sFirstName;
        if( m_sNameSuffix != null && m_sNameSuffix != "" )
        {
            sFirstName += ", " + m_sNameSuffix;
        }
        m_indiIndexCreator.AddIndividualToIndex(sFirstName, m_sSurname, m_bUnknownName, alterEgo, lifeDates, m_bConcealed, relativeFilename, sUserRef);

        // Add entries for this individual's other names
        if (!m_bConcealed && !m_bUnknownName)
        {
          string other_name = "";
          for (int i = 1; (other_name = m_ir.GetName(i)) != ""; i++)
          {
            string other_firstName = "";
            string other_surname = "";
            other_name = MainForm.s_config.CapitaliseName(other_name, ref other_firstName, ref other_surname); // Also splits name into first name and surname
            m_indiIndexCreator.AddIndividualToIndex(other_firstName, other_surname, m_bUnknownName, alterEgo, lifeDates, m_bConcealed, relativeFilename, sUserRef);
          }
        }
      }
    }

    // Extracts the data from the MARR event for the fr record.
    private string AddMarriageEvent( CFamilyRecord fr, string sourceRefs, out CPGDate marriageDate, out string marriageNote, out string marriagePlace )
    {
      // Find out when they were married
      marriageDate = null;
      marriagePlace = "";
      sourceRefs = "";
      marriageNote = "";
      foreach (CFamilyEventStructure fes in fr.m_alFamilyEventStructures)
      {
        if (fes.Type == "MARR")
        {
          if (fes.m_eventDetail != null)
          {
            marriageDate = fes.m_eventDetail.m_dateValue;

            if (fes.m_eventDetail.m_placeStructure != null)
            {
              if (fes.m_eventDetail.m_placeStructure.m_sPlaceName != "")
                marriagePlace = String.Concat(" ", MainForm.s_config.m_sPlaceWord, " ", EscapeHTML(fes.m_eventDetail.m_placeStructure.m_sPlaceName, false));
            }

            sourceRefs = AddSources(ref m_alReferenceList, fes.m_eventDetail.m_alSourceCitations);

            if (fes.m_eventDetail.m_alNoteStructures != null)
            {
              foreach (CNoteStructure ns in fes.m_eventDetail.m_alNoteStructures)
              {
                if (ns.Text != null && ns.Text.Length > 0)
                {
                  if (marriageNote != "")
                  {
                    marriageNote += "\n";
                  }

                  if (MainForm.s_config.m_bObfuscateEmails)
                  {
                    marriageNote += ObfuscateEmail(ns.Text);
                  }
                  else
                  {
                    marriageNote += ns.Text;
                  }
                }
              }
            }
            break;
          }
        }
      }
      return sourceRefs;
    }

    // Extracts the data from the DEAT event for the given individual and adds it if it was an event in the current individual's lifetime.
    private string AddSpouseDeath( CIndividualRecord spouse, string spouseLink )
    {
      string sourceRefs = "";
      string place = "";
      if (spouse.Visibility() == CIndividualRecord.EVisibility.Visible)
      {
        // Record death of irSubject if within this person's lifetime
        CPGDate spouseDeathDate = null;
        foreach (CIndividualEventStructure ies in spouse.m_alIndividualEventStructures)
        {
          if (ies.Type == "DEAT")
          {
            if (ies.m_eventDetail != null)
            {
              spouseDeathDate = ies.m_eventDetail.m_dateValue;
              if (spouseDeathDate != null)
              {
                if (m_qdateInferredDeathday == null || m_qdateInferredDeathday.m_date == null || spouseDeathDate.CompareTo(m_qdateInferredDeathday.m_date) <= 0)
                {
                  if (ies.m_eventDetail.m_placeStructure != null)
                  {
                    if (ies.m_eventDetail.m_placeStructure.m_sPlaceName != "")
                      place = String.Concat(" ", MainForm.s_config.m_sPlaceWord, " ", EscapeHTML(ies.m_eventDetail.m_placeStructure.m_sPlaceName, false));
                  }

                  sourceRefs = AddSources(ref m_alReferenceList, ies.m_eventDetail.m_alSourceCitations);

                  if (spouseDeathDate != null)
                  {
                    CIEvent iEvent = new CIEvent(spouseDeathDate, "_SPOUSEDIED", String.Concat("death of ", spouseLink, place, ".", sourceRefs), "", null, false, MainForm.s_config.m_bCapitaliseEventDescriptions);
                    m_alEventList.Add(iEvent);
                  }
                  // else its an attribute.
                  else
                  {
                    CIEvent iEvent = new CIEvent(null, "_SPOUSEDIED", String.Concat("death of ", spouseLink, place, ".", sourceRefs), "", null, false, MainForm.s_config.m_bCapitaliseEventDescriptions);
                    m_alAttributeList.Add(iEvent);
                  }
                }
                break;
              }
            }
          }
        }
      }
      return sourceRefs;
    }

    // Adds birth, baptism, death etc of the children in the given fr.
    private void AddChildrensEvents( CFamilyRecord fr )
    {
      // Find out all the children.
      foreach (CChild ch in fr.m_alChildren)
      {
        CIndividualRecord child = ch.m_ir;
        if (child.Visibility() == CIndividualRecord.EVisibility.Invisible)
        {
          continue;
        }
        if (child != null)
        {
          bool childConcealed = (child.Visibility() == CIndividualRecord.EVisibility.Restricted);

          string childSex = "child";
          if (!childConcealed)
          {
            if (child.Sex == 'M')
            {
              childSex = "son";
            }
            else if (child.Sex == 'F')
            {
              childSex = "daughter";
            }
          }

          string childLink = MakeLink(child);
          string sourceRefs = "";

          if (!childConcealed)
          {
            // Add death of children if happened in irSubject's lifetime.
            // Note this is done before birth because the way the subsequent sort works it will put death after birth.
            CEventDetail childDeathday = child.GetEvent("DEAT");
            if (childDeathday == null)
            {
              childDeathday = child.GetEvent("BURI");
            }
            if (childDeathday == null)
            {
              childDeathday = child.GetEvent("CREM");
            }

            string deathPlace = "";
            CPGDate childDeathdate = null;

            if (childDeathday != null)
            {
              childDeathdate = childDeathday.m_dateValue;

              if (childDeathday.m_placeStructure != null)
              {
                if (childDeathday.m_placeStructure.m_sPlaceName != "")
                {
                  deathPlace = String.Concat(" ", MainForm.s_config.m_sPlaceWord, " ", EscapeHTML(childDeathday.m_placeStructure.m_sPlaceName, false));
                }
              }
            }

            if (childDeathdate != null && m_qdateInferredDeathday != null && m_qdateInferredDeathday.m_date != null && (childDeathdate.CompareTo(m_qdateInferredDeathday.m_date) <= 0))
            {
              sourceRefs = AddSources(ref m_alReferenceList, childDeathday.m_alSourceCitations);
              CIEvent iEvent = new CIEvent(childDeathdate, "_CHILDDIED", String.Concat("death of ", childSex, " ", childLink, deathPlace, ".", sourceRefs), "", null, false, MainForm.s_config.m_bCapitaliseEventDescriptions);
              m_alEventList.Add(iEvent);
            }
          }

          // Add birth of children.
          // Note this is done after deaths because the way the subsequent sort works it will put death after birth.
          CEventDetail childBirthday = child.GetEvent("BIRT");
          if (childBirthday == null)
          {
            childBirthday = child.GetEvent("CHR");
          }
          if (childBirthday == null)
          {
            childBirthday = child.GetEvent("BAPM");
          }

          string birthPlace = "";
          CPGDate childBirthdate = null;
          sourceRefs = "";

          if (childBirthday != null && !childConcealed)
          {
            childBirthdate = childBirthday.m_dateValue;

            if (childBirthday.m_placeStructure != null)
            {
              if (childBirthday.m_placeStructure.m_sPlaceName != "")
              {
                birthPlace = String.Concat(" ", MainForm.s_config.m_sPlaceWord, " ", EscapeHTML(childBirthday.m_placeStructure.m_sPlaceName, false));
              }
            }
            sourceRefs = AddSources(ref m_alReferenceList, childBirthday.m_alSourceCitations);
          }

          if (childBirthdate == null)
          {
            CIEvent iEvent = new CIEvent(null, "_CHILDBORN", String.Concat("birth of ", childSex, " ", childLink, birthPlace, ".", sourceRefs), "", null, true, MainForm.s_config.m_bCapitaliseEventDescriptions);
            m_alAttributeList.Add(iEvent);
          }
          else
          {
            CIEvent iEvent = new CIEvent(childBirthdate, "_CHILDBORN", String.Concat("birth of ", childSex, " ", childLink, birthPlace, ".", sourceRefs), "", null, true, MainForm.s_config.m_bCapitaliseEventDescriptions);
            m_alEventList.Add(iEvent);
          }
        }
      }
    }

    // Works through all the events records for this individual and extracts information from them.
    private void AddEvents()
    {
      if (m_ir.m_alIndividualEventStructures != null && !m_bConcealed)
      {
        foreach (CIndividualEventStructure ies in m_ir.m_alIndividualEventStructures)
        {
          ProcessEvent( ies, null);
          if (ies.Type == "TITL")
          {
            if (m_sNameTitle.Length > 0)
            {
              m_sNameTitle += " ";
            }
            m_sNameTitle += ies.Value;
          }
        }
      }
    }

    // Extracts the name information from the individual record.
    private void ConstructName()
    {
      // Construct the guy's name
      if (m_bConcealed && !MainForm.s_config.m_bUseWithheldNames)
      {
        m_sFirstName = "";
        m_sSurname = m_sName = MainForm.s_config.m_sConcealedName;
      }
      else
      {
        m_sName = MainForm.s_config.CapitaliseName(m_sName, ref m_sFirstName, ref m_sSurname); // Also splits name into first name and surname
      }
      if (m_sName == "")
      {
        m_sFirstName = "";
        m_sSurname = m_sName = MainForm.s_config.m_sUnknownName;
        m_bUnknownName = true;
      }

      // Remember other name records
      if (!m_bConcealed && !m_bUnknownName)
      {
        CNameAndSource nasOther;
        for (int i = 1; (nasOther = m_ir.GetNameAndSource(i)) != null; i++)
        {
          string sFirstNameOther = "";
          string sSurnameOther = "";
          nasOther.m_sName = MainForm.s_config.CapitaliseName(nasOther.m_sName, ref sFirstNameOther, ref sSurnameOther); // Also splits name into first name and surname
          nasOther.m_sSourceHtml = AddSources(ref m_alReferenceList, nasOther.m_alSources);
          m_alOtherNames.Add(nasOther);
        }
      }

      if (m_bConcealed && !MainForm.s_config.m_bUseWithheldNames)
      {
        m_sFullName = MainForm.s_config.m_sConcealedName;
      }
      else
      {
        m_sFullName = m_ir.FullName;
        string sDummy = "";
        m_sFullName = MainForm.s_config.CapitaliseName(m_sFullName, ref sDummy, ref sDummy); // Also splits name into first name and surname
      }
      if (m_sFullName == "")
      {
        m_sFullName = MainForm.s_config.m_sUnknownName;
      }

      if (m_sNameTitle.Length > 0)
      {
        m_sFullName = String.Concat(m_sNameTitle, " ", m_sFullName);
      }
      if (m_bConcealed)
      {
        m_sNickName = "";
        m_sUsedName = "";
      }
      else
      {
        m_sNickName = m_ir.NickName;
        m_sUsedName = m_ir.UsedName;
      }

      // Add general source references
      ArrayList alNameSourcesList = new ArrayList(m_ir.m_alSourceCitations);
      m_sNameSources = "";
      if (!m_bConcealed)
      {
        m_ir.AddMainNameSources(ref alNameSourcesList);
        m_sNameSources = AddSources(ref m_alReferenceList, alNameSourcesList);
      }
    }

    // Creates a file and writes into it the HTML for the individual's page.
    private void OutputHTML( string title )
    {
      CHTMLFile f = null;
      string pageDescription = "GEDmill GEDCOM to HTML page for " + m_sName;
      string keywords = "family tree history " + m_sName;
      string relativeFilename = GetIndividualHTMLFilename(m_ir);
      string fullFilename = String.Concat(MainForm.s_config.m_sOutputFolder, "\\", relativeFilename);

      try
      {
        f = new CHTMLFile(fullFilename, title, pageDescription, keywords); // Creates a new file, and puts standard header html into it.

        if (f != null)
        {
          OutputPageHeader(f.m_sw, m_sPreviousChildLink, m_sNextChildLink, true, true);

          if (MainForm.s_config.m_bShowMiniTrees)
          {
            OutputMiniTree(f);
          }
          f.m_sw.WriteLine("    <div class=\"hr\" />");
          f.m_sw.WriteLine("");
          f.m_sw.WriteLine("    <div id=\"page\"> <!-- page -->");

          OutputMultimedia(f);

          f.m_sw.WriteLine("      <div id=\"main\">");



          f.m_sw.WriteLine("        <div id=\"summary\">");
          OutputNames(f);
          OutputIndividualSummary(f);
          f.m_sw.WriteLine("        </div> <!-- summary -->");

          if (!MainForm.s_config.m_bShowMiniTrees)
          {
            OutputParentNames(f);
          }

          if (!m_bConcealed)
          {
            m_alEventList.Sort();
            OutputEvents(f);

            OutputAttributes(f);
            OutputNotes(f);
            OutputSourceReferences(f);
          }

          f.m_sw.WriteLine("      </div> <!-- main -->");

          f.m_sw.WriteLine("");

          // Add footer (Record date, W3C sticker, GEDmill credit etc.)
          OutputFooter( f, m_ir );

          f.m_sw.WriteLine("    </div> <!-- page -->");
        }
      }
      catch (IOException e)
      {
        LogFile.TheLogFile.WriteLine(LogFile.DT_HTML, LogFile.EDebugLevel.Error, "Caught IO Exception(4) : " + e.ToString());
      }
      catch (ArgumentException e)
      {
        LogFile.TheLogFile.WriteLine(LogFile.DT_HTML, LogFile.EDebugLevel.Error, "Caught Argument Exception(4) : " + e.ToString());
      }
      finally
      {
        if (f != null)
        {
          // Close adds the standard footer to the file
          f.Close(); 
        }
      }
    }

    // Outputs the HTML for the list of Sources referenced in the page.
    private void OutputSourceReferences(CHTMLFile f)
    {
      if (m_alReferenceList.Count > 0)
      {
        f.m_sw.WriteLine("        <div id=\"references\">");
        f.m_sw.WriteLine("          <h1>Sources</h1>");
        f.m_sw.WriteLine("          <ul>");

        for (uint i = 0; i < m_alReferenceList.Count; i++)
        {
          CSourceCitation sc = (CSourceCitation)(m_alReferenceList[(int)i]);

          string extraInfo = "";
          CSourceRecord sr = m_gedcom.GetSourceRecord(sc.GetXref());

          // Publication facts //
          if (sr != null && sr.m_sSourcePublicationFacts != null && sr.m_sSourcePublicationFacts != "")
          {
            string pubFacts;
            if (MainForm.s_config.m_bObfuscateEmails)
            {
              pubFacts = ObfuscateEmail(sr.m_sSourcePublicationFacts);
            }
            else
            {
              pubFacts = sr.m_sSourcePublicationFacts;
            }

            if (pubFacts.Length > 7 && pubFacts.ToUpper().Substring(0, 7) == "HTTP://")
            {
              pubFacts = String.Concat("<a href=\"", pubFacts, "\">", EscapeHTML(pubFacts, false), "</a>");
              extraInfo += String.Concat("\n                <li>", pubFacts, "</li>");
            }
            else
            {
              extraInfo += String.Concat("\n                <li>", EscapeHTML(pubFacts, false), "</li>");
            }
          }

          // Where within source //
          string whereWithinSource = sc.GetWhereWithinSource();
          if (whereWithinSource != null && whereWithinSource.Length > 0)
          {
            extraInfo += String.Concat("\n                <li>", EscapeHTML(whereWithinSource, false), "</li>");
          }

          // Certainty assessment //
          string certaintyAssessment = sc.GetCertaintyAssessment();
          if (certaintyAssessment != null && certaintyAssessment.Length > 0)
          {
            extraInfo += String.Concat("\n                <li>", EscapeHTML(certaintyAssessment, false), "</li>");
          }

          // Surround any extra info in its own list
          if (extraInfo.Length > 0)
          {
            extraInfo = String.Concat("\n              <ul>", extraInfo, "\n              </ul>");
          }

          // Finally write source link and extra info
          f.m_sw.WriteLine(String.Concat("            <li>", sc.MakeLinkText(i + 1), extraInfo, "</li>"));
        }
        f.m_sw.WriteLine("          </ul>");
        f.m_sw.WriteLine("        </div> <!-- references -->");
      }
    }

    // Outputs the HTML for the Notes section of the page
    private void OutputNotes(CHTMLFile f)
    {
      if (m_ir.m_alNoteStructures.Count > 0)
      {
        // Generate notes list into a local array before adding header title. This is to cope with the case where all notes are nothing but blanks.
        ArrayList note_strings = new ArrayList(m_ir.m_alNoteStructures.Count);

        foreach (CNoteStructure ns in m_ir.m_alNoteStructures)
        {
          if (ns.Text != null && ns.Text.Length > 0)
          {
            string noteText;
            if (MainForm.s_config.m_bObfuscateEmails)
            {
              noteText = ObfuscateEmail(ns.Text);
            }
            else
            {
              noteText = ns.Text;
            }

            string sourceRefs = "";
            if (ns.m_alSourceCitations != null)
            {
              sourceRefs = AddSources(ref m_alReferenceList, ns.m_alSourceCitations);
            }
            note_strings.Add(String.Concat("            <li>", EscapeHTML(noteText, false), sourceRefs, "</li>"));
          }
        }

        if (note_strings.Count > 0)
        {
          f.m_sw.WriteLine("        <div id=\"notes\">");
          f.m_sw.WriteLine("          <h1>Notes</h1>");
          f.m_sw.WriteLine("          <ul>");

          foreach (string note_string in note_strings)
          {
            f.m_sw.WriteLine(note_string);
          }

          f.m_sw.WriteLine("          </ul>");
          f.m_sw.WriteLine("        </div> <!-- notes -->");
        }
      }
    }

    // Outputs the HTML for the Other Facts section of the page.
    private void OutputAttributes(CHTMLFile f)
    {
      if (m_alAttributeList.Count > 0)
      {
        f.m_sw.WriteLine("        <div id=\"facts\">");
        f.m_sw.WriteLine("          <h1>Other facts</h1>");
        f.m_sw.WriteLine("          <table>");

        for (int i = 0; i < m_alAttributeList.Count; i++)
        {
          CIEvent iEvent = (CIEvent)m_alAttributeList[i];

          string importance;
          if (iEvent.Important)
          {
            importance = " class=\"important\"";
          }
          else
          {
            importance = "";  
          }

          string attrNote = "";
          string noteString = iEvent.Note;
          if (noteString != null)
          {
            attrNote = String.Concat("<p class=\"eventNote\">", EscapeHTML(noteString, false), "</p>");
          }

          f.m_sw.WriteLine("            <tr>");
          f.m_sw.WriteLine("              <td class=\"date\"><p>&nbsp;</p></td>");
          f.m_sw.WriteLine("              <td class=\"event\"><p{0}>{1}</p>{2}</td>", importance, iEvent.ToString(), attrNote);
          f.m_sw.WriteLine("            </tr>");

        }
        f.m_sw.WriteLine("          </table>");
        f.m_sw.WriteLine("        </div> <!-- facts -->");
      }
    }

    // Outputs the HTML for the Life History section of the page.
    private void OutputEvents(CHTMLFile f)
    {
      if (m_alEventList.Count > 0)
      {
        f.m_sw.WriteLine("        <div id=\"events\">");
        f.m_sw.WriteLine("          <h1>Life History</h1>");
        f.m_sw.WriteLine("          <table>");

        for (int i = 0; i < m_alEventList.Count; i++)
        {
          CIEvent iEvent = (CIEvent)m_alEventList[i];

          string importance;
          if (iEvent.Important)
          {
            importance = " class=\"important\"";
          }
          else
          {
            importance = "";
          }

          string eventNote = "";
          string overviewString = iEvent.Overview;
          if (overviewString != null && overviewString != "")
          {
            eventNote = String.Concat("<p class=\"eventNote\">", EscapeHTML(overviewString, false), "</p>");
          }
          string noteString = iEvent.Note;
          if (noteString != null && noteString != "")
          {
            eventNote += String.Concat("<p class=\"eventNote\">", EscapeHTML(noteString, false), "</p>");
          }
          string preference = "";
          if (iEvent.Preference == CIEvent.EPreference.First)
          {
            preference = " (most likely)";
          }
          else if (iEvent.Preference == CIEvent.EPreference.Subsequent)
          {
            preference = " (less likely)";
          }
          f.m_sw.WriteLine("            <tr>");
          f.m_sw.WriteLine("              <td class=\"date\"><p{0}>{1}</p></td>", importance, EscapeHTML(iEvent.Date, false));
          f.m_sw.WriteLine("              <td class=\"event\"><p{0}>{1}</p>{2}{3}</td>", importance, iEvent.ToString(), preference, eventNote);
          f.m_sw.WriteLine("            </tr>");

        }
        f.m_sw.WriteLine("          </table>");
        f.m_sw.WriteLine("        </div> <!-- events -->");
      }
    }

    // Writes the "Parents" section of the page to the HTML file. 
    private void OutputParentNames(CHTMLFile f)
    {
      if (m_alParents.Count > 0)
      {
        f.m_sw.WriteLine("        <div id=\"parents\">");
        f.m_sw.WriteLine("          <h1>Parents</h1>");

        string sChild = "Child";
        if (m_ir.Sex == 'M')
        {
          sChild = "Son";
        }
        else if (m_ir.Sex == 'F')
        {
          sChild = "Daughter";
        }

        for (int i = 0; i < m_alParents.Count; i++)
        {
          CHusbandAndWife parents = (CHusbandAndWife)m_alParents[i];
          string sParents = "";
          if (parents.m_irHusband != null && parents.m_irHusband.Visibility() == CIndividualRecord.EVisibility.Visible)
          {
            sParents = MakeLink(parents.m_irHusband);
          }
          if (parents.m_irWife != null && parents.m_irWife.Visibility() == CIndividualRecord.EVisibility.Visible)
          {
            string wifeName = MakeLink(parents.m_irWife);
            if (sParents == "")
            {
              sParents = wifeName;
            }
            else
            {
              sParents += " & " + wifeName;
            }
          }
          if (sParents != "")
          {
            f.m_sw.WriteLine(String.Concat("          <p>", sChild, " of ", sParents, ".</p>"));
          }
        }
        f.m_sw.WriteLine("        </div> <!-- parents -->");
      }
    }

    // Writes the individual's lifespan and occupation to the HTML file. 
    private void OutputIndividualSummary(CHTMLFile f)
    {
      f.m_sw.WriteLine("          <div id=\"individualSummary\">");

      string sBirthday;
      if (m_dateActualBirthday != null)
      {
        sBirthday = m_dateActualBirthday.ToString() + m_sBirthdaySourceRefs;
      }
      else
      {
        sBirthday = "";
      }

      string sDeathday;
      if (m_dateActualDeathday != null)
      {
        sDeathday = m_dateActualDeathday.ToString() + m_sDeathdaySourceRefs;
      }
      else
      {
        sDeathday = "";
      }

      if (m_dateActualBirthday != null || m_dateActualDeathday != null)
      {
        f.m_sw.WriteLine(String.Concat("            <p>", sBirthday, " - ", sDeathday, "</p>"));
      }
      if (MainForm.s_config.m_bOccupationHeadline && m_sOccupation != null && m_sOccupation != "")
      {
        f.m_sw.WriteLine(String.Concat("            <p>", m_sOccupation, "</p>"));
      }
      if (m_bConcealed)
      {
        f.m_sw.WriteLine("            <p>Information about this individual has been withheld.</p>");
      }
      f.m_sw.WriteLine("          </div> <!-- individualSummary -->");
    }

    // Writes the individual's names to the HTML file. 
    private void OutputNames(CHTMLFile f)
    {
      f.m_sw.WriteLine("          <div id=\"names\">");
      if (m_sFullName != m_sName)
      {
        f.m_sw.WriteLine(String.Concat("            <h2>", EscapeHTML(m_sFullName, false), "</h2>"));
      }
      if (m_sUsedName != "" && m_sNickName != "")
      {
        m_sUsedName += ", ";
      }
      string nicknames = "";
      if (m_sUsedName != "" || m_sNickName != "")
      {
        nicknames = String.Concat(" <span class=\"nicknames\">(", EscapeHTML(m_sUsedName, false), EscapeHTML(m_sNickName, false), ")</span>");
      }
      f.m_sw.WriteLine(String.Concat("            <h1>", EscapeHTML(m_sName, false), m_sNameSources, nicknames, "</h1>"));
      foreach (CNameAndSource other_name in m_alOtherNames)
      {
        f.m_sw.WriteLine(String.Concat("            <h2>also known as ", EscapeHTML(other_name.m_sName, false), other_name.m_sSourceHtml, "</h2>"));
      }
      f.m_sw.WriteLine("          </div> <!-- names -->");
    }

    // Writes the HTML for the multimedia files associated with this record. 
    private void OutputMultimedia(CHTMLFile f)
    {
      if (m_alMultimediaList.Count > 0)
      {
        CIMultimedia iMultimedia = (CIMultimedia)m_alMultimediaList[0];
        f.m_sw.WriteLine("    <div id=\"photos\">");
        f.m_sw.WriteLine("      <div id=\"mainphoto\">");
        string non_pic_small_filename = "multimedia/" + MainForm.NonPicFilename(iMultimedia.m_sFormat, true, MainForm.s_config.m_bLinkOriginalPicture);
        string non_pic_main_filename = "multimedia/" + MainForm.NonPicFilename(iMultimedia.m_sFormat, false, MainForm.s_config.m_bLinkOriginalPicture);
        string image_title = "";
        string alt_name = m_sFullName;
        if (iMultimedia.m_sTitle != null)
        {
          image_title = iMultimedia.m_sTitle;
          alt_name = iMultimedia.m_sTitle;
        }
        if (MainForm.s_config.m_bLinkOriginalPicture)
        {
          if (iMultimedia.m_nWidth != 0 && iMultimedia.m_nHeight != 0)
          {
            // Must be a picture.
            if (iMultimedia.m_sLargeFilename.Length > 0)
            {
              f.m_sw.WriteLine(String.Concat("        <a href=\"", iMultimedia.m_sLargeFilename, "\" id=\"mainphoto_link\"><img id=\"mainphoto_img\" src=\"", iMultimedia.m_sFilename, "\" alt=\"", alt_name, "\" /></a>"));
            }
            else
            {
              f.m_sw.WriteLine(String.Concat("        <img id=\"mainphoto_img\" src=\"", iMultimedia.m_sFilename, "\" alt=\"I", alt_name, "\" />"));
            }
          }
          else
          {
            // Must be a non-picture multimedia file.
            if (iMultimedia.m_sLargeFilename.Length > 0)
            {
              f.m_sw.WriteLine(String.Concat("        <a href=\"", iMultimedia.m_sLargeFilename, "\" id=\"mainphoto_link\"><img id=\"mainphoto_img\" src=\"", non_pic_main_filename, "\" alt=\"", alt_name, "\" /></a>"));
            }
            else
            {
              f.m_sw.WriteLine(String.Concat("        <img id=\"mainphoto_img\" src=\"", non_pic_main_filename, "\" alt=\"", alt_name, "\" />"));
            }
          }

        }
        else // Not linking to original picture.
        {
          if (iMultimedia.m_nWidth != 0 && iMultimedia.m_nHeight != 0)
          {
            // Must be a picture.
            f.m_sw.WriteLine(String.Concat("        <img id=\"mainphoto_img\" src=\"", iMultimedia.m_sFilename, "\" alt=\"", alt_name, "\" />"));
          }
          else
          {
            // Must be a non-picture multimedia file.
            f.m_sw.WriteLine(String.Concat("        <img id=\"mainphoto_img\" src=\"", non_pic_main_filename, "\" alt=\"", alt_name, "\" />"));
          }
        }
        f.m_sw.WriteLine(String.Concat("        <p id=\"mainphoto_title\">", image_title, "</p>"));
        f.m_sw.WriteLine("      </div>");

        if (m_alMultimediaList.Count > 1 && MainForm.s_config.m_bAllowMultipleImages)
        {
          f.m_sw.WriteLine("      <div id=\"miniphotos\">");

          for (int i = 0; i < m_alMultimediaList.Count; i++)
          {
            iMultimedia = (CIMultimedia)m_alMultimediaList[i];

            non_pic_small_filename = "multimedia/" + MainForm.NonPicFilename(iMultimedia.m_sFormat, true, MainForm.s_config.m_bLinkOriginalPicture);
            non_pic_main_filename = "multimedia/" + MainForm.NonPicFilename(iMultimedia.m_sFormat, false, MainForm.s_config.m_bLinkOriginalPicture);

            string largeFilenameArg;
            if (iMultimedia.m_sLargeFilename != null && iMultimedia.m_sLargeFilename.Length > 0)
            {
              largeFilenameArg = String.Concat("'", iMultimedia.m_sLargeFilename, "'");
            }
            else
            {
              largeFilenameArg = "null";
            }

            f.m_sw.WriteLine("         <div class=\"miniphoto\">");
            if (iMultimedia.m_nWidth != 0 && iMultimedia.m_nHeight != 0)
            {
              // Must be a picture.
              // Scale mini pic down to thumbnail.
              Rectangle newArea = new Rectangle(0, 0, iMultimedia.m_nWidth, iMultimedia.m_nHeight);
              MainForm.ScaleAreaToFit(ref newArea, MainForm.s_config.m_uMaxThumbnailImageWidth, MainForm.s_config.m_uMaxThumbnailImageHeight);

              f.m_sw.WriteLine(String.Concat("          <img style=\"width:", newArea.Width, "px; height:", newArea.Height, "px; margin-bottom:", MainForm.s_config.m_uMaxThumbnailImageHeight - newArea.Height, "px;\" class=\"miniphoto_img\" src=\"", iMultimedia.m_sFilename, "\" alt=\"Click to select\" onclick=\"updateMainPhoto('", iMultimedia.m_sFilename, "','", EscapeJavascript(iMultimedia.m_sTitle), "',", largeFilenameArg, ")\" />"));
            }
            else
            {
              // Other multimedia.
              f.m_sw.WriteLine(String.Concat("          <img style=\"width:", MainForm.s_config.m_uMaxThumbnailImageWidth, "px; height:", MainForm.s_config.m_uMaxThumbnailImageHeight, "px;\" class=\"miniphoto_img\" src=\"", non_pic_small_filename, "\" alt=\"Click to select\" onclick=\"updateMainPhoto('", non_pic_main_filename, "','", EscapeJavascript(iMultimedia.m_sTitle), "',", largeFilenameArg, ")\" />"));
            }
            f.m_sw.WriteLine("         </div>");
          }

          f.m_sw.WriteLine("      </div>");
        }
        f.m_sw.WriteLine("    </div> <!-- photos -->");
      }
    }

    // Writes the HTML for the mini tree diagram, including the image alMap data. 
    private void OutputMiniTree(CHTMLFile f)
    {
      System.Drawing.Imaging.ImageFormat imageFormat;
      string miniTreeExtn;
      string imageFormatString = MainForm.s_config.m_sMiniTreeImageFormat;
      switch (imageFormatString)
      {
        case "png":
          imageFormat = System.Drawing.Imaging.ImageFormat.Png;
          miniTreeExtn = "png";
          break;
        default:
          imageFormat = System.Drawing.Imaging.ImageFormat.Gif;
          miniTreeExtn = "gif";
          break;
      }

      CTreeDrawer treeDrawer = new CTreeDrawer(m_gedcom);
      string relativeTreeFilename = String.Concat("tree", m_ir.m_xref, ".", miniTreeExtn);
      string fullTreeFilename = String.Concat(MainForm.s_config.m_sOutputFolder, "\\", relativeTreeFilename);
      ArrayList map = treeDrawer.CreateMiniTree(m_paintbox, m_ir, fullTreeFilename, MainForm.s_config.m_nTargetTreeWidth, imageFormat);
      if (map != null)
      {
        // Add space to height so that IE's horiz scroll bar has room and doesn't create a vertical scroll bar.
        f.m_sw.WriteLine(String.Format("    <div id=\"minitree\" style=\"height:{0}px;\">", treeDrawer.Height + 20)); 
        f.m_sw.WriteLine("      <map name=\"treeMap\" id=\"tree\">");
        foreach (CMiniTreeMap mapItem in map)
        {
          if (mapItem.m_bLinkable)
          {
            string href = GetIndividualHTMLFilename(mapItem.m_ir);
            f.m_sw.WriteLine(String.Concat("        <area alt=\"", mapItem.m_sName, "\" coords=\"", mapItem.m_x1, ",", mapItem.m_y1, ",", mapItem.m_x2, ",", mapItem.m_y2, "\" href=\"", href, "\" shape=\"rect\" />"));
          }
        }
        f.m_sw.WriteLine("      </map>");
        f.m_sw.WriteLine(String.Concat("      <img src=\"", relativeTreeFilename, "\"  usemap=\"#treeMap\" alt=\"Mini tree diagram\"/>"));
        f.m_sw.WriteLine("    </div>");
      }
    }

    // If only one occupation for this individual, and it has no associated date, this method 
    // ensures that we show it only in the title, not in the other facts section as well.
    private void RemoveLoneOccupation()
    {
      bool bSanityCheck = false;
      if (MainForm.s_config.m_bOccupationHeadline)
      {
        if (m_alOccupations.Count == 1)
        {
          if (((COccupationCounter)m_alOccupations[0]).m_date == null)
          {
            // Remove from attributeList
            for (int i = 0; i < m_alAttributeList.Count; i++)
            {
              CIEvent iEvent = (CIEvent)m_alAttributeList[i];
              if (iEvent.Type == "OCCU")
              {
                m_alAttributeList.RemoveAt(i);
                bSanityCheck = true;
                break;
              }
            }
            if (!bSanityCheck)
            {
              LogFile.TheLogFile.WriteLine(LogFile.DT_HTML, LogFile.EDebugLevel.Warning, "Expected to find occupation event");
            }
          }
        }
      }
    }

    // Extracts the data from the given event, and creates a CIEvent instance for it and adds it to the list of events.
    // Does specific processing if appropriate to the event.
    // linkToOtherParty is an href link to the other party concerned in the event. Typically this is for fr events such as engagement, marriage etc where
    // the other party would be the partner.
    private void ProcessEvent(CEventStructure es, string linkToOtherParty )
    {
      LogFile.TheLogFile.WriteLine(LogFile.DT_HTML, LogFile.EDebugLevel.Note, String.Format("ProcessEvent( {0}, {1} )", es.Type != null ? es.Type : "null", es.Value != null ? es.Value : "null"));

      if (es.Type == null)
      {
        return;
      }
      string utype = es.Type.ToUpper();
      string subtype = es.m_sSubtype;

      // Strip trailing _ that FTM seems sometimes to include
      while (subtype.Length > 0 && subtype.Substring(subtype.Length - 1, 1) == "_")
      {
        subtype = subtype.Substring(0, subtype.Length - 1);
      }

      // Useful holder vars
      CPGDate date;
      string place;
      string escaped_description = "";
      string address = "";
      string url = "";
      string cause = "";

      bool important = false;
      date = null;
      place = "";
      string place_word = MainForm.s_config.m_sPlaceWord;
      string alternative_place_word = "and"; // For want of anything better...
      string alternative_place = "";
      if (es.m_eventDetail != null)
      {
        date = es.m_eventDetail.m_dateValue;
        if (es.m_eventDetail.m_placeStructure != null)
        {
          place = es.m_eventDetail.m_placeStructure.m_sPlaceName;
        }
        alternative_place = es.m_eventDetail.m_sAlternativePlace;

        if (es.m_eventDetail.m_addressStructure != null)
        {
          address = es.m_eventDetail.m_addressStructure.ToString();
          url = es.m_eventDetail.m_addressStructure.GetUrl();
        }

        cause = es.m_eventDetail.m_sCauseOfEvent;
      }



      string sourceRefs = "";
      if (es.m_eventDetail != null && es.Type != "MARR" && es.Type != "TITL") // Marriage handled separately later.
      {
        sourceRefs = AddSources(ref m_alReferenceList, es.m_eventDetail.m_alSourceCitations);
      }

      bool bNeedValue = false;
      bool bOnlyIncludeIfNotePresent = false;
      bool bIncludeOccupation = false;

      // First occurrence of an event in GEDCOM is the "preferred" one, where in real life there can be only one of the event (e.g. BIRT)
      bool bTypeIsAOneOff = false;

      // Fix for Family Tree Maker 2008 which exports occupation as generic EVEN events.
      // It also puts occupation in PLAC field, but this is already accommodated later.
      if (es.Type == "EVEN" && subtype.ToLower() == "occupation")
      {
        es.Type = "OCCU";
      }

      switch (utype)
      {
        case "BIRT":
          if (es is CIndividualEventStructure)
          {
            bTypeIsAOneOff = true;
            if (m_qdateInferredBirthday != null)
            {
              // Throw away lesser qualified birthday inferences.
              if (m_qdateInferredBirthday.m_eqQualification > CPGQualifiedDate.EQualification.Birth) // ">" here means "further from the truth".
              {
                m_qdateInferredBirthday = null;
              }
            }
            if (m_qdateInferredBirthday == null) // Take first BIRT we come across. In GEDCOM this means it is the preferred event.
            {
              m_qdateInferredBirthday = new CPGQualifiedDate(date, CPGQualifiedDate.EQualification.Birth);
            }
            m_sBirthdaySourceRefs = sourceRefs;
          }
          escaped_description = "born";
          important = true;
          break;

        case "CHR":
          if (es is CIndividualEventStructure)
          {
            if (m_qdateInferredBirthday != null)
            {
              // Throw away lesser qualified birthday inferences.
              if (m_qdateInferredBirthday.m_eqQualification > CPGQualifiedDate.EQualification.Christening) // ">" here means "further from the truth".
              {
                m_qdateInferredBirthday = null;
              }
            }
            if (m_qdateInferredBirthday == null) // In the absence of a BIRT event this will have to do.
            {
              m_qdateInferredBirthday = new CPGQualifiedDate(date, CPGQualifiedDate.EQualification.Christening);
              m_sBirthdaySourceRefs = sourceRefs;
            }
          }
          escaped_description = "christened";
          break;

        case "BAPM":
          if (es is CIndividualEventStructure)
          {
            if (m_qdateInferredBirthday != null)
            {
              // Throw away lesser qualified birthday inferences.
              if (m_qdateInferredBirthday.m_eqQualification > CPGQualifiedDate.EQualification.Baptism) // ">" here means "further from the truth".
              {
                m_qdateInferredBirthday = null;
              }
            }
            if (m_qdateInferredBirthday == null) // In the absence of a BIRT event this will have to do.
            {
              m_qdateInferredBirthday = new CPGQualifiedDate(date, CPGQualifiedDate.EQualification.Baptism);
              m_sBirthdaySourceRefs = sourceRefs;
            }
          }
          escaped_description = "baptised";
          break;

        case "DEAT":
          bTypeIsAOneOff = true;
          if (es is CIndividualEventStructure)
          {
            if (m_qdateInferredDeathday != null)
            {
              // Throw away lesser qualified birthday inferences.
              if (m_qdateInferredDeathday.m_eqQualification > CPGQualifiedDate.EQualification.Death) // ">" here means "further from the truth".
              {
                m_qdateInferredDeathday = null;
              }
            }
            if (m_qdateInferredDeathday == null) // Take first DEAT we come across. In GEDCOM this means it is the preferred event.
            {
              m_qdateInferredDeathday = new CPGQualifiedDate(date, CPGQualifiedDate.EQualification.Death);
            }
            m_sDeathdaySourceRefs = sourceRefs;
          }
          escaped_description = "died";
          important = true;
          break;

        case "BURI":
          bTypeIsAOneOff = true;
          if (es is CIndividualEventStructure)
          {
            if (m_qdateInferredDeathday != null)
            {
              // Throw away lesser qualified birthday inferences.
              if (m_qdateInferredDeathday.m_eqQualification > CPGQualifiedDate.EQualification.Burial) // ">" here means "further from the truth".
              {
                m_qdateInferredDeathday = null;
              }
            }
            if (m_qdateInferredDeathday == null) // In the absence of a DEAT event this will have to do.
            {
              m_qdateInferredDeathday = new CPGQualifiedDate(date, CPGQualifiedDate.EQualification.Burial);
              m_sDeathdaySourceRefs = sourceRefs;
            }
          }
          escaped_description = "buried";
          break;

        case "CREM":
          bTypeIsAOneOff = true;
          if (es is CIndividualEventStructure)
          {
            if (m_qdateInferredDeathday != null)
            {
              // Throw away lesser qualified birthday inferences.
              if (m_qdateInferredDeathday.m_eqQualification > CPGQualifiedDate.EQualification.Cremation) // ">" here means "further from the truth".
              {
                m_qdateInferredDeathday = null;
              }
            }
            if (m_qdateInferredDeathday == null) // In the absence of a DEAT event this will have to do.
            {
              m_qdateInferredDeathday = new CPGQualifiedDate(date, CPGQualifiedDate.EQualification.Cremation);
              m_sDeathdaySourceRefs = sourceRefs;
            }
          }
          escaped_description = "cremated";
          break;

        case "ADOP":
          escaped_description = "adopted";
          if (es.m_eventDetail != null)
          {
            CFamilyRecord adopFam = m_gedcom.GetFamilyRecord(es.m_eventDetail.m_xrefFam);
            CIndividualRecord adopHusb = null;
            CIndividualRecord adopWife = null;
            if (adopFam != null && es.m_eventDetail != null)
            {
              if (es.m_eventDetail.m_bAdoptedByHusband)
              {
                adopHusb = m_gedcom.GetIndividualRecord(adopFam.m_xrefHusband);
              }

              if (es.m_eventDetail.m_bAdoptedByWife)
              {
                adopWife = m_gedcom.GetIndividualRecord(adopFam.m_xrefWife);
              }

              if (adopHusb != null || adopWife != null)
              {
                escaped_description += " by ";
                if (adopHusb != null && adopHusb.Visibility() != CIndividualRecord.EVisibility.Invisible)
                {
                  escaped_description += MakeLink(adopHusb);
                  if (adopWife != null)
                  {
                    escaped_description += " and ";
                  }
                }
                if (adopWife != null && adopWife.Visibility() != CIndividualRecord.EVisibility.Invisible)
                {
                  escaped_description += MakeLink(adopWife);
                }
              }
            }
          }

          break;

        case "BARM":
          escaped_description = "bar mitzvah";
          break;

        case "BASM":
          escaped_description = "bat mitzvah";
          break;

        case "BLES":
          escaped_description = "blessing";
          break;

        case "CHRA":
          escaped_description = "christened (as adult)";
          break;

        case "CONF":
          escaped_description = "confirmed";
          break;

        case "FCOM":
          escaped_description = "first communion";
          break;

        case "ORDN":
          escaped_description = "ordained";
          break;

        case "NATU":
          escaped_description = "naturalized";
          break;

        case "EMIG":
          escaped_description = "emigrated";
          place_word = "from";
          alternative_place_word = "to";
          break;

        case "IMMI":
          escaped_description = "immigrated";
          place_word = "to";
          alternative_place_word = "from";
          break;
        /*  handled as fr event below
                case "CENS":
                  escaped_description = "recorded in census";
                  break;*/

        case "PROB":
          escaped_description = "probate";
          break;

        case "WILL":
          escaped_description = "wrote will";
          break;

        case "GRAD":
          escaped_description = "graduated";
          break;

        case "RETI":
          escaped_description = "retired";
          break;

        case "EVEN":
          if (subtype != null && subtype != "")
          {
            escaped_description = EscapeHTML(subtype, false);
          }
          else
          {
            escaped_description = "other event";
          }
          if (es.Value != null && es.Value != "")
          {
              escaped_description += ": " + es.Value;
          }
          break;

        case "CAST":
          escaped_description = "caste";
          if (es.Value != null && es.Value != "")
            escaped_description = String.Concat(escaped_description, " ", EscapeHTML(es.Value, false));
          else
            bNeedValue = true;
          break;

        case "DSCR":
          escaped_description = "physical description";
          if (es.Value != null && es.Value != "")
            escaped_description = String.Concat(escaped_description, " ", EscapeHTML(es.Value, false));
          else
            bNeedValue = true;
          break;

        case "EDUC":
          escaped_description = "educated";
          if (es.Value != null && es.Value != "")
            escaped_description = String.Concat(escaped_description, " ", EscapeHTML(es.Value, false));
          else
            bNeedValue = true;
          break;

        case "IDNO":
          escaped_description = "ID number";
          if (es.Value != null && es.Value != "")
            escaped_description = String.Concat(escaped_description, " ", EscapeHTML(es.Value, false));
          else
            bNeedValue = true;
          break;

        case "NATI":
          escaped_description = "nationality";
          if (es.Value != null && es.Value != "")
            escaped_description = String.Concat(escaped_description, " ", EscapeHTML(es.Value, false));
          else
            bNeedValue = true;
          break;

        case "NCHI":
          bTypeIsAOneOff = true;
          escaped_description = "number of children";
          if (es.Value != null && es.Value != "")
            escaped_description = String.Concat(escaped_description, " ", EscapeHTML(es.Value, false));
          else
            bNeedValue = true;
          break;

        case "NMR":
          bTypeIsAOneOff = true;
          escaped_description = "number of marriages";
          if (es.Value != null && es.Value != "")
            escaped_description = String.Concat(escaped_description, " ", EscapeHTML(es.Value, false));
          else
            bNeedValue = true;

          break;

        case "OCCU":
          escaped_description = "occupation";
          if (es.Value != null && es.Value != "")
          {
            COccupationCounter oc = new COccupationCounter(EscapeHTML(es.Value, false) + sourceRefs, date);
            m_alOccupations.Add(oc);
            escaped_description = String.Concat(escaped_description, " ", EscapeHTML(es.Value, false));
            bIncludeOccupation = true;
          }
          else
            bNeedValue = true;
          break;

        case "PROP":
          escaped_description = "property";
          if (es.Value != null && es.Value != "")
            escaped_description = String.Concat(escaped_description, " ", EscapeHTML(es.Value, false));
          else
            bNeedValue = true;
          break;

        case "RELI":
          escaped_description = "religion";
          if (es.Value != null && es.Value != "")
            escaped_description = String.Concat(escaped_description, " ", EscapeHTML(es.Value, false));
          else
            bNeedValue = true;
          break;

        case "RESI":
          escaped_description = "resident";
          if (es.Value != null && es.Value != "")
            escaped_description = String.Concat(escaped_description, " ", EscapeHTML(es.Value, false));
          else
            bNeedValue = false; // Special case, we need the "at" word left in for this.
          break;

        case "SSN":
          escaped_description = "Social Security number";
          if (es.Value != null && es.Value != "")
            escaped_description = String.Concat(escaped_description, " ", EscapeHTML(es.Value, false));
          else
            bNeedValue = true;
          break;

        case "TITL":
          /* This is handled as a special case outside of event processing*/
          place = ""; // Clear place to avoid creating spurious event entry
          break;

        case "FACT":
          escaped_description = "other fact";
          if (es.Value != null && es.Value != "")
            escaped_description = String.Concat(escaped_description, " ", EscapeHTML(es.Value, false));
          else
            bNeedValue = true;
          break;


        case "_NMR": // _NMR Brother's Keeper
          bTypeIsAOneOff = true;
          escaped_description = "never married";
          if (es.Value != null && es.Value != "")
            escaped_description = String.Concat(escaped_description, " ", EscapeHTML(es.Value, false));
          else
            bNeedValue = true;
          break;

        case "_AKA": // _AKA Brother's Keeper
        case "_AKAN": // _AKAN Brother's Keeper
          escaped_description = "also known as";
          if (es.Value != null && es.Value != "")
            escaped_description = String.Concat(escaped_description, " ", EscapeHTML(es.Value, false));
          else
            bNeedValue = true;
          break;

        // Now the fr events:
        case "ANUL":
          escaped_description = "annulment of marriage";
          if (es.Value != null && es.Value != "")
            escaped_description = String.Concat(escaped_description, " ", EscapeHTML(es.Value, false));
          else
            bNeedValue = true;
          if (linkToOtherParty != null && linkToOtherParty != "")
          {
            escaped_description = String.Concat(escaped_description, " to ", linkToOtherParty);
          }

          break;

        case "CENS":
          escaped_description = "recorded in census";
          break;

        case "DIV":
          if (es.Value != null && (es.Value == "N" || es.Value == "n"))
          {
            place = ""; // Clear place to prevent this event being shown
          }
          else
          {
            escaped_description = "divorced";
            if (linkToOtherParty != null && linkToOtherParty != "")
            {
              escaped_description = String.Concat(escaped_description, " from ", linkToOtherParty);
            }
          }


          break;

        case "DIVF":
          escaped_description = "filing of divorce";
          if (linkToOtherParty != null && linkToOtherParty != "")
          {
            escaped_description = String.Concat(escaped_description, " from ", linkToOtherParty);
          }
          break;

        case "ENGA":
          escaped_description = "engagement";
          if (linkToOtherParty != null && linkToOtherParty != "")
          {
            escaped_description = String.Concat(escaped_description, " to ", linkToOtherParty);
          }
          break;

        case "MARB":
          escaped_description = "publication of banns of marriage";
          if (linkToOtherParty != null && linkToOtherParty != "")
          {
            escaped_description = String.Concat(escaped_description, " to ", linkToOtherParty);
          }

          break;

        case "MARC":
          escaped_description = "contract of marriage";
          if (linkToOtherParty != null && linkToOtherParty != "")
          {
            escaped_description = String.Concat(escaped_description, " to ", linkToOtherParty);
          }

          break;

        case "MARR":
          /* This is handled as a special case outside of event processing*/
          place = ""; // Clear place to avoid creating spurious event entry
          break;

        case "MARL":
          escaped_description = "licence obtained for marriage";
          if (linkToOtherParty != null && linkToOtherParty != "")
          {
            escaped_description = String.Concat(escaped_description, " to ", linkToOtherParty);
          }

          break;

        case "MARS":
          escaped_description = "settlement of marriage";
          if (linkToOtherParty != null && linkToOtherParty != "")
          {
            escaped_description = String.Concat(escaped_description, " to ", linkToOtherParty);
          }

          break;

        case "GEDMILL_ADOPTION_OF_CHILD":
          if (es.m_eventDetail != null)
          {
            CIndividualRecord adoptedChild = m_gedcom.GetIndividualRecord(es.m_eventDetail.m_xrefAdoptedChild);
            if (adoptedChild != null)
            {
              escaped_description = String.Concat("adopted ", MakeLink(adoptedChild));
            }
          }
          break;

        default:
          escaped_description = "unknown event";
          if (es.Value != null && es.Value != "")
            escaped_description = String.Concat(escaped_description, " ", EscapeHTML(es.Value, false));
          else
            bNeedValue = true;
          break;
      }

      if (MainForm.s_config.m_bCapitaliseEventDescriptions)
      {
        Capitalise(ref escaped_description);
      }

      if (place != "")
      {
        // It seems some earlier GEDCOM has PLAC value filled with the event value, and the event value blank. Accomodate this:
        if ((es.Value == null || es.Value == "") && bNeedValue)
        {
          escaped_description += " " + EscapeHTML(place, false);
          if (utype == "OCCU")
          {
            COccupationCounter oc = new COccupationCounter(place,date);
            m_alOccupations.Add(oc);
          }
          place = "";
          bIncludeOccupation = true; // Needed to include occupation event, (without date or place), in page.
        }
        else
        {
          escaped_description += String.Concat(" ", place_word, " ", EscapeHTML(place, false));
          if (alternative_place != null && alternative_place.Length > 0)
          {
            escaped_description += String.Concat(" ", alternative_place_word, " ", EscapeHTML(alternative_place, false));
          }
        }
      }

      if (address != "")
      {
        if (escaped_description.Length > 0)
        {
          escaped_description += " (" + EscapeHTML(address, false) + ")";
        }
        else
        {
          escaped_description = EscapeHTML(address, false);
        }
      }

      if (url != "")
      {
        if (escaped_description.Length > 0)
        {
          escaped_description += " (<a href=\"" + (url) + "\">" + (url) + "</a>)";
        }
        else
        {
          escaped_description = "<a href=\"" + (url) + "\">" + (url) + "</a>";
        }
      }

      string overview = "";
      if (es.m_eventDetail != null && es.m_eventDetail.m_sOverview != null && es.m_eventDetail.m_sOverview != "")
      {
        overview = es.m_eventDetail.m_sOverview;
      }

      if (escaped_description == "")
      {
        return; // In the case of MARR and TITL and DIV N we don't want to add anything here.
      }

      escaped_description += ".";
      escaped_description += sourceRefs;

      string eventNote = "";

      if (cause != "")
      {
        cause = EscapeHTML(cause, false);
        if (MainForm.s_config.m_bCapitaliseEventDescriptions)
        {
          Capitalise(ref cause);
        }
        if (eventNote.Length > 0)
        {
          eventNote += "\n";
        }
        if (MainForm.s_config.m_bObfuscateEmails)
        {
          eventNote += ObfuscateEmail(cause);
        }
        else
        {
          eventNote += cause;
        }
      }

      if (es.m_eventDetail != null)
      {
        if (es.m_eventDetail.m_alNoteStructures != null)
        {
          foreach (CNoteStructure ns in es.m_eventDetail.m_alNoteStructures)
          {
            if (ns.Text != null && ns.Text.Length > 0)
            {
              if (eventNote != "")
              {
                eventNote += "\n";
              }
              if (MainForm.s_config.m_bObfuscateEmails)
              {
                eventNote += ObfuscateEmail(ns.Text);
              }
              else
              {
                eventNote += ns.Text;
              }
            }
          }
        }
      }

      CIEvent iEvent = null;

      if (!bOnlyIncludeIfNotePresent || eventNote != "")
      {
        if (date != null)
        {
          iEvent = new CIEvent(date, utype, escaped_description, overview, eventNote, important, MainForm.s_config.m_bCapitaliseEventDescriptions);
          m_alEventList.Add(iEvent);
        }
        // else its an attribute.
        else
        {
          // Don't include plain "Died" and nothing else. Roots Magic seems to use this just to signify that person died. But it appears on every single page and looks silly.
          // GSP Family Tree puts lots of blank tags (OCCU, CHR, SEX, NOTE, etc.etc). Don't display those without meaning
          // Note CHR is contentious, as other s/w may use a CHR with no other info to mean that they were christened. GSP it appears puts a CHR for everyone?
          if ((utype != "DEAT" && utype != "BIRT" && utype != "CHR" && utype != "OCCU") || place != "" || eventNote != "" || bIncludeOccupation)
          {
            iEvent = new CIEvent(null, utype, escaped_description, overview, eventNote, important, MainForm.s_config.m_bCapitaliseEventDescriptions);
            m_alAttributeList.Add(iEvent);
          }
        }
      }

      if (iEvent != null && bTypeIsAOneOff)
      {
        if (m_htFirstFoundEvent.ContainsKey(utype))
        {
          // We have multiple occurences of this event. Mark the one we saw first as 'preferred'.
          CIEvent firstEvent = (CIEvent)m_htFirstFoundEvent[utype];
          if (firstEvent != null)
          {
            firstEvent.Preference = CIEvent.EPreference.First;
            iEvent.Preference = CIEvent.EPreference.Subsequent;
          }
        }
        else
        {
          m_htFirstFoundEvent[utype] = iEvent;
        }
      }

    }

    // Adds the given source citations to the given list of referenced sources, and returns an HTML link string.
    private static string AddSources( ref ArrayList referenceList, ArrayList sourceCitations )
    {
      LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Note, "AddSources()" );

      string sourceRefs = "";
      foreach( CSourceCitation sc in sourceCitations )
      {
        int sourceNumber = -1;

        if( !sc.Restricted )
        {
          // Is source already in list?
          for( int i = 0; i<referenceList.Count; ++i )
          {
            if( ((CSourceCitation)referenceList[i]).SameAs( sc ) )
            {
              sourceNumber = i;
              break;
            }
          }

          bool bComma = false;
          if( sourceRefs != "" )
          {
            bComma = true;
          }

          if( sourceNumber == -1 )
          {
            sourceNumber = referenceList.Count;
            referenceList.Add( sc );
          }

          sourceRefs += sc.MakeLinkNumber( (uint)(sourceNumber+1), bComma );
        }
      }
      return sourceRefs;
    }

    // Picks the individual's occupation closest to the given date, within the given limits.
    private static string BestOccupation(ArrayList occupations, CPGDate givenDate, CPGDate lowerLimit, CPGDate upperLimit)
    {
      int minDifference;
      if (lowerLimit == null || upperLimit == null)
      {
        minDifference = Int32.MaxValue;
      }
      else
      {
        minDifference = Math.Abs(CPGDate.Difference(lowerLimit, upperLimit));
      }

      COccupationCounter bestOc = null;

      foreach (COccupationCounter oc in occupations)
      {
        if (oc.m_date == null)
        {
          // Dateless occupation assumed to be the generic answer
          return oc.m_sName;
        }
        else
        {
          int sdifference = CPGDate.Difference(givenDate, oc.m_date);
          int difference = Math.Abs(sdifference);
          if (Math.Sign(sdifference) == -1)
          {
            // favours occupations before date rather than after it.
            difference *= 3;
            difference /= 2;
          }
          if (Math.Abs(difference) < minDifference)
          {
            minDifference = difference;
            bestOc = oc;
          }
        }
      }

      if (bestOc == null)
        return "";

      return bestOc.m_sName;
    }

    // Creates a string describing the marital status of the given fr. Prepends the string provided in marriageNote.    
    private static string BuildMaritalStatusNote(CFamilyRecord fr, string marriageNote)
    {
      if (fr.m_sStatus != null && fr.m_sStatus != "")
      {
        if (marriageNote != "")
        {
          marriageNote += "\n";
        }
        if (fr.m_sStatus.ToLower() == "unknown") // Nasty hack for Family Historian using strings to denote marital status
        {
          marriageNote += "Marital status unknown";
        }
        else
        {
          marriageNote += fr.m_sStatus;
        }
      }
      return marriageNote;
    }

  }
}
