/* 
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
 */

using System;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Imaging;
using System.IO;
using GDModel;
using GEDmill.MiniTree;
using GEDmill.Model;
using GKCore.Logging;
using GKCore.Types;
using GDModel.Providers.GEDCOM;

namespace GEDmill.HTML
{
    public class CreatorRecordIndividual : CreatorRecord
    {
        private static readonly ILogger fLogger = LogManager.GetLogger(CConfig.LOG_FILE, CConfig.LOG_LEVEL, typeof(CreatorRecordIndividual).Name);

        // The individual record that we are creating the page for.
        private GDMIndividualRecord fIndiRec;

        // Indicates that this individual should have most of the record data excluded from the website for privacy.
        private bool fConcealed;

        // List of the events in the individual's life history.
        private List<Event> fEventList;

        // List of other facts known about the individual.
        private List<Event> fAttributeList;

        // List of sources referenced by this page.
        private List<GDMSourceCitation> fReferenceList;

        // List of occupations known for the individual
        private List<OccupationCounter> fOccupations;

        // An HTML link to the previous sibling in this fr.
        private string fPreviousChildLink;

        // An HTML link to the next sibling in this fr.
        private string fNextChildLink;

        // List of aliases and other names for the individual
        private List<NameAndSource> fOtherNames;

        // A date inferred for the individual's birthday, with associated quality so that it can be rejected in favour of more certain information.
        private QualifiedDate fInferredBirthday;

        // The individual's date of birth
        private GDMDateValue fActualBirthday;

        // A date inferred for the individual's date of death, with associated quality so that it can be rejected in favour of more certain information.
        private QualifiedDate fInferredDeathday;

        // The individual's date of death
        private GDMDateValue fActualDeathday;

        // Records first occurrence of one-off events, so that they may be marked as "preferred"
        private Dictionary<string, Event> fFirstFoundEvent;

        // The sources giving the individual's birth date
        private string fBirthdaySourceRefs;

        // The sources giving the individual's death date
        private string fDeathdaySourceRefs;

        // The individual's title (GEDCOM:TITL)
        private string fNameTitle;

        // Indicates that a name for this individual is not available
        private bool fUnknownName;

        // The individual's main name
        private string fName;

        // The suffix on the individual's name (e.g. "Snr")
        private string fNameSuffix;

        // The individual's first name
        private string fFirstName;

        // The individual's surname
        private string fSurname;

        // The individual's fully expanded name
        private string fFullName;

        // The individual's nickname
        private string fNickName;

        // The individual's commonly used name
        private string fUsedName;

        // The sources giving the individual's name
        private string fNameSources;

        // The individual's occupation, for display at the page head
        private string fOccupation;

        // All the frParents of the individual. May be more than one pair if individual was associated with more than one fr.
        private List<HusbandAndWife> fParents;

        // A reference to the index creator, so that individual pages can be added to the index as they are created.
        private CreatorIndexIndividuals fIndiIndexCreator;

        // The paintbox with which to draw the mini tree.
        private Paintbox fPaintbox;


        public CreatorRecordIndividual(GDMTree tree, IProgressCallback progress, string w3cfile, GDMIndividualRecord ir, CreatorIndexIndividuals indiIndexCreator, Paintbox paintbox) : base(tree, progress, w3cfile)
        {
            fIndiRec = ir;
            fIndiIndexCreator = indiIndexCreator;
            fPaintbox = paintbox;
            fFirstFoundEvent = new Dictionary<string, Event>();
            fBirthdaySourceRefs = "";
            fDeathdaySourceRefs = "";
            fNameTitle = "";
            fUnknownName = false;
            fName = fIndiRec.GetPrimaryFullName();
            fNameSuffix = ""/*fIndiRec.NameSuffix*/; // TODO
            fFirstName = "";
            fSurname = "";
            fOccupation = "";
            fConcealed = !fIndiRec.GetVisibility();
            fEventList = new List<Event>();
            fAttributeList = new List<Event>();
            fReferenceList = new List<GDMSourceCitation>();
            fOccupations = new List<OccupationCounter>();
            fPreviousChildLink = "";
            fNextChildLink = "";
            fOtherNames = new List<NameAndSource>();
            fInferredBirthday = null;
            fActualBirthday = null;
            fInferredDeathday = null;
            fActualDeathday = null;
            fParents = new List<HusbandAndWife>();
        }

        // The main method that causes the page to be created.
        public bool Create(Stats stats)
        {
            fLogger.WriteInfo("CCreatorRecordIndividual.Create()");

            if (fIndiRec == null) {
                return false;
            }

            if (!fIndiRec.GetVisibility()) {
                return false;
            }

            // Collect together multimedia links
            if (CConfig.Instance.AllowMultimedia && !fConcealed) {
                AddMultimedia(fIndiRec.MultimediaLinks, string.Concat(fIndiRec.XRef, "mm"), string.Concat(fIndiRec.XRef, "mo"), CConfig.Instance.MaxImageWidth, CConfig.Instance.MaxImageHeight, stats);
            }

            AddEvents();

            RemoveLoneOccupation();

            var lifeDatesX = fIndiRec.GetLifeDates();
            fActualBirthday = (lifeDatesX.BirthEvent == null) ? null : lifeDatesX.BirthEvent.Date;
            fActualDeathday = (lifeDatesX.DeathEvent == null) ? null : lifeDatesX.DeathEvent.Date;

            ConstructName();

            GDMDateValue age30;
            if (fInferredBirthday != null) {
                age30 = new GDMDateValue();
                age30.Assign(fInferredBirthday.Date);
            } else {
                age30 = new GDMDateValue();
                age30.SetDateTime(DateTime.Now);
            }
            try {
                ((GDMDate)age30.Value).Year += (short)CConfig.Instance.AgeForOccupation;
            } catch { }

            // We should have birthday and deathday by now, so find longest occupation
            if (!fConcealed) {
                fOccupation = BestOccupation(fOccupations, age30, (fInferredBirthday != null) ? fInferredBirthday.Date : null, (fInferredDeathday != null) ? fInferredDeathday.Date : null); // Picks occupation with longest time span
            }

            // Go through all families this person was a irSubject to
            if (!fConcealed) {
                foreach (GDMSpouseToFamilyLink spLink in fIndiRec.SpouseToFamilyLinks) {
                    GDMFamilyRecord fr = fTree.GetPtrValue<GDMFamilyRecord>(spLink);
                    if (fr != null) {
                        // Find the irSubject's name
                        GDMIndividualRecord spouse = null;
                        string spouseLink = "";
                        spouse = fTree.GetSpouseBy(fr, fIndiRec);
                        if (spouse != null && spouse.GetVisibility()) {
                            spouseLink = MakeLink(spouse);
                        }

                        // Add fr events as events connected to this individual
                        foreach (GDMFamilyEvent fes in fr.Events) {
                            ProcessEvent(fes, spouseLink);
                        }

                        AddChildrensEvents(fr);

                        AddMarriage(spouse, spouseLink, fr);
                    }
                }
                AddParentsAndSiblings();
            }

            string birthyear = "";
            string deathyear = "";
            if (!fConcealed) {
                if (fInferredBirthday != null && fInferredBirthday.Date != null) {
                    birthyear = fInferredBirthday.Date.GetDisplayStringExt(DateFormat.dfYYYY, false, false);
                }
                if (fInferredDeathday != null && fInferredDeathday.Date != null) {
                    deathyear = fInferredDeathday.Date.GetDisplayStringExt(DateFormat.dfYYYY, false, false);
                }
            }

            string title = fName; //"Fred Bloggs 1871-1921"
            string lifeDates = "";
            if (!fConcealed) {
                if (birthyear != "" || deathyear != "") {
                    lifeDates = string.Concat(birthyear, "-", deathyear);
                    title = string.Concat(fName, " ", lifeDates);
                }
            }

            AddIndividualIndexEntry(lifeDates);

            OutputHTML(title);

            return true;
        }

        // Adds the marriage associated with the fr record to the list of events. Also adds irSubject death if within this person's lifetime.
        private void AddMarriage(GDMIndividualRecord spouse, string spouseLink, GDMFamilyRecord fr)
        {
            // Find wedding date
            if (spouse != null) {
                string sourceRefs = AddSpouseDeath(spouse, spouseLink);

                GDMDateValue marriageDate;
                string marriageNote;
                string marriagePlace;
                sourceRefs = AddMarriageEvent(fr, sourceRefs, out marriageDate, out marriageNote, out marriagePlace);

                marriageNote = BuildMaritalStatusNote(fr, marriageNote);

                // Add fr record notes to marriage event
                foreach (GDMNotes ns in fr.Notes) {
                    if (marriageNote != "") {
                        marriageNote += "\n";
                    }

                    marriageNote += GetNoteText(ns);
                }

                string marriedString = "married ";
                if (fr.Status == GDMMarriageStatus.MarrNotRegistered) {
                    marriedString = "partner of ";
                }

                if (marriageDate != null) {
                    Event iEvent = new Event(marriageDate, "_MARRIAGE", string.Concat(marriedString, spouseLink, marriagePlace, ".", sourceRefs), "", marriageNote, true, CConfig.Instance.CapitaliseEventDescriptions);
                    fEventList.Add(iEvent);
                } else {
                    Event iEvent = new Event(marriageDate, "_MARRIAGE", string.Concat(marriedString, spouseLink, marriagePlace, ".", sourceRefs), "", marriageNote, true, CConfig.Instance.CapitaliseEventDescriptions);
                    // Marriages go at the front of the list so that they appear first in "Other facts"
                    fAttributeList.Insert(0, iEvent);
                }
            }
        }

        private string GetNoteText(GDMNotes ns)
        {
            GDMLines noteLines = fTree.GetNoteLines(ns);
            string result;
            if (CConfig.Instance.ObfuscateEmails) {
                result = ObfuscateEmail(noteLines.Text);
            } else {
                result = noteLines.Text;
            }

            return result;
        }

        // Goes through all families this person was a irSibling in and finds their frParents and siblings.
        private void AddParentsAndSiblings()
        {
            // Set a limit for date comparisons
            DateTime dtNow = DateTime.Now;

            // Go through all families this person was a irSibling in
            foreach (GDMChildToFamilyLink childLink in fIndiRec.ChildToFamilyLinks) {
                var famRec = fTree.GetPtrValue(childLink);
                if (famRec != null) {
                    GDMIndividualRecord husband = fTree.GetPtrValue(famRec.Husband);
                    GDMIndividualRecord wife = fTree.GetPtrValue(famRec.Wife);

                    if (husband != null || wife != null) {
                        fParents.Add(new HusbandAndWife(husband, wife));
                    }

                    // Get all the children in order, to find previous and next irSibling
                    GDMDateValue testBirthday = (fInferredBirthday != null) ? fInferredBirthday.Date : null;

                    if (testBirthday == null) {
                        testBirthday = new GDMDateValue();
                        testBirthday.SetDateTime(dtNow);
                    }

                    int previousDifference = -100 * 365; // 100 years should be enough
                    int nextDifference = 100 * 365;

                    foreach (var childPtr in famRec.Children) {
                        if (childPtr.XRef == fIndiRec.XRef)
                            continue;

                        var child = fTree.GetPtrValue<GDMIndividualRecord>(childPtr);
                        if (child != null) {
                            if (!child.GetVisibility())
                                continue;

                            GDMCustomEvent childBirthday = child.FindEvent("BIRT");
                            if (childBirthday == null) {
                                childBirthday = child.FindEvent("CHR");
                            }
                            if (childBirthday == null) {
                                childBirthday = child.FindEvent("BAPM");
                            }

                            GDMDateValue childBirthdate = null;
                            if (childBirthday != null)
                                childBirthdate = childBirthday.Date;
                            if (childBirthdate == null) {
                                childBirthdate = new GDMDateValue();
                                childBirthdate.SetDateTime(dtNow);
                            }

                            int difference = Extensions.GetEventsYearsDiff(testBirthday, childBirthdate);
                            if (difference < 0) {
                                if (difference > previousDifference) {
                                    previousDifference = difference;
                                    fPreviousChildLink = MakeLink(child, "previous child");
                                }
                            } else if (difference > 0) {
                                if (difference < nextDifference) {
                                    nextDifference = difference;
                                    fNextChildLink = MakeLink(child, "next child");
                                }
                            } else {
                                fNextChildLink = MakeLink(child, "next child");
                            }
                        }
                    }
                }
            }
        }

        // Adds this individual page to the index of pages.
        private void AddIndividualIndexEntry(string lifeDates)
        {
            string relativeFilename = GetIndividualHTMLFilename(fIndiRec);
            // Create some strings to use in index entry
            string sUserRef = "";
            if (fIndiRec.UserReferences.Count > 0) {
                GDMUserReference urn = fIndiRec.UserReferences[0];
                sUserRef = EscapeHTML(urn.StringValue, false);
                if (sUserRef.Length > 0) {
                    sUserRef = string.Concat(" [", sUserRef, "]");
                }
            }
            string alterEgo = "";
            if (CConfig.Instance.IncludeNickNamesInIndex) {
                if (fNickName != "") {
                    alterEgo = string.Concat("(", fNickName, ") ");
                } else if (fUsedName != "") {
                    alterEgo = string.Concat("(", fUsedName, ") ");
                }
            }

            if (fIndiIndexCreator != null) {
                // Add index entry for this individuals main name (or hidden/unknown string)
                string sFirstName = fFirstName;
                if (!string.IsNullOrEmpty(fNameSuffix)) {
                    sFirstName += ", " + fNameSuffix;
                }
                fIndiIndexCreator.AddIndividualToIndex(sFirstName, fSurname, fUnknownName, alterEgo, lifeDates, fConcealed, relativeFilename, sUserRef);

                // Add entries for this individual's other names
                if (!fConcealed && !fUnknownName) {
                    string other_name = "";
                    for (int i = 1; (other_name = fIndiRec.GetName(i)) != ""; i++) {
                        string other_firstName = "";
                        string other_surname = "";
                        other_name = GMHelper.CapitaliseName(other_name, ref other_firstName, ref other_surname); // Also splits name into first name and surname
                        fIndiIndexCreator.AddIndividualToIndex(other_firstName, other_surname, fUnknownName, alterEgo, lifeDates, fConcealed, relativeFilename, sUserRef);
                    }
                }
            }
        }

        // Extracts the data from the MARR event for the fr record.
        private string AddMarriageEvent(GDMFamilyRecord fr, string sourceRefs, out GDMDateValue marriageDate, out string marriageNote, out string marriagePlace)
        {
            // Find out when they were married
            marriageDate = null;
            marriagePlace = "";
            sourceRefs = "";
            marriageNote = "";
            foreach (GDMFamilyEvent fes in fr.Events) {
                if (fes.GetTagName() == "MARR") {
                    {
                        marriageDate = fes.Date;

                        if (fes.Place != null) {
                            if (fes.Place.StringValue != "")
                                marriagePlace = string.Concat(" ", CConfig.Instance.PlaceWord, " ", EscapeHTML(fes.Place.StringValue, false));
                        }

                        sourceRefs = AddSources(ref fReferenceList, fes.SourceCitations);

                        if (fes.Notes != null) {
                            foreach (GDMNotes ns in fes.Notes) {
                                if (marriageNote != "") {
                                    marriageNote += "\n";
                                }

                                marriageNote += GetNoteText(ns);
                            }
                        }
                        break;
                    }
                }
            }
            return sourceRefs;
        }

        // Extracts the data from the DEAT event for the given individual and adds it if it was an event in the current individual's lifetime.
        private string AddSpouseDeath(GDMIndividualRecord spouse, string spouseLink)
        {
            string sourceRefs = "";
            string place = "";
            if (spouse.GetVisibility()) {
                // Record death of irSubject if within this person's lifetime
                GDMDateValue spouseDeathDate = null;
                foreach (GDMCustomEvent ies in spouse.Events) {
                    if (ies.GetTagName() == "DEAT") {
                        {
                            spouseDeathDate = ies.Date;
                            if (spouseDeathDate != null) {
                                if (fInferredDeathday == null || fInferredDeathday.Date == null || spouseDeathDate.CompareTo(fInferredDeathday.Date) <= 0) {
                                    if (ies.Place != null) {
                                        if (ies.Place.StringValue != "")
                                            place = string.Concat(" ", CConfig.Instance.PlaceWord, " ", EscapeHTML(ies.Place.StringValue, false));
                                    }

                                    sourceRefs = AddSources(ref fReferenceList, ies.SourceCitations);

                                    if (spouseDeathDate != null) {
                                        Event iEvent = new Event(spouseDeathDate, "_SPOUSEDIED", string.Concat("death of ", spouseLink, place, ".", sourceRefs), "", null, false, CConfig.Instance.CapitaliseEventDescriptions);
                                        fEventList.Add(iEvent);
                                    } else {
                                        Event iEvent = new Event(null, "_SPOUSEDIED", string.Concat("death of ", spouseLink, place, ".", sourceRefs), "", null, false, CConfig.Instance.CapitaliseEventDescriptions);
                                        fAttributeList.Add(iEvent);
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
        private void AddChildrensEvents(GDMFamilyRecord famRec)
        {
            // Find out all the children.
            foreach (var childPtr in famRec.Children) {
                var child = fTree.GetPtrValue<GDMIndividualRecord>(childPtr);

                if (child != null) {
                    bool childConcealed = !child.GetVisibility();

                    string childSex = "child";
                    if (!childConcealed) {
                        if (child.Sex == GDMSex.svMale) {
                            childSex = "son";
                        } else if (child.Sex == GDMSex.svFemale) {
                            childSex = "daughter";
                        }
                    }

                    string childLink = MakeLink(child);
                    string sourceRefs = "";

                    if (!childConcealed) {
                        // Add death of children if happened in irSubject's lifetime.
                        // Note this is done before birth because the way the subsequent sort works it will put death after birth.
                        GDMCustomEvent childDeathday = child.FindEvent("DEAT");
                        if (childDeathday == null) {
                            childDeathday = child.FindEvent("BURI");
                        }
                        if (childDeathday == null) {
                            childDeathday = child.FindEvent("CREM");
                        }

                        string deathPlace = "";
                        GDMDateValue childDeathdate = null;

                        if (childDeathday != null) {
                            childDeathdate = childDeathday.Date;

                            if (childDeathday.Place != null) {
                                if (childDeathday.Place.StringValue != "") {
                                    deathPlace = string.Concat(" ", CConfig.Instance.PlaceWord, " ", EscapeHTML(childDeathday.Place.StringValue, false));
                                }
                            }
                        }

                        if (childDeathdate != null && fInferredDeathday != null && fInferredDeathday.Date != null && (childDeathdate.CompareTo(fInferredDeathday.Date) <= 0)) {
                            sourceRefs = AddSources(ref fReferenceList, childDeathday.SourceCitations);
                            Event iEvent = new Event(childDeathdate, "_CHILDDIED", string.Concat("death of ", childSex, " ", childLink, deathPlace, ".", sourceRefs), "", null, false, CConfig.Instance.CapitaliseEventDescriptions);
                            fEventList.Add(iEvent);
                        }
                    }

                    // Add birth of children.
                    // Note this is done after deaths because the way the subsequent sort works it will put death after birth.
                    GDMCustomEvent childBirthday = child.FindEvent("BIRT");
                    if (childBirthday == null) {
                        childBirthday = child.FindEvent("CHR");
                    }
                    if (childBirthday == null) {
                        childBirthday = child.FindEvent("BAPM");
                    }

                    string birthPlace = "";
                    GDMDateValue childBirthdate = null;
                    sourceRefs = "";

                    if (childBirthday != null && !childConcealed) {
                        childBirthdate = childBirthday.Date;

                        if (childBirthday.Place != null) {
                            if (childBirthday.Place.StringValue != "") {
                                birthPlace = string.Concat(" ", CConfig.Instance.PlaceWord, " ", EscapeHTML(childBirthday.Place.StringValue, false));
                            }
                        }
                        sourceRefs = AddSources(ref fReferenceList, childBirthday.SourceCitations);
                    }

                    if (childBirthdate == null) {
                        Event iEvent = new Event(null, "_CHILDBORN", string.Concat("birth of ", childSex, " ", childLink, birthPlace, ".", sourceRefs), "", null, true, CConfig.Instance.CapitaliseEventDescriptions);
                        fAttributeList.Add(iEvent);
                    } else {
                        Event iEvent = new Event(childBirthdate, "_CHILDBORN", string.Concat("birth of ", childSex, " ", childLink, birthPlace, ".", sourceRefs), "", null, true, CConfig.Instance.CapitaliseEventDescriptions);
                        fEventList.Add(iEvent);
                    }
                }
            }
        }

        // Works through all the events records for this individual and extracts information from them.
        private void AddEvents()
        {
            if (fIndiRec.Events != null && !fConcealed) {
                foreach (GDMCustomEvent ies in fIndiRec.Events) {
                    ProcessEvent(ies, null);
                    if (ies.GetTagName() == "TITL") {
                        if (fNameTitle.Length > 0) {
                            fNameTitle += " ";
                        }
                        fNameTitle += ies.StringValue;
                    }
                }
            }
        }

        // Extracts the name information from the individual record.
        private void ConstructName()
        {
            // Construct the guy's name
            if (fConcealed && !CConfig.Instance.UseWithheldNames) {
                fFirstName = "";
                fSurname = fName = CConfig.Instance.ConcealedName;
            } else {
                fName = GMHelper.CapitaliseName(fName, ref fFirstName, ref fSurname); // Also splits name into first name and surname
            }
            if (fName == "") {
                fFirstName = "";
                fSurname = fName = CConfig.Instance.UnknownName;
                fUnknownName = true;
            }

            // Remember other name records
            if (!fConcealed && !fUnknownName) {
                NameAndSource nasOther;
                for (int i = 1; (nasOther = fIndiRec.GetNameAndSource(i)) != null; i++) {
                    string sFirstNameOther = "";
                    string sSurnameOther = "";
                    nasOther.Name = GMHelper.CapitaliseName(nasOther.Name, ref sFirstNameOther, ref sSurnameOther); // Also splits name into first name and surname
                    nasOther.SourceHtml = AddSources(ref fReferenceList, nasOther.Sources);
                    fOtherNames.Add(nasOther);
                }
            }

            if (fConcealed && !CConfig.Instance.UseWithheldNames) {
                fFullName = CConfig.Instance.ConcealedName;
            } else {
                fFullName = fIndiRec.GetPrimaryFullName();
                string sDummy = "";
                fFullName = GMHelper.CapitaliseName(fFullName, ref sDummy, ref sDummy); // Also splits name into first name and surname
            }
            if (fFullName == "") {
                fFullName = CConfig.Instance.UnknownName;
            }

            if (fNameTitle.Length > 0) {
                fFullName = string.Concat(fNameTitle, " ", fFullName);
            }
            if (fConcealed) {
                fNickName = "";
                fUsedName = "";
            } else {
                fNickName = /*fIndiRec.NickName*/""; // TODO
                fUsedName = /*fIndiRec.UsedName*/""; // TODO
            }

            // Add general source references
            fNameSources = "";
            if (!fConcealed) {
                //fIndiRec.AddMainNameSources(ref alNameSourcesList);
                fNameSources = AddSources(ref fReferenceList, fIndiRec.SourceCitations);
            }
        }

        // Creates a file and writes into it the HTML for the individual's page.
        private void OutputHTML(string title)
        {
            HTMLFile f = null;
            string pageDescription = "GEDmill GEDCOM to HTML page for " + fName;
            string keywords = "family tree history " + fName;
            string relativeFilename = GetIndividualHTMLFilename(fIndiRec);
            string fullFilename = string.Concat(CConfig.Instance.OutputFolder, "\\", relativeFilename);

            try {
                f = new HTMLFile(fullFilename, title, pageDescription, keywords); // Creates a new file, and puts standard header html into it.

                if (f != null) {
                    OutputPageHeader(f, fPreviousChildLink, fNextChildLink, true, true);

                    if (CConfig.Instance.ShowMiniTrees) {
                        OutputMiniTree(f);
                    }
                    f.WriteLine("    <div class=\"hr\" />");
                    f.WriteLine("");
                    f.WriteLine("    <div id=\"page\"> <!-- page -->");

                    OutputMultimedia(f);

                    f.WriteLine("      <div id=\"main\">");

                    f.WriteLine("        <div id=\"summary\">");
                    OutputNames(f);
                    OutputIndividualSummary(f);
                    f.WriteLine("        </div> <!-- summary -->");

                    if (!CConfig.Instance.ShowMiniTrees) {
                        OutputParentNames(f);
                    }

                    if (!fConcealed) {
                        fEventList.Sort();
                        OutputEvents(f);
                        OutputAttributes(f);
                        OutputNotes(f, fIndiRec.Notes);
                        OutputSourceReferences(f);
                    }

                    f.WriteLine("      </div> <!-- main -->");

                    f.WriteLine("");

                    // Add footer (Record date, W3C sticker, GEDmill credit etc.)
                    OutputFooter(f, fIndiRec);

                    f.WriteLine("    </div> <!-- page -->");
                }
            } catch (IOException e) {
                fLogger.WriteError("Caught IO Exception(4) : ", e);
            } catch (ArgumentException e) {
                fLogger.WriteError("Caught Argument Exception(4) : ", e);
            } finally {
                if (f != null) {
                    // Close adds the standard footer to the file
                    f.Close();
                }
            }
        }

        // Outputs the HTML for the list of Sources referenced in the page.
        private void OutputSourceReferences(HTMLFile f)
        {
            if (fReferenceList.Count > 0) {
                f.WriteLine("        <div id=\"references\">");
                f.WriteLine("          <h1>Sources</h1>");
                f.WriteLine("          <ul>");

                for (uint i = 0; i < fReferenceList.Count; i++) {
                    GDMSourceCitation sourCit = fReferenceList[(int)i];

                    string extraInfo = "";
                    var source = fTree.GetPtrValue<GDMSourceRecord>(sourCit);

                    // Publication facts
                    if (source != null && source.Publication.Lines.Text != null && source.Publication.Lines.Text != "") {
                        string pubFacts;
                        if (CConfig.Instance.ObfuscateEmails) {
                            pubFacts = ObfuscateEmail(source.Publication.Lines.Text);
                        } else {
                            pubFacts = source.Publication.Lines.Text;
                        }

                        if (pubFacts.Length > 7 && pubFacts.ToUpper().Substring(0, 7) == "HTTP://") {
                            pubFacts = string.Concat("<a href=\"", pubFacts, "\">", EscapeHTML(pubFacts, false), "</a>");
                            extraInfo += string.Concat("\n                <li>", pubFacts, "</li>");
                        } else {
                            extraInfo += string.Concat("\n                <li>", EscapeHTML(pubFacts, false), "</li>");
                        }
                    }

                    // Where within source
                    // TODO
                    /*string whereWithinSource = sc.GetWhereWithinSource();
                    if (whereWithinSource != null && whereWithinSource.Length > 0) {
                        extraInfo += string.Concat("\n                <li>", EscapeHTML(whereWithinSource, false), "</li>");
                    }*/

                    // Certainty assessment
                    // TODO
                    /*string certaintyAssessment = sc.GetCertaintyAssessment();
                    if (certaintyAssessment != null && certaintyAssessment.Length > 0) {
                        extraInfo += string.Concat("\n                <li>", EscapeHTML(certaintyAssessment, false), "</li>");
                    }*/

                    // Surround any extra info in its own list
                    if (extraInfo.Length > 0) {
                        extraInfo = string.Concat("\n              <ul>", extraInfo, "\n              </ul>");
                    }

                    // Finally write source link and extra info
                    f.WriteLine("<li>{0}{1}</li>", fTree.MakeLinkText(sourCit, i + 1), extraInfo);
                }
                f.WriteLine("          </ul>");
                f.WriteLine("        </div> <!-- references -->");
            }
        }

        // Outputs the HTML for the Other Facts section of the page.
        private void OutputAttributes(HTMLFile f)
        {
            if (fAttributeList.Count > 0) {
                f.WriteLine("        <div id=\"facts\">");
                f.WriteLine("          <h1>Other facts</h1>");
                f.WriteLine("          <table>");

                for (int i = 0; i < fAttributeList.Count; i++) {
                    Event iEvent = fAttributeList[i];

                    string importance = iEvent.Important ? " class=\"important\"" : "";
                    string attrNote = MakeNote(iEvent.Note);

                    f.WriteLine("            <tr>");
                    f.WriteLine("              <td class=\"date\"><p>&nbsp;</p></td>");
                    f.WriteLine("              <td class=\"event\"><p{0}>{1}</p>{2}</td>", importance, iEvent.ToString(), attrNote);
                    f.WriteLine("            </tr>");
                }
                f.WriteLine("          </table>");
                f.WriteLine("        </div> <!-- facts -->");
            }
        }

        // Outputs the HTML for the Life History section of the page.
        private void OutputEvents(HTMLFile f)
        {
            if (fEventList.Count > 0) {
                f.WriteLine("        <div id=\"events\">");
                f.WriteLine("          <h1>Life History</h1>");
                f.WriteLine("          <table>");

                for (int i = 0; i < fEventList.Count; i++) {
                    Event iEvent = fEventList[i];

                    string importance = iEvent.Important ? " class=\"important\"" : "";
                    string eventNote = MakeNote(iEvent.Overview);
                    eventNote += MakeNote(iEvent.Note);

                    string preference = "";
                    if (iEvent.Preference == EventPreference.First) {
                        preference = " (most likely)";
                    } else if (iEvent.Preference == EventPreference.Subsequent) {
                        preference = " (less likely)";
                    }
                    f.WriteLine("            <tr>");
                    f.WriteLine("              <td class=\"date\"><p{0}>{1}</p></td>", importance, EscapeHTML(iEvent.Date, false));
                    f.WriteLine("              <td class=\"event\"><p{0}>{1}</p>{2}{3}</td>", importance, iEvent.ToString(), preference, eventNote);
                    f.WriteLine("            </tr>");
                }
                f.WriteLine("          </table>");
                f.WriteLine("        </div> <!-- events -->");
            }
        }

        // Writes the "Parents" section of the page to the HTML file. 
        private void OutputParentNames(HTMLFile f)
        {
            if (fParents.Count > 0) {
                f.WriteLine("        <div id=\"parents\">");
                f.WriteLine("          <h1>Parents</h1>");

                string sChild = "Child";
                if (fIndiRec.Sex == GDMSex.svMale) {
                    sChild = "Son";
                } else if (fIndiRec.Sex == GDMSex.svFemale) {
                    sChild = "Daughter";
                }

                for (int i = 0; i < fParents.Count; i++) {
                    HusbandAndWife parents = fParents[i];
                    string sParents = "";
                    if (parents.Husband != null && parents.Husband.GetVisibility()) {
                        sParents = MakeLink(parents.Husband);
                    }
                    if (parents.Wife != null && parents.Wife.GetVisibility()) {
                        string wifeName = MakeLink(parents.Wife);
                        if (sParents == "") {
                            sParents = wifeName;
                        } else {
                            sParents += " & " + wifeName;
                        }
                    }
                    if (sParents != "") {
                        f.WriteLine(string.Concat("          <p>", sChild, " of ", sParents, ".</p>"));
                    }
                }
                f.WriteLine("        </div> <!-- parents -->");
            }
        }

        // Writes the individual's lifespan and occupation to the HTML file. 
        private void OutputIndividualSummary(HTMLFile f)
        {
            f.WriteLine("          <div id=\"individualSummary\">");

            string sBirthday;
            if (fActualBirthday != null) {
                sBirthday = fActualBirthday.GetDisplayStringExt(DateFormat.dfYYYY_MM_DD, true, false) + fBirthdaySourceRefs;
            } else {
                sBirthday = "";
            }

            string sDeathday;
            if (fActualDeathday != null) {
                sDeathday = fActualDeathday.GetDisplayStringExt(DateFormat.dfYYYY_MM_DD, true, false) + fDeathdaySourceRefs;
            } else {
                sDeathday = "";
            }

            if (fActualBirthday != null || fActualDeathday != null) {
                f.WriteLine(string.Concat("            <p>", sBirthday, " - ", sDeathday, "</p>"));
            }
            if (CConfig.Instance.OccupationHeadline && fOccupation != null && fOccupation != "") {
                f.WriteLine(string.Concat("            <p>", fOccupation, "</p>"));
            }
            if (fConcealed) {
                f.WriteLine("            <p>Information about this individual has been withheld.</p>");
            }
            f.WriteLine("          </div> <!-- individualSummary -->");
        }

        // Writes the individual's names to the HTML file. 
        private void OutputNames(HTMLFile f)
        {
            f.WriteLine("          <div id=\"names\">");
            if (fFullName != fName) {
                f.WriteLine(string.Concat("            <h2>", EscapeHTML(fFullName, false), "</h2>"));
            }
            if (fUsedName != "" && fNickName != "") {
                fUsedName += ", ";
            }
            string nicknames = "";
            if (fUsedName != "" || fNickName != "") {
                nicknames = string.Concat(" <span class=\"nicknames\">(", EscapeHTML(fUsedName, false), EscapeHTML(fNickName, false), ")</span>");
            }
            f.WriteLine(string.Concat("            <h1>", EscapeHTML(fName, false), fNameSources, nicknames, "</h1>"));
            foreach (NameAndSource other_name in fOtherNames) {
                f.WriteLine(string.Concat("            <h2>also known as ", EscapeHTML(other_name.Name, false), other_name.SourceHtml, "</h2>"));
            }
            f.WriteLine("          </div> <!-- names -->");
        }

        // Writes the HTML for the multimedia files associated with this record. 
        private void OutputMultimedia(HTMLFile f)
        {
            if (fMultimediaList.Count > 0) {
                Multimedia iMultimedia = fMultimediaList[0];
                f.WriteLine("    <div id=\"photos\">");
                f.WriteLine("      <div id=\"mainphoto\">");

                string non_pic_small_filename = "multimedia/" + GMHelper.NonPicFilename(iMultimedia.Format, true, CConfig.Instance.LinkOriginalPicture);
                string non_pic_main_filename = "multimedia/" + GMHelper.NonPicFilename(iMultimedia.Format, false, CConfig.Instance.LinkOriginalPicture);

                string imageTitle = "";
                string altName = fFullName;

                if (iMultimedia.Title != null) {
                    imageTitle = iMultimedia.Title;
                    altName = iMultimedia.Title;
                }

                if (CConfig.Instance.LinkOriginalPicture) {
                    if (iMultimedia.Width != 0 && iMultimedia.Height != 0) {
                        // Must be a picture.
                        if (iMultimedia.LargeFileName.Length > 0) {
                            f.WriteLine(string.Concat("        <a href=\"", iMultimedia.LargeFileName, "\" id=\"mainphoto_link\"><img id=\"mainphoto_img\" src=\"", iMultimedia.FileName, "\" alt=\"", altName, "\" /></a>"));
                        } else {
                            f.WriteLine(string.Concat("        <img id=\"mainphoto_img\" src=\"", iMultimedia.FileName, "\" alt=\"I", altName, "\" />"));
                        }
                    } else {
                        // Must be a non-picture multimedia file.
                        if (iMultimedia.LargeFileName.Length > 0) {
                            f.WriteLine(string.Concat("        <a href=\"", iMultimedia.LargeFileName, "\" id=\"mainphoto_link\"><img id=\"mainphoto_img\" src=\"", non_pic_main_filename, "\" alt=\"", altName, "\" /></a>"));
                        } else {
                            f.WriteLine(string.Concat("        <img id=\"mainphoto_img\" src=\"", non_pic_main_filename, "\" alt=\"", altName, "\" />"));
                        }
                    }
                } else {
                    // Not linking to original picture.
                    if (iMultimedia.Width != 0 && iMultimedia.Height != 0) {
                        // Must be a picture.
                        f.WriteLine(string.Concat("        <img id=\"mainphoto_img\" src=\"", iMultimedia.FileName, "\" alt=\"", altName, "\" />"));
                    } else {
                        // Must be a non-picture multimedia file.
                        f.WriteLine(string.Concat("        <img id=\"mainphoto_img\" src=\"", non_pic_main_filename, "\" alt=\"", altName, "\" />"));
                    }
                }
                f.WriteLine(string.Concat("        <p id=\"mainphoto_title\">", imageTitle, "</p>"));
                f.WriteLine("      </div>");

                if (fMultimediaList.Count > 1 && CConfig.Instance.AllowMultipleImages) {
                    f.WriteLine("      <div id=\"miniphotos\">");

                    for (int i = 0; i < fMultimediaList.Count; i++) {
                        iMultimedia = (Multimedia)fMultimediaList[i];

                        non_pic_small_filename = "multimedia/" + GMHelper.NonPicFilename(iMultimedia.Format, true, CConfig.Instance.LinkOriginalPicture);
                        non_pic_main_filename = "multimedia/" + GMHelper.NonPicFilename(iMultimedia.Format, false, CConfig.Instance.LinkOriginalPicture);

                        string largeFilenameArg;
                        if (!string.IsNullOrEmpty(iMultimedia.LargeFileName)) {
                            largeFilenameArg = string.Concat("'", iMultimedia.LargeFileName, "'");
                        } else {
                            largeFilenameArg = "null";
                        }

                        f.WriteLine("         <div class=\"miniphoto\">");
                        if (iMultimedia.Width != 0 && iMultimedia.Height != 0) {
                            // Must be a picture.
                            // Scale mini pic down to thumbnail.
                            Rectangle newArea = new Rectangle(0, 0, iMultimedia.Width, iMultimedia.Height);
                            GMHelper.ScaleAreaToFit(ref newArea, CConfig.Instance.MaxThumbnailImageWidth, CConfig.Instance.MaxThumbnailImageHeight);

                            f.WriteLine(string.Concat("          <img style=\"width:", newArea.Width, "px; height:", newArea.Height, "px; margin-bottom:", CConfig.Instance.MaxThumbnailImageHeight - newArea.Height, "px;\" class=\"miniphoto_img\" src=\"", iMultimedia.FileName, "\" alt=\"Click to select\" onclick=\"updateMainPhoto('", iMultimedia.FileName, "','", EscapeJavascript(iMultimedia.Title), "',", largeFilenameArg, ")\" />"));
                        } else {
                            // Other multimedia.
                            f.WriteLine(string.Concat("          <img style=\"width:", CConfig.Instance.MaxThumbnailImageWidth, "px; height:", CConfig.Instance.MaxThumbnailImageHeight, "px;\" class=\"miniphoto_img\" src=\"", non_pic_small_filename, "\" alt=\"Click to select\" onclick=\"updateMainPhoto('", non_pic_main_filename, "','", EscapeJavascript(iMultimedia.Title), "',", largeFilenameArg, ")\" />"));
                        }
                        f.WriteLine("         </div>");
                    }

                    f.WriteLine("      </div>");
                }
                f.WriteLine("    </div> <!-- photos -->");
            }
        }

        // Writes the HTML for the mini tree diagram, including the image alMap data. 
        private void OutputMiniTree(HTMLFile f)
        {
            ImageFormat imageFormat;
            string miniTreeExtn;
            string imageFormatString = CConfig.Instance.MiniTreeImageFormat;
            switch (imageFormatString) {
                case "png":
                    imageFormat = ImageFormat.Png;
                    miniTreeExtn = "png";
                    break;
                default:
                    imageFormat = ImageFormat.Gif;
                    miniTreeExtn = "gif";
                    break;
            }

            TreeDrawer treeDrawer = new TreeDrawer(fTree);
            string relativeTreeFilename = string.Concat("tree", fIndiRec.XRef, ".", miniTreeExtn);
            string fullTreeFilename = string.Concat(CConfig.Instance.OutputFolder, "\\", relativeTreeFilename);
            var map = treeDrawer.CreateMiniTree(fPaintbox, fIndiRec, fullTreeFilename, CConfig.Instance.TargetTreeWidth, imageFormat);
            if (map != null) {
                // Add space to height so that IE's horiz scroll bar has room and doesn't create a vertical scroll bar.
                f.WriteLine("    <div id=\"minitree\" style=\"height:{0}px;\">", treeDrawer.Height + 20);
                f.WriteLine("      <map name=\"treeMap\" id=\"tree\">");
                foreach (MiniTreeMap mapItem in map) {
                    if (mapItem.Linkable) {
                        string href = GetIndividualHTMLFilename(mapItem.IndiRec);
                        f.WriteLine(string.Concat("        <area alt=\"", mapItem.Name, "\" coords=\"", mapItem.X1, ",", mapItem.Y1, ",", mapItem.X2, ",", mapItem.Y2, "\" href=\"", href, "\" shape=\"rect\" />"));
                    }
                }
                f.WriteLine("      </map>");
                f.WriteLine("      <img src=\"{0}\"  usemap=\"#treeMap\" alt=\"Mini tree diagram\"/>", relativeTreeFilename);
                f.WriteLine("    </div>");
            }
        }

        // If only one occupation for this individual, and it has no associated date, this method 
        // ensures that we show it only in the title, not in the other facts section as well.
        private void RemoveLoneOccupation()
        {
            bool sanityCheck = false;
            if (CConfig.Instance.OccupationHeadline) {
                if (fOccupations.Count == 1 && fOccupations[0].Date == null) {
                    // Remove from attributeList
                    for (int i = 0; i < fAttributeList.Count; i++) {
                        Event iEvent = fAttributeList[i];
                        if (iEvent.Type == "OCCU") {
                            fAttributeList.RemoveAt(i);
                            sanityCheck = true;
                            break;
                        }
                    }
                    if (!sanityCheck) {
                        fLogger.WriteWarn("Expected to find occupation event");
                    }
                }
            }
        }

        // Extracts the data from the given event, and creates a CIEvent instance for it and adds it to the list of events.
        // Does specific processing if appropriate to the event.
        // linkToOtherParty is an href link to the other party concerned in the event. Typically this is for fr events such as engagement, marriage etc where
        // the other party would be the partner.
        private void ProcessEvent(GDMCustomEvent es, string linkToOtherParty)
        {
            fLogger.WriteInfo(string.Format("ProcessEvent( {0}, {1} )", es.GetTagName(), es.StringValue));

            if (es.GetTagName() == null) {
                return;
            }
            string utype = es.GetTagName().ToUpper();
            string subtype = es.StringValue;

            // Strip trailing _ that FTM seems sometimes to include
            while (subtype.Length > 0 && subtype[subtype.Length - 1] == '_') {
                subtype = subtype.Substring(0, subtype.Length - 1);
            }

            // Useful holder vars
            GDMDateValue date;
            string place;
            string escapedDescription = "";
            string address = "";
            string url = "";
            string cause = "";

            bool important = false;
            date = null;
            place = "";
            string placeWord = CConfig.Instance.PlaceWord;
            string alternativePlaceWord = "and"; // For want of anything better...
            string alternativePlace = "";
            if (es.Date != null) {
                date = es.Date;
                if (es.Place != null) {
                    place = es.Place.StringValue;
                }

                if (es.Address != null) {
                    address = es.Address.Lines.Text;
                    if (es.Address.WebPages.Count > 0)
                        url = es.Address.WebPages[0].StringValue;
                }
                cause = es.Cause;
            }

            string sourceRefs = "";
            if (es.GetTagName() != "MARR" && es.GetTagName() != "TITL") {
                // Marriage handled separately later.
                sourceRefs = AddSources(ref fReferenceList, es.SourceCitations);
            }

            bool needValue = false;
            bool onlyIncludeIfNotePresent = false;
            bool includeOccupation = false;

            // First occurrence of an event in GEDCOM is the "preferred" one, where in real life 
            // there can be only one of the event (e.g. BIRT)
            bool typeIsAOneOff = false;

            switch (utype) {
                case "BIRT":
                    typeIsAOneOff = true;
                    if (fInferredBirthday != null) {
                        // Throw away lesser qualified birthday inferences.
                        if (fInferredBirthday.Qualification > DateQualification.Birth) {
                            // ">" here means "further from the truth".
                            fInferredBirthday = null;
                        }
                    } else {
                        // Take first BIRT we come across. In GEDCOM this means it is the preferred event.
                        fInferredBirthday = new QualifiedDate(date, DateQualification.Birth);
                    }
                    fBirthdaySourceRefs = sourceRefs;
                    escapedDescription = "born";
                    important = true;
                    break;

                case "CHR":
                    if (fInferredBirthday != null) {
                        // Throw away lesser qualified birthday inferences.
                        if (fInferredBirthday.Qualification > DateQualification.Christening) {
                            // ">" here means "further from the truth".
                            fInferredBirthday = null;
                        }
                    } else {
                        // In the absence of a BIRT event this will have to do.
                        fInferredBirthday = new QualifiedDate(date, DateQualification.Christening);
                        fBirthdaySourceRefs = sourceRefs;
                    }
                    escapedDescription = "christened";
                    break;

                case "BAPM":
                    if (fInferredBirthday != null) {
                        // Throw away lesser qualified birthday inferences.
                        if (fInferredBirthday.Qualification > DateQualification.Baptism) {
                            // ">" here means "further from the truth".
                            fInferredBirthday = null;
                        }
                    } else {
                        // In the absence of a BIRT event this will have to do.
                        fInferredBirthday = new QualifiedDate(date, DateQualification.Baptism);
                        fBirthdaySourceRefs = sourceRefs;
                    }
                    escapedDescription = "baptised";
                    break;

                case "DEAT":
                    typeIsAOneOff = true;
                    if (fInferredDeathday != null) {
                        // Throw away lesser qualified birthday inferences.
                        if (fInferredDeathday.Qualification > DateQualification.Death) {
                            // ">" here means "further from the truth".
                            fInferredDeathday = null;
                        }
                    } else {
                        // Take first DEAT we come across. In GEDCOM this means it is the preferred event.
                        fInferredDeathday = new QualifiedDate(date, DateQualification.Death);
                    }
                    fDeathdaySourceRefs = sourceRefs;
                    escapedDescription = "died";
                    important = true;
                    break;

                case "BURI":
                    typeIsAOneOff = true;
                    if (fInferredDeathday != null) {
                        // Throw away lesser qualified birthday inferences.
                        if (fInferredDeathday.Qualification > DateQualification.Burial) {
                            // ">" here means "further from the truth".
                            fInferredDeathday = null;
                        }
                    } else {
                        // In the absence of a DEAT event this will have to do.
                        fInferredDeathday = new QualifiedDate(date, DateQualification.Burial);
                        fDeathdaySourceRefs = sourceRefs;
                    }
                    escapedDescription = "buried";
                    break;

                case "CREM":
                    typeIsAOneOff = true;
                    if (fInferredDeathday != null) {
                        // Throw away lesser qualified birthday inferences.
                        if (fInferredDeathday.Qualification > DateQualification.Cremation) {
                            // ">" here means "further from the truth".
                            fInferredDeathday = null;
                        }
                    } else {
                        // In the absence of a DEAT event this will have to do.
                        fInferredDeathday = new QualifiedDate(date, DateQualification.Cremation);
                        fDeathdaySourceRefs = sourceRefs;
                    }
                    escapedDescription = "cremated";
                    break;

                case "ADOP":
                    escapedDescription = "adopted";
                    break;

                case "BARM":
                    escapedDescription = "bar mitzvah";
                    break;

                case "BASM":
                    escapedDescription = "bat mitzvah";
                    break;

                case "BLES":
                    escapedDescription = "blessing";
                    break;

                case "CHRA":
                    escapedDescription = "christened (as adult)";
                    break;

                case "CONF":
                    escapedDescription = "confirmed";
                    break;

                case "FCOM":
                    escapedDescription = "first communion";
                    break;

                case "ORDN":
                    escapedDescription = "ordained";
                    break;

                case "NATU":
                    escapedDescription = "naturalized";
                    break;

                case "EMIG":
                    escapedDescription = "emigrated";
                    placeWord = "from";
                    alternativePlaceWord = "to";
                    break;

                case "IMMI":
                    escapedDescription = "immigrated";
                    placeWord = "to";
                    alternativePlaceWord = "from";
                    break;

                case "PROB":
                    escapedDescription = "probate";
                    break;

                case "WILL":
                    escapedDescription = "wrote will";
                    break;

                case "GRAD":
                    escapedDescription = "graduated";
                    break;

                case "RETI":
                    escapedDescription = "retired";
                    break;

                case "EVEN":
                    if (!string.IsNullOrEmpty(subtype)) {
                        escapedDescription = EscapeHTML(subtype, false);
                    } else {
                        escapedDescription = "other event";
                    }
                    if (!string.IsNullOrEmpty(es.StringValue)) {
                        escapedDescription += ": " + es.StringValue;
                    }
                    break;

                case "CAST":
                    escapedDescription = "caste";
                    if (!string.IsNullOrEmpty(es.StringValue))
                        escapedDescription = string.Concat(escapedDescription, " ", EscapeHTML(es.StringValue, false));
                    else
                        needValue = true;
                    break;

                case "DSCR":
                    escapedDescription = "physical description";
                    if (!string.IsNullOrEmpty(es.StringValue))
                        escapedDescription = string.Concat(escapedDescription, " ", EscapeHTML(es.StringValue, false));
                    else
                        needValue = true;
                    break;

                case "EDUC":
                    escapedDescription = "educated";
                    if (!string.IsNullOrEmpty(es.StringValue))
                        escapedDescription = string.Concat(escapedDescription, " ", EscapeHTML(es.StringValue, false));
                    else
                        needValue = true;
                    break;

                case "IDNO":
                    escapedDescription = "ID number";
                    if (!string.IsNullOrEmpty(es.StringValue))
                        escapedDescription = string.Concat(escapedDescription, " ", EscapeHTML(es.StringValue, false));
                    else
                        needValue = true;
                    break;

                case "NATI":
                    escapedDescription = "nationality";
                    if (!string.IsNullOrEmpty(es.StringValue))
                        escapedDescription = string.Concat(escapedDescription, " ", EscapeHTML(es.StringValue, false));
                    else
                        needValue = true;
                    break;

                case "NCHI":
                    typeIsAOneOff = true;
                    escapedDescription = "number of children";
                    if (!string.IsNullOrEmpty(es.StringValue))
                        escapedDescription = string.Concat(escapedDescription, " ", EscapeHTML(es.StringValue, false));
                    else
                        needValue = true;
                    break;

                case "NMR":
                    typeIsAOneOff = true;
                    escapedDescription = "number of marriages";
                    if (!string.IsNullOrEmpty(es.StringValue))
                        escapedDescription = string.Concat(escapedDescription, " ", EscapeHTML(es.StringValue, false));
                    else
                        needValue = true;
                    break;

                case "OCCU":
                    escapedDescription = "occupation";
                    if (!string.IsNullOrEmpty(es.StringValue)) {
                        OccupationCounter oc = new OccupationCounter(EscapeHTML(es.StringValue, false) + sourceRefs, date);
                        fOccupations.Add(oc);
                        escapedDescription = string.Concat(escapedDescription, " ", EscapeHTML(es.StringValue, false));
                        includeOccupation = true;
                    } else
                        needValue = true;
                    break;

                case "PROP":
                    escapedDescription = "property";
                    if (!string.IsNullOrEmpty(es.StringValue))
                        escapedDescription = string.Concat(escapedDescription, " ", EscapeHTML(es.StringValue, false));
                    else
                        needValue = true;
                    break;

                case "RELI":
                    escapedDescription = "religion";
                    if (!string.IsNullOrEmpty(es.StringValue))
                        escapedDescription = string.Concat(escapedDescription, " ", EscapeHTML(es.StringValue, false));
                    else
                        needValue = true;
                    break;

                case "RESI":
                    escapedDescription = "resident";
                    if (!string.IsNullOrEmpty(es.StringValue))
                        escapedDescription = string.Concat(escapedDescription, " ", EscapeHTML(es.StringValue, false));
                    else
                        needValue = false; // Special case, we need the "at" word left in for this.
                    break;

                case "SSN":
                    escapedDescription = "Social Security number";
                    if (!string.IsNullOrEmpty(es.StringValue))
                        escapedDescription = string.Concat(escapedDescription, " ", EscapeHTML(es.StringValue, false));
                    else
                        needValue = true;
                    break;

                case "TITL":
                    /* This is handled as a special case outside of event processing*/
                    place = ""; // Clear place to avoid creating spurious event entry
                    break;

                case "FACT":
                    escapedDescription = "other fact";
                    if (!string.IsNullOrEmpty(es.StringValue))
                        escapedDescription = string.Concat(escapedDescription, " ", EscapeHTML(es.StringValue, false));
                    else
                        needValue = true;
                    break;


                case "_NMR": // _NMR Brother's Keeper
                    typeIsAOneOff = true;
                    escapedDescription = "never married";
                    if (!string.IsNullOrEmpty(es.StringValue))
                        escapedDescription = string.Concat(escapedDescription, " ", EscapeHTML(es.StringValue, false));
                    else
                        needValue = true;
                    break;

                case "_AKA": // _AKA Brother's Keeper
                case "_AKAN": // _AKAN Brother's Keeper
                    escapedDescription = "also known as";
                    if (!string.IsNullOrEmpty(es.StringValue))
                        escapedDescription = string.Concat(escapedDescription, " ", EscapeHTML(es.StringValue, false));
                    else
                        needValue = true;
                    break;

                // Now the fr events:
                case "ANUL":
                    escapedDescription = "annulment of marriage";
                    if (!string.IsNullOrEmpty(es.StringValue))
                        escapedDescription = string.Concat(escapedDescription, " ", EscapeHTML(es.StringValue, false));
                    else
                        needValue = true;
                    if (!string.IsNullOrEmpty(linkToOtherParty)) {
                        escapedDescription = string.Concat(escapedDescription, " to ", linkToOtherParty);
                    }
                    break;

                case "CENS":
                    escapedDescription = "recorded in census";
                    break;

                case "DIV":
                    if (es.StringValue != null && (es.StringValue == "N" || es.StringValue == "n")) {
                        place = ""; // Clear place to prevent this event being shown
                    } else {
                        escapedDescription = "divorced";
                        if (!string.IsNullOrEmpty(linkToOtherParty)) {
                            escapedDescription = string.Concat(escapedDescription, " from ", linkToOtherParty);
                        }
                    }
                    break;

                case "DIVF":
                    escapedDescription = "filing of divorce";
                    if (!string.IsNullOrEmpty(linkToOtherParty)) {
                        escapedDescription = string.Concat(escapedDescription, " from ", linkToOtherParty);
                    }
                    break;

                case "ENGA":
                    escapedDescription = "engagement";
                    if (!string.IsNullOrEmpty(linkToOtherParty)) {
                        escapedDescription = string.Concat(escapedDescription, " to ", linkToOtherParty);
                    }
                    break;

                case "MARB":
                    escapedDescription = "publication of banns of marriage";
                    if (!string.IsNullOrEmpty(linkToOtherParty)) {
                        escapedDescription = string.Concat(escapedDescription, " to ", linkToOtherParty);
                    }
                    break;

                case "MARC":
                    escapedDescription = "contract of marriage";
                    if (!string.IsNullOrEmpty(linkToOtherParty)) {
                        escapedDescription = string.Concat(escapedDescription, " to ", linkToOtherParty);
                    }
                    break;

                case "MARR":
                    /* This is handled as a special case outside of event processing*/
                    place = ""; // Clear place to avoid creating spurious event entry
                    break;

                case "MARL":
                    escapedDescription = "licence obtained for marriage";
                    if (!string.IsNullOrEmpty(linkToOtherParty)) {
                        escapedDescription = string.Concat(escapedDescription, " to ", linkToOtherParty);
                    }
                    break;

                case "MARS":
                    escapedDescription = "settlement of marriage";
                    if (!string.IsNullOrEmpty(linkToOtherParty)) {
                        escapedDescription = string.Concat(escapedDescription, " to ", linkToOtherParty);
                    }
                    break;

                default:
                    escapedDescription = "unknown event";
                    if (!string.IsNullOrEmpty(es.StringValue))
                        escapedDescription = string.Concat(escapedDescription, " ", EscapeHTML(es.StringValue, false));
                    else
                        needValue = true;
                    break;
            }

            if (CConfig.Instance.CapitaliseEventDescriptions) {
                GMHelper.Capitalise(ref escapedDescription);
            }

            if (place != "") {
                // It seems some earlier GEDCOM has PLAC value filled with the event value, and the event value blank. Accomodate this:
                if ((string.IsNullOrEmpty(es.StringValue)) && needValue) {
                    escapedDescription += " " + EscapeHTML(place, false);
                    if (utype == "OCCU") {
                        OccupationCounter oc = new OccupationCounter(place, date);
                        fOccupations.Add(oc);
                    }
                    place = "";
                    includeOccupation = true; // Needed to include occupation event, (without date or place), in page.
                } else {
                    escapedDescription += string.Concat(" ", placeWord, " ", EscapeHTML(place, false));
                    if (!string.IsNullOrEmpty(alternativePlace)) {
                        escapedDescription += string.Concat(" ", alternativePlaceWord, " ", EscapeHTML(alternativePlace, false));
                    }
                }
            }

            if (address != "") {
                if (escapedDescription.Length > 0) {
                    escapedDescription += " (" + EscapeHTML(address, false) + ")";
                } else {
                    escapedDescription = EscapeHTML(address, false);
                }
            }

            if (url != "") {
                if (escapedDescription.Length > 0) {
                    escapedDescription += string.Format(" (<a href=\"{0}\">{0}</a>)", url);
                } else {
                    escapedDescription = string.Format("<a href=\"{0}\">{0}</a>", url);
                }
            }

            string overview = "";
            if (!string.IsNullOrEmpty(es.Classification)) {
                overview = es.Classification;
            }

            if (escapedDescription == "") {
                return; // In the case of MARR and TITL and DIV N we don't want to add anything here.
            }

            escapedDescription += ".";
            escapedDescription += sourceRefs;

            string eventNote = "";

            if (cause != "") {
                cause = EscapeHTML(cause, false);
                if (CConfig.Instance.CapitaliseEventDescriptions) {
                    GMHelper.Capitalise(ref cause);
                }
                if (eventNote.Length > 0) {
                    eventNote += "\n";
                }
                if (CConfig.Instance.ObfuscateEmails) {
                    eventNote += ObfuscateEmail(cause);
                } else {
                    eventNote += cause;
                }
            }

            foreach (GDMNotes ns in es.Notes) {
                if (eventNote != "") {
                    eventNote += "\n";
                }

                eventNote += GetNoteText(ns);
            }

            Event iEvent = null;

            if (!onlyIncludeIfNotePresent || eventNote != "") {
                if (date != null) {
                    iEvent = new Event(date, utype, escapedDescription, overview, eventNote, important, CConfig.Instance.CapitaliseEventDescriptions);
                    fEventList.Add(iEvent);
                } else {
                    // Don't include plain "Died" and nothing else. Roots Magic seems to use this just to signify that person died. But it appears on every single page and looks silly.
                    // GSP Family Tree puts lots of blank tags (OCCU, CHR, SEX, NOTE, etc.etc). Don't display those without meaning
                    // Note CHR is contentious, as other s/w may use a CHR with no other info to mean that they were christened. GSP it appears puts a CHR for everyone?
                    if ((utype != "DEAT" && utype != "BIRT" && utype != "CHR" && utype != "OCCU") || place != "" || eventNote != "" || includeOccupation) {
                        iEvent = new Event(null, utype, escapedDescription, overview, eventNote, important, CConfig.Instance.CapitaliseEventDescriptions);
                        fAttributeList.Add(iEvent);
                    }
                }
            }

            if (iEvent != null && typeIsAOneOff) {
                if (fFirstFoundEvent.ContainsKey(utype)) {
                    // We have multiple occurences of this event. Mark the one we saw first as 'preferred'.
                    Event firstEvent = fFirstFoundEvent[utype];
                    if (firstEvent != null) {
                        firstEvent.Preference = EventPreference.First;
                        iEvent.Preference = EventPreference.Subsequent;
                    }
                } else {
                    fFirstFoundEvent[utype] = iEvent;
                }
            }
        }

        // Adds the given source citations to the given list of referenced sources, and returns an HTML link string.
        private static string AddSources(ref List<GDMSourceCitation> referenceList, GDMList<GDMSourceCitation> sourceCitations)
        {
            string sourceRefs = "";
            foreach (GDMSourceCitation sourCit in sourceCitations) {
                int sourceNumber = -1;

                // Is source already in list?
                for (int i = 0; i < referenceList.Count; ++i) {
                    if (referenceList[i].XRef == sourCit.XRef) {
                        sourceNumber = i;
                        break;
                    }
                }

                bool bComma = false;
                if (sourceRefs != "") {
                    bComma = true;
                }

                if (sourceNumber == -1) {
                    sourceNumber = referenceList.Count;
                    referenceList.Add(sourCit);
                }

                sourceRefs += sourCit.MakeLinkNumber((uint)(sourceNumber + 1), bComma);
            }
            return sourceRefs;
        }

        // Picks the individual's occupation closest to the given date, within the given limits.
        private static string BestOccupation(List<OccupationCounter> occupations, GDMDateValue givenDate,
                                             GDMDateValue lowerLimit, GDMDateValue upperLimit)
        {
            int minDifference;
            if (lowerLimit == null || upperLimit == null) {
                minDifference = Int32.MaxValue;
            } else {
                minDifference = Math.Abs(Extensions.GetEventsYearsDiff(lowerLimit, upperLimit));
            }

            OccupationCounter bestOc = null;

            foreach (OccupationCounter oc in occupations) {
                if (oc.Date == null) {
                    // Dateless occupation assumed to be the generic answer
                    return oc.Name;
                } else {
                    int sdifference = Extensions.GetEventsYearsDiff(givenDate, oc.Date);
                    int difference = Math.Abs(sdifference);
                    if (Math.Sign(sdifference) == -1) {
                        // favours occupations before date rather than after it.
                        difference *= 3;
                        difference /= 2;
                    }
                    if (Math.Abs(difference) < minDifference) {
                        minDifference = difference;
                        bestOc = oc;
                    }
                }
            }

            if (bestOc == null)
                return "";

            return bestOc.Name;
        }

        // Creates a string describing the marital status of the given fr. Prepends the string provided in marriageNote.    
        private static string BuildMaritalStatusNote(GDMFamilyRecord fr, string marriageNote)
        {
            if (marriageNote != "") {
                marriageNote += "\n";
            }

            // Nasty hack for Family Historian using strings to denote marital status
            if (fr.Status == GDMMarriageStatus.Unknown) {
                marriageNote += "Marital status unknown";
            } else {
                marriageNote += fr.Status.ToString();
            }

            return marriageNote;
        }
    }
}
