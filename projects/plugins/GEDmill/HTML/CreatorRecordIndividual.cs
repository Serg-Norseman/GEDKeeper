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
using System.IO;
using BSLib;
using GDModel;
using GDModel.Providers.GEDCOM;
using GEDmill.MiniTree;
using GEDmill.Model;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;
using GKL = GKCore.Logging;

namespace GEDmill.HTML
{
    public class CreatorRecordIndividual : CreatorRecord
    {
        private static readonly GKL.ILogger fLogger = GKL.LogManager.GetLogger(GMConfig.LOG_FILE, GMConfig.LOG_LEVEL, typeof(CreatorRecordIndividual).Name);

        // The individual record that we are creating the page for.
        private GDMIndividualRecord fIndiRec;

        private GDMPersonalName fPrimaryName;

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

        // The suffix on the individual's name (e.g. "Snr")
        private string fNameSuffix;

        // The individual's first name
        private string fFirstName;

        // The individual's surname
        private string fSurname;

        // The individual's fully name
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


        public CreatorRecordIndividual(IBaseContext context, ILangMan langMan, GDMIndividualRecord ir, CreatorIndexIndividuals indiIndexCreator) : base(context, langMan)
        {
            fIndiRec = ir;
            fIndiIndexCreator = indiIndexCreator;
            fFirstFoundEvent = new Dictionary<string, Event>();
            fBirthdaySourceRefs = "";
            fDeathdaySourceRefs = "";
            fNameTitle = "";
            fUnknownName = false;
            fPrimaryName = fIndiRec.GetPrimaryPersonalName();
            fFullName = fIndiRec.GetPrimaryFullName();
            fNameSuffix = ""/*fIndiRec.NameSuffix*/; // TODO
            fFirstName = "";
            fSurname = "";
            fOccupation = "";
            fConcealed = !GMHelper.GetVisibility(fIndiRec);
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

        /// <summary>
        /// The main method that causes the page to be created.
        /// </summary>
        public bool Create(Stats stats, MTTree miniTree)
        {
            fLogger.WriteInfo("CreatorRecordIndividual.Create()");

            if (fIndiRec == null) {
                return false;
            }

            if (!GMHelper.GetVisibility(fIndiRec)) {
                return false;
            }

            // Collect together multimedia links
            if (GMConfig.Instance.AllowMultimedia && !fConcealed && fIndiRec.HasMultimediaLinks) {
                AddMultimedia(fIndiRec.MultimediaLinks, string.Concat(fIndiRec.XRef, "mm"), string.Concat(fIndiRec.XRef, "mo"), 
                              GMConfig.Instance.MaxImageWidth, GMConfig.Instance.MaxImageHeight, stats);
            }

            AddEvents();

            RemoveLoneOccupation();

            var lifeDatesX = fIndiRec.GetLifeEvents();
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
                ((GDMDate)age30.Value).Year += (short)GMConfig.Instance.AgeForOccupation;
            } catch { }

            // We should have birthday and deathday by now, so find longest occupation
            if (!fConcealed) {
                // Picks occupation with longest time span
                fOccupation = BestOccupation(fOccupations, age30, (fInferredBirthday != null) ? fInferredBirthday.Date : null, (fInferredDeathday != null) ? fInferredDeathday.Date : null);
            }

            // Go through all families this person was a irSubject to
            if (!fConcealed) {
                foreach (GDMSpouseToFamilyLink spLink in fIndiRec.SpouseToFamilyLinks) {
                    GDMFamilyRecord fr = fTree.GetPtrValue<GDMFamilyRecord>(spLink);
                    if (fr != null) {
                        string spouseLink = "";
                        // Find the irSubject's name
                        GDMIndividualRecord spouse = fTree.GetSpouseBy(fr, fIndiRec);
                        if (spouse != null && GMHelper.GetVisibility(spouse)) {
                            spouseLink = MakeLink(spouse);
                        }

                        // Add fr events as events connected to this individual
                        foreach (GDMCustomEvent fes in fr.Events) {
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

            string title = fFullName; //"Fred Bloggs 1871-1921"
            string lifeDates = "";
            if (!fConcealed) {
                if (birthyear != "" || deathyear != "") {
                    lifeDates = string.Concat(birthyear, "-", deathyear);
                    title = string.Concat(fFullName, " ", lifeDates);
                }
            }

            AddIndividualIndexEntry(lifeDates);

            OutputHTML(title, miniTree);

            return true;
        }

        /// <summary>
        /// Adds the marriage associated with the fr record to the list of events. Also adds irSubject death if within this person's lifetime.
        /// </summary>
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

                string marriedString = GetMarriedString(fr);
                marriedString += " ";

                if (marriageDate != null) {
                    var iEvent = new Event(marriageDate, "_MARRIAGE", string.Concat(marriedString, spouseLink, marriagePlace, ".", sourceRefs), "", marriageNote, true, GMConfig.Instance.CapitaliseEventDescriptions);
                    fEventList.Add(iEvent);
                } else {
                    var iEvent = new Event(marriageDate, "_MARRIAGE", string.Concat(marriedString, spouseLink, marriagePlace, ".", sourceRefs), "", marriageNote, true, GMConfig.Instance.CapitaliseEventDescriptions);
                    // Marriages go at the front of the list so that they appear first in "Other facts"
                    fAttributeList.Insert(0, iEvent);
                }
            }
        }

        private string GetMarriedString(GDMFamilyRecord fr)
        {
            string result;
            switch (fr.Status) {
                case GDMMarriageStatus.Unknown:
                default:
                    result = fLangMan.LS(PLS.MaritalStatusUnknown);
                    break;
                case GDMMarriageStatus.MarrRegistered:
                    result = fLangMan.LS(PLS.Married);
                    break;
                case GDMMarriageStatus.MarrNotRegistered:
                    result = fLangMan.LS(PLS.PartnerOf);
                    break;
                case GDMMarriageStatus.MarrDivorced:
                    result = fLangMan.LS(PLS.divorced);
                    break;
            }
            return result;
        }

        /// <summary>
        /// Goes through all families this person was a irSibling in and finds their frParents and siblings.
        /// </summary>
        private void AddParentsAndSiblings()
        {
            // Set a limit for date comparisons
            DateTime dtNow = DateTime.Now;

            // Go through all families this person was a irSibling in
            foreach (GDMChildToFamilyLink childLink in fIndiRec.ChildToFamilyLinks) {
                var famRec = fTree.GetPtrValue(childLink);
                if (famRec != null) {
                    GDMIndividualRecord husband, wife;
                    fTree.GetSpouses(famRec, out husband, out wife);

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
                            if (!GMHelper.GetVisibility(child))
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

                            int difference = GetEventsYearsDiff(testBirthday, childBirthdate);
                            if (difference < 0) {
                                if (difference > previousDifference) {
                                    previousDifference = difference;
                                    fPreviousChildLink = MakeLink(child, fLangMan.LS(PLS.previous_child));
                                }
                            } else if (difference > 0) {
                                if (difference < nextDifference) {
                                    nextDifference = difference;
                                    fNextChildLink = MakeLink(child, fLangMan.LS(PLS.next_child));
                                }
                            } else {
                                fNextChildLink = MakeLink(child, fLangMan.LS(PLS.next_child));
                            }
                        }
                    }
                }
            }
        }

        /// <summary>
        /// Adds this individual page to the index of pages.
        /// </summary>
        private void AddIndividualIndexEntry(string lifeDates)
        {
            string relativeFilename = GetIndividualHTMLFilename(fIndiRec);
            // Create some strings to use in index entry
            string userRef = "";
            if (fIndiRec.HasUserReferences) {
                GDMUserReference urn = fIndiRec.UserReferences[0];
                userRef = EscapeHTML(urn.StringValue, false);
                if (userRef.Length > 0) {
                    userRef = string.Concat(" [", userRef, "]");
                }
            }
            string alterEgo = "";
            if (GMConfig.Instance.IncludeNickNamesInIndex) {
                if (fNickName != "") {
                    alterEgo = string.Concat("(", fNickName, ") ");
                } else if (fUsedName != "") {
                    alterEgo = string.Concat("(", fUsedName, ") ");
                }
            }

            if (fIndiIndexCreator != null) {
                // Add index entry for this individuals main name (or hidden/unknown string)
                string firstName = fFirstName;
                if (!string.IsNullOrEmpty(fNameSuffix)) {
                    firstName += ", " + fNameSuffix;
                }
                fIndiIndexCreator.AddIndividualToIndex(firstName, fSurname, fUnknownName, alterEgo, lifeDates, fConcealed, relativeFilename, userRef);

                // Add entries for this individual's other names
                if (!fConcealed && !fUnknownName) {
                    for (int i = 1; i < fIndiRec.PersonalNames.Count; i++) {
                        string otherFirstName, otherSurname;
                        GMHelper.CapitaliseName(fIndiRec.PersonalNames[i], out otherFirstName, out otherSurname); // Also splits name into first name and surname
                        fIndiIndexCreator.AddIndividualToIndex(otherFirstName, otherSurname, fUnknownName, alterEgo, lifeDates, fConcealed, relativeFilename, userRef);
                    }
                }
            }
        }

        /// <summary>
        /// Extracts the data from the MARR event for the fr record.
        /// </summary>
        private string AddMarriageEvent(GDMFamilyRecord fr, string sourceRefs, out GDMDateValue marriageDate, out string marriageNote, out string marriagePlace)
        {
            // Find out when they were married
            marriageDate = null;
            marriagePlace = "";
            sourceRefs = "";
            marriageNote = "";
            foreach (var fes in fr.Events) {
                if (fes.GetTagName() == "MARR") {
                    marriageDate = fes.Date;

                    if (fes.HasPlace) {
                        string strPlace = fes.Place.StringValue;
                        if (strPlace != "")
                            marriagePlace = string.Concat(" ", GMConfig.Instance.PlaceWord, " ", EscapeHTML(strPlace, false));
                    }

                    sourceRefs = AddSources(ref fReferenceList, fes.SourceCitations);

                    if (fes.HasNotes) {
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
            return sourceRefs;
        }

        /// <summary>
        /// Extracts the data from the DEAT event for the given individual and adds it if it was an event in the current individual's lifetime.
        /// </summary>
        private string AddSpouseDeath(GDMIndividualRecord spouse, string spouseLink)
        {
            string sourceRefs = "";
            string place = "";
            if (GMHelper.GetVisibility(spouse)) {
                foreach (var ies in spouse.Events) {
                    if (ies.GetTagName() == "DEAT") {
                        // Record death of irSubject if within this person's lifetime
                        GDMDateValue spouseDeathDate = ies.Date;
                        if (spouseDeathDate != null) {
                            if (fInferredDeathday == null || fInferredDeathday.Date == null || spouseDeathDate.CompareTo(fInferredDeathday.Date) <= 0) {
                                if (ies.HasPlace) {
                                    string strPlace = ies.Place.StringValue;
                                    if (strPlace != "")
                                        place = string.Concat(" ", GMConfig.Instance.PlaceWord, " ", EscapeHTML(strPlace, false));
                                }

                                sourceRefs = AddSources(ref fReferenceList, ies.SourceCitations);
                                var iEvent = new Event(spouseDeathDate, "_SPOUSEDIED", string.Concat(fLangMan.LS(PLS.death_of), " ", spouseLink, place, ".", sourceRefs), "", null, false, GMConfig.Instance.CapitaliseEventDescriptions);
                                fEventList.Add(iEvent);
                            }
                            break;
                        }
                    }
                }
            }
            return sourceRefs;
        }

        /// <summary>
        /// Adds birth, baptism, death etc of the children in the given fr.
        /// </summary>
        private void AddChildrensEvents(GDMFamilyRecord famRec)
        {
            // Find out all the children.
            foreach (var childPtr in famRec.Children) {
                var child = fTree.GetPtrValue<GDMIndividualRecord>(childPtr);

                if (child != null) {
                    bool childConcealed = !GMHelper.GetVisibility(child);

                    string childSex = fLangMan.LS(PLS.Child);
                    if (!childConcealed) {
                        if (child.Sex == GDMSex.svMale) {
                            childSex = fLangMan.LS(PLS.Son);
                        } else if (child.Sex == GDMSex.svFemale) {
                            childSex = fLangMan.LS(PLS.Daughter);
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

                            if (childDeathday.HasPlace) {
                                string strPlace = childDeathday.Place.StringValue;
                                if (strPlace != "") {
                                    deathPlace = string.Concat(" ", GMConfig.Instance.PlaceWord, " ", EscapeHTML(strPlace, false));
                                }
                            }
                        }

                        if (childDeathdate != null && fInferredDeathday != null && fInferredDeathday.Date != null && (childDeathdate.CompareTo(fInferredDeathday.Date) <= 0)) {
                            sourceRefs = AddSources(ref fReferenceList, childDeathday.SourceCitations);
                            Event iEvent = new Event(childDeathdate, "_CHILDDIED", string.Concat(fLangMan.LS(PLS.death_of), " ", childSex, " ", childLink, deathPlace, ".", sourceRefs), "", null, false, GMConfig.Instance.CapitaliseEventDescriptions);
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

                        if (childBirthday.HasPlace) {
                            string strPlace = childBirthday.Place.StringValue;
                            if (strPlace != "") {
                                birthPlace = string.Concat(" ", GMConfig.Instance.PlaceWord, " ", EscapeHTML(strPlace, false));
                            }
                        }
                        sourceRefs = AddSources(ref fReferenceList, childBirthday.SourceCitations);
                    }

                    if (childBirthdate == null) {
                        var iEvent = new Event(null, "_CHILDBORN", string.Concat(fLangMan.LS(PLS.birth_of), " ", childSex, " ", childLink, birthPlace, ".", sourceRefs), "", null, true, GMConfig.Instance.CapitaliseEventDescriptions);
                        fAttributeList.Add(iEvent);
                    } else {
                        var iEvent = new Event(childBirthdate, "_CHILDBORN", string.Concat(fLangMan.LS(PLS.birth_of), " ", childSex, " ", childLink, birthPlace, ".", sourceRefs), "", null, true, GMConfig.Instance.CapitaliseEventDescriptions);
                        fEventList.Add(iEvent);
                    }
                }
            }
        }

        /// <summary>
        /// Works through all the events records for this individual and extracts information from them.
        /// </summary>
        private void AddEvents()
        {
            if (fIndiRec.HasEvents && !fConcealed) {
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

        /// <summary>
        /// Extracts the name information from the individual record.
        /// </summary>
        private void ConstructName()
        {
            // Construct the guy's name
            if (fConcealed && !GMConfig.Instance.UseWithheldNames) {
                fFirstName = "";
                fSurname = fFullName = GMConfig.Instance.ConcealedName;
            } else {
                fFullName = GMHelper.CapitaliseName(fPrimaryName, out fFirstName, out fSurname); // Also splits name into first name and surname
            }
            if (fFullName == "") {
                fFirstName = "";
                fSurname = fFullName = GMConfig.Instance.UnknownName;
                fUnknownName = true;
            }

            // Remember other name records
            if (!fConcealed && !fUnknownName) {
                for (int i = 1; i < fIndiRec.PersonalNames.Count; i++) {
                    GDMPersonalName pns = fIndiRec.PersonalNames[i];

                    string dummy;
                    NameAndSource nasOther = new NameAndSource(GMHelper.CapitaliseName(pns, out dummy, out dummy));
                    nasOther.Sources.AddRange(pns.SourceCitations);
                    nasOther.SourceHtml = AddSources(ref fReferenceList, nasOther.Sources);
                    fOtherNames.Add(nasOther);
                }
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
                fNameSources = AddSources(ref fReferenceList, fIndiRec.SourceCitations);
            }
        }

        /// <summary>
        /// Creates a file and writes into it the HTML for the individual's page.
        /// </summary>
        private void OutputHTML(string title, MTTree miniTree)
        {
            HTMLFile f = null;
            string pageDescription = fLangMan.LS(PLS.PageDescription) + " " + fFullName;
            string keywords = fLangMan.LS(PLS.Keywords) + " " + fFullName;
            string relativeFilename = GetIndividualHTMLFilename(fIndiRec);
            string fullFilename = string.Concat(GMConfig.Instance.OutputFolder, "\\", relativeFilename);

            try {
                f = new HTMLFile(fLangMan, fullFilename, title, pageDescription, keywords); // Creates a new file, and puts standard header html into it.

                if (f != null) {
                    OutputPageHeader(f, fPreviousChildLink, fNextChildLink, true);

                    if (GMConfig.Instance.ShowMiniTrees) {
                        OutputMiniTree(f, miniTree);
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

                    if (!GMConfig.Instance.ShowMiniTrees) {
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

        /// <summary>
        /// Outputs the HTML for the list of Sources referenced in the page.
        /// </summary>
        private void OutputSourceReferences(HTMLFile f)
        {
            if (fReferenceList.Count > 0) {
                f.WriteLine("        <div id=\"references\">");
                f.WriteLine("          <h1>{0}</h1>", fLangMan.LS(PLS.Sources));
                f.WriteLine("          <ul>");

                for (int i = 0; i < fReferenceList.Count; i++) {
                    GDMSourceCitation sourCit = fReferenceList[i];

                    string extraInfo = "";
                    var source = fTree.GetPtrValue<GDMSourceRecord>(sourCit);

                    // Publication facts
                    if (source != null && source.Publication.Lines.Text != null && source.Publication.Lines.Text != "") {
                        string pubFacts;
                        if (GMConfig.Instance.ObfuscateEmails) {
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
                    f.WriteLine("<li>{0}{1}</li>", MakeLinkText(fTree, sourCit, i + 1), extraInfo);
                }
                f.WriteLine("          </ul>");
                f.WriteLine("        </div> <!-- references -->");
            }
        }

        /// <summary>
        /// Outputs the HTML for the Other Facts section of the page.
        /// </summary>
        private void OutputAttributes(HTMLFile f)
        {
            if (fAttributeList.Count > 0) {
                f.WriteLine("        <div id=\"facts\">");
                f.WriteLine("          <h1>{0}</h1>", fLangMan.LS(PLS.OtherFacts));
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

        /// <summary>
        /// Outputs the HTML for the Life History section of the page.
        /// </summary>
        private void OutputEvents(HTMLFile f)
        {
            if (fEventList.Count > 0) {
                f.WriteLine("        <div id=\"events\">");
                f.WriteLine("          <h1>{0}</h1>", fLangMan.LS(PLS.LifeHistory));
                f.WriteLine("          <table>");

                for (int i = 0; i < fEventList.Count; i++) {
                    Event iEvent = fEventList[i];

                    string importance = iEvent.Important ? " class=\"important\"" : "";
                    string eventNote = MakeNote(iEvent.Overview);
                    eventNote += MakeNote(iEvent.Note);

                    string preference = "";
                    if (iEvent.Preference == EventPreference.First) {
                        preference = fLangMan.LS(PLS.most_likely);
                    } else if (iEvent.Preference == EventPreference.Subsequent) {
                        preference = fLangMan.LS(PLS.less_likely);
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

        /// <summary>
        /// Writes the "Parents" section of the page to the HTML file. 
        /// </summary>
        private void OutputParentNames(HTMLFile f)
        {
            if (fParents.Count > 0) {
                f.WriteLine("        <div id=\"parents\">");
                f.WriteLine("          <h1>{0}</h1>", fLangMan.LS(PLS.Parents));

                string sChild = fLangMan.LS(PLS.Child);
                if (fIndiRec.Sex == GDMSex.svMale) {
                    sChild = fLangMan.LS(PLS.Son);
                } else if (fIndiRec.Sex == GDMSex.svFemale) {
                    sChild = fLangMan.LS(PLS.Daughter);
                }

                for (int i = 0; i < fParents.Count; i++) {
                    HusbandAndWife parents = fParents[i];
                    string sParents = "";
                    if (parents.Husband != null && GMHelper.GetVisibility(parents.Husband)) {
                        sParents = MakeLink(parents.Husband);
                    }
                    if (parents.Wife != null && GMHelper.GetVisibility(parents.Wife)) {
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

        /// <summary>
        /// Writes the individual's lifespan and occupation to the HTML file. 
        /// </summary>
        private void OutputIndividualSummary(HTMLFile f)
        {
            f.WriteLine("          <div id=\"individualSummary\">");

            string birthday;
            if (fActualBirthday != null) {
                birthday = fActualBirthday.GetDisplayStringExt(DateFormat.dfYYYY_MM_DD, true, false) + fBirthdaySourceRefs;
            } else {
                birthday = "";
            }

            string deathday;
            if (fActualDeathday != null) {
                deathday = fActualDeathday.GetDisplayStringExt(DateFormat.dfYYYY_MM_DD, true, false) + fDeathdaySourceRefs;
            } else {
                deathday = "";
            }

            if (fActualBirthday != null || fActualDeathday != null) {
                f.WriteLine(string.Concat("            <p>", birthday, " - ", deathday, "</p>"));
            }
            if (GMConfig.Instance.OccupationHeadline && !string.IsNullOrEmpty(fOccupation)) {
                f.WriteLine(string.Concat("            <p>", fOccupation, "</p>"));
            }
            if (fConcealed) {
                f.WriteLine("            <p>{0}</p>", fLangMan.LS(PLS.InformationWithheld));
            }
            f.WriteLine("          </div> <!-- individualSummary -->");
        }

        /// <summary>
        /// Writes the individual's names to the HTML file. 
        /// </summary>
        private void OutputNames(HTMLFile f)
        {
            f.WriteLine("          <div id=\"names\">");

            if (fUsedName != "" && fNickName != "") {
                fUsedName += ", ";
            }
            string nicknames = "";
            if (fUsedName != "" || fNickName != "") {
                nicknames = string.Concat(" <span class=\"nicknames\">(", EscapeHTML(fUsedName, false), EscapeHTML(fNickName, false), ")</span>");
            }

            f.WriteLine(string.Concat("            <h1>", EscapeHTML(fFullName, false), fNameSources, nicknames, "</h1>"));

            foreach (NameAndSource otherName in fOtherNames) {
                f.WriteLine(string.Concat("            <h2>", fLangMan.LS(PLS.also_known_as), " ", EscapeHTML(otherName.Name, false), otherName.SourceHtml, "</h2>"));
            }

            f.WriteLine("          </div> <!-- names -->");
        }

        /// <summary>
        /// Writes the HTML for the multimedia files associated with this record. 
        /// </summary>
        private void OutputMultimedia(HTMLFile f)
        {
            if (fMultimediaList.Count > 0) {
                Multimedia iMultimedia = fMultimediaList[0];
                f.WriteLine("    <div id=\"photos\">");
                f.WriteLine("      <div id=\"mainphoto\">");

                string non_pic_small_filename = "multimedia/" + NonPicFilename(iMultimedia.Format, true, GMConfig.Instance.LinkOriginalPicture);
                string non_pic_main_filename = "multimedia/" + NonPicFilename(iMultimedia.Format, false, GMConfig.Instance.LinkOriginalPicture);

                string imageTitle = "";
                string altName = fFullName;

                if (iMultimedia.Title != null) {
                    imageTitle = iMultimedia.Title;
                    altName = iMultimedia.Title;
                }

                if (GMConfig.Instance.LinkOriginalPicture) {
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

                if (fMultimediaList.Count > 1 && GMConfig.Instance.AllowMultipleImages) {
                    f.WriteLine("      <div id=\"miniphotos\">");

                    for (int i = 0; i < fMultimediaList.Count; i++) {
                        iMultimedia = fMultimediaList[i];

                        non_pic_small_filename = "multimedia/" + NonPicFilename(iMultimedia.Format, true, GMConfig.Instance.LinkOriginalPicture);
                        non_pic_main_filename = "multimedia/" + NonPicFilename(iMultimedia.Format, false, GMConfig.Instance.LinkOriginalPicture);

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
                            ExtRect newArea = new ExtRect(0, 0, iMultimedia.Width, iMultimedia.Height);
                            GMHelper.ScaleAreaToFit(ref newArea, GMConfig.Instance.MaxThumbnailImageWidth, GMConfig.Instance.MaxThumbnailImageHeight);

                            f.WriteLine(string.Concat("          <img style=\"width:", newArea.Width, "px; height:", newArea.Height, "px; margin-bottom:", GMConfig.Instance.MaxThumbnailImageHeight - newArea.Height, "px;\" class=\"miniphoto_img\" src=\"", iMultimedia.FileName, "\" alt=\"Click to select\" onclick=\"updateMainPhoto('", iMultimedia.FileName, "','", EscapeJavascript(iMultimedia.Title), "',", largeFilenameArg, ")\" />"));
                        } else {
                            // Other multimedia.
                            f.WriteLine(string.Concat("          <img style=\"width:", GMConfig.Instance.MaxThumbnailImageWidth, "px; height:", GMConfig.Instance.MaxThumbnailImageHeight, "px;\" class=\"miniphoto_img\" src=\"", non_pic_small_filename, "\" alt=\"Click to select\" onclick=\"updateMainPhoto('", non_pic_main_filename, "','", EscapeJavascript(iMultimedia.Title), "',", largeFilenameArg, ")\" />"));
                        }
                        f.WriteLine("         </div>");
                    }

                    f.WriteLine("      </div>");
                }
                f.WriteLine("    </div> <!-- photos -->");
            }
        }

        /// <summary>
        /// Writes the HTML for the mini tree diagram, including the image alMap data. 
        /// </summary>
        private void OutputMiniTree(HTMLFile f, MTTree miniTree)
        {
            string miniTreeExtn = "png";

            string relativeTreeFilename = string.Concat("tree", fIndiRec.XRef, ".", miniTreeExtn);
            string fullTreeFilename = string.Concat(GMConfig.Instance.OutputFolder, "\\", relativeTreeFilename);

            var map = miniTree.CreateMiniTree(fIndiRec, fullTreeFilename, GMConfig.Instance.TargetTreeWidth);
            if (map != null) {
                // Add space to height so that IE's horiz scroll bar has room and doesn't create a vertical scroll bar.
                f.WriteLine("    <div id=\"minitree\" style=\"height:{0}px;\">", miniTree.Height + 20);
                f.WriteLine("      <map name=\"treeMap\" id=\"tree\">");
                foreach (MTMap mapItem in map) {
                    if (mapItem.Linkable) {
                        string href = GetIndividualHTMLFilename(mapItem.IndiRec);
                        f.WriteLine(string.Concat("        <area alt=\"", mapItem.Name, "\" coords=\"", mapItem.X1, ",", mapItem.Y1, ",", mapItem.X2, ",", mapItem.Y2, "\" href=\"", href, "\" shape=\"rect\" />"));
                    }
                }
                f.WriteLine("      </map>");
                f.WriteLine("      <img src=\"{0}\"  usemap=\"#treeMap\" alt=\"{1}\"/>", relativeTreeFilename, fLangMan.LS(PLS.MiniTreeDiagram));
                f.WriteLine("    </div>");
            }
        }

        /// <summary>
        /// If only one occupation for this individual, and it has no associated date, this method ensures
        /// that we show it only in the title, not in the other facts section as well.
        /// </summary>
        private void RemoveLoneOccupation()
        {
            bool sanityCheck = false;
            if (GMConfig.Instance.OccupationHeadline) {
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

        /// <summary>
        /// Extracts the data from the given event, and creates a CIEvent instance for it and adds it to the list of events.
        /// Does specific processing if appropriate to the event.
        /// linkToOtherParty is an href link to the other party concerned in the event.
        /// Typically this is for fr events such as engagement, marriage etc where the other party would be the partner.
        /// </summary>
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
            string escapedDescription = "";
            string address = "";
            string url = "";
            string cause = "";
            bool important = false;
            GDMDateValue date = null;
            string place = "";
            string placeWord = GMConfig.Instance.PlaceWord;
            string alternativePlaceWord = fLangMan.LS(PLS.and); // For want of anything better...
            string alternativePlace = "";
            if (es.Date != null) {
                date = es.Date;
                if (es.HasPlace) {
                    place = es.Place.StringValue;
                }

                if (es.HasAddress) {
                    var addr = es.Address;
                    address = addr.Lines.Text;
                    if (addr.WebPages.Count > 0)
                        url = addr.WebPages[0].StringValue;
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
                    escapedDescription = fLangMan.LS(PLS.born);
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
                    escapedDescription = fLangMan.LS(PLS.christened);
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
                    escapedDescription = fLangMan.LS(PLS.baptised);
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
                    escapedDescription = fLangMan.LS(PLS.died);
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
                    escapedDescription = fLangMan.LS(PLS.buried);
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
                    escapedDescription = fLangMan.LS(PLS.cremated);
                    break;

                case "ADOP":
                    escapedDescription = fLangMan.LS(PLS.adopted);
                    break;

                case "BARM":
                    escapedDescription = fLangMan.LS(PLS.bar_mitzvah);
                    break;

                case "BASM":
                    escapedDescription = fLangMan.LS(PLS.bat_mitzvah);
                    break;

                case "BLES":
                    escapedDescription = fLangMan.LS(PLS.blessing);
                    break;

                case "CHRA":
                    escapedDescription = fLangMan.LS(PLS.christened_as_adult);
                    break;

                case "CONF":
                    escapedDescription = fLangMan.LS(PLS.confirmed);
                    break;

                case "FCOM":
                    escapedDescription = fLangMan.LS(PLS.first_communion);
                    break;

                case "ORDN":
                    escapedDescription = fLangMan.LS(PLS.ordained);
                    break;

                case "NATU":
                    escapedDescription = fLangMan.LS(PLS.naturalized);
                    break;

                case "EMIG":
                    escapedDescription = fLangMan.LS(PLS.emigrated);
                    placeWord = fLangMan.LS(PLS.from);
                    alternativePlaceWord = fLangMan.LS(PLS.to);
                    break;

                case "IMMI":
                    escapedDescription = fLangMan.LS(PLS.immigrated);
                    placeWord = fLangMan.LS(PLS.to);
                    alternativePlaceWord = fLangMan.LS(PLS.from);
                    break;

                case "PROB":
                    escapedDescription = fLangMan.LS(PLS.probate);
                    break;

                case "WILL":
                    escapedDescription = fLangMan.LS(PLS.wrote_will);
                    break;

                case "GRAD":
                    escapedDescription = fLangMan.LS(PLS.graduated);
                    break;

                case "RETI":
                    escapedDescription = fLangMan.LS(PLS.retired);
                    break;

                case "EVEN":
                    if (!string.IsNullOrEmpty(subtype)) {
                        escapedDescription = EscapeHTML(subtype, false);
                    } else {
                        escapedDescription = fLangMan.LS(PLS.other_event);
                    }
                    if (!string.IsNullOrEmpty(es.StringValue)) {
                        escapedDescription += ": " + es.StringValue;
                    }
                    break;

                case "CAST":
                    escapedDescription = fLangMan.LS(PLS.caste);
                    if (!string.IsNullOrEmpty(es.StringValue))
                        escapedDescription = string.Concat(escapedDescription, " ", EscapeHTML(es.StringValue, false));
                    else
                        needValue = true;
                    break;

                case "DSCR":
                    escapedDescription = fLangMan.LS(PLS.physical_description);
                    if (!string.IsNullOrEmpty(es.StringValue))
                        escapedDescription = string.Concat(escapedDescription, " ", EscapeHTML(es.StringValue, false));
                    else
                        needValue = true;
                    break;

                case "EDUC":
                    escapedDescription = fLangMan.LS(PLS.educated);
                    if (!string.IsNullOrEmpty(es.StringValue))
                        escapedDescription = string.Concat(escapedDescription, " ", EscapeHTML(es.StringValue, false));
                    else
                        needValue = true;
                    break;

                case "IDNO":
                    escapedDescription = fLangMan.LS(PLS.ID_number);
                    if (!string.IsNullOrEmpty(es.StringValue))
                        escapedDescription = string.Concat(escapedDescription, " ", EscapeHTML(es.StringValue, false));
                    else
                        needValue = true;
                    break;

                case "NATI":
                    escapedDescription = fLangMan.LS(PLS.nationality);
                    if (!string.IsNullOrEmpty(es.StringValue))
                        escapedDescription = string.Concat(escapedDescription, " ", EscapeHTML(es.StringValue, false));
                    else
                        needValue = true;
                    break;

                case "NCHI":
                    typeIsAOneOff = true;
                    escapedDescription = fLangMan.LS(PLS.number_of_children);
                    if (!string.IsNullOrEmpty(es.StringValue))
                        escapedDescription = string.Concat(escapedDescription, " ", EscapeHTML(es.StringValue, false));
                    else
                        needValue = true;
                    break;

                case "NMR":
                    typeIsAOneOff = true;
                    escapedDescription = fLangMan.LS(PLS.number_of_marriages);
                    if (!string.IsNullOrEmpty(es.StringValue))
                        escapedDescription = string.Concat(escapedDescription, " ", EscapeHTML(es.StringValue, false));
                    else
                        needValue = true;
                    break;

                case "OCCU":
                    escapedDescription = fLangMan.LS(PLS.occupation);
                    if (!string.IsNullOrEmpty(es.StringValue)) {
                        OccupationCounter oc = new OccupationCounter(EscapeHTML(es.StringValue, false) + sourceRefs, date);
                        fOccupations.Add(oc);
                        escapedDescription = string.Concat(escapedDescription, " ", EscapeHTML(es.StringValue, false));
                        includeOccupation = true;
                    } else
                        needValue = true;
                    break;

                case "PROP":
                    escapedDescription = fLangMan.LS(PLS.property);
                    if (!string.IsNullOrEmpty(es.StringValue))
                        escapedDescription = string.Concat(escapedDescription, " ", EscapeHTML(es.StringValue, false));
                    else
                        needValue = true;
                    break;

                case "RELI":
                    escapedDescription = fLangMan.LS(PLS.religion);
                    if (!string.IsNullOrEmpty(es.StringValue))
                        escapedDescription = string.Concat(escapedDescription, " ", EscapeHTML(es.StringValue, false));
                    else
                        needValue = true;
                    break;

                case "RESI":
                    escapedDescription = fLangMan.LS(PLS.resident);
                    if (!string.IsNullOrEmpty(es.StringValue))
                        escapedDescription = string.Concat(escapedDescription, " ", EscapeHTML(es.StringValue, false));
                    else
                        needValue = false; // Special case, we need the "at" word left in for this.
                    break;

                case "SSN":
                    escapedDescription = fLangMan.LS(PLS.Social_Security_number);
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
                    escapedDescription = fLangMan.LS(PLS.other_fact);
                    if (!string.IsNullOrEmpty(es.StringValue))
                        escapedDescription = string.Concat(escapedDescription, " ", EscapeHTML(es.StringValue, false));
                    else
                        needValue = true;
                    break;


                case "_NMR": // _NMR Brother's Keeper
                    typeIsAOneOff = true;
                    escapedDescription = fLangMan.LS(PLS.never_married);
                    if (!string.IsNullOrEmpty(es.StringValue))
                        escapedDescription = string.Concat(escapedDescription, " ", EscapeHTML(es.StringValue, false));
                    else
                        needValue = true;
                    break;

                case "_AKA": // _AKA Brother's Keeper
                case "_AKAN": // _AKAN Brother's Keeper
                    escapedDescription = fLangMan.LS(PLS.also_known_as);
                    if (!string.IsNullOrEmpty(es.StringValue))
                        escapedDescription = string.Concat(escapedDescription, " ", EscapeHTML(es.StringValue, false));
                    else
                        needValue = true;
                    break;

                // Now the fr events:
                case "ANUL":
                    escapedDescription = fLangMan.LS(PLS.annulment_of_marriage);
                    if (!string.IsNullOrEmpty(es.StringValue))
                        escapedDescription = string.Concat(escapedDescription, " ", EscapeHTML(es.StringValue, false));
                    else
                        needValue = true;
                    if (!string.IsNullOrEmpty(linkToOtherParty)) {
                        escapedDescription = string.Concat(escapedDescription, " ", fLangMan.LS(PLS.to), " ", linkToOtherParty);
                    }
                    break;

                case "CENS":
                    escapedDescription = fLangMan.LS(PLS.recorded_in_census);
                    break;

                case "DIV":
                    if (es.StringValue != null && (es.StringValue == "N" || es.StringValue == "n")) {
                        place = ""; // Clear place to prevent this event being shown
                    } else {
                        escapedDescription = fLangMan.LS(PLS.divorced);
                        if (!string.IsNullOrEmpty(linkToOtherParty)) {
                            escapedDescription = string.Concat(escapedDescription, " ", fLangMan.LS(PLS.from), " ", linkToOtherParty);
                        }
                    }
                    break;

                case "DIVF":
                    escapedDescription = fLangMan.LS(PLS.filing_of_divorce);
                    if (!string.IsNullOrEmpty(linkToOtherParty)) {
                        escapedDescription = string.Concat(escapedDescription, " ", fLangMan.LS(PLS.from), " ", linkToOtherParty);
                    }
                    break;

                case "ENGA":
                    escapedDescription = fLangMan.LS(PLS.engagement);
                    if (!string.IsNullOrEmpty(linkToOtherParty)) {
                        escapedDescription = string.Concat(escapedDescription, " ", fLangMan.LS(PLS.to), " ", linkToOtherParty);
                    }
                    break;

                case "MARB":
                    escapedDescription = fLangMan.LS(PLS.publication_of_banns_of_marriage);
                    if (!string.IsNullOrEmpty(linkToOtherParty)) {
                        escapedDescription = string.Concat(escapedDescription, " ", fLangMan.LS(PLS.to), " ", linkToOtherParty);
                    }
                    break;

                case "MARC":
                    escapedDescription = fLangMan.LS(PLS.contract_of_marriage);
                    if (!string.IsNullOrEmpty(linkToOtherParty)) {
                        escapedDescription = string.Concat(escapedDescription, " ", fLangMan.LS(PLS.to), " ", linkToOtherParty);
                    }
                    break;

                case "MARR":
                    /* This is handled as a special case outside of event processing*/
                    place = ""; // Clear place to avoid creating spurious event entry
                    break;

                case "MARL":
                    escapedDescription = fLangMan.LS(PLS.licence_obtained_for_marriage);
                    if (!string.IsNullOrEmpty(linkToOtherParty)) {
                        escapedDescription = string.Concat(escapedDescription, " ", fLangMan.LS(PLS.to), " ", linkToOtherParty);
                    }
                    break;

                case "MARS":
                    escapedDescription = fLangMan.LS(PLS.settlement_of_marriage);
                    if (!string.IsNullOrEmpty(linkToOtherParty)) {
                        escapedDescription = string.Concat(escapedDescription, " ", fLangMan.LS(PLS.to), " ", linkToOtherParty);
                    }
                    break;

                default:
                    escapedDescription = fLangMan.LS(PLS.unknown_event);
                    if (!string.IsNullOrEmpty(es.StringValue))
                        escapedDescription = string.Concat(escapedDescription, " ", EscapeHTML(es.StringValue, false));
                    else
                        needValue = true;
                    break;
            }

            if (GMConfig.Instance.CapitaliseEventDescriptions) {
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
                if (GMConfig.Instance.CapitaliseEventDescriptions) {
                    GMHelper.Capitalise(ref cause);
                }
                if (eventNote.Length > 0) {
                    eventNote += "\n";
                }
                if (GMConfig.Instance.ObfuscateEmails) {
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
                    iEvent = new Event(date, utype, escapedDescription, overview, eventNote, important, GMConfig.Instance.CapitaliseEventDescriptions);
                    fEventList.Add(iEvent);
                } else {
                    // Don't include plain "Died" and nothing else. Roots Magic seems to use this just to signify that person died. But it appears on every single page and looks silly.
                    // GSP Family Tree puts lots of blank tags (OCCU, CHR, SEX, NOTE, etc.etc). Don't display those without meaning
                    // Note CHR is contentious, as other s/w may use a CHR with no other info to mean that they were christened. GSP it appears puts a CHR for everyone?
                    if ((utype != "DEAT" && utype != "BIRT" && utype != "CHR" && utype != "OCCU") || place != "" || eventNote != "" || includeOccupation) {
                        iEvent = new Event(null, utype, escapedDescription, overview, eventNote, important, GMConfig.Instance.CapitaliseEventDescriptions);
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

        /// <summary>
        /// Adds the given source citations to the given list of referenced sources, and returns an HTML link string.
        /// </summary>
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

                bool bComma = (sourceRefs != "");

                if (sourceNumber == -1) {
                    sourceNumber = referenceList.Count;
                    referenceList.Add(sourCit);
                }

                sourceRefs += MakeLinkNumber(sourCit, (sourceNumber + 1), bComma);
            }
            return sourceRefs;
        }

        /// <summary>
        /// Picks the individual's occupation closest to the given date, within the given limits.
        /// </summary>
        private static string BestOccupation(List<OccupationCounter> occupations, GDMDateValue givenDate,
                                             GDMDateValue lowerLimit, GDMDateValue upperLimit)
        {
            int minDifference;
            if (lowerLimit == null || upperLimit == null) {
                minDifference = int.MaxValue;
            } else {
                minDifference = Math.Abs(GetEventsYearsDiff(lowerLimit, upperLimit));
            }

            OccupationCounter bestOc = null;

            foreach (OccupationCounter oc in occupations) {
                if (oc.Date == null) {
                    // Dateless occupation assumed to be the generic answer
                    return oc.Name;
                } else {
                    int sdifference = GetEventsYearsDiff(givenDate, oc.Date);
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

        /// <summary>
        /// Creates a string describing the marital status of the given fr. Prepends the string provided in marriageNote.    
        /// </summary>
        private string BuildMaritalStatusNote(GDMFamilyRecord fr, string marriageNote)
        {
            if (marriageNote != "") {
                marriageNote += "\n";
            }

            // Nasty hack for Family Historian using strings to denote marital status
            marriageNote += GetMarriedString(fr);

            return marriageNote;
        }

        private static int GetEventsYearsDiff(GDMDateValue ev1, GDMDateValue ev2, bool currentEnd = true)
        {
            int result = -1;

            try {
                int dt1 = GKUtils.GetChronologicalYear(ev1);
                int dt2 = GKUtils.GetChronologicalYear(ev2);

                if (currentEnd && dt2 == 0) {
                    dt2 = DateTime.Now.Year;
                }

                if (dt1 != 0 && dt2 != 0) {
                    result = Math.Abs(dt2 - dt1);
                }
            } catch (Exception ex) {
                fLogger.WriteError("CreatorRecordIndividual.GetEventsYearsDiff()", ex);
            }

            return result;
        }
    }
}
