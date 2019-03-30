/* CCreatorIndexIndividuals.cs
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
using System.Globalization;
using System.IO;
using GKCommon.GEDCOM;

namespace GEDmill.HTML
{
    /// <summary>
    /// Constructs the HTML for the index pages for the individuals records.
    /// </summary>
    public class CreatorIndexIndividuals : Creator
    {
        // This is the list of all the individual records that need to be in the index.
        private ArrayList fIndividualIndex;


        public CreatorIndexIndividuals(GEDCOMTree gedcom, IProgressCallback progress, string sW3cfile) : base(gedcom, progress, sW3cfile)
        {
            fIndividualIndex = new ArrayList();
        }

        // The main method that causes the index to be created. (Note that m_individualIndex needs to be populated first using AddIndividualToIndex() ) 
        public void Create()
        {
            LogFile.Instance.WriteLine(LogFile.DT_HTML, LogFile.EDebugLevel.Note, "Creating individuals index...");

            // Sort the index
            AlphabetComparer comparer = new AlphabetComparer();
            fIndividualIndex.Sort(comparer);

            // Split into letter groups
            ArrayList alLetters = CreateIndexLetters();

            // Split across multiple pages
            ArrayList alPages = CreateIndexPages(alLetters);

            // Create header navbar
            string sHeadingsLinks = CreateIndexNavbar(alPages);

            // Output HTML index pages
            foreach (IndexPage page in alPages) {
                OutputIndividualsIndexPage(fTree, sHeadingsLinks, page);
            }
        }

        // Creates a string for the index text and adds it to the index arraylist.
        public void AddIndividualToIndex(string firstName, string surname, bool unknownName, string alterEgo, string lifeDates,
            bool concealed, string relativeFilename, string userRef)
        {
            string sCommaFirstName = "";
            if (firstName != "") {
                sCommaFirstName = ", " + firstName;
            }

            string indexEntry = String.Concat(surname, sCommaFirstName, " ", alterEgo, lifeDates);
            if (unknownName) {
                // Put this entry with the no-surname people
                indexEntry = ",_" + indexEntry;
            }
            if (!concealed || MainForm.Config.UseWithheldNames) {
                AddIndividualToIndex(indexEntry, relativeFilename, userRef);
            }
        }

        // Internal method to add individuals to the index. Use the public one in normal circumstances.
        private void AddIndividualToIndex(string title, string fullFilename, string extraText)
        {
            StringTuple stringtuple = new StringTuple(title, fullFilename, extraText);
            fIndividualIndex.Add(stringtuple);
        }

        // Creates the page header navbar containing links to the initial letters in the index.
        private static string CreateIndexNavbar(ArrayList pages)
        {
            string headingsLinks = "";
            foreach (IndexPage indexpage in pages) {
                bool firstLetter = true;
                foreach (IndexLetter indexletter in indexpage.Letters) {
                    if (headingsLinks != "") {
                        headingsLinks += "&nbsp;";
                    }
                    if (firstLetter) {
                        firstLetter = false;
                        headingsLinks = String.Concat(headingsLinks, "<a href=\"", indexpage.FileName, "\">", indexletter.Initial, "</a>");
                    } else {
                        headingsLinks = String.Concat(headingsLinks, "<a href=\"", indexpage.FileName, "#", indexletter.Initial, "\">", indexletter.Initial, "</a>");
                    }
                }
            }
            return headingsLinks;
        }

        // Splits the index across multiple pages
        private static ArrayList CreateIndexPages(ArrayList letters)
        {
            ArrayList pages = new ArrayList();
            uint uLetters = (uint)(letters.Count);
            uint uIndisPerPage;
            if (MainForm.Config.MultiPageIndexes == false) {
                // Set to 0 for all names in one page.
                uIndisPerPage = 0;
            } else {
                uIndisPerPage = MainForm.Config.IndividualsPerIndexPage;
            }
            uint uIndiAccumulator = 0;
            uint uCurrentPage = 0;
            string currentPageName = String.Format("individuals{0}.{1}", ++uCurrentPage, MainForm.Config.HtmlExtension);
            IndexPage indexpageCurrent = new IndexPage(currentPageName);
            uint uLetter = 0;
            while (uLetter < uLetters) {
                uint uCurrentIndis = (uint)(((IndexLetter)letters[(int)uLetter]).Items.Count);
                if (uIndisPerPage != 0 && uIndiAccumulator + uCurrentIndis > uIndisPerPage) {
                    uint uWith = (uint)(uIndiAccumulator + uCurrentIndis - uIndisPerPage);
                    uint uWithout = uIndisPerPage - uIndiAccumulator;
                    if (uWith < uWithout || uIndiAccumulator == 0) {
                        // Better to include it.
                        indexpageCurrent.TotalIndis += ((IndexLetter)letters[(int)uLetter]).Items.Count;
                        indexpageCurrent.Letters.Add(letters[(int)uLetter++]);
                    }
                    // Start new page.
                    pages.Add(indexpageCurrent);
                    currentPageName = String.Format("individuals{0}.{1}", ++uCurrentPage, MainForm.Config.HtmlExtension);
                    indexpageCurrent = new IndexPage(currentPageName);
                    uIndiAccumulator = 0;
                } else {
                    indexpageCurrent.TotalIndis += ((IndexLetter)letters[(int)uLetter]).Items.Count;
                    indexpageCurrent.Letters.Add(letters[(int)uLetter++]);
                    uIndiAccumulator += uCurrentIndis;
                }
            }
            if (indexpageCurrent != null) {
                pages.Add(indexpageCurrent);
            }
            return pages;
        }

        // Collects together the first letters of the items in the index
        private ArrayList CreateIndexLetters()
        {
            ArrayList alLetters = new ArrayList();
            string sLastInitial = "";
            string sLastTitle = "";
            ArrayList sCurrentLetterList = null;
            foreach (StringTuple tuple in fIndividualIndex) {
                string sName = tuple.First;
                string sLink = tuple.Second;
                string sExtras = tuple.Third;

                string sInitial;
                string sTitle;
                if (sName != null && sName.Length > 0) {
                    sInitial = sName.Substring(0, 1);
                    if (sInitial == ",") {
                        // TODO: handle no surname in such a way that names starting with commas don't count.
                        sInitial = "-";
                        sTitle = MainForm.Config.NoSurname;
                    } else {
                        sTitle = sInitial;
                    }

                    int nCmp = 0;
                    if (sLastInitial == "" && sInitial != "") // Z,A
                    {
                        nCmp = 1;
                    } else if (sLastInitial == "-" && sInitial != "-") {
                        nCmp = 1;
                    } else {
                        nCmp = String.Compare(sInitial, sLastInitial, true, CultureInfo.CurrentCulture);
                    }
                    if (nCmp != 0) {
                        if (sCurrentLetterList != null) {
                            IndexLetter letter = new IndexLetter(sLastInitial, sLastTitle, sCurrentLetterList);
                            alLetters.Add(letter);
                            sCurrentLetterList = null;
                        }
                        if (sCurrentLetterList == null) {
                            sCurrentLetterList = new ArrayList();
                        }
                        sLastInitial = sInitial;
                        sLastTitle = sTitle;

                    }
                    sCurrentLetterList.Add(tuple);
                }

            }
            if (sCurrentLetterList != null) {
                IndexLetter letter = new IndexLetter(sLastInitial, sLastTitle, sCurrentLetterList);
                alLetters.Add(letter);
                sCurrentLetterList = null;
            }
            return alLetters;
        }

        // Generates the HTML file for the given page of the index.
        private static void OutputIndividualsIndexPage(GEDCOMTree gedcom, string headingsLinks, IndexPage indexPage)
        {
            LogFile.Instance.WriteLine(LogFile.DT_HTML, LogFile.EDebugLevel.Note, "OutputIndividualsIndexPage()");

            string sOwner = MainForm.Config.OwnersName;
            if (sOwner != "") {
                sOwner = " of " + sOwner;
            }

            string sFullFilename = MainForm.Config.OutputFolder;
            if (sFullFilename != "") {
                sFullFilename += "\\";
            }
            sFullFilename += indexPage.FileName;

            HTMLFile f = null;
            try {
                f = new HTMLFile(sFullFilename, MainForm.Config.IndexTitle, "Index of all individuals in the family tree" + sOwner, "individuals index family tree people history dates"); // Creates a new file, and puts standard header html into it.

                OutputPageHeader(f.Writer, "", "", false, true);

                f.Writer.WriteLine("    <div class=\"hr\" />");
                f.Writer.WriteLine("");

                f.Writer.WriteLine("  <div id=\"page\">");
                f.Writer.WriteLine(String.Concat("    <h1 class=\"centred\">", MainForm.Config.IndexTitle, "</h1>"));

                if (headingsLinks != "") {
                    f.Writer.WriteLine("    <div id=\"headingsLinks\">");
                    f.Writer.WriteLine(String.Concat("      <p>", headingsLinks, "</p>"));
                    f.Writer.WriteLine("    </div>");
                }

                OutputIndexPage(indexPage, f);

                f.Writer.WriteLine("  </div> <!-- page -->");
            } catch (IOException e) {
                LogFile.Instance.WriteLine(LogFile.DT_HTML, LogFile.EDebugLevel.Error, "Caught IO Exception(3) : " + e.ToString());
            } catch (ArgumentException e) {
                LogFile.Instance.WriteLine(LogFile.DT_HTML, LogFile.EDebugLevel.Error, "Caught Argument Exception(3) : " + e.ToString());
            } finally {
                if (f != null) {
                    f.Close();
                }
            }
        }

        // Generates the core of the HTML file for the given page of the index.
        private static void OutputIndexPage(IndexPage indexPage, HTMLFile f)
        {
            int nTotal = indexPage.TotalIndis + indexPage.Letters.Count;

            if (indexPage.Letters.Count > 0) {
                ArrayList alFirstHalf = new ArrayList();
                ArrayList alSecondHalf = new ArrayList();
                ArrayList alCurrentHalf = alFirstHalf;

                int nHalfWay;
                if (nTotal < 20) {
                    // If less than 20 individuals, list them in one nColumn.
                    // Set half-way pointer beyond end so that it is never reached.
                    nHalfWay = nTotal + 1;
                } else {
                    nHalfWay = (nTotal + 1) / 2;
                }
                int nAdded = 0;

                foreach (IndexLetter letter in indexPage.Letters) {
                    if (nAdded == nHalfWay) {
                        // Don't add heading letter to bottom of first half.
                        alCurrentHalf = alSecondHalf;
                    }

                    // Add heading letter.
                    alCurrentHalf.Add(new StringTuple(letter.Title, ""));
                    ++nAdded;

                    // Add indis.
                    foreach (StringTuple tuple in letter.Items) {
                        if (nAdded == nHalfWay) {
                            alCurrentHalf = alSecondHalf;
                        }
                        alCurrentHalf.Add(tuple);
                        ++nAdded;
                    }
                }

                // Output HTML.
                OutputIndexPageColumns(f, alFirstHalf, alSecondHalf);
            } else {
                f.Writer.WriteLine("    <p>There are no individuals to list.</p>");
            }
        }

        // Outputs the HTML table that lists the names in two columns.
        private static void OutputIndexPageColumns(HTMLFile f, ArrayList firstHalf, ArrayList secondHalf)
        {
            f.Writer.WriteLine("    <table id=\"index\">");
            int i = 0, j = 0;
            while (i < firstHalf.Count || j < secondHalf.Count) {
                string sLink1 = "&nbsp;";
                string sLink2 = "&nbsp;";
                string sExtras1 = "&nbsp;";
                string sExtras2 = "&nbsp;";
                if (i < firstHalf.Count) {
                    StringTuple tuple = (StringTuple)firstHalf[i];
                    string sName = EscapeHTML(tuple.First, true);
                    if (sName.Length >= 7 && sName.Substring(0, 7) == ",&nbsp;") // Hack for no surname.
                    {
                        if (sName.Length == 7) {
                            sName = MainForm.Config.UnknownName;
                        } else {
                            sName = sName.Substring(7);
                        }
                    } else if (sName.Length >= 6 && sName.Substring(0, 6) == ",_&lt;")// Hack for unknown name.
                      {
                        sName = sName.Substring(2);
                    }
                    string sLink = tuple.Second;
                    if (sLink != "") {
                        sLink1 = String.Concat("<a href=\"", sLink, "\">", sName, "</a>");
                    } else if (sName == MainForm.Config.NoSurname) // Hack for no surname.
                      {
                        sLink1 = String.Concat("<h2 id=\"-\">", sName, "</h2>");
                    } else {
                        sLink1 = String.Concat("<h2 id=\"", sName[0], "\">", sName, "</h2>");
                    }

                    sExtras1 = tuple.Third;
                    ++i;
                }
                if (j < secondHalf.Count) {
                    StringTuple tuple = (StringTuple)secondHalf[j];
                    string sName = EscapeHTML(tuple.First, true);
                    if (sName.Length >= 7 && sName.Substring(0, 7) == ",&nbsp;") // Hack for no surname.
                    {
                        if (sName.Length == 7) {
                            sName = MainForm.Config.UnknownName;
                        } else {
                            sName = sName.Substring(7);
                        }
                    } else if (sName.Length >= 6 && sName.Substring(0, 6) == ",_&lt;")// Hack for unknown name.
                      {
                        sName = sName.Substring(2);
                    }

                    string sLink = tuple.Second;
                    if (sLink != "") {
                        sLink2 = String.Concat("<a href=\"", sLink, "\">", sName, "</a>");
                    } else if (sName == MainForm.Config.NoSurname) // Hack for no surname.
                      {
                        sLink2 = String.Concat("<h2 id=\"-\">", sName, "</h2>");
                    } else {
                        sLink2 = String.Concat("<h2 id=\"", sName[0], "\">", sName, "</h2>");
                    }

                    sExtras2 = tuple.Third;
                    ++j;
                }
                if (MainForm.Config.IncludeUserRefInIndex) {
                    f.Writer.WriteLine(String.Concat("        <tr><td>", sExtras1, "</td><td>", sLink1, "</td><td>", sExtras2, "</td><td>", sLink2, "</td></tr>"));
                } else {
                    f.Writer.WriteLine(String.Concat("        <tr><td>", sLink1, "</td><td>", sLink2, "</td></tr>"));
                }
            }
            f.Writer.WriteLine("    </table>");
        }
    }
}
