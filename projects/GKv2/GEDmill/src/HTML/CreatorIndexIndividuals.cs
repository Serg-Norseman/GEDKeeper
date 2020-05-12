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
using System.Globalization;
using System.IO;
using GDModel;
using GEDmill.Model;
using GKCore.Logging;

namespace GEDmill.HTML
{
    /// <summary>
    /// Constructs the HTML for the index pages for the individuals records.
    /// </summary>
    public class CreatorIndexIndividuals : Creator
    {
        private static readonly ILogger fLogger = LogManager.GetLogger(CConfig.LOG_FILE, CConfig.LOG_LEVEL, typeof(CreatorIndexIndividuals).Name);


        // This is the list of all the individual records that need to be in the index.
        private List<StringTuple> fIndividualIndex;


        public CreatorIndexIndividuals(GDMTree tree, IProgressCallback progress, string sW3cfile) : base(tree, progress, sW3cfile)
        {
            fIndividualIndex = new List<StringTuple>();
        }

        // The main method that causes the index to be created. (Note that m_individualIndex needs to be populated first using AddIndividualToIndex() ) 
        public void Create()
        {
            fLogger.WriteInfo("Creating individuals index...");

            // Sort the index
            //AlphabetComparer comparer = new AlphabetComparer();
            //fIndividualIndex.Sort(comparer);
            // FIXME

            // Split into letter groups
            List<IndexLetter> letters = CreateIndexLetters();

            // Split across multiple pages
            List<IndexPage> pages = CreateIndexPages(letters);

            // Create header navbar
            string headingsLinks = CreateIndexNavbar(pages);

            // Output HTML index pages
            foreach (IndexPage page in pages) {
                OutputIndividualsIndexPage(headingsLinks, page);
            }
        }

        // Creates a string for the index text and adds it to the index arraylist.
        public void AddIndividualToIndex(string firstName, string surname, bool unknownName, string alterEgo, string lifeDates,
            bool concealed, string relativeFilename, string userRef)
        {
            string commaFirstName = "";
            if (firstName != "") {
                commaFirstName = ", " + firstName;
            }

            string indexEntry = string.Concat(surname, commaFirstName, " ", alterEgo, lifeDates);
            if (unknownName) {
                // Put this entry with the no-surname people
                indexEntry = ",_" + indexEntry;
            }

            if (!concealed || CConfig.Instance.UseWithheldNames) {
                fIndividualIndex.Add(new StringTuple(indexEntry, relativeFilename, userRef));
            }
        }

        // Creates the page header navbar containing links to the initial letters in the index.
        private static string CreateIndexNavbar(List<IndexPage> pages)
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
                        headingsLinks = string.Concat(headingsLinks, "<a href=\"", indexpage.FileName, "\">", indexletter.Initial, "</a>");
                    } else {
                        headingsLinks = string.Concat(headingsLinks, "<a href=\"", indexpage.FileName, "#", indexletter.Initial, "\">", indexletter.Initial, "</a>");
                    }
                }
            }
            return headingsLinks;
        }

        // Splits the index across multiple pages
        private static List<IndexPage> CreateIndexPages(List<IndexLetter> letters)
        {
            var pages = new List<IndexPage>();
            uint uLetters = (uint)letters.Count;
            uint uIndisPerPage;
            if (CConfig.Instance.MultiPageIndexes == false) {
                // Set to 0 for all names in one page.
                uIndisPerPage = 0;
            } else {
                uIndisPerPage = CConfig.Instance.IndividualsPerIndexPage;
            }
            uint uIndiAccumulator = 0;
            uint uCurrentPage = 0;
            string currentPageName = string.Format("individuals{0}.{1}", ++uCurrentPage, CConfig.Instance.HtmlExtension);
            IndexPage indexpageCurrent = new IndexPage(currentPageName);
            uint uLetter = 0;
            while (uLetter < uLetters) {
                uint uCurrentIndis = (uint)(letters[(int)uLetter].Items.Count);
                if (uIndisPerPage != 0 && uIndiAccumulator + uCurrentIndis > uIndisPerPage) {
                    uint uWith = (uint)(uIndiAccumulator + uCurrentIndis - uIndisPerPage);
                    uint uWithout = uIndisPerPage - uIndiAccumulator;
                    if (uWith < uWithout || uIndiAccumulator == 0) {
                        // Better to include it.
                        indexpageCurrent.TotalIndis += letters[(int)uLetter].Items.Count;
                        indexpageCurrent.Letters.Add(letters[(int)uLetter++]);
                    }
                    // Start new page.
                    pages.Add(indexpageCurrent);
                    currentPageName = string.Format("individuals{0}.{1}", ++uCurrentPage, CConfig.Instance.HtmlExtension);
                    indexpageCurrent = new IndexPage(currentPageName);
                    uIndiAccumulator = 0;
                } else {
                    indexpageCurrent.TotalIndis += letters[(int)uLetter].Items.Count;
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
        private List<IndexLetter> CreateIndexLetters()
        {
            var letters = new List<IndexLetter>();
            string lastInitial = "";
            string lastTitle = "";
            List<StringTuple> currentLetterList = null;
            foreach (StringTuple tuple in fIndividualIndex) {
                string sName = tuple.First;
                string sLink = tuple.Second;
                string sExtras = tuple.Third;

                string sInitial;
                string sTitle;
                if (!string.IsNullOrEmpty(sName)) {
                    sInitial = sName.Substring(0, 1);
                    if (sInitial == ",") {
                        // TODO: handle no surname in such a way that names starting with commas don't count.
                        sInitial = "-";
                        sTitle = CConfig.Instance.NoSurname;
                    } else {
                        sTitle = sInitial;
                    }

                    int nCmp = 0;
                    if (lastInitial == "" && sInitial != "") { // Z,A
                        nCmp = 1;
                    } else if (lastInitial == "-" && sInitial != "-") {
                        nCmp = 1;
                    } else {
                        nCmp = string.Compare(sInitial, lastInitial, true, CultureInfo.CurrentCulture);
                    }
                    if (nCmp != 0) {
                        if (currentLetterList != null) {
                            IndexLetter letter = new IndexLetter(lastInitial, lastTitle, currentLetterList);
                            letters.Add(letter);
                            currentLetterList = null;
                        }
                        if (currentLetterList == null) {
                            currentLetterList = new List<StringTuple>();
                        }
                        lastInitial = sInitial;
                        lastTitle = sTitle;
                    }
                    currentLetterList.Add(tuple);
                }
            }
            if (currentLetterList != null) {
                IndexLetter letter = new IndexLetter(lastInitial, lastTitle, currentLetterList);
                letters.Add(letter);
                currentLetterList = null;
            }
            return letters;
        }

        // Generates the HTML file for the given page of the index.
        private void OutputIndividualsIndexPage(string headingsLinks, IndexPage indexPage)
        {
            fLogger.WriteInfo("OutputIndividualsIndexPage()");

            string ownerName = CConfig.Instance.OwnersName;
            if (ownerName != "") {
                ownerName = " of " + ownerName;
            }

            string fullFilename = CConfig.Instance.OutputFolder;
            if (fullFilename != "") {
                fullFilename += "\\";
            }
            fullFilename += indexPage.FileName;

            HTMLFile f = null;
            try {
                f = new HTMLFile(fullFilename, CConfig.Instance.IndexTitle, "Index of all individuals in the family tree" + ownerName, "individuals index family tree people history dates"); // Creates a new file, and puts standard header html into it.

                OutputPageHeader(f, "", "", false, true);

                f.WriteLine("    <div class=\"hr\" />");
                f.WriteLine("");

                f.WriteLine("  <div id=\"page\">");
                f.WriteLine("    <h1 class=\"centred\">{0}</h1>", CConfig.Instance.IndexTitle);

                if (headingsLinks != "") {
                    f.WriteLine("    <div id=\"headingsLinks\">");
                    f.WriteLine("      <p>{0}</p>", headingsLinks);
                    f.WriteLine("    </div>");
                }

                OutputIndexPage(indexPage, f);

                f.WriteLine("  </div> <!-- page -->");
            } catch (IOException e) {
                fLogger.WriteError("Caught IO Exception(3) : ", e);
            } catch (ArgumentException e) {
                fLogger.WriteError("Caught Argument Exception(3) : ", e);
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
                List<StringTuple> firstHalf = new List<StringTuple>();
                List<StringTuple> secondHalf = new List<StringTuple>();
                List<StringTuple> currentHalf = firstHalf;

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
                        currentHalf = secondHalf;
                    }

                    // Add heading letter.
                    currentHalf.Add(new StringTuple(letter.Title, ""));
                    ++nAdded;

                    // Add indis.
                    foreach (StringTuple tuple in letter.Items) {
                        if (nAdded == nHalfWay) {
                            currentHalf = secondHalf;
                        }
                        currentHalf.Add(tuple);
                        ++nAdded;
                    }
                }

                // Output HTML.
                OutputIndexPageColumns(f, firstHalf, secondHalf);
            } else {
                f.WriteLine("    <p>There are no individuals to list.</p>");
            }
        }

        // Outputs the HTML table that lists the names in two columns.
        private static void OutputIndexPageColumns(HTMLFile f, List<StringTuple> firstHalf, List<StringTuple> secondHalf)
        {
            f.WriteLine("    <table id=\"index\">");
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
                            sName = CConfig.Instance.UnknownName;
                        } else {
                            sName = sName.Substring(7);
                        }
                    } else if (sName.Length >= 6 && sName.Substring(0, 6) == ",_&lt;")// Hack for unknown name.
                      {
                        sName = sName.Substring(2);
                    }
                    string sLink = tuple.Second;
                    if (sLink != "") {
                        sLink1 = string.Concat("<a href=\"", sLink, "\">", sName, "</a>");
                    } else if (sName == CConfig.Instance.NoSurname) // Hack for no surname.
                      {
                        sLink1 = string.Concat("<h2 id=\"-\">", sName, "</h2>");
                    } else {
                        sLink1 = string.Concat("<h2 id=\"", sName[0], "\">", sName, "</h2>");
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
                            sName = CConfig.Instance.UnknownName;
                        } else {
                            sName = sName.Substring(7);
                        }
                    } else if (sName.Length >= 6 && sName.Substring(0, 6) == ",_&lt;")// Hack for unknown name.
                      {
                        sName = sName.Substring(2);
                    }

                    string sLink = tuple.Second;
                    if (sLink != "") {
                        sLink2 = string.Concat("<a href=\"", sLink, "\">", sName, "</a>");
                    } else if (sName == CConfig.Instance.NoSurname) // Hack for no surname.
                      {
                        sLink2 = string.Concat("<h2 id=\"-\">", sName, "</h2>");
                    } else {
                        sLink2 = string.Concat("<h2 id=\"", sName[0], "\">", sName, "</h2>");
                    }

                    sExtras2 = tuple.Third;
                    ++j;
                }
                if (CConfig.Instance.IncludeUserRefInIndex) {
                    f.WriteLine(string.Concat("        <tr><td>", sExtras1, "</td><td>", sLink1, "</td><td>", sExtras2, "</td><td>", sLink2, "</td></tr>"));
                } else {
                    f.WriteLine(string.Concat("        <tr><td>", sLink1, "</td><td>", sLink2, "</td></tr>"));
                }
            }
            f.WriteLine("    </table>");
        }
    }
}
