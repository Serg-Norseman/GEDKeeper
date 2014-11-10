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
using System.IO;
using System.Globalization;

namespace GEDmill.HTMLClasses
{
  // Constructs the HTML for the index pages for the individuals records.
  public class CCreatorIndexIndividuals : CCreator
  {
    // This is the list of all the individual records that need to be in the index.
    private ArrayList m_individualIndex;
 
    // Constructor
    public CCreatorIndexIndividuals( CGedcom gedcom, IProgressCallback progress, string sW3cfile ) : base( gedcom, progress, sW3cfile )
    {
      m_individualIndex = new ArrayList();
    }

      // The main method that causes the index to be created. (Note that m_individualIndex needs to be populated first using AddIndividualToIndex() ) 
    public void Create()
    {
      LogFile.TheLogFile.WriteLine(LogFile.DT_HTML, LogFile.EDebugLevel.Note, "Creating individuals index...");

      // Sort the index
      CAlphabetComparer comparer = new CAlphabetComparer();
      m_individualIndex.Sort(comparer);

      // Split into letter groups
      ArrayList alLetters = CreateIndexLetters();

      // Split across multiple pages
      ArrayList alPages = CreateIndexPages(alLetters);

      // Create header navbar
      string sHeadingsLinks = CreateIndexNavbar(alPages);

      // Output HTML index pages
      foreach (CIndexPage page in alPages)
      {
        OutputIndividualsIndexPage(m_gedcom, sHeadingsLinks, page);
      }
    }

    // Creates a string for the index text and adds it to the index arraylist.
    public void AddIndividualToIndex(string sFirstName, string sSurname, bool bUnknownName, string sAlterEgo, string sLifeDates, bool bConcealed, string sRelativeFilename, string sUserRef)
    {
      string sCommaFirstName = "";
      if (sFirstName != "")
      {
        sCommaFirstName = ", " + sFirstName;
      }

      string indexEntry = String.Concat( sSurname, sCommaFirstName, " ", sAlterEgo, sLifeDates );
      if( bUnknownName )
      {
        // Put this entry with the no-surname people
        indexEntry = ",_" + indexEntry; 
      }
      if( !bConcealed || MainForm.s_config.m_bUseWithheldNames )
      {
        AddIndividualToIndex( indexEntry, sRelativeFilename, sUserRef );
      }
    }

    // Internal method to add individuals to the index. Use the public one in normal circumstances.
    private void AddIndividualToIndex( string sTitle, string sFullFilename, string sExtraText )
    {
      CStringTuple stringtuple = new CStringTuple( sTitle, sFullFilename, sExtraText );
      m_individualIndex.Add( stringtuple );
    }

    // Creates the page header navbar containing links to the initial letters in the index.
    private static string CreateIndexNavbar( ArrayList alPages )
    {
      string sHeadingsLinks = "";
      foreach (CIndexPage indexpage in alPages)
      {
        bool bFirstLetter = true;
        foreach (CIndexLetter indexletter in indexpage.m_alLetters)
        {
            if (sHeadingsLinks != "")
            {
                sHeadingsLinks += "&nbsp;";
            }
          if (bFirstLetter)
          {
            bFirstLetter = false;
            sHeadingsLinks = String.Concat(sHeadingsLinks, "<a href=\"", indexpage.m_sFilename, "\">", indexletter.m_sInitial, "</a>");
          }
          else
          {
            sHeadingsLinks = String.Concat(sHeadingsLinks, "<a href=\"", indexpage.m_sFilename, "#", indexletter.m_sInitial, "\">", indexletter.m_sInitial, "</a>");
          }
        }
      }
      return sHeadingsLinks;
    }

    // Splits the index across multiple pages
    private static ArrayList CreateIndexPages(ArrayList alLetters)
    {
      ArrayList alPages = new ArrayList();
      uint uLetters = (uint)(alLetters.Count);
      uint uIndisPerPage;
      if( MainForm.s_config.m_bMultiPageIndexes == false )
      {
        // Set to 0 for all names in one page.
        uIndisPerPage = 0;
      }
      else
      {
        uIndisPerPage = MainForm.s_config.m_uIndividualsPerIndexPage;
      }
      uint uIndiAccumulator = 0;
      uint uCurrentPage = 0;
      string sCurrentPageName = String.Format("individuals{0}.{1}", ++uCurrentPage, MainForm.s_config.m_sHtmlExtension);
      CIndexPage indexpageCurrent = new CIndexPage(sCurrentPageName);
      uint uLetter = 0;
      while (uLetter < uLetters)
      {
        uint uCurrentIndis = (uint)(((CIndexLetter)alLetters[(int)uLetter]).m_alItems.Count);
        if (uIndisPerPage != 0 && uIndiAccumulator + uCurrentIndis > uIndisPerPage)
        {
          uint uWith = (uint)(uIndiAccumulator + uCurrentIndis - uIndisPerPage);
          uint uWithout = uIndisPerPage - uIndiAccumulator;
          if (uWith < uWithout || uIndiAccumulator == 0)
          {
            // Better to include it.
            indexpageCurrent.m_nTotalIndis += ((CIndexLetter)alLetters[(int)uLetter]).m_alItems.Count;
            indexpageCurrent.m_alLetters.Add(alLetters[(int)uLetter++]);
          }
          // Start new page.
          alPages.Add(indexpageCurrent);
          sCurrentPageName = String.Format("individuals{0}.{1}", ++uCurrentPage, MainForm.s_config.m_sHtmlExtension);
          indexpageCurrent = new CIndexPage(sCurrentPageName);
          uIndiAccumulator = 0;
        }
        else
        {
          indexpageCurrent.m_nTotalIndis += ((CIndexLetter)alLetters[(int)uLetter]).m_alItems.Count;
          indexpageCurrent.m_alLetters.Add(alLetters[(int)uLetter++]);
          uIndiAccumulator += uCurrentIndis;
        }
      }
      if (indexpageCurrent != null)
      {
        alPages.Add(indexpageCurrent);
      }
      return alPages;
    }

    // Collects together the first letters of the items in the index
    private ArrayList CreateIndexLetters()
    {
      ArrayList alLetters = new ArrayList();
      string sLastInitial = "";
      string sLastTitle = "";
      ArrayList sCurrentLetterList = null;
      foreach (CStringTuple tuple in m_individualIndex)
      {
        string sName = tuple.m_sFirst;
        string sLink = tuple.m_sSecond;
        string sExtras = tuple.m_sThird;

        string sInitial;
        string sTitle;
        if (sName != null && sName.Length > 0)
        {
          sInitial = sName.Substring(0, 1);
          if (sInitial == ",")
          {
            // TODO: handle no surname in such a way that names starting with commas don't count.
            sInitial = "-";
            sTitle = MainForm.s_config.m_sNoSurname;
          }
          else
          {
            sTitle = sInitial;
          }

          int nCmp = 0;
          if (sLastInitial == "" && sInitial != "") // Z,A
          {
            nCmp = 1;
          }
          else if (sLastInitial == "-" && sInitial != "-")
          {
            nCmp = 1;
          }

          else
          {
            nCmp = String.Compare(sInitial,sLastInitial,true,CultureInfo.CurrentCulture);
          }
          if (nCmp != 0)
          {
            if (sCurrentLetterList != null)
            {
              CIndexLetter letter = new CIndexLetter(sLastInitial, sLastTitle, sCurrentLetterList);
              alLetters.Add(letter);
              sCurrentLetterList = null;
            }
            if (sCurrentLetterList == null)
            {
              sCurrentLetterList = new ArrayList();
            }
            sLastInitial = sInitial;
            sLastTitle = sTitle;

          }
          sCurrentLetterList.Add(tuple);
        }

      }
      if (sCurrentLetterList != null)
      {
        CIndexLetter letter = new CIndexLetter(sLastInitial, sLastTitle, sCurrentLetterList);
        alLetters.Add(letter);
        sCurrentLetterList = null;
      }
      return alLetters;
    }

    // Generates the HTML file for the given page of the index.
    private static void OutputIndividualsIndexPage(CGedcom gedcom, string sHeadingsLinks, CIndexPage indexpage)
    {
      LogFile.TheLogFile.WriteLine(LogFile.DT_HTML, LogFile.EDebugLevel.Note, "OutputIndividualsIndexPage()");

      string sOwner = MainForm.s_config.m_sOwnersName;
      if (sOwner != "")
      {
          sOwner = " of " + sOwner;
      }

      string sFullFilename = MainForm.s_config.m_sOutputFolder;
      if (sFullFilename != "")
      {
          sFullFilename += "\\";
      }
      sFullFilename += indexpage.m_sFilename;

      CHTMLFile f = null;
      try
      {
        f = new CHTMLFile(sFullFilename, MainForm.s_config.m_sIndexTitle, "Index of all individuals in the family tree" + sOwner, "individuals index family tree people history dates"); // Creates a new file, and puts standard header html into it.

        OutputPageHeader(f.m_sw, "", "", false, true);

        f.m_sw.WriteLine("    <div class=\"hr\" />");
        f.m_sw.WriteLine("");

        f.m_sw.WriteLine("  <div id=\"page\">");
        f.m_sw.WriteLine(String.Concat("    <h1 class=\"centred\">", MainForm.s_config.m_sIndexTitle, "</h1>"));

        if (sHeadingsLinks != "")
        {
          f.m_sw.WriteLine("    <div id=\"headingsLinks\">");
          f.m_sw.WriteLine(String.Concat("      <p>", sHeadingsLinks, "</p>"));
          f.m_sw.WriteLine("    </div>");
        }

        OutputIndexPage(indexpage, f);

        f.m_sw.WriteLine("  </div> <!-- page -->");
      }
      catch (IOException e)
      {
        LogFile.TheLogFile.WriteLine(LogFile.DT_HTML, LogFile.EDebugLevel.Error, "Caught IO Exception(3) : " + e.ToString());
      }
      catch (ArgumentException e)
      {
        LogFile.TheLogFile.WriteLine(LogFile.DT_HTML, LogFile.EDebugLevel.Error, "Caught Argument Exception(3) : " + e.ToString());
      }
      finally
      {
        if (f != null)
        {
          f.Close();
        }
      }

    }

    // Generates the core of the HTML file for the given page of the index.
    private static void OutputIndexPage(CIndexPage indexpage, CHTMLFile f)
    {
      int nTotal = indexpage.m_nTotalIndis + indexpage.m_alLetters.Count;

      if (indexpage.m_alLetters.Count > 0)
      {
        ArrayList alFirstHalf = new ArrayList();
        ArrayList alSecondHalf = new ArrayList();
        ArrayList alCurrentHalf = alFirstHalf;

        int nHalfWay;
        if (nTotal < 20)
        {
          // If less than 20 individuals, list them in one nColumn.
          // Set half-way pointer beyond end so that it is never reached.
          nHalfWay = nTotal + 1; 
        }
        else
        {
          nHalfWay = (nTotal + 1) / 2;
        }
        int nAdded = 0;

        foreach (CIndexLetter letter in indexpage.m_alLetters)
        {
          if (nAdded == nHalfWay)
          {
            // Don't add heading letter to bottom of first half.
            alCurrentHalf = alSecondHalf;
          }

          // Add heading letter.
          alCurrentHalf.Add(new CStringTuple(letter.m_sTitle, ""));
          ++nAdded;

          // Add indis.
          foreach (CStringTuple tuple in letter.m_alItems)
          {
            if (nAdded == nHalfWay)
            {
              alCurrentHalf = alSecondHalf;
            }
            alCurrentHalf.Add(tuple);
            ++nAdded;
          }
        }

        // Output HTML.
        OutputIndexPageColumns(f, alFirstHalf, alSecondHalf);
      }
      else
      {
        f.m_sw.WriteLine("    <p>There are no individuals to list.</p>");
      }
    }

    // Outputs the HTML table that lists the names in two columns.
    private static void OutputIndexPageColumns(CHTMLFile f, ArrayList alFirstHalf, ArrayList alSecondHalf)
    {
      f.m_sw.WriteLine("    <table id=\"index\">");
      int i = 0, j = 0;
      while (i < alFirstHalf.Count || j < alSecondHalf.Count)
      {
        string sLink1 = "&nbsp;";
        string sLink2 = "&nbsp;";
        string sExtras1 = "&nbsp;";
        string sExtras2 = "&nbsp;";
        if (i < alFirstHalf.Count)
        {
          CStringTuple tuple = (CStringTuple)alFirstHalf[i];
          string sName = EscapeHTML(tuple.m_sFirst, true);
          if (sName.Length >= 7 && sName.Substring(0, 7) == ",&nbsp;") // Hack for no surname.
          {
            if (sName.Length == 7)
            {
              sName = MainForm.s_config.m_sUnknownName;
            }
            else
            {
              sName = sName.Substring(7);
            }
          }
          else if (sName.Length >= 6 && sName.Substring(0, 6) == ",_&lt;")// Hack for unknown name.
          {
            sName = sName.Substring(2);
          }
          string sLink = tuple.m_sSecond;
          if (sLink != "")
          {
              sLink1 = String.Concat("<a href=\"", sLink, "\">", sName, "</a>");
          }
          else if (sName == MainForm.s_config.m_sNoSurname) // Hack for no surname.
          {
              sLink1 = String.Concat("<h2 id=\"-\">", sName, "</h2>");
          }
          else
          {
              sLink1 = String.Concat("<h2 id=\"", sName[0], "\">", sName, "</h2>");
          }

          sExtras1 = tuple.m_sThird;
          ++i;
        }
        if (j < alSecondHalf.Count)
        {
          CStringTuple tuple = (CStringTuple)alSecondHalf[j];
          string sName = EscapeHTML(tuple.m_sFirst, true);
          if (sName.Length >= 7 && sName.Substring(0, 7) == ",&nbsp;") // Hack for no surname.
          {
            if (sName.Length == 7)
            {
              sName = MainForm.s_config.m_sUnknownName;
            }
            else
            {
              sName = sName.Substring(7);
            }
          }
          else if (sName.Length >= 6 && sName.Substring(0, 6) == ",_&lt;")// Hack for unknown name.
          {
            sName = sName.Substring(2);
          }

          string sLink = tuple.m_sSecond;
          if (sLink != "")
          {
              sLink2 = String.Concat("<a href=\"", sLink, "\">", sName, "</a>");
          }
          else if (sName == MainForm.s_config.m_sNoSurname) // Hack for no surname.
          {
              sLink2 = String.Concat("<h2 id=\"-\">", sName, "</h2>");
          }
          else
          {
              sLink2 = String.Concat("<h2 id=\"", sName[0], "\">", sName, "</h2>");
          }

          sExtras2 = tuple.m_sThird;
          ++j;
        }
        if (MainForm.s_config.m_bIncludeUserRefInIndex)
        {
          f.m_sw.WriteLine(String.Concat("        <tr><td>", sExtras1, "</td><td>", sLink1, "</td><td>", sExtras2, "</td><td>", sLink2, "</td></tr>"));
        }
        else
        {
          f.m_sw.WriteLine(String.Concat("        <tr><td>", sLink1, "</td><td>", sLink2, "</td></tr>"));
        }
      }
      f.m_sw.WriteLine("    </table>");
    }
  }
}
