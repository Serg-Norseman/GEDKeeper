/* CCreatorRecordSource.cs
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
using System.IO;

namespace GEDmill.HTMLClasses
{
    // Creates the HTML for a source record page
    public class CCreatorRecordSource : CCreatorRecord
    {
    // The source record that we are creating the page for.
    private CSourceRecord m_sr;

    // Constructor
        public CCreatorRecordSource( CGedcom gedcom, IProgressCallback progress, string w3cfile, CSourceRecord sr ) : base( gedcom, progress, w3cfile )
        {
      m_sr = sr;
        }

        // The main method that causes the page to be created.
        public bool Create(CStats stats)
    {
      // Sanity check
      if( m_sr == null )
      {
        return false;
      }

      // Create the strings to use for the HTML file.
      string sPageDescription = "GEDmill GEDCOM to HTML page for " + m_sr.m_sSourceDescriptiveTitle;
      string sKeywords = "family tree history " + m_sr.m_sSourceDescriptiveTitle;
      string sFilename = String.Concat( MainForm.s_config.m_sOutputFolder, "\\sour", m_sr.m_xref );
      string sFullFilename = String.Concat(sFilename, ".", MainForm.s_config.m_sHtmlExtension);

      CHTMLFile f = null;
      try
      {
        // Create a new file with an HTML header.    
        f = new CHTMLFile( sFullFilename, m_sr.DescriptiveTitle, sPageDescription, sKeywords ); 

        // Create a navbar to main site, front page etc.
        OutputPageHeader( f.m_sw, "", "", true, true );

        f.m_sw.WriteLine("    <div class=\"hr\"></div>");
        f.m_sw.WriteLine("");
        f.m_sw.WriteLine("    <div id=\"page\"> <!-- page -->");

        // Write the page's title text.
        f.m_sw.WriteLine("      <div id=\"main\">");
        f.m_sw.WriteLine("        <div id=\"summary\">");
        f.m_sw.WriteLine("          <div id=\"names\">");
        string sName = m_sr.DescriptiveTitle;
        if( sName == "" )
        {
          sName = "Source ";
          bool bGotSourceName = false;
          // Try user reference number
          foreach( CUserReferenceNumber urn in m_sr.m_alUserReferenceNumbers )
          {
            if( urn.m_sUserReferenceNumber != "" )
            {
              sName += urn.m_sUserReferenceNumber;
              bGotSourceName = true;
              break;
            }
          }
          if( !bGotSourceName && m_sr.m_sAutomatedRecordId != null && m_sr.m_sAutomatedRecordId != "" )
          {
            sName += m_sr.m_sAutomatedRecordId;
          }
          else if( !bGotSourceName )
          {
            sName += m_sr.m_xref;
          }
        }
        f.m_sw.WriteLine(String.Concat("            <h1>",EscapeHTML(sName,false ),"</h1>"));

        // Add repository information
        foreach( CSourceRepositoryCitation src in m_sr.m_alSourceRepositoryCitations )
        {
          string xrefRepo = src.m_xrefRepo;
          CRepositoryRecord rr = m_gedcom.GetRepositoryRecord( xrefRepo );
          if( rr != null )
          {
              if (rr.m_sNameOfRepository != null && rr.m_sNameOfRepository != "")
              {
                  f.m_sw.WriteLine(String.Concat("            <h2>", EscapeHTML(rr.m_sNameOfRepository, false), "</h2>"));
              }
            if( rr.m_alNoteStructures != null && rr.m_alNoteStructures.Count > 0 )
            {
              foreach( CNoteStructure ns in rr.m_alNoteStructures )
              {
                string noteText;
                if( MainForm.s_config.m_bObfuscateEmails )
                {
                  noteText = ObfuscateEmail( ns.Text );
                }
                else
                {
                  noteText = ns.Text;
                }
                f.m_sw.WriteLine(String.Concat("            <p>", EscapeHTML( noteText, false ), "</p>" ) );
              }
            }
          }
          if( src.m_alNoteStructures != null && src.m_alNoteStructures.Count > 0 )
          {
            foreach( CNoteStructure ns in src.m_alNoteStructures )
            {
              string noteText;
              if( MainForm.s_config.m_bObfuscateEmails )
              {
                noteText = ObfuscateEmail( ns.Text );
              }
              else
              {
                noteText = ns.Text;
              }

              f.m_sw.WriteLine(String.Concat("            <p>", EscapeHTML( noteText, false ), "</p>" ) );
            }
          }

        }

        // Add Publication Information
        string sPubFacts;
        if( MainForm.s_config.m_bObfuscateEmails )
        {
          sPubFacts = ObfuscateEmail( m_sr.m_sSourcePublicationFacts );
        }
        else
        {
          sPubFacts = m_sr.m_sSourcePublicationFacts;
        }
        if( sPubFacts != null && sPubFacts != "" )
        {
          if( sPubFacts.Length > 7 && sPubFacts.ToUpper().Substring(0,7) == "HTTP://" )
          {
            f.m_sw.WriteLine(String.Concat("            <h2>","<a href=\"", sPubFacts, "\">", EscapeHTML(sPubFacts,false), "</a>","</h2>"));
          }
          else
          {
            f.m_sw.WriteLine(String.Concat("            <h2>",EscapeHTML(sPubFacts,false),"</h2>"));
          }
        }

        f.m_sw.WriteLine("          </div> <!-- names -->");
        f.m_sw.WriteLine("        </div> <!-- summary -->");

        // Collect together multimedia links.
        if( MainForm.s_config.m_bAllowMultimedia && m_sr.m_alUniqueFileRefs != null )
        {
          m_sr.m_alUniqueFileRefs.Sort( new CMultimediaFileReference.OrderComparer() );

          // Fill m_alMultimediaList:
          AddMultimedia( null, m_sr.m_alUniqueFileRefs, String.Concat(m_sr.m_xref,"mms"), String.Concat(m_sr.m_xref,"mos"), MainForm.s_config.m_uMaxSourceImageWidth, MainForm.s_config.m_uMaxSourceImageHeight, stats );
        }

        // Add pics
        OutputMultimedia(f);

        // Add textFromSource
        string sCleanText = m_sr.m_sTextFromSource;
        CGedcom.ParseWhitespace( ref sCleanText );
        if( MainForm.s_config.m_bObfuscateEmails )
        {
          sCleanText = ObfuscateEmail( sCleanText );
        }
        if( sCleanText != null && sCleanText != "" )
        {
          f.m_sw.WriteLine("        <div id=\"text\">");
          f.m_sw.WriteLine("          <h1>Text</h1>");
          f.m_sw.WriteLine("          <p class=\"pretext\">");
          f.m_sw.WriteLine( EscapeHTML( sCleanText,false) );
          f.m_sw.WriteLine("            </p>");
          f.m_sw.WriteLine("        </div> <!-- text -->");
        }

        // Add notes
        if( m_sr.m_alNoteStructures.Count > 0 )
        {
          // Generate notes list into a local array before adding header title. This is to cope with the case where all notes are nothing but blanks.
          ArrayList alNoteStrings = new ArrayList(m_sr.m_alNoteStructures.Count);

          foreach( CNoteStructure ns in m_sr.m_alNoteStructures )
          {
            if( ns.Text != null && ns.Text.Length > 0 )
            {
              string sNoteText;
              if( MainForm.s_config.m_bObfuscateEmails )
              {
                sNoteText = ObfuscateEmail( ns.Text );
              }
              else
              {
                sNoteText = ns.Text;
              }

              string sourceRefs = "";
              alNoteStrings.Add( String.Concat("          <li>",EscapeHTML(sNoteText,false),sourceRefs,"</li>") );
            }
          }

          if( alNoteStrings.Count > 0 )
          {
            f.m_sw.WriteLine("        <div id=\"notes\">");
            f.m_sw.WriteLine("          <h1>Notes</h1>");
            f.m_sw.WriteLine("          <ul>");

            foreach( string note_string in alNoteStrings )
            {
              f.m_sw.WriteLine(note_string);
            }

            f.m_sw.WriteLine("          </ul>");
            f.m_sw.WriteLine("        </div> <!-- notes -->");
          }
        }

        if( MainForm.s_config.m_bSupressBackreferences == false )
        {
          if( m_sr.m_alBackreferences != null && m_sr.m_alBackreferences.Count > 0 )
          {
            f.m_sw.WriteLine("        <div id=\"citations\">" );
            f.m_sw.WriteLine("          <h1>Citations</h1>");
            f.m_sw.WriteLine("          <ul>");

            Hashtable htBackrefs = m_sr.MakeBackRefList();

            IDictionaryEnumerator enumerator = htBackrefs.GetEnumerator();
            while( enumerator.MoveNext() )
            {
              CIndividualRecord ir = (CIndividualRecord)(enumerator.Value);
              if( ir != null && !ir.Restricted )
              {
                string link = MakeLink(ir);
                if( link != "" )
                {
                  f.m_sw.WriteLine( String.Concat( "            <li>", link, "</li>" ) );
                }
              }
            }

            f.m_sw.WriteLine("          </ul>");
            f.m_sw.WriteLine("        </div> <!-- citations -->");

          }
        }

        f.m_sw.WriteLine("      </div> <!-- main -->");

        // Add footer (Record date, W3C sticker, GEDmill credit etc.)
        OutputFooter( f, m_sr );

        f.m_sw.WriteLine("    </div> <!-- page -->");
      }
      catch( IOException e )
      {
        LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Error, "Caught IO Exception(6) : " + e.ToString() );
      }
      catch( ArgumentException e )
      {
        LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Error, "Caught Argument Exception(6) : " + e.ToString() );
      }
      finally
      {
        if( f != null )
        {
          f.Close(); // Adds standard footer to the file
        }
      }
      return true;
    }


    // Writes the HTML for the multimedia files associated with this record. 
    private void OutputMultimedia(CHTMLFile f)
    {
      string sNonPicMainFilename = "";
      if( m_alMultimediaList.Count > 0 )
      {
        f.m_sw.WriteLine("        <div id=\"sourcePics\">");
        foreach( CIMultimedia iMultimedia in m_alMultimediaList )
        {
          sNonPicMainFilename = "multimedia/" + MainForm.NonPicFilename( iMultimedia.m_sFormat, false, MainForm.s_config.m_bLinkOriginalPicture );

          string sImageTitle = "";
          string sAltName = "";
          if( iMultimedia.m_sTitle != null )
          {
            sImageTitle = iMultimedia.m_sTitle;
            sAltName = iMultimedia.m_sTitle;
          }

          f.m_sw.WriteLine(String.Concat("          <p>"));

          if( iMultimedia.m_nWidth != 0 && iMultimedia.m_nHeight != 0 )
          {
            // Must be a picture.
            if( sAltName == "" )
            {
              sAltName = "Image for this source";
            }
            if( iMultimedia.m_sLargeFilename.Length > 0 )
            {
              f.m_sw.WriteLine(String.Concat("            <a href=\"",iMultimedia.m_sLargeFilename,"\"><img src=\"",iMultimedia.m_sFilename,"\" alt=\"", sAltName, "\" /></a>")); // TODO: clip and scale properly. Use MainForm.s_config to set a max scale
            }
            else
            {
              f.m_sw.WriteLine(String.Concat("            <img src=\"",iMultimedia.m_sFilename,"\" alt=\"", sAltName, "\" />")); // TODO: clip and scale properly. Use MainForm.s_config to set a max scale
            }
          }
          else
          {
            // Other multimedia
            if( sAltName == "" )
            {
              sAltName = "Media for this source";
            }

            if( MainForm.s_config.m_bLinkOriginalPicture )
            {
              f.m_sw.WriteLine(String.Concat("            <a href=\"",iMultimedia.m_sFilename,"\"><img src=\"",sNonPicMainFilename,"\" alt=\"", sAltName, "\" /></a>")); // TODO: clip and scale properly. Use MainForm.s_config to set a max scale
            }
            else
            {
              f.m_sw.WriteLine(String.Concat("            <img src=\"",sNonPicMainFilename,"\" alt=\"", sAltName, "\" />")); // TODO: clip and scale properly. Use MainForm.s_config to set a max scale
            }
          }
          f.m_sw.WriteLine(String.Concat("          </p>"));
          if( sImageTitle != "" )
          {
            f.m_sw.WriteLine(String.Concat("          <p id=\"sourcepic_title\">",sImageTitle,"</p>"));
          }
        }
        f.m_sw.WriteLine("        </div> <!-- sourcePics -->");
      }
    }


    }
}
