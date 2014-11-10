/* CCreatorFrontPage.cs
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
using System.Globalization;
using System.Drawing;

namespace GEDmill.HTMLClasses
{
  // Creates the HTML for the front (landing) page
  public class CCreatorFrontPage : CCreator
  {
    // Statistics about the website (number of files etc.)
    CStats m_stats;

    // Constructor
    public CCreatorFrontPage( CGedcom gedcom, IProgressCallback progress, string w3cfile, CStats stats ) : base( gedcom, progress, w3cfile )
    {
      m_stats = stats;
    }

    // The main method that causes the front page to be created. 
    public void Create()
    {
      string sPageDescription = "GEDmill GEDCOM to HTML family history website";
      string sKeywords = "family tree history " + MainForm.s_config.m_sOwnersName;
      string sTitle = MainForm.s_config.m_sTitle;

      CHTMLFile f = null;
      try
      {
        f = new CHTMLFile( MainForm.s_config.FrontPageURL, sTitle, sPageDescription, sKeywords ); // Creates a new file, and puts standard header html into it.
        f.m_sw.WriteLine("  <div id=\"page\"> <!-- page -->");
        f.m_sw.WriteLine("    <div id=\"cover\"> <!-- cover -->");


        f.m_sw.WriteLine(String.Concat("       <h1>", EscapeHTML(sTitle,false ), "</h1>"));

        if( MainForm.s_config.m_sFrontPageImageFilename != "" )
        {
          Rectangle newArea = new Rectangle( 0,0,0,0 );
          string pictureFile = CopyMultimedia( MainForm.s_config.m_sFrontPageImageFilename, "", 0, 0, ref newArea, null );
          if( pictureFile != null && pictureFile != "" )
          {
            f.m_sw.WriteLine(String.Concat("       <p><img src=\"" + pictureFile + "\" alt=\"Front page image\" /></p>"));
          }
        }

        if( MainForm.s_config.m_sCommentaryText!=null && MainForm.s_config.m_sCommentaryText!="" )
        {
          if( MainForm.s_config.m_bCommentaryIsHtml )
          {
            f.m_sw.WriteLine(String.Concat("       <p>", MainForm.s_config.m_sCommentaryText, "</p>" ));
          }
          else
          {
            f.m_sw.WriteLine(String.Concat("       <p>", EscapeHTML( MainForm.s_config.m_sCommentaryText, false ), "</p>" ));
          }
        }

        if( MainForm.s_config.m_bFrontPageStats )
        {
          string sIndividuals;
          if( m_stats.m_unIndividuals == 0 )
          {
            sIndividuals = "no";
          }
          else
          {
            sIndividuals = m_stats.m_unIndividuals.ToString();
          }
          sIndividuals += " individual";
          if( m_stats.m_unIndividuals != 1 )
          {
            sIndividuals += "s";
          }

          string sSources;
          if( m_stats.m_unSources == 0 )
          {
            sSources = "";
          }
          else
          {
            sSources = String.Concat(", cross-referenced to ", m_stats.m_unSources.ToString(), " source" );
          }
          if( m_stats.m_unSources > 1 )
          {
            sSources += "s";
          }

          string sMultimedia;
          string sFileType;
          if( m_stats.m_bNonPicturesIncluded )
          {
            sFileType = "multimedia file";
          }
          else
          {
            sFileType = "image";
          }
          if( m_stats.m_unMultimediaFiles == 0 )
          {
            sMultimedia = "";
          }
          else
          {
            sMultimedia = String.Concat(". There are links to ", m_stats.m_unMultimediaFiles.ToString(), " ", sFileType );
          }
          if( m_stats.m_unMultimediaFiles > 1 )
          {
            sMultimedia += "s";
          }

          f.m_sw.WriteLine(String.Concat("       <p>This website contains records on ",sIndividuals,sSources,sMultimedia, ".</p>" ));
        } // End front page stats

        f.m_sw.WriteLine("       <div id=\"links\"> <!-- links -->");
        f.m_sw.WriteLine(String.Concat("         <p><a href=\"individuals1.",MainForm.s_config.m_sHtmlExtension,"\">", MainForm.s_config.m_sIndexTitle, "</a></p>"));
        f.m_sw.WriteLine("       </div> <!-- links -->");
        if( MainForm.s_config.m_alKeyIndividuals != null && MainForm.s_config.m_alKeyIndividuals.Count > 0 )
        {
          // Although in theory you might want a restricted individual as a key individual, (they still form part of the tree), in practice this isn't allowed:
          ArrayList alCensoredKeyIndividuals = new ArrayList(MainForm.s_config.m_alKeyIndividuals.Count);

          foreach( string keyXref in MainForm.s_config.m_alKeyIndividuals )
          {
            CIndividualRecord air = m_gedcom.GetIndividualRecord( keyXref );
            if( air != null )
            {
              alCensoredKeyIndividuals.Add( MakeLink(air) );
            }
          }

          if( alCensoredKeyIndividuals.Count > 0 )
          {
            string plural = "";
            if( alCensoredKeyIndividuals.Count > 1 )
            {
              plural = "s";
            }
            f.m_sw.WriteLine( "         <div id=\"keyindividuals\">");
            f.m_sw.WriteLine(String.Concat("           <p>Key Individual",plural,":</p>"));
            f.m_sw.WriteLine( "           <ul>");
            foreach( string air_link in alCensoredKeyIndividuals )
            {
              f.m_sw.WriteLine( String.Concat("             <li>", air_link, "</li>") );
            }
            f.m_sw.WriteLine( "           </ul>");
            f.m_sw.WriteLine( "         </div> <!-- keyindividuals -->");
          }
        }

        string byEmail = "";
        // Email contact address
        if( MainForm.s_config.m_sUserEmailAddress != null && MainForm.s_config.m_sUserEmailAddress.Length > 0  )
        {
          byEmail = String.Concat(" by <a href=\"mailto:",MainForm.s_config.m_sUserEmailAddress,"\">",EscapeHTML(MainForm.s_config.m_sUserEmailAddress, false),"</a>" );
        }


        // Add brand and contact label
        f.m_sw.WriteLine(String.Concat("       <p>Website created",byEmail," using GEDmill.</p>"));
        // Add last update string
        if( MainForm.s_config.m_bAddHomePageCreateTime )
        {
          DateTime dt = DateTime.Now;
          string update_date_string = dt.ToString("dd MMMM yyyy", DateTimeFormatInfo.InvariantInfo);

          f.m_sw.WriteLine(String.Concat("       <p>Created on ",update_date_string,".</p>"));
        }

        // Add link to users main website
        if( MainForm.s_config.m_sMainWebsiteLink != "" )
        {
          f.m_sw.WriteLine( String.Concat("    <p><a href=\"", MainForm.s_config.m_sMainWebsiteLink, "\">Return to main site</a></p>") );
        }

        f.m_sw.WriteLine("    </div> <!-- cover -->");
        f.m_sw.WriteLine("  </div> <!-- page -->");
      }
      catch( IOException e )
      {
        LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Error, "Caught IO Exception(7) : " + e.ToString() ); 
      }
      catch( ArgumentException e )
      {
        LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Error, "Caught Argument Exception(7) : " + e.ToString() );
      }
      finally
      {
        if( f != null )
        {
          // Add standard footer to the file
          f.Close(); 
        }
      }
    }
  }
}
