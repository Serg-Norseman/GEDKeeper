/* CCreatorHelppage.cs
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
using System.IO;

namespace GEDmill.HTMLClasses
{
    /// <summary>
    /// Summary description for CCreatorHelppage.
    /// </summary>
    public class CCreatorHelppage : CCreator
    {
    // Constructor
    public CCreatorHelppage( CGedcom gedcom, IProgressCallback progress, string w3cfile ) : base( gedcom, progress, w3cfile )
    {
    }

    // The main method that causes the help page to be created. 
    public void Create()
    {
      FileStream fsHelpSource = null;
      StreamReader srHelpSource = null;
      try
      {
        fsHelpSource = new FileStream( MainForm.s_config.m_sApplicationPath + "\\helpsource.html", FileMode.Open );
        srHelpSource = new StreamReader( fsHelpSource, System.Text.Encoding.UTF8 );
      }
      catch( IOException e )
      {
        LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Error, String.Format("Caught io exception while loading help file source: {0}", e.ToString() ) );
        fsHelpSource = null;
        srHelpSource = null;
      }

      if( fsHelpSource != null && srHelpSource != null )
      {

        string sPageDescription = "GEDmill GEDCOM to HTML family history website";
        string sKeywords = "family tree history " + MainForm.s_config.m_sOwnersName;
        string sTitle = MainForm.s_config.m_sTitle;

        CHTMLFile f = null;
        try
        {
          // Create a new file and put standard header html into it.
          f = new CHTMLFile( MainForm.s_config.HelppageURL, sTitle, sPageDescription, sKeywords ); 

          OutputPageHeader(f.m_sw, "", "", true, false);

          f.m_sw.WriteLine("  <div id=\"page\"> <!-- page -->");

          // Copy in the help html source
          string sHelpLine;
          while( null!= (sHelpLine = srHelpSource.ReadLine()) )
          {
            f.m_sw.WriteLine( sHelpLine );
          }

          f.m_sw.WriteLine("  </div> <!-- page -->");
        }
        catch( IOException e )
        {
          LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Error, "Caught IO Exception : " + e.ToString() ); 
        }
        catch( ArgumentException e )
        {
          LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Error, "Caught Argument Exception : " + e.ToString() );
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
      if( srHelpSource != null )
      {
        srHelpSource.Close();
      }
      if( fsHelpSource != null )
      {
        fsHelpSource.Close();
      }
    }
    }
}
