/* CHTMLFile.cs
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
using System.Globalization;

namespace GEDmill.HTMLClasses
{
    // Used to create an HTML file with a standard header and footer.
    public class CHTMLFile
    {
        public FileStream m_fs;
        public StreamWriter m_sw;

        // Returns true if the given sFilename would exist on the Windows Desktop.
        // This is a hack ( because I don't know the official way to find the path of the user's Desktop ).
        public static bool IsDesktop( string filename )
        {
            string path_part = System.IO.Path.GetDirectoryName( filename );
            // Strip trailing slashes
            while( path_part.Length > 0 && path_part.Substring( path_part.Length-1, 1 ) == "\\" )
            {
                path_part = path_part.Substring( 0, path_part.Length-1 );
            }
            int folder_index = path_part.LastIndexOf( '\\' );
            if( folder_index > 0 && (folder_index+1) < path_part.Length )
            {
                string folder_name = path_part.Substring( folder_index+1 );
                if( folder_name == "Desktop" )
                {   
                    return( true );
                }
            }
            return( false );
        }

        // Constructor. Creates a file with the given name and writes a standard HTML header.
        public CHTMLFile( string filename, string title, string description, string keywords )
        {
            // This is for CJ who ended up with 17000 files plastered all over her desktop...
            if( IsDesktop( filename ) )
            {
                throw new CHTMLException( String.Format("A problem occurred when creating an HTML file:\r\nGEDmill will not place files onto the Desktop." ) );
            }

            LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Note, "CHTMLFile : " + filename ); 

            if( File.Exists( filename ) )
            {
                // Delete any current file
                File.SetAttributes( filename, FileAttributes.Normal );
                File.Delete( filename );
            }
            m_fs = null;
            try
            {
                m_fs = new FileStream( filename, FileMode.Create );
            }
            catch( System.NotSupportedException )
            {
                throw new CHTMLException( String.Format("A problem occurred when creating an HTML file:\r\nThe path or filename {0} is not valid.", filename ) );
            }

            if( m_fs != null )
            {
                string charsetString;
                System.Text.Encoding encoding;
                if( MainForm.s_config.m_ecHtmlCharset==ECharset.ISO8859_1 )
                {
                    charsetString = "iso-8859-1";
                    encoding = System.Text.Encoding.GetEncoding( "iso-8859-1" );
                }
                else if( MainForm.s_config.m_bUseBom )
                {
                    charsetString = "utf-8";
                    encoding = System.Text.Encoding.UTF8;
                }
                else
                {
                    charsetString = "utf-8";
                    encoding = new UTF8EncodingWithoutPreamble();
                }
                
                m_sw = new StreamWriter( m_fs, encoding );

                string date;
                DateTime dt = DateTime.Now;
                date = dt.ToString("yyyy-MM-dd", DateTimeFormatInfo.InvariantInfo);

                m_sw.WriteLine("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">");
                m_sw.WriteLine("<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">");
                m_sw.WriteLine("  <head>");
                m_sw.WriteLine("    <link rel=\"stylesheet\" type=\"text/css\" href=\"" + MainForm.s_config.m_sStylesheetFilename + ".css\" />");
                if( MainForm.s_config.m_bAllowMultipleImages ) // Multiple images feature is currently (10Dec08) the only thing that uses javascript
                {
                    m_sw.WriteLine("    <script type=\"text/javascript\" src=\"gedmill.js\"></script>");
                }
                m_sw.WriteLine("    <meta http-equiv=\"Content-Type\" content=\"text/html; charset="+charsetString+"\" />");
                m_sw.WriteLine("    <meta http-equiv=\"imagetoolbar\" content=\"no\" />");
                m_sw.WriteLine(String.Concat("    <meta name=\"Title\" content=\"",title, "\" />"));
                m_sw.WriteLine(String.Concat("    <meta name=\"Description\" content=\"", description, "\" />"));
                m_sw.WriteLine(String.Concat("    <meta name=\"Keywords\" content=\"", keywords, "\" />"));
                m_sw.WriteLine("    <meta name=\"Version\" content=\"1.00\" />");
                m_sw.WriteLine(String.Concat("    <meta name=\"VersionDate\" content=\"",date,"\" />"));
                m_sw.WriteLine(String.Concat("    <title>",title,"</title>"));
                m_sw.WriteLine("  </head>");
                m_sw.WriteLine("  ");
                m_sw.WriteLine("  <body>");
            } // end if (m_fs != null )
        }

        // Write HTML footer and close the file.
        public void Close()
        {
            m_sw.WriteLine("  </body>");
            m_sw.WriteLine("</html>");
            m_sw.Close();
        }

        // A class just like .Net's UTF8Encoding, except it doesn't write the Byte Order Mark (BOM).
        public class UTF8EncodingWithoutPreamble : System.Text.UTF8Encoding
        {
          private static byte[] s_preamble = new byte[0];

          public override byte[] GetPreamble() 
          {
            return s_preamble;
          }
        }
    }
}
