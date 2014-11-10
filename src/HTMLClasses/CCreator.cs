/* CCreator.cs
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
using System.Drawing;
using System.Collections;
using GEDmill.LLClasses;

namespace GEDmill.HTMLClasses
{
  // Base class providing general functionality required by all classes that create HTML pages.
  public abstract class CCreator
  {
    // The raw data that we are turning into a website.
    protected CGedcom m_gedcom;

    // Pointer to the window showing the progress bar, so that web page creation progress can be shown to user.
    private IProgressCallback m_progressWindow;

    // The same multimedia file may be referenced multiple times. 
    // This hash prevents it being copied to the output directory more than once.
    private static Hashtable s_htCopiedFiles = new Hashtable();

    // The sFilename for the Valid XHTML sticker image.
    private string m_sW3CFile;

    // Constructor
    protected CCreator( CGedcom gedcom, IProgressCallback progress, string sW3cfile )
    {
      m_gedcom = gedcom;
      m_progressWindow = progress;
      m_sW3CFile = sW3cfile;
    }

    // This clears the static list of all multimedia files copied to the output directory (and possibly renamed).
    public static void ClearCopiedFilesList()
    {
      s_htCopiedFiles.Clear();
    } 

    // Converts all HTML characters into their escaped versions
    // Set bHardSpace to false if you want to keep first space as breakable, true if you want all nbsp.
    // TODO: Surely there is a .Net function to do this?
    // TODO: Might want to preserve <a> links in the HTML in case user has specified them in their data.
    protected static string EscapeHTML(string sOriginal, bool bHardSpace)
    {
      LogFile.TheLogFile.WriteLine(LogFile.DT_HTML, LogFile.EDebugLevel.Note, String.Format("EscapeHTML({0})", sOriginal));

      uint uTabSpaces = MainForm.s_config.m_uTabSpaces;

      if (sOriginal == null)
      {
          return "&lt;null&gt;";
      }

      System.Text.StringBuilder sb = new System.Text.StringBuilder(sOriginal.Length);
      uint uTabPos = 0;
      bool bDoneCRLF = false;
      bool bDoneSpace = false;
      int nLength = sOriginal.Length;
      int n = 0;
      foreach (char c in sOriginal)
      {
        switch (c)
        {
          case (char)0x91:
          case (char)0x92:
            sb.Append("'");
            bDoneCRLF = false;
            bDoneSpace = false;
            uTabPos++;
            break;
          case (char)0x93:
          case (char)0x94:
            sb.Append("\"");
            bDoneCRLF = false;
            bDoneSpace = false;
            uTabPos++;
            break;
          case '<':
            sb.Append("&lt;");
            bDoneCRLF = false;
            bDoneSpace = false;
            uTabPos++;
            break;
          case '>':
            sb.Append("&gt;");
            bDoneCRLF = false;
            bDoneSpace = false;
            uTabPos++;
            break;
          case '\"':
            sb.Append("&quot;");
            bDoneCRLF = false;
            bDoneSpace = false;
            uTabPos++;
            break;
          case '&':
            sb.Append("&amp;");
            bDoneCRLF = false;
            bDoneSpace = false;
            uTabPos++;
            break;
          case ' ':
            if (bDoneSpace || bHardSpace)
            {
              sb.Append("&nbsp;");
            }
            else
            {
              sb.Append(' ');
              bDoneSpace = true;
            }
            bDoneCRLF = false;
            uTabPos++;
            break;
          case '\n':
            if (!bDoneCRLF)
            {
              sb.Append("<br />");

            }
            bDoneCRLF = false; // To allow multiple CRLFs to produce multiple <BR />s
            bDoneSpace = false;
            uTabPos = 0;
            break;
          case '\r':
            if (!bDoneCRLF)
            {
              sb.Append("<br />");
              bDoneCRLF = true;
            }
            bDoneSpace = false;
            uTabPos = 0;
            break;
          case '\t':
            do
            {
              sb.Append("&nbsp;");
              uTabPos++;
            }
            while ((uTabPos % uTabSpaces) != 0);
            bDoneSpace = true;
            break;
          default:

            sb.Append(c);
            bDoneCRLF = false;
            bDoneSpace = false;
            uTabPos++;
            break;
        }
        ++n;
      }
      return sb.ToString();
    }

    // Converts all Javascript characters into their escaped versions
    protected static string EscapeJavascript(string sOriginal)
    {
        if (sOriginal == null)
        {
            return "";
        }

      System.Text.StringBuilder sb = new System.Text.StringBuilder(sOriginal.Length);

      foreach (char c in sOriginal)
      {
        switch (c)
        {
          case '\'':
            sb.Append("\\'");
            break;
          default:
            sb.Append(c);
            break;
        }
      }
      return sb.ToString();
    }

    // Converts all invalid sFilename characters into underscores
    protected static string EscapeFilename(string sOriginal)
    {
      LogFile.TheLogFile.WriteLine(LogFile.DT_HTML, LogFile.EDebugLevel.Note, String.Format("EscapeFilename({0})", sOriginal));

      if (sOriginal == null)
      {
          return "_";
      }

      const string sValidChars = " 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz$%'`-@{}~!#()&_^";

      System.Text.StringBuilder sb = new System.Text.StringBuilder(sOriginal.Length);
      int nLength = sOriginal.Length;
      int n = 0;
      foreach (char c in sOriginal)
      {
        char cc = c;
        if (sValidChars.IndexOf(c) < 0)
        {
          cc = '_';
        }
        sb.Append(cc);
        ++n;
      }
      return sb.ToString();
    }

    // Modifies the provided string to have its first letter capitalised and the rest unchanged.
    public static void Capitalise(ref string s)
    {
      if (s != null && s != "")
      {
        s = Char.ToUpper(s[0]) + s.Substring(1);
      }
      else
      {
        s = "";
      }
    }

    // Returns a string with all email addresses replaced by value of sReplacement.
    protected static string ObfuscateEmail(string sText)
    {
      if (sText == null)
      {
        return null;
      }
      int nLength = sText.Length;
      System.Text.StringBuilder sb = new System.Text.StringBuilder(nLength);
      int i = 0;
      int nNameStart = -1;
      int nState = 0;
      const string sInvalidNameChars = ",\"£$^&*()+=]}[{':;,<>?/\\|`¬#~";
      const string sReplacement = "<email address>";

      while (i < nLength)
      {
        char c = sText[i];

        switch (nState)
        {
          case 0:
            // Not seen anything special.
            if (!char.IsWhiteSpace(c) && c != '@' && c != '.' && sInvalidNameChars.IndexOf(c) < 0)
            {
              // Possible name char, remember where name starts.
              nState = 1;
              nNameStart = i;
            }
            else
            {
              // Can't be an email name. Add it verbatim.
              sb.Append(c);
              nState = 0;
            }
            break;
          case 1:
            // Seen one or more name chars.
            if (c == '@')
            {
              // Now looking for domain.
              nState = 2;
            }
            else if (!char.IsWhiteSpace(c) && sInvalidNameChars.IndexOf(c) < 0)
            {
              // Continue looking through a possible name string.
            }
            else
            {
              // Can't be an email address. Add what we've got so far and return
              // to hunting email addresses.
              sb.Append(sText.Substring(nNameStart, i - nNameStart));
              sb.Append(c);
              nState = 0;
            }
            break;
          case 2:
            // Seen at sign, now looking for domain
            if (!char.IsWhiteSpace(c) && c != '@' && c != '.' && sInvalidNameChars.IndexOf(c) < 0)
            {
              // Possible domain char.
              // Now looking for dot among domain chars.
              nState = 3; 
            }
            else
            {
              // Can't be an email address. Add what we've got so far and return
              // to hunting email addresses.
              sb.Append(sText.Substring(nNameStart, i - nNameStart));
              sb.Append(c);
              nState = 0;
            }
            break;
          case 3:
            // Looking for first dot among domain chars
            if (c == '.')
            {
              // Now looking for another domain.
              nState = 4; 
            }
            else if (!char.IsWhiteSpace(c) && c != '@' && sInvalidNameChars.IndexOf(c) < 0)
            {
              // A possible domain char, keep looking for dot.
            }
            else
            {
              // Can't be an email address. Add what we've got so far and return
              // to hunting email addresses.
              sb.Append(sText.Substring(nNameStart, i - nNameStart));
              sb.Append(c);
              nState = 0;
            }
            break;
          case 4:
            // Looking for valid domain char to start next domain portion.
            if (!char.IsWhiteSpace(c) && c != '@' && c != '.' && sInvalidNameChars.IndexOf(c) < 0)
            {
              // A valid domain char. Look for another dot , or end.
              nState = 5; 
            }
            else
            {
              // Can't be an email address. Add what we've got so far and return
              // to hunting email addresses.
              sb.Append(sText.Substring(nNameStart, i - nNameStart));
              sb.Append(c);
              nState = 0;
            }
            break;
          case 5:
            // Looking for a dot or end of domain among valid domain chars
            if (c == '.')
            {
              // Read rest of domain part.
              nState = 6; 
            }
            else if (!char.IsWhiteSpace(c) && c != '@' && sInvalidNameChars.IndexOf(c) < 0)
            {
              // Valid domain name. Keep looking for dot or end.
            }
            else if (c != '@')
            {
              // Found complete email address
              sb.Append(sReplacement);
              sb.Append(c);
              nState = 0;
            }
            else
            {
              // Can't be an email address. Add what we've got so far and return
              // to hunting email addresses.
              sb.Append(sText.Substring(nNameStart, i - nNameStart));
              sb.Append(c);
              nState = 0;
            }
            break;
          case 6:
            // Looking for valid domain char to start next domain portion, or can end here if address is (name@add.add.)
            if (!char.IsWhiteSpace(c) && c != '@' && c != '.' && sInvalidNameChars.IndexOf(c) < 0)
            {
              // A valid domain char. Look for another dot , or end.
              nState = 5; 
            }
            else
            {
              // Found complete email address (ending in a full-stop).
              sb.Append(sReplacement);
              sb.Append('.');
              sb.Append(c);
              nState = 0;
            }
            break;

        } // End of switch.
        ++i;
      }

      // Add anything remaining in email addr buffer.
      if (nState == 5 || nState == 6)
      {
        // Found complete email address.
        sb.Append(sReplacement);
        if (nState == 6)
        {
          // We ended on a dot.
          sb.Append('.');
        }
      }
      else if (nState > 0)
      {
        sb.Append(sText.Substring(nNameStart, i - nNameStart));
      }
      return sb.ToString();
    }

    // Generates navbar at top of page, in header div
    protected static void OutputPageHeader(StreamWriter sw, string sPreviousChildLink, string sNextChildLink, bool bIncludeIndexLink, bool bIncludeHelpLink)
    {
      if (MainForm.s_config.m_bIncludeNavbar)
      {
        string sFrontPageLink = "";
        if (MainForm.s_config.m_sFrontPageFilename != "")
        {
          sFrontPageLink += String.Concat("<a href=\"", MainForm.s_config.m_sFrontPageFilename, ".", MainForm.s_config.m_sHtmlExtension, "\">front page</a>");
        }
        string sMainSiteLink = "";
        if (MainForm.s_config.m_sMainWebsiteLink != "")
        {
          sMainSiteLink += String.Concat("<a href=\"", MainForm.s_config.m_sMainWebsiteLink, "\">main site</a>");
        }
        string sHelpPageLink = "";
        if (MainForm.s_config.m_bIncludeHelppage)
        {
          sHelpPageLink += String.Concat("<a href=\"help.", MainForm.s_config.m_sHtmlExtension, "\">help</a>");
        }
        bool bIncludeNavbar = sPreviousChildLink != ""
          || sNextChildLink != ""
          || bIncludeIndexLink
          || sFrontPageLink != ""
          || sMainSiteLink != ""
          || sHelpPageLink != "";

        if (bIncludeNavbar)
        {
          sw.WriteLine("    <div id=\"header\">");
          sw.WriteLine("      <ul>");

          if (sPreviousChildLink != "")
          {
            sw.WriteLine(String.Concat("        <li>", sPreviousChildLink, "</li>"));
          }

          if (sNextChildLink != "")
          {
            sw.WriteLine(String.Concat("        <li>", sNextChildLink, "</li>"));
          }

          if (bIncludeIndexLink)
          {
            sw.WriteLine(String.Concat("        <li><a href=\"individuals1.",
              MainForm.s_config.m_sHtmlExtension,
              "\">index</a></li>"));
          }

          if (sFrontPageLink != "")
          {
            sw.WriteLine(String.Concat("        <li>", sFrontPageLink, "</li>"));
          }

          if (sMainSiteLink != "")
          {
            sw.WriteLine(String.Concat("        <li>", sMainSiteLink, "</li>"));
          }

          if (bIncludeHelpLink && sHelpPageLink != "")
          {
            sw.WriteLine(String.Concat("        <li>", sHelpPageLink, "</li>"));
          }

          sw.WriteLine("      </ul>");

          sw.WriteLine("    </div> <!-- header -->");
          sw.WriteLine("");
        }
      }
    }

    // Copies a file from the user's source directory to the website output directory, renaming and resizing as appropriate.
    // Returns the sFilename of the copy.
    // sArea is sAsid sub-part of image
    // sArea is changed to reflect new image size
    // sArea can be {0,0,0,0} meaning use whole image
    // stats can be null if we don't care about keeping count of the multimedia files.
    public static string CopyMultimedia( string sFullFilename, string sNewFilename, uint uMaxWidth, uint uMaxHeight, ref Rectangle rectArea, CStats stats )
    {
      LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Note, String.Format("CopyMultimedia( {0}, {1}, {2} )", sFullFilename, uMaxWidth, uMaxHeight ) );

      string sResult = sFullFilename;

      if( sNewFilename == "" )
      {
        sNewFilename = Path.GetFileName( sFullFilename );
      }

      try
      {
        string sAsidFilename;
        if (rectArea.Width == 0)
        {
            sAsidFilename = sFullFilename;
        }
        else
        {
            sAsidFilename = String.Concat(sFullFilename, ".", rectArea.X.ToString(), ",", rectArea.Y.ToString(), ",", rectArea.Width.ToString(), ",", rectArea.Height.ToString());
        }

        if( uMaxWidth != 0 && uMaxHeight != 0 )
        {
          sAsidFilename = String.Concat( sAsidFilename, "(", uMaxWidth.ToString(), "x", uMaxHeight.ToString(), ")");
        }

        if( sFullFilename != null && MainForm.s_config.m_sOutputFolder != null && MainForm.s_config.m_sOutputFolder != "" )
        {
          // Have we already copied the sFilename?
          if( s_htCopiedFiles.ContainsKey( sAsidFilename ) )
          {
            CFilenameAndSize filenameAndSize = (CFilenameAndSize)s_htCopiedFiles[ sAsidFilename ];
            sResult = filenameAndSize.m_sFilename;
            rectArea.Width = filenameAndSize.m_nWidth;
            rectArea.Height = filenameAndSize.m_nHeight;
          }
          else
          {
            // Copy file into output directory
            if( MainForm.s_config.m_bCopyMultimedia )
            {
              string sImageFolder = MainForm.s_config.m_sImageFolder;
              string sOutputFolder = MainForm.s_config.m_sOutputFolder;

              if (sImageFolder != "")
              {
                  sImageFolder = sImageFolder + '\\';
              }
              if (sOutputFolder != "")
              {
                  sOutputFolder = sOutputFolder + '\\';
              }

              string sCopyFilename = String.Concat(sImageFolder, sNewFilename);
              string sAbsImageFolder = String.Concat(sOutputFolder,sImageFolder);
              string sAbsCopyFilename = String.Concat(sAbsImageFolder, sNewFilename);

              // If image folder doesn't exist, create it
              if( !File.Exists( sAbsImageFolder ) && !Directory.Exists( sAbsImageFolder ) ) // TODO: this returns false if it exists but you don't have permission!
              {
                Directory.CreateDirectory( sAbsImageFolder ); // TODO: catch failure to create, e.g. output folder not there yet
              }

              // If new sFilename already exists, append a number and keep trying
              uint uCopy = 0;
              string sFilePart = Path.GetFileNameWithoutExtension( sCopyFilename );
              string sExtnPart = Path.GetExtension( sCopyFilename );
              while( File.Exists( sAbsCopyFilename ) )
              {
                const string sAdditionalLetters = "abcdefghijklmnopqrstuvwxyz";
                if( MainForm.s_config.m_bRenameMultimedia == false )
                {
                  uint nCopyPlus = uCopy + 2;
                  sCopyFilename = String.Concat( sImageFolder, sFilePart, "-", nCopyPlus.ToString(), sExtnPart );
                }
                else if( uCopy >= sAdditionalLetters.Length )
                {
                  // Once all the extra letters have been used up, put number as "-n", where n starts from 2.
                  uint nCopyMinus = uCopy - (uint)(sAdditionalLetters.Length - 2);
                  sCopyFilename = String.Concat( sImageFolder, sFilePart, "-", nCopyMinus.ToString(), sExtnPart );
                }
                else
                {
                  sCopyFilename = String.Concat( sImageFolder, sFilePart, sAdditionalLetters[(int)uCopy], sExtnPart );
                }
                uCopy++;

                sAbsCopyFilename = String.Concat( sOutputFolder,sCopyFilename );
              }

              LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Note, String.Format("Copying \"{0}\" to \"{1}\"",sFullFilename, sAbsCopyFilename ) );
              File.Copy( sFullFilename, sAbsCopyFilename, true );
              File.SetAttributes( sFullFilename, System.IO.FileAttributes.Normal ); // Make any Read-Only files read-write.
              if( uMaxWidth != 0 && uMaxHeight != 0 )
              {
                // It must be a picture file
                sCopyFilename = ConvertAndCropImage( sOutputFolder, sCopyFilename, ref rectArea, uMaxWidth, uMaxHeight );
              }
              s_htCopiedFiles[ sAsidFilename ] = new CFilenameAndSize( sCopyFilename, rectArea.Width, rectArea.Height );
              sResult = sCopyFilename;
            }
            else
            {
              if( MainForm.s_config.m_bRelativiseMultimedia )
              {
                // TODO: make path of sFilename relative to MainForm.s_config.m_outputFolder
                string sRelativeFilename = sFullFilename;
                sResult = sRelativeFilename;
              }
            }
            if( null != stats )
            {
              stats.m_unMultimediaFiles++;
            } 
          }
        }
        sResult = sResult.Replace( '\\', '/' );
      }
      catch( IOException e )
      {
        LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Error, "Caught IO Exception : " + e.ToString() );
        sResult = "";
      }
      catch( ArgumentException e )
      {
        LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Error, "Caught Argument Exception : " + e.ToString() );
        sResult = "";
      }
      catch( CHTMLException e )
      {
        LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Error, "Caught HTML Exception : " + e.ToString() );
        sResult = "";
      }
      catch( Exception e )
      {
        LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Error, "Caught generic exception : " + e.ToString() );
        sResult = "";
      }

      return sResult;
    }

    // Outputs the HTML to display the W3C Valid XHTML image on the page.
    protected void OutputValiditySticker(CHTMLFile f)
    {
      f.m_sw.WriteLine("<p class=\"plain\">");
      f.m_sw.WriteLine("<a href=\"http://validator.w3.org/check?uri=referer\"><img");
      f.m_sw.WriteLine("src=\"" + m_sW3CFile + "\"");
      f.m_sw.WriteLine("style=\"margin-top:4px\"");
      f.m_sw.WriteLine("alt=\"Valid XHTML 1.0 Strict\" height=\"31\" width=\"88\" /></a>");
      f.m_sw.WriteLine("</p>");
    }

    // Creates link HTML for the individual e.g. <a href="indiI1.html">Fred Bloggs</a>
    protected static string MakeLink( CIndividualRecord ir )
    {
      string sName = ir.Name;
      string sDummy = "";
      if (sName == "")
      {
          sName = MainForm.s_config.m_sUnknownName;
      }
      else if ((ir.Visibility() != CIndividualRecord.EVisibility.Visible) && !MainForm.s_config.m_bUseWithheldNames)
      {
          sName = MainForm.s_config.m_sConcealedName;
      }
      else
      {
          sName = MainForm.s_config.CapitaliseName(sName, ref sDummy, ref sDummy);
      }

      return MakeLink( ir, sName );
    }

    // Creates link HTML for the individual e.g. <a href="indiI1.html">Next Child</a>. Uses name provided by caller.
    protected static string MakeLink( CIndividualRecord ir, string sName )
    {
      string sLink;
      if( ir.Visibility() == CIndividualRecord.EVisibility.Invisible )
      {
        // TODO: Why are we linking to invisible people?
        sLink = CCreator.EscapeHTML(sName, true);
      }
      else
      {
        sLink = String.Concat("<a href=\"", GetIndividualHTMLFilename(ir), "\">", CCreator.EscapeHTML(sName, false), "</a>");
      }

      return sLink;
    }

    // Returns a string to use as a sFilename for this individual's HTML page.
    // The string is just the sFilename, not a fully qualified path.
    protected static string GetIndividualHTMLFilename( CIndividualRecord ir )
    {
      string sRelativeFilename = String.Concat( "indi", ir.m_xref, ".", MainForm.s_config.m_sHtmlExtension );
      if( MainForm.s_config.m_bUserRecFilename )
      {
        if( ir.m_alUserReferenceNumbers.Count > 0 )
        {
          CUserReferenceNumber urn = (CUserReferenceNumber)ir.m_alUserReferenceNumbers[0];
          string sFilenameUserRef = EscapeFilename(urn.m_sUserReferenceNumber);
          if( sFilenameUserRef.Length > 0 )
          {
            sRelativeFilename = String.Concat( "indi", sFilenameUserRef, ".", MainForm.s_config.m_sHtmlExtension );
          }
        }
      }    
      return sRelativeFilename;
    }

    // Crops the specified image file to the given size. Also converts non-standard formats to standard ones.
    // Returns sFilename in case extension has changed.
    // sArea is changed to reflect new image size
    private static string ConvertAndCropImage( string sFolder, string sFilename, ref Rectangle rectArea, uint uMaxWidth, uint uMaxHeight )
    {
      LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Note, String.Format("ConvertAndCropImage( {0}, {1} )", sFolder!=null?sFolder:"null", sFilename!=null?sFilename:"null") );

      string sAbsFilename = String.Concat( sFolder, sFilename );

      Image image = null;
      try
      {
        image = Image.FromFile( sAbsFilename );
      }
      catch( OutOfMemoryException )
      {
        // Image is not a GDI compatible format
        image = null;
      }

      if( image == null )
      {
        throw( new CHTMLException("Unknown image format for file " + sAbsFilename ) ); // Let caller sort it out.
      }

      Rectangle rectNewArea;
      if( rectArea.Width <= 0 || rectArea.Height <= 0 )
      {
        SizeF s = image.PhysicalDimension;
        if( s.Width <= uMaxWidth && s.Height <= uMaxHeight )
        {
          uMaxWidth = (uint)s.Width;
          uMaxHeight = (uint)s.Height;
          // Nothing needs to be done, bitmap already correct size.
          // Carry on with conversion.
        }
        rectNewArea = new Rectangle( 0, 0, (int)s.Width, (int)s.Height );
        rectArea.X = 0;
        rectArea.Y = 0;
        rectArea.Width = rectNewArea.Width;
        rectArea.Height = rectNewArea.Height;

      }
      else
      {
        rectNewArea = new Rectangle( 0, 0, rectArea.Width, rectArea.Height );
      }

      if( uMaxWidth != 0 && uMaxHeight != 0 )
      {
        // If image is too big then shrink it. (Can't always use GetThumbnailImage because that might use embedded thumbnail).
        MainForm.ScaleAreaToFit( ref rectNewArea, uMaxWidth, uMaxHeight );
      }

      Bitmap bitmapNew = new Bitmap( rectNewArea.Width, rectNewArea.Height, System.Drawing.Imaging.PixelFormat.Format24bppRgb );

      Graphics graphicsNew = Graphics.FromImage( bitmapNew );

      graphicsNew.DrawImage(image, rectNewArea, rectArea, GraphicsUnit.Pixel );
      image.Dispose();

      // Find which format to save in. TODO: There must be a more elegant way!!
      string sExtn = Path.GetExtension( sFilename );
      string sFilepart = Path.GetDirectoryName( sFilename );
      sFilepart += "\\" + Path.GetFileNameWithoutExtension( sFilename );
      System.Drawing.Imaging.ImageFormat imageFormat;
      switch( sExtn.ToLower() )
      {
        case ".jpg":
        case ".jpeg":
          sExtn = ".jpg";
          imageFormat = System.Drawing.Imaging.ImageFormat.Jpeg;
          break;
        case ".gif":
          imageFormat = System.Drawing.Imaging.ImageFormat.Gif;
          break;
        case ".bmp":
          imageFormat = System.Drawing.Imaging.ImageFormat.Bmp;
          break;
        case ".tif":
        case ".tiff":
          // Tif's don't display in browsers, so convert to png.
          imageFormat = System.Drawing.Imaging.ImageFormat.Png;
          sExtn = ".png";
          break;
        case ".exif":
          imageFormat = System.Drawing.Imaging.ImageFormat.Exif;
          break;
        case ".png":
          imageFormat = System.Drawing.Imaging.ImageFormat.Png;
          break;
        default:
          imageFormat = System.Drawing.Imaging.ImageFormat.Jpeg;
          break;
      }

      string sFilenameNew = sFilepart + sExtn;
      string sAbsFilenameNew = String.Concat( sFolder, sFilenameNew );
      try
      {
        if( File.Exists( sAbsFilename ) )
        {
          // Delete the old file (e.g. if converting from tif to png)
          File.Delete( sAbsFilename );
        }
      }
      catch( Exception e )
      {
        LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Warning,
          String.Format("Caught exception while removing old bitmap file {0} : {1}",
          sAbsFilename, e.ToString() ) );
      }
      try
      {
        if( File.Exists( sAbsFilenameNew ) )
        {
          // Delete any existing file
          File.SetAttributes( sAbsFilenameNew, FileAttributes.Normal );
          File.Delete( sAbsFilenameNew );
        }
        bitmapNew.Save( sAbsFilenameNew, imageFormat );
      }
      catch( Exception e )
      {
        LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Warning,
          String.Format("Caught exception while writing bitmap file {0} : {1}",
          sFilenameNew, e.ToString() ) );
        sFilenameNew = "";
      }
      graphicsNew.Dispose();
      bitmapNew.Dispose();

      rectArea = rectNewArea;
      return sFilenameNew;
    }
  }
}
