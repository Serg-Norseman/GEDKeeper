/* CCreatorRecord.cs
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
using GEDmill.LLClasses;
using System.Drawing;
using System.IO;

namespace GEDmill.HTMLClasses
{
  // Base class for the creators of individual and source pages
  public class CCreatorRecord : CCreator
  {
    // All the images and other multimedia files associated with this record.
    protected ArrayList m_alMultimediaList;

    // Constructor
    protected CCreatorRecord( CGedcom gedcom, IProgressCallback progress, string w3cfile ) : base( gedcom, progress, w3cfile )
    {
      m_alMultimediaList = new ArrayList();
    }

    // Adds the given multimedia links to the given multimedia list.
    protected void AddMultimedia( ArrayList multimediaLinks, ArrayList userMultimediaFileReferences, string sMmPrefix, string sMmLargePrefix, uint uMaxWidth, uint uMaxHeight, CStats stats )
    {
      //int nFilesAdded = 0;

      if( multimediaLinks != null )
      {
        foreach( CMultimediaLink ml in multimediaLinks )
        {
          ArrayList alFileRefs = ml.GetFileReferences();
          if( alFileRefs != null )
          {
            // Add the pics from the GEDCOM record.
            AddMultimediaFileReferences( alFileRefs, sMmPrefix, sMmLargePrefix, uMaxWidth, uMaxHeight, stats );
          }
        }
      }
      if( userMultimediaFileReferences != null ) 
      {
        // Add extra pics added by the user on indi exclude screen.
        AddMultimediaFileReferences( userMultimediaFileReferences, sMmPrefix, sMmLargePrefix, uMaxWidth, uMaxHeight, stats );
      }
    }

    // Adds an HTML page footer (Record date, W3C sticker, GEDmill credit etc.)
    protected void OutputFooter( CHTMLFile f, CRecord r )
    {
      f.m_sw.WriteLine("      <div id=\"footer\">");
      if ( (r.m_alUserReferenceNumbers.Count > 0)
        || (r.m_sAutomatedRecordId != null && r.m_sAutomatedRecordId != "")
        || (r.m_changeDate != null)
        || (MainForm.s_config.m_sCustomFooter != ""))
      {
        foreach( CUserReferenceNumber urn in r.m_alUserReferenceNumbers )
        {
          string idType = EscapeHTML( urn.m_sUserReferenceType, false );
          if( idType == "" )
          {
            idType = "User reference number";
          }
          f.m_sw.WriteLine( String.Concat("        <p>", idType, ": ", EscapeHTML(urn.m_sUserReferenceNumber, false), "</p>") );
        }

        if (r.m_sAutomatedRecordId != null && r.m_sAutomatedRecordId != "")
        {
          f.m_sw.WriteLine(String.Concat("        <p>Record ", r.m_sAutomatedRecordId, "</p>"));
        }
        if (r.m_changeDate != null)
        {
          CPGDate changeDate = CPGDate.Parse(r.m_changeDate.m_sChangeDate);
          string changeTime = r.m_changeDate.m_sTimeValue;
          if (changeDate != null && changeTime != null)
          {
            if (changeTime != "")
            {
              changeTime = " " + changeTime;
            }
            f.m_sw.WriteLine(String.Concat("        <p id=\"changedate\">Record last changed ", changeDate.ToString(), changeTime, "</p>"));
          }
        }
        if (MainForm.s_config.m_sCustomFooter != "")
        {
          if (MainForm.s_config.m_bFooterIsHtml)
          {
            f.m_sw.WriteLine(String.Concat("        <p>", MainForm.s_config.m_sCustomFooter, "</p>"));
          }
          else
          {
            f.m_sw.WriteLine(String.Concat("        <p>", EscapeHTML(MainForm.s_config.m_sCustomFooter, false), "</p>"));
          }
        }
      }
      f.m_sw.WriteLine("      </div> <!-- footer -->");

      f.m_sw.WriteLine("<p class=\"plain\">Page created using GEDmill " + MainForm.m_sSoftwareVersion + "</p>");

      if (MainForm.s_config.m_bIncludeValiditySticker)
      {
        OutputValiditySticker(f);
      }
    }

    // Adds the given list of file references to the multimedia list.
    private void AddMultimediaFileReferences( ArrayList alFileRefs, string sMmPrefix, string sMmLargePrefix, uint uMaxWidth, uint uMaxHeight, CStats stats )
    {
      if( alFileRefs == null )
      {
        return;
      }

      foreach( CMultimediaFileReference mfr in alFileRefs )
      {

        if( mfr.m_sMultimediaFileReference == null || mfr.m_bVisible == false )
        {
          // No sFilename, or else user chose not to show this picture
          continue;
        }

        string sCopyFilename = "";
        int nMmOrdering = mfr.m_nOrderIndex;
        string sMmTitle = mfr.m_sDescriptiveTitle;
        string sMmFilename = mfr.m_sMultimediaFileReference;
        string sMmFormat = mfr.m_sMultimediaFormat;
        Rectangle rectArea = new Rectangle(0,0,0,0);
        string sExtnPart;
        bool bBlockThisMediaType = false;

        // Don't trust extension on sFilename. Use our own. (Happens for .tmp files from embedded data)
        switch( sMmFormat )
        {
          case "bmp":
            sExtnPart = ".bmp";
            break;
          case "gif":
            sExtnPart = ".gif";
            break;
          case "jpg":
          case "jpeg":
            sExtnPart = ".jpg";
            break;
          case "tiff":
          case "tif":
            sExtnPart = ".tif";
            break;
          case "png":
            sExtnPart = ".png";
            break;
          case "ole":
            bBlockThisMediaType = true;
            sExtnPart = ".ole";
            break;
          default:
            sExtnPart = Path.GetExtension( sMmFilename );
            if( sExtnPart.ToUpper() == ".TMP" )
            {
              sExtnPart = "." + sMmFormat;
            }
            break;
        }
        string sOriginalFilename = Path.GetFileName( sMmFilename );

        bool bPictureFormat = mfr.IsPictureFormat();

        if( bPictureFormat || MainForm.s_config.m_bAllowNonPictures )
        {
          if( !bPictureFormat && MainForm.s_config.m_bAllowNonPictures )
          {
            stats.m_bNonPicturesIncluded = true;
          }

          string sNewFilename = sOriginalFilename;
          if( sMmFilename != null && sMmFilename != "" )
          {
            // Give multimedia files a standard name
            if( MainForm.s_config.m_bRenameOriginalPicture )
            {
              //string sFilePart = sMmPrefix;//String.Concat( mm_prefix, nMultimediaFiles.ToString() );
              sNewFilename = String.Concat(sMmPrefix, sExtnPart.ToLower());
            }

            if( !bBlockThisMediaType )
            {
              if( bPictureFormat )
              {
                if( mfr.m_asidPair != null )
                {
                  Rectangle rectAsidArea = mfr.m_asidPair.m_rectArea;
                  rectArea = new Rectangle( rectAsidArea.X, rectAsidArea.Y, rectAsidArea.Width, rectAsidArea.Height );
                }
                sCopyFilename = CopyMultimedia( sMmFilename, sNewFilename, uMaxWidth, uMaxHeight, ref rectArea, stats );
              }
              else
              {
                sCopyFilename = CopyMultimedia( sMmFilename, sNewFilename, 0, 0, ref rectArea, stats );
              }
            }
          }


          if( sCopyFilename != null && sCopyFilename != "" )
          {
            string sLargeFilename = "";
            // Copy original original version
            if( MainForm.s_config.m_bLinkOriginalPicture )
            {
              if( MainForm.s_config.m_bRenameOriginalPicture )
              {
                //string sFilePart = sMmLargePrefix;
                sLargeFilename = String.Concat(sMmLargePrefix, sExtnPart.ToLower());
              }
              else
              {
                sLargeFilename = sOriginalFilename;
              }

              Rectangle rectLargeArea = new Rectangle(0,0,0,0);
              sLargeFilename = CopyMultimedia( sMmFilename, sLargeFilename, 0, 0, ref rectLargeArea, null );
            }

            // Add format and new sFilename to multimedia list
            CIMultimedia imm = new CIMultimedia( nMmOrdering, sMmFormat, sMmTitle, sCopyFilename, sLargeFilename, rectArea.Width, rectArea.Height );
            m_alMultimediaList.Add( imm );
          }
          else
          {
            // Happens e.g. when original file doesn't exist.
          }
        }
      }
    }
  }
}
