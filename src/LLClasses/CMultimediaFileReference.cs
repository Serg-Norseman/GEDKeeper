/* CMultimediaFileReference.cs
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

namespace GEDmill.LLClasses
{
    // GEDCOM 'FILE'. See GEDCOM standard for details on GEDCOM data.
    public class CMultimediaFileReference : GEDmill.LLClasses.CParserObject
    {
        // Filename
        public string m_sMultimediaFileReference; 

        // File format (gif, jpg, bmp etc.)
        public string m_sMultimediaFormat = "";

        // Title of picture
        public string m_sDescriptiveTitle;

        // Pointer to continuation object for fragmented files
        public string m_xrefObj; 

        // Asid pairs define a region of interest in a larger image (used by Calico Pie's Family Historian)
        public CAsidPair m_asidPair;

        // True if this file ref came from gedcom file, so user can't delete or modify it.
        public bool m_bFromGEDCOM;

        // True if user has elected to include this file in the website
        public bool m_bVisible;

        // Used to control order of pictures in web page. Lower number = nearer top.
        public int m_nOrderIndex;

        // True if the file this refers to was embedded inline in the gedcom.
        public bool m_bEmbedded;

        // Need to keep xref of embedded files, because sFilename will be different every time GEDCOM is loaded.
        public string m_xrefEmbedded;

        // Reference to MFR from which this was copied, if any. Used to commit changes to MFRs made by user in pictures dialog.
        public CMultimediaFileReference m_mfrOriginal; 

        // Constructor
        public CMultimediaFileReference( CGedcom gedcom ) : base( gedcom )
        {
            m_bFromGEDCOM = true;
            m_bVisible = true;
            m_nOrderIndex = 0;
            m_mfrOriginal = null;
            m_asidPair = null;
            m_xrefEmbedded = "";
            m_sMultimediaFileReference = "";
        }

        // Copy constructor
        public CMultimediaFileReference( CMultimediaFileReference mfr, bool bKeepRefToOriginal ) : base( mfr.Gedcom )
        {
            CopyFrom( mfr );

            if( bKeepRefToOriginal )
            {
                m_mfrOriginal = mfr;
            }
            else
            {
                m_mfrOriginal = null;
            }
        }

        // Assignment
        public void CopyFrom( CMultimediaFileReference mfr )
        {
            m_sMultimediaFileReference = mfr.m_sMultimediaFileReference;
            m_sMultimediaFormat = mfr.m_sMultimediaFormat;
            m_sDescriptiveTitle = mfr.m_sDescriptiveTitle;
            m_xrefObj = mfr.m_xrefObj;
            if( mfr.m_asidPair != null )
            {
                m_asidPair = new CAsidPair( mfr.m_asidPair );
            }
            else
            {
                m_asidPair = null;
            }
            m_bFromGEDCOM = mfr.m_bFromGEDCOM;
            m_bEmbedded = mfr.m_bEmbedded;
            m_bVisible = mfr.m_bVisible;
            m_nOrderIndex = mfr.m_nOrderIndex;
            m_xrefEmbedded = mfr.m_xrefEmbedded;

            m_mfrOriginal = null;
        }

        // Parser
        public static CMultimediaFileReference Parse( CGedcom gedcom, int nLevel )
        {
            CGedcomLine gedcomLine;

            // Temporary holders for class members. Saves constructing a class early.
            string sMultimediaFileReference;
            string sMultimediaFormat = "";
            string sSourceMediaType = "";

            // There must be one of these, it defines the object.
            if ((gedcomLine = gedcom.GetLine(nLevel, "FILE")) == null)
            {
                // Not one of us
                return null;
            }
            sMultimediaFileReference = gedcomLine.LineItem;
            gedcom.IncrementLineIndex(1);

            bool bParsingFinished;
            do
            {
                bParsingFinished = true;


                // There must be one of these, standard specifies {1:1}
                if( (gedcomLine = gedcom.GetLine(nLevel+1, "FORM")) != null )
                {
                    sMultimediaFormat = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);

                    // There may be one of these, standard specifies {0:1}
                    if( (gedcomLine = gedcom.GetLine(nLevel+2, "TYPE")) != null )
                    {
                        sSourceMediaType = gedcomLine.LineItem;
                        gedcom.IncrementLineIndex(1);
                    }
                    else if( (gedcomLine = gedcom.GetLine(nLevel+2, "MEDI")) != null )
                    {
                        sSourceMediaType = gedcomLine.LineItem;
                        gedcom.IncrementLineIndex(1);
                    }
                
                    bParsingFinished = false;
                }
                else if( ( gedcomLine = gedcom.GetLine()).Level > nLevel )
                {
                    LogFile.TheLogFile.WriteLine( LogFile.DT_GEDCOM, LogFile.EDebugLevel.Warning, "Unknown tag :" );
                    LogFile.TheLogFile.WriteLine( LogFile.DT_GEDCOM, LogFile.EDebugLevel.Warning, gedcomLine.ToString() );
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }
            }
            while( !bParsingFinished );

            // Parsing went ok. Construct a new object and return it.
            CMultimediaFileReference mfr = new CMultimediaFileReference( gedcom );
            mfr.m_sMultimediaFileReference = sMultimediaFileReference;
            mfr.m_sMultimediaFormat = sMultimediaFormat; 
            return mfr;
        }

        // Returns true if the multimedia referenced is a static image (not video, audio, document etc.)
        public bool IsPictureFormat()
        {
            bool bPictureFormat = true;

            if( m_sMultimediaFormat != "bmp"
                && m_sMultimediaFormat != "gif"
                && m_sMultimediaFormat != "jpg"
                && m_sMultimediaFormat != "png"
                && m_sMultimediaFormat != "jpeg"
                && m_sMultimediaFormat != "tiff"
                && m_sMultimediaFormat != "tif" )
            {
                string sExtn = System.IO.Path.GetExtension( m_sMultimediaFileReference );
                switch( sExtn.ToLower() )
                {
                    case ".jpg":
                    case ".jpeg":
                    case ".gif":
                    case ".bmp":
                    case ".tif":
                    case ".tiff":
                    case ".exif":
                    case ".png":
                        break;
                    default:
                        bPictureFormat = false;
                        break;
                }
            }
            return bPictureFormat;
        }
    
        // To order the pictures on the web page
        public class OrderComparer : IComparer
        {
            public int Compare(object x, object y) 
            {
                CMultimediaFileReference mfrX = null;
                CMultimediaFileReference mfrY = null;
                int nOrderX = 0;
                int nOrderY = 0;

                if( x != null && x is CMultimediaFileReference )
                {
                    mfrX = (CMultimediaFileReference)x;
                    nOrderX = mfrX.m_nOrderIndex;
                }
                if( y != null && y is CMultimediaFileReference )
                {
                    mfrY = (CMultimediaFileReference)y;
                    nOrderY = mfrY.m_nOrderIndex;
                }

                if( mfrX == null )
                {
                    if( mfrY == null )
                    {
                        return 0;
                    }
                    return 1;
                }
                
                if( mfrY == null )
                {
                    return -1;
                }

                return nOrderX - nOrderY;
            }
        }   
    }
}
