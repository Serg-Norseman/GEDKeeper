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
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using GKCommon.GEDCOM;

namespace GEDmill.HTML
{
    /// <summary>
    /// Base class for the creators of individual and source pages
    /// </summary>
    public class CreatorRecord : Creator
    {
        // All the images and other multimedia files associated with this record.
        protected List<Multimedia> fMultimediaList;


        protected CreatorRecord(GEDCOMTree gedcom, IProgressCallback progress, string w3cfile) : base(gedcom, progress, w3cfile)
        {
            fMultimediaList = new List<Multimedia>();
        }

        // Adds the given multimedia links to the given multimedia list.
        protected void AddMultimedia(GEDCOMList<GEDCOMMultimediaLink> multimediaLinks, string mmPrefix, string mmLargePrefix, uint maxWidth, uint maxHeight, Stats stats)
        {
            // TODO: ml.GetFileReferences();
            var fileRefs = new List<GEDCOMFileReference>();
            foreach (var mmLink in multimediaLinks) {
                if (mmLink.IsPointer) {
                    var mmRec = mmLink.Value as GEDCOMMultimediaRecord;
                    if (!mmRec.GetVisibility()) {
                        // user chose not to show this picture
                        continue;
                    }

                    foreach (var fileRef in mmRec.FileReferences) {
                        fileRefs.Add(fileRef);
                    }
                } else {
                    foreach (var fileRef in mmLink.FileReferences) {
                        fileRefs.Add(fileRef);
                    }
                }
            }

            if (multimediaLinks != null) {
                // Add extra pics added by the user on indi exclude screen.
                AddMultimediaFileReferences(fileRefs, mmPrefix, mmLargePrefix, maxWidth, maxHeight, stats);
            }
        }

        // Adds an HTML page footer (Record date, W3C sticker, GEDmill credit etc.)
        protected void OutputFooter(HTMLFile f, GEDCOMRecord r)
        {
            f.Writer.WriteLine("      <div id=\"footer\">");
            if ((r.UserReferences.Count > 0)
              || (!string.IsNullOrEmpty(r.AutomatedRecordID))
              || (r.ChangeDate != null)
              || (MainForm.Config.CustomFooter != "")) {
                foreach (GEDCOMUserReference urn in r.UserReferences) {
                    string idType = EscapeHTML(urn.ReferenceType, false);
                    if (idType == "") {
                        idType = "User reference number";
                    }
                    f.Writer.WriteLine(String.Concat("        <p>", idType, ": ", EscapeHTML(urn.StringValue, false), "</p>"));
                }

                if (!string.IsNullOrEmpty(r.AutomatedRecordID)) {
                    f.Writer.WriteLine(String.Concat("        <p>Record ", r.AutomatedRecordID, "</p>"));
                }
                if (r.ChangeDate != null) {
                    GEDCOMChangeDate changeDate = r.ChangeDate;
                    string dtx = r.ChangeDate.ToString();
                    if (changeDate != null && dtx != null) {
                        if (dtx != "") {
                            dtx = " " + dtx;
                        }
                        f.Writer.WriteLine(String.Concat("        <p id=\"changedate\">Record last changed ", dtx.ToString(), "</p>"));
                    }
                }
                if (MainForm.Config.CustomFooter != "") {
                    if (MainForm.Config.FooterIsHtml) {
                        f.Writer.WriteLine(String.Concat("        <p>", MainForm.Config.CustomFooter, "</p>"));
                    } else {
                        f.Writer.WriteLine(String.Concat("        <p>", EscapeHTML(MainForm.Config.CustomFooter, false), "</p>"));
                    }
                }
            }
            f.Writer.WriteLine("      </div> <!-- footer -->");

            f.Writer.WriteLine("<p class=\"plain\">Page created using GEDmill " + CConfig.SoftwareVersion + "</p>");

            if (MainForm.Config.IncludeValiditySticker) {
                OutputValiditySticker(f);
            }
        }

        // Adds the given list of file references to the multimedia list.
        private void AddMultimediaFileReferences(List<GEDCOMFileReference> fileRefs, string mmPrefix, string mmLargePrefix, uint maxWidth, uint maxHeight, Stats stats)
        {
            if (fileRefs == null) {
                return;
            }

            for (int i = 0; i < fileRefs.Count; i++) {
                GEDCOMFileReferenceWithTitle mfr = fileRefs[i] as GEDCOMFileReferenceWithTitle;
                string sCopyFilename = "";
                int nMmOrdering = i;
                string sMmTitle = mfr.Title;
                string sMmFilename = mfr.StringValue;
                string sMmFormat = mfr.MultimediaFormat.ToString();
                Rectangle rectArea = new Rectangle(0, 0, 0, 0);
                string sExtnPart;
                bool bBlockThisMediaType = false;

                // Don't trust extension on sFilename. Use our own. (Happens for .tmp files from embedded data)
                switch (sMmFormat) {
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
                        sExtnPart = Path.GetExtension(sMmFilename);
                        if (sExtnPart.ToUpper() == ".TMP") {
                            sExtnPart = "." + sMmFormat;
                        }
                        break;
                }
                string sOriginalFilename = Path.GetFileName(sMmFilename);

                bool bPictureFormat = mfr.IsPictureFormat();
                if (bPictureFormat || MainForm.Config.AllowNonPictures) {
                    if (!bPictureFormat && MainForm.Config.AllowNonPictures) {
                        stats.NonPicturesIncluded = true;
                    }

                    string sNewFilename = sOriginalFilename;
                    if (!string.IsNullOrEmpty(sMmFilename)) {
                        // Give multimedia files a standard name
                        if (MainForm.Config.RenameOriginalPicture) {
                            //string sFilePart = sMmPrefix;//String.Concat( mm_prefix, nMultimediaFiles.ToString() );
                            sNewFilename = String.Concat(mmPrefix, sExtnPart.ToLower());
                        }

                        if (!bBlockThisMediaType) {
                            if (bPictureFormat) {
                                // TODO
                                /*if (mfr.m_asidPair != null) {
                                    Rectangle rectAsidArea = mfr.m_asidPair.m_rectArea;
                                    rectArea = new Rectangle(rectAsidArea.X, rectAsidArea.Y, rectAsidArea.Width, rectAsidArea.Height);
                                }*/
                                sCopyFilename = CopyMultimedia(sMmFilename, sNewFilename, maxWidth, maxHeight, ref rectArea, stats);
                            } else {
                                sCopyFilename = CopyMultimedia(sMmFilename, sNewFilename, 0, 0, ref rectArea, stats);
                            }
                        }
                    }

                    if (!string.IsNullOrEmpty(sCopyFilename)) {
                        string sLargeFilename = "";
                        // Copy original original version
                        if (MainForm.Config.LinkOriginalPicture) {
                            if (MainForm.Config.RenameOriginalPicture) {
                                //string sFilePart = sMmLargePrefix;
                                sLargeFilename = String.Concat(mmLargePrefix, sExtnPart.ToLower());
                            } else {
                                sLargeFilename = sOriginalFilename;
                            }

                            Rectangle rectLargeArea = new Rectangle(0, 0, 0, 0);
                            sLargeFilename = CopyMultimedia(sMmFilename, sLargeFilename, 0, 0, ref rectLargeArea, null);
                        }

                        // Add format and new sFilename to multimedia list
                        Multimedia imm = new Multimedia(nMmOrdering, sMmFormat, sMmTitle, sCopyFilename, sLargeFilename, rectArea.Width, rectArea.Height);
                        fMultimediaList.Add(imm);
                    } else {
                        // Happens e.g. when original file doesn't exist.
                    }
                }
            }
        }
    }
}
