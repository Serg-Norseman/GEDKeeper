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
using System.Drawing;
using System.IO;
using GDModel;
using GEDmill.Model;

namespace GEDmill.HTML
{
    /// <summary>
    /// Base class for the creators of individual and source pages
    /// </summary>
    public class CreatorRecord : Creator
    {
        // All the images and other multimedia files associated with this record.
        protected List<Multimedia> fMultimediaList;


        protected CreatorRecord(GDMTree tree, IProgressCallback progress, string w3cfile) : base(tree, progress, w3cfile)
        {
            fMultimediaList = new List<Multimedia>();
        }

        // Adds the given multimedia links to the given multimedia list.
        protected void AddMultimedia(GDMList<GDMMultimediaLink> multimediaLinks, string mmPrefix,
                                     string mmLargePrefix, uint maxWidth, uint maxHeight, Stats stats)
        {
            // TODO: ml.GetFileReferences();
            var fileRefs = new List<GDMFileReference>();
            foreach (var mmLink in multimediaLinks) {
                if (mmLink.IsPointer) {
                    var mmRec = mmLink.Value as GDMMultimediaRecord;
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
        protected void OutputFooter(HTMLFile f, GDMRecord r)
        {
            f.WriteLine("      <div id=\"footer\">");
            if ((r.UserReferences.Count > 0)
              || (!string.IsNullOrEmpty(r.AutomatedRecordID))
              || (r.ChangeDate != null)
              || (CConfig.Instance.CustomFooter != "")) {
                foreach (GDMUserReference urn in r.UserReferences) {
                    string idType = EscapeHTML(urn.ReferenceType, false);
                    if (idType == "") {
                        idType = "User reference number";
                    }
                    f.WriteLine("<p>{0}: {1}</p>", idType, EscapeHTML(urn.StringValue, false));
                }

                if (!string.IsNullOrEmpty(r.AutomatedRecordID)) {
                    f.WriteLine("<p>Record {0}</p>", r.AutomatedRecordID);
                }

                if (r.ChangeDate != null) {
                    GDMChangeDate changeDate = r.ChangeDate;
                    string dtx = r.ChangeDate.ToString();
                    if (changeDate != null && dtx != null) {
                        if (dtx != "") {
                            dtx = " " + dtx;
                        }
                        f.WriteLine("<p id=\"changedate\">Record last changed {0}</p>", dtx);
                    }
                }

                if (CConfig.Instance.CustomFooter != "") {
                    if (CConfig.Instance.FooterIsHtml) {
                        f.WriteLine("<p>{0}</p>", CConfig.Instance.CustomFooter);
                    } else {
                        f.WriteLine("<p>{0}</p>", EscapeHTML(CConfig.Instance.CustomFooter, false));
                    }
                }
            }
            f.WriteLine("      </div> <!-- footer -->");

            f.WriteLine("<p class=\"plain\">Page created using GEDmill {0}</p>", CConfig.SoftwareVersion);

            if (CConfig.Instance.IncludeValiditySticker) {
                OutputValiditySticker(f);
            }
        }

        // Adds the given list of file references to the multimedia list.
        private void AddMultimediaFileReferences(List<GDMFileReference> fileRefs, string mmPrefix,
                                                 string mmLargePrefix, uint maxWidth, uint maxHeight, Stats stats)
        {
            if (fileRefs == null) {
                return;
            }

            for (int i = 0; i < fileRefs.Count; i++) {
                GDMFileReferenceWithTitle mfr = fileRefs[i] as GDMFileReferenceWithTitle;
                string copyFilename = "";
                int nMmOrdering = i;
                string mmTitle = mfr.Title;
                string mmFilename = mfr.StringValue;
                string mmFormat = mfr.MultimediaFormat.ToString();
                Rectangle rectArea = new Rectangle(0, 0, 0, 0);
                string extPart;
                bool blockThisMediaType = false;

                // Don't trust extension on sFilename. Use our own. (Happens for .tmp files from embedded data)
                switch (mmFormat) {
                    case "bmp":
                        extPart = ".bmp";
                        break;
                    case "gif":
                        extPart = ".gif";
                        break;
                    case "jpg":
                    case "jpeg":
                        extPart = ".jpg";
                        break;
                    case "tiff":
                    case "tif":
                        extPart = ".tif";
                        break;
                    case "png":
                        extPart = ".png";
                        break;
                    case "ole":
                        blockThisMediaType = true;
                        extPart = ".ole";
                        break;
                    default:
                        extPart = Path.GetExtension(mmFilename);
                        if (extPart.ToUpper() == ".TMP") {
                            extPart = "." + mmFormat;
                        }
                        break;
                }
                string originalFilename = Path.GetFileName(mmFilename);

                bool pictureFormat = mfr.IsPictureFormat();
                if (pictureFormat || CConfig.Instance.AllowNonPictures) {
                    if (!pictureFormat && CConfig.Instance.AllowNonPictures) {
                        stats.NonPicturesIncluded = true;
                    }

                    string newFilename = originalFilename;
                    if (!string.IsNullOrEmpty(mmFilename)) {
                        // Give multimedia files a standard name
                        if (CConfig.Instance.RenameOriginalPicture) {
                            //string sFilePart = sMmPrefix;//string.Concat( mm_prefix, nMultimediaFiles.ToString() );
                            newFilename = string.Concat(mmPrefix, extPart.ToLower());
                        }

                        if (!blockThisMediaType) {
                            if (pictureFormat) {
                                // TODO
                                /*if (mfr.m_asidPair != null) {
                                    Rectangle rectAsidArea = mfr.m_asidPair.m_rectArea;
                                    rectArea = new Rectangle(rectAsidArea.X, rectAsidArea.Y, rectAsidArea.Width, rectAsidArea.Height);
                                }*/
                                copyFilename = CopyMultimedia(mmFilename, newFilename, maxWidth, maxHeight, ref rectArea, stats);
                            } else {
                                copyFilename = CopyMultimedia(mmFilename, newFilename, 0, 0, ref rectArea, stats);
                            }
                        }
                    }

                    if (!string.IsNullOrEmpty(copyFilename)) {
                        string largeFilename = "";
                        // Copy original original version
                        if (CConfig.Instance.LinkOriginalPicture) {
                            if (CConfig.Instance.RenameOriginalPicture) {
                                //string sFilePart = sMmLargePrefix;
                                largeFilename = string.Concat(mmLargePrefix, extPart.ToLower());
                            } else {
                                largeFilename = originalFilename;
                            }

                            Rectangle rectLargeArea = new Rectangle(0, 0, 0, 0);
                            largeFilename = CopyMultimedia(mmFilename, largeFilename, 0, 0, ref rectLargeArea, null);
                        }

                        // Add format and new sFilename to multimedia list
                        Multimedia imm = new Multimedia(nMmOrdering, mmFormat, mmTitle, copyFilename, largeFilename, rectArea.Width, rectArea.Height);
                        fMultimediaList.Add(imm);
                    } else {
                        // Happens e.g. when original file doesn't exist.
                    }
                }
            }
        }

        // Outputs the HTML for the Notes section of the page
        protected static void OutputNotes(HTMLFile f, GDMList<GDMNotes> notes)
        {
            if (notes.Count > 0) {
                // Generate notes list into a local array before adding header title. This is to cope with the case where all notes are nothing but blanks.
                var note_strings = new List<string>(notes.Count);

                foreach (GDMNotes ns in notes) {
                    string noteText = CConfig.Instance.ObfuscateEmails ? ObfuscateEmail(ns.Lines.Text) : ns.Lines.Text;
                    note_strings.Add(string.Concat("<li>", EscapeHTML(noteText, false), "</li>"));
                }

                if (note_strings.Count > 0) {
                    f.WriteLine("<div id=\"notes\">");
                    f.WriteLine("<h1>Notes</h1>");
                    f.WriteLine("<ul>");

                    foreach (string note_string in note_strings) {
                        f.WriteLine(note_string);
                    }

                    f.WriteLine("</ul>");
                    f.WriteLine("</div> <!-- notes -->");
                }
            }
        }
    }
}
