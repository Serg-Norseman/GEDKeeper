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
using System.Collections;
using System.Collections.Generic;
using System.IO;
using GEDmill.Model;
using GKCommon.GEDCOM;
using GKCore.Logging;

namespace GEDmill.HTML
{
    /// <summary>
    /// Creates the HTML for a source record page
    /// </summary>
    public class CreatorRecordSource : CreatorRecord
    {
        private static readonly ILogger fLogger = LogManager.GetLogger(CConfig.LOG_FILE, CConfig.LOG_LEVEL, typeof(CreatorRecordSource).Name);

        // The source record that we are creating the page for.
        private GEDCOMSourceRecord fSourceRecord;


        public CreatorRecordSource(GEDCOMTree gedcom, IProgressCallback progress, string w3cfile, GEDCOMSourceRecord sr) : base(gedcom, progress, w3cfile)
        {
            fSourceRecord = sr;
        }

        // The main method that causes the page to be created.
        public bool Create(Stats stats)
        {
            // Sanity check
            if (fSourceRecord == null) {
                return false;
            }

            // Create the strings to use for the HTML file.
            string sPageDescription = "GEDmill GEDCOM to HTML page for " + fSourceRecord.ShortTitle;
            string sKeywords = "family tree history " + fSourceRecord.ShortTitle;
            string sFilename = string.Concat(CConfig.Instance.OutputFolder, "\\sour", fSourceRecord.XRef);
            string sFullFilename = string.Concat(sFilename, ".", CConfig.Instance.HtmlExtension);

            HTMLFile f = null;
            try {
                // Create a new file with an HTML header.    
                f = new HTMLFile(sFullFilename, fSourceRecord.ShortTitle, sPageDescription, sKeywords);

                // Create a navbar to main site, front page etc.
                OutputPageHeader(f, "", "", true, true);

                f.WriteLine("    <div class=\"hr\"></div>");
                f.WriteLine("");
                f.WriteLine("    <div id=\"page\"> <!-- page -->");

                // Write the page's title text.
                f.WriteLine("      <div id=\"main\">");
                f.WriteLine("        <div id=\"summary\">");
                f.WriteLine("          <div id=\"names\">");
                string sName = fSourceRecord.ShortTitle;
                if (sName == "") {
                    sName = "Source ";
                    bool bGotSourceName = false;
                    // Try user reference number
                    foreach (GEDCOMUserReference urn in fSourceRecord.UserReferences) {
                        if (urn.StringValue != "") {
                            sName += urn.StringValue;
                            bGotSourceName = true;
                            break;
                        }
                    }
                    if (!bGotSourceName && fSourceRecord.AutomatedRecordID != null && fSourceRecord.AutomatedRecordID != "") {
                        sName += fSourceRecord.AutomatedRecordID;
                    } else if (!bGotSourceName) {
                        sName += fSourceRecord.XRef;
                    }
                }
                f.WriteLine(string.Concat("            <h1>", EscapeHTML(sName, false), "</h1>"));

                // Add repository information
                foreach (GEDCOMRepositoryCitation src in fSourceRecord.RepositoryCitations) {
                    GEDCOMRepositoryRecord rr = src.Value as GEDCOMRepositoryRecord;
                    if (rr != null) {
                        if (!string.IsNullOrEmpty(rr.RepositoryName)) {
                            f.WriteLine(string.Concat("            <h2>", EscapeHTML(rr.RepositoryName, false), "</h2>"));
                        }

                        foreach (GEDCOMNotes ns in rr.Notes) {
                            string noteText;
                            if (CConfig.Instance.ObfuscateEmails) {
                                noteText = ObfuscateEmail(ns.Notes.Text);
                            } else {
                                noteText = ns.Notes.Text;
                            }
                            f.WriteLine(string.Concat("            <p>", EscapeHTML(noteText, false), "</p>"));
                        }
                    }

                    if (src.Notes != null && src.Notes.Count > 0) {
                        foreach (GEDCOMNotes ns in src.Notes) {
                            string noteText;
                            if (CConfig.Instance.ObfuscateEmails) {
                                noteText = ObfuscateEmail(ns.Notes.Text);
                            } else {
                                noteText = ns.Notes.Text;
                            }

                            f.WriteLine(string.Concat("            <p>", EscapeHTML(noteText, false), "</p>"));
                        }
                    }
                }

                // Add Publication Information
                string sPubFacts;
                if (CConfig.Instance.ObfuscateEmails) {
                    sPubFacts = ObfuscateEmail(fSourceRecord.Publication.Text);
                } else {
                    sPubFacts = fSourceRecord.Publication.Text;
                }
                if (!string.IsNullOrEmpty(sPubFacts)) {
                    if (sPubFacts.Length > 7 && sPubFacts.ToUpper().Substring(0, 7) == "HTTP://") {
                        f.WriteLine(string.Concat("            <h2>", "<a href=\"", sPubFacts, "\">", EscapeHTML(sPubFacts, false), "</a>", "</h2>"));
                    } else {
                        f.WriteLine(string.Concat("            <h2>", EscapeHTML(sPubFacts, false), "</h2>"));
                    }
                }

                f.WriteLine("          </div> <!-- names -->");
                f.WriteLine("        </div> <!-- summary -->");

                // Collect together multimedia links.
                if (CConfig.Instance.AllowMultimedia) {
                    // Fill m_alMultimediaList:
                    AddMultimedia(fSourceRecord.MultimediaLinks, string.Concat(fSourceRecord.XRef, "mms"), string.Concat(fSourceRecord.XRef, "mos"), CConfig.Instance.MaxSourceImageWidth, CConfig.Instance.MaxSourceImageHeight, stats);
                }

                // Add pics
                OutputMultimedia(f);

                // Add textFromSource
                string sCleanText = fSourceRecord.Text.Text;
                if (CConfig.Instance.ObfuscateEmails) {
                    sCleanText = ObfuscateEmail(sCleanText);
                }
                if (!string.IsNullOrEmpty(sCleanText)) {
                    f.WriteLine("        <div id=\"text\">");
                    f.WriteLine("          <h1>Text</h1>");
                    f.WriteLine("          <p class=\"pretext\">");
                    f.WriteLine(EscapeHTML(sCleanText, false));
                    f.WriteLine("            </p>");
                    f.WriteLine("        </div> <!-- text -->");
                }

                // Add notes
                if (fSourceRecord.Notes.Count > 0) {
                    // Generate notes list into a local array before adding header title. This is to cope with the case where all notes are nothing but blanks.
                    var alNoteStrings = new List<string>(fSourceRecord.Notes.Count);

                    foreach (GEDCOMNotes ns in fSourceRecord.Notes) {
                        if (!string.IsNullOrEmpty(ns.Notes.Text)) {
                            string sNoteText;
                            if (CConfig.Instance.ObfuscateEmails) {
                                sNoteText = ObfuscateEmail(ns.Notes.Text);
                            } else {
                                sNoteText = ns.Notes.Text;
                            }

                            string sourceRefs = "";
                            alNoteStrings.Add(string.Concat("          <li>", EscapeHTML(sNoteText, false), sourceRefs, "</li>"));
                        }
                    }

                    if (alNoteStrings.Count > 0) {
                        f.WriteLine("        <div id=\"notes\">");
                        f.WriteLine("          <h1>Notes</h1>");
                        f.WriteLine("          <ul>");

                        foreach (string note_string in alNoteStrings) {
                            f.WriteLine(note_string);
                        }

                        f.WriteLine("          </ul>");
                        f.WriteLine("        </div> <!-- notes -->");
                    }
                }

                if (CConfig.Instance.SupressBackreferences == false) {
                    f.WriteLine("        <div id=\"citations\">");
                    f.WriteLine("          <h1>Citations</h1>");
                    f.WriteLine("          <ul>");

                    Hashtable htBackrefs = fSourceRecord.MakeBackReferences();
                    IDictionaryEnumerator enumerator = htBackrefs.GetEnumerator();
                    while (enumerator.MoveNext()) {
                        GEDCOMIndividualRecord ir = (GEDCOMIndividualRecord)(enumerator.Value);
                        if (ir != null && ir.GetVisibility()) {
                            string link = MakeLink(ir);
                            if (link != "") {
                                f.WriteLine(string.Concat("            <li>", link, "</li>"));
                            }
                        }
                    }

                    f.WriteLine("          </ul>");
                    f.WriteLine("        </div> <!-- citations -->");
                }

                f.WriteLine("      </div> <!-- main -->");

                // Add footer (Record date, W3C sticker, GEDmill credit etc.)
                OutputFooter(f, fSourceRecord);

                f.WriteLine("    </div> <!-- page -->");
            } catch (IOException e) {
                fLogger.WriteError("Caught IO Exception(6) : ", e);
            } catch (ArgumentException e) {
                fLogger.WriteError("Caught Argument Exception(6) : ", e);
            } finally {
                if (f != null) {
                    f.Close(); // Adds standard footer to the file
                }
            }
            return true;
        }

        // Writes the HTML for the multimedia files associated with this record. 
        private void OutputMultimedia(HTMLFile f)
        {
            string sNonPicMainFilename = "";
            if (fMultimediaList.Count > 0) {
                f.WriteLine("        <div id=\"sourcePics\">");
                foreach (Multimedia iMultimedia in fMultimediaList) {
                    sNonPicMainFilename = "multimedia/" + GMHelper.NonPicFilename(iMultimedia.Format, false, CConfig.Instance.LinkOriginalPicture);

                    string sImageTitle = "";
                    string sAltName = "";
                    if (iMultimedia.Title != null) {
                        sImageTitle = iMultimedia.Title;
                        sAltName = iMultimedia.Title;
                    }

                    f.WriteLine(string.Concat("          <p>"));

                    if (iMultimedia.Width != 0 && iMultimedia.Height != 0) {
                        // Must be a picture.
                        if (sAltName == "") {
                            sAltName = "Image for this source";
                        }
                        if (iMultimedia.LargeFileName.Length > 0) {
                            f.WriteLine(string.Concat("            <a href=\"", iMultimedia.LargeFileName, "\"><img src=\"", iMultimedia.FileName, "\" alt=\"", sAltName, "\" /></a>")); // TODO: clip and scale properly. Use MainForm.s_config to set a max scale
                        } else {
                            f.WriteLine(string.Concat("            <img src=\"", iMultimedia.FileName, "\" alt=\"", sAltName, "\" />")); // TODO: clip and scale properly. Use MainForm.s_config to set a max scale
                        }
                    } else {
                        // Other multimedia
                        if (sAltName == "") {
                            sAltName = "Media for this source";
                        }

                        if (CConfig.Instance.LinkOriginalPicture) {
                            f.WriteLine(string.Concat("            <a href=\"", iMultimedia.FileName, "\"><img src=\"", sNonPicMainFilename, "\" alt=\"", sAltName, "\" /></a>")); // TODO: clip and scale properly. Use MainForm.s_config to set a max scale
                        } else {
                            f.WriteLine(string.Concat("            <img src=\"", sNonPicMainFilename, "\" alt=\"", sAltName, "\" />")); // TODO: clip and scale properly. Use MainForm.s_config to set a max scale
                        }
                    }
                    f.WriteLine(string.Concat("          </p>"));
                    if (sImageTitle != "") {
                        f.WriteLine(string.Concat("          <p id=\"sourcepic_title\">", sImageTitle, "</p>"));
                    }
                }
                f.WriteLine("        </div> <!-- sourcePics -->");
            }
        }
    }
}
