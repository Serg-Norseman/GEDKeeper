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
            string sFilename = String.Concat(MainForm.Config.OutputFolder, "\\sour", fSourceRecord.XRef);
            string sFullFilename = String.Concat(sFilename, ".", MainForm.Config.HtmlExtension);

            HTMLFile f = null;
            try {
                // Create a new file with an HTML header.    
                f = new HTMLFile(sFullFilename, fSourceRecord.ShortTitle, sPageDescription, sKeywords);

                // Create a navbar to main site, front page etc.
                OutputPageHeader(f.Writer, "", "", true, true);

                f.Writer.WriteLine("    <div class=\"hr\"></div>");
                f.Writer.WriteLine("");
                f.Writer.WriteLine("    <div id=\"page\"> <!-- page -->");

                // Write the page's title text.
                f.Writer.WriteLine("      <div id=\"main\">");
                f.Writer.WriteLine("        <div id=\"summary\">");
                f.Writer.WriteLine("          <div id=\"names\">");
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
                f.Writer.WriteLine(String.Concat("            <h1>", EscapeHTML(sName, false), "</h1>"));

                // Add repository information
                foreach (GEDCOMRepositoryCitation src in fSourceRecord.RepositoryCitations) {
                    GEDCOMRepositoryRecord rr = src.Value as GEDCOMRepositoryRecord;
                    if (rr != null) {
                        if (!string.IsNullOrEmpty(rr.RepositoryName)) {
                            f.Writer.WriteLine(String.Concat("            <h2>", EscapeHTML(rr.RepositoryName, false), "</h2>"));
                        }

                        foreach (GEDCOMNotes ns in rr.Notes) {
                            string noteText;
                            if (MainForm.Config.ObfuscateEmails) {
                                noteText = ObfuscateEmail(ns.Notes.Text);
                            } else {
                                noteText = ns.Notes.Text;
                            }
                            f.Writer.WriteLine(String.Concat("            <p>", EscapeHTML(noteText, false), "</p>"));
                        }
                    }

                    if (src.Notes != null && src.Notes.Count > 0) {
                        foreach (GEDCOMNotes ns in src.Notes) {
                            string noteText;
                            if (MainForm.Config.ObfuscateEmails) {
                                noteText = ObfuscateEmail(ns.Notes.Text);
                            } else {
                                noteText = ns.Notes.Text;
                            }

                            f.Writer.WriteLine(String.Concat("            <p>", EscapeHTML(noteText, false), "</p>"));
                        }
                    }
                }

                // Add Publication Information
                string sPubFacts;
                if (MainForm.Config.ObfuscateEmails) {
                    sPubFacts = ObfuscateEmail(fSourceRecord.Publication.Text);
                } else {
                    sPubFacts = fSourceRecord.Publication.Text;
                }
                if (!string.IsNullOrEmpty(sPubFacts)) {
                    if (sPubFacts.Length > 7 && sPubFacts.ToUpper().Substring(0, 7) == "HTTP://") {
                        f.Writer.WriteLine(String.Concat("            <h2>", "<a href=\"", sPubFacts, "\">", EscapeHTML(sPubFacts, false), "</a>", "</h2>"));
                    } else {
                        f.Writer.WriteLine(String.Concat("            <h2>", EscapeHTML(sPubFacts, false), "</h2>"));
                    }
                }

                f.Writer.WriteLine("          </div> <!-- names -->");
                f.Writer.WriteLine("        </div> <!-- summary -->");

                // Collect together multimedia links.
                if (MainForm.Config.AllowMultimedia) {
                    // Fill m_alMultimediaList:
                    AddMultimedia(fSourceRecord.MultimediaLinks, String.Concat(fSourceRecord.XRef, "mms"), String.Concat(fSourceRecord.XRef, "mos"), MainForm.Config.MaxSourceImageWidth, MainForm.Config.MaxSourceImageHeight, stats);
                }

                // Add pics
                OutputMultimedia(f);

                // Add textFromSource
                string sCleanText = fSourceRecord.Text.Text;
                if (MainForm.Config.ObfuscateEmails) {
                    sCleanText = ObfuscateEmail(sCleanText);
                }
                if (!string.IsNullOrEmpty(sCleanText)) {
                    f.Writer.WriteLine("        <div id=\"text\">");
                    f.Writer.WriteLine("          <h1>Text</h1>");
                    f.Writer.WriteLine("          <p class=\"pretext\">");
                    f.Writer.WriteLine(EscapeHTML(sCleanText, false));
                    f.Writer.WriteLine("            </p>");
                    f.Writer.WriteLine("        </div> <!-- text -->");
                }

                // Add notes
                if (fSourceRecord.Notes.Count > 0) {
                    // Generate notes list into a local array before adding header title. This is to cope with the case where all notes are nothing but blanks.
                    var alNoteStrings = new List<string>(fSourceRecord.Notes.Count);

                    foreach (GEDCOMNotes ns in fSourceRecord.Notes) {
                        if (!string.IsNullOrEmpty(ns.Notes.Text)) {
                            string sNoteText;
                            if (MainForm.Config.ObfuscateEmails) {
                                sNoteText = ObfuscateEmail(ns.Notes.Text);
                            } else {
                                sNoteText = ns.Notes.Text;
                            }

                            string sourceRefs = "";
                            alNoteStrings.Add(String.Concat("          <li>", EscapeHTML(sNoteText, false), sourceRefs, "</li>"));
                        }
                    }

                    if (alNoteStrings.Count > 0) {
                        f.Writer.WriteLine("        <div id=\"notes\">");
                        f.Writer.WriteLine("          <h1>Notes</h1>");
                        f.Writer.WriteLine("          <ul>");

                        foreach (string note_string in alNoteStrings) {
                            f.Writer.WriteLine(note_string);
                        }

                        f.Writer.WriteLine("          </ul>");
                        f.Writer.WriteLine("        </div> <!-- notes -->");
                    }
                }

                if (MainForm.Config.SupressBackreferences == false) {
                    f.Writer.WriteLine("        <div id=\"citations\">");
                    f.Writer.WriteLine("          <h1>Citations</h1>");
                    f.Writer.WriteLine("          <ul>");

                    Hashtable htBackrefs = fSourceRecord.MakeBackReferences();
                    IDictionaryEnumerator enumerator = htBackrefs.GetEnumerator();
                    while (enumerator.MoveNext()) {
                        GEDCOMIndividualRecord ir = (GEDCOMIndividualRecord)(enumerator.Value);
                        if (ir != null && ir.GetVisibility()) {
                            string link = MakeLink(ir);
                            if (link != "") {
                                f.Writer.WriteLine(String.Concat("            <li>", link, "</li>"));
                            }
                        }
                    }

                    f.Writer.WriteLine("          </ul>");
                    f.Writer.WriteLine("        </div> <!-- citations -->");
                }

                f.Writer.WriteLine("      </div> <!-- main -->");

                // Add footer (Record date, W3C sticker, GEDmill credit etc.)
                OutputFooter(f, fSourceRecord);

                f.Writer.WriteLine("    </div> <!-- page -->");
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
                f.Writer.WriteLine("        <div id=\"sourcePics\">");
                foreach (Multimedia iMultimedia in fMultimediaList) {
                    sNonPicMainFilename = "multimedia/" + MainForm.NonPicFilename(iMultimedia.Format, false, MainForm.Config.LinkOriginalPicture);

                    string sImageTitle = "";
                    string sAltName = "";
                    if (iMultimedia.Title != null) {
                        sImageTitle = iMultimedia.Title;
                        sAltName = iMultimedia.Title;
                    }

                    f.Writer.WriteLine(String.Concat("          <p>"));

                    if (iMultimedia.Width != 0 && iMultimedia.Height != 0) {
                        // Must be a picture.
                        if (sAltName == "") {
                            sAltName = "Image for this source";
                        }
                        if (iMultimedia.LargeFileName.Length > 0) {
                            f.Writer.WriteLine(String.Concat("            <a href=\"", iMultimedia.LargeFileName, "\"><img src=\"", iMultimedia.FileName, "\" alt=\"", sAltName, "\" /></a>")); // TODO: clip and scale properly. Use MainForm.s_config to set a max scale
                        } else {
                            f.Writer.WriteLine(String.Concat("            <img src=\"", iMultimedia.FileName, "\" alt=\"", sAltName, "\" />")); // TODO: clip and scale properly. Use MainForm.s_config to set a max scale
                        }
                    } else {
                        // Other multimedia
                        if (sAltName == "") {
                            sAltName = "Media for this source";
                        }

                        if (MainForm.Config.LinkOriginalPicture) {
                            f.Writer.WriteLine(String.Concat("            <a href=\"", iMultimedia.FileName, "\"><img src=\"", sNonPicMainFilename, "\" alt=\"", sAltName, "\" /></a>")); // TODO: clip and scale properly. Use MainForm.s_config to set a max scale
                        } else {
                            f.Writer.WriteLine(String.Concat("            <img src=\"", sNonPicMainFilename, "\" alt=\"", sAltName, "\" />")); // TODO: clip and scale properly. Use MainForm.s_config to set a max scale
                        }
                    }
                    f.Writer.WriteLine(String.Concat("          </p>"));
                    if (sImageTitle != "") {
                        f.Writer.WriteLine(String.Concat("          <p id=\"sourcepic_title\">", sImageTitle, "</p>"));
                    }
                }
                f.Writer.WriteLine("        </div> <!-- sourcePics -->");
            }
        }
    }
}
