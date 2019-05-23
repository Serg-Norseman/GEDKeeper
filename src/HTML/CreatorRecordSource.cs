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
using GDModel;
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
        private GDMSourceRecord fSourceRecord;


        public CreatorRecordSource(GDMTree gedcom, IProgressCallback progress, string w3cfile, GDMSourceRecord sr) : base(gedcom, progress, w3cfile)
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
            string pageDescription = "GEDmill GEDCOM to HTML page for " + fSourceRecord.ShortTitle;
            string keywords = "family tree history " + fSourceRecord.ShortTitle;
            string filename = string.Concat(CConfig.Instance.OutputFolder, "\\sour", fSourceRecord.XRef);
            string fullFilename = string.Concat(filename, ".", CConfig.Instance.HtmlExtension);

            HTMLFile f = null;
            try {
                // Create a new file with an HTML header.    
                f = new HTMLFile(fullFilename, fSourceRecord.ShortTitle, pageDescription, keywords);

                // Create a navbar to main site, front page etc.
                OutputPageHeader(f, "", "", true, true);

                f.WriteLine("    <div class=\"hr\" />");
                f.WriteLine("");
                f.WriteLine("    <div id=\"page\"> <!-- page -->");

                // Write the page's title text.
                f.WriteLine("      <div id=\"main\">");
                f.WriteLine("        <div id=\"summary\">");
                f.WriteLine("          <div id=\"names\">");
                string sourName = fSourceRecord.ShortTitle;
                if (sourName == "") {
                    sourName = "Source ";
                    bool gotSourceName = false;
                    // Try user reference number
                    foreach (GDMUserReference urn in fSourceRecord.UserReferences) {
                        if (urn.StringValue != "") {
                            sourName += urn.StringValue;
                            gotSourceName = true;
                            break;
                        }
                    }
                    if (!gotSourceName && fSourceRecord.AutomatedRecordID != null && fSourceRecord.AutomatedRecordID != "") {
                        sourName += fSourceRecord.AutomatedRecordID;
                    } else if (!gotSourceName) {
                        sourName += fSourceRecord.XRef;
                    }
                }
                f.WriteLine("<h1>{0}</h1>", EscapeHTML(sourName, false));

                // Add repository information
                foreach (GDMRepositoryCitation src in fSourceRecord.RepositoryCitations) {
                    GDMRepositoryRecord rr = src.Value as GDMRepositoryRecord;
                    if (rr != null) {
                        if (!string.IsNullOrEmpty(rr.RepositoryName)) {
                            f.WriteLine("<h2>{0}</h2>", EscapeHTML(rr.RepositoryName, false));
                        }

                        foreach (GDMNotes ns in rr.Notes) {
                            string noteText;
                            if (CConfig.Instance.ObfuscateEmails) {
                                noteText = ObfuscateEmail(ns.Notes.Text);
                            } else {
                                noteText = ns.Notes.Text;
                            }
                            f.WriteLine("<p>{0}</p>", EscapeHTML(noteText, false));
                        }
                    }

                    if (src.Notes != null && src.Notes.Count > 0) {
                        foreach (GDMNotes ns in src.Notes) {
                            string noteText;
                            if (CConfig.Instance.ObfuscateEmails) {
                                noteText = ObfuscateEmail(ns.Notes.Text);
                            } else {
                                noteText = ns.Notes.Text;
                            }
                            f.WriteLine("<p>{0}</p>", EscapeHTML(noteText, false));
                        }
                    }
                }

                // Add Publication Information
                string pubFacts;
                if (CConfig.Instance.ObfuscateEmails) {
                    pubFacts = ObfuscateEmail(fSourceRecord.Publication.Text);
                } else {
                    pubFacts = fSourceRecord.Publication.Text;
                }
                if (!string.IsNullOrEmpty(pubFacts)) {
                    if (pubFacts.Length > 7 && pubFacts.ToUpper().Substring(0, 7) == "HTTP://") {
                        f.WriteLine("<h2><a href=\"{0}\">{1}</a></h2>", pubFacts, EscapeHTML(pubFacts, false));
                    } else {
                        f.WriteLine("<h2>{0}</h2>", EscapeHTML(pubFacts, false));
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
                string cleanText = fSourceRecord.Text.Text;
                if (CConfig.Instance.ObfuscateEmails) {
                    cleanText = ObfuscateEmail(cleanText);
                }
                if (!string.IsNullOrEmpty(cleanText)) {
                    f.WriteLine("        <div id=\"text\">");
                    f.WriteLine("          <h1>Text</h1>");
                    f.WriteLine("          <p class=\"pretext\">");
                    f.WriteLine(EscapeHTML(cleanText, false));
                    f.WriteLine("            </p>");
                    f.WriteLine("        </div> <!-- text -->");
                }

                // Add notes
                OutputNotes(f, fSourceRecord.Notes);

                if (CConfig.Instance.SupressBackreferences == false) {
                    f.WriteLine("        <div id=\"citations\">");
                    f.WriteLine("          <h1>Citations</h1>");
                    f.WriteLine("          <ul>");

                    var htBackrefs = fSourceRecord.MakeBackReferences();
                    IDictionaryEnumerator enumerator = htBackrefs.GetEnumerator();
                    while (enumerator.MoveNext()) {
                        GDMIndividualRecord ir = (GDMIndividualRecord)(enumerator.Value);
                        if (ir != null && ir.GetVisibility()) {
                            string link = MakeLink(ir);
                            if (link != "") {
                                f.WriteLine("<li>{0}</li>", link);
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
            string nonPicMainFilename = "";
            if (fMultimediaList.Count > 0) {
                f.WriteLine("        <div id=\"sourcePics\">");
                foreach (Multimedia iMultimedia in fMultimediaList) {
                    nonPicMainFilename = "multimedia/" + GMHelper.NonPicFilename(iMultimedia.Format, false, CConfig.Instance.LinkOriginalPicture);

                    string imageTitle = "";
                    string altName = "";

                    if (iMultimedia.Title != null) {
                        imageTitle = iMultimedia.Title;
                        altName = iMultimedia.Title;
                    }

                    f.WriteLine(string.Concat("          <p>"));

                    if (iMultimedia.Width != 0 && iMultimedia.Height != 0) {
                        // Must be a picture.
                        if (altName == "") {
                            altName = "Image for this source";
                        }
                        if (iMultimedia.LargeFileName.Length > 0) {
                            f.WriteLine(string.Concat("            <a href=\"", iMultimedia.LargeFileName, "\"><img src=\"", iMultimedia.FileName, "\" alt=\"", altName, "\" /></a>")); // TODO: clip and scale properly. Use MainForm.s_config to set a max scale
                        } else {
                            f.WriteLine(string.Concat("            <img src=\"", iMultimedia.FileName, "\" alt=\"", altName, "\" />")); // TODO: clip and scale properly. Use MainForm.s_config to set a max scale
                        }
                    } else {
                        // Other multimedia
                        if (altName == "") {
                            altName = "Media for this source";
                        }

                        if (CConfig.Instance.LinkOriginalPicture) {
                            f.WriteLine(string.Concat("            <a href=\"", iMultimedia.FileName, "\"><img src=\"", nonPicMainFilename, "\" alt=\"", altName, "\" /></a>")); // TODO: clip and scale properly. Use MainForm.s_config to set a max scale
                        } else {
                            f.WriteLine(string.Concat("            <img src=\"", nonPicMainFilename, "\" alt=\"", altName, "\" />")); // TODO: clip and scale properly. Use MainForm.s_config to set a max scale
                        }
                    }
                    f.WriteLine(string.Concat("          </p>"));
                    if (imageTitle != "") {
                        f.WriteLine("<p id=\"sourcepic_title\">{0}</p>", imageTitle);
                    }
                }
                f.WriteLine("        </div> <!-- sourcePics -->");
            }
        }
    }
}
