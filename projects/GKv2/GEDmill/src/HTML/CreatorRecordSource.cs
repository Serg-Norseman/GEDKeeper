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
using System.Collections;
using System.IO;
using GDModel;
using GEDmill.Model;
using GKCore.Interfaces;
using GKCore.Logging;

namespace GEDmill.HTML
{
    /// <summary>
    /// Creates the HTML for a source record page
    /// </summary>
    public class CreatorRecordSource : CreatorRecord
    {
        private static readonly ILogger fLogger = LogManager.GetLogger(GMConfig.LOG_FILE, GMConfig.LOG_LEVEL, typeof(CreatorRecordSource).Name);

        // The source record that we are creating the page for.
        private GDMSourceRecord fSourceRecord;


        public CreatorRecordSource(GDMTree tree, IProgressCallback progress, ILangMan langMan, GDMSourceRecord sr) : base(tree, progress, langMan)
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
            string pageDescription = fLangMan.LS(PLS.LSID_PageDescription) + " " + fSourceRecord.ShortTitle;
            string keywords = fLangMan.LS(PLS.LSID_Keywords) + " " + fSourceRecord.ShortTitle;
            string filename = string.Concat(GMConfig.Instance.OutputFolder, "\\sour", fSourceRecord.XRef);
            string fullFilename = string.Concat(filename, ".html");

            HTMLFile f = null;
            try {
                // Create a new file with an HTML header.    
                f = new HTMLFile(fLangMan, fullFilename, fSourceRecord.ShortTitle, pageDescription, keywords);

                // Create a navbar to main site, front page etc.
                OutputPageHeader(f, "", "", true);

                f.WriteLine("    <div class=\"hr\" />");
                f.WriteLine("");
                f.WriteLine("    <div id=\"page\"> <!-- page -->");

                // Write the page's title text.
                f.WriteLine("      <div id=\"main\">");
                f.WriteLine("        <div id=\"summary\">");
                f.WriteLine("          <div id=\"names\">");
                string sourName = fSourceRecord.ShortTitle;
                if (sourName == "") {
                    sourName = fLangMan.LS(PLS.LSID_Source) + " ";
                    bool gotSourceName = false;
                    // Try user reference number
                    foreach (var userRef in fSourceRecord.UserReferences) {
                        if (userRef.StringValue != "") {
                            sourName += userRef.StringValue;
                            gotSourceName = true;
                            break;
                        }
                    }
                    if (!gotSourceName && !string.IsNullOrEmpty(fSourceRecord.AutomatedRecordID)) {
                        sourName += fSourceRecord.AutomatedRecordID;
                    } else if (!gotSourceName) {
                        sourName += fSourceRecord.XRef;
                    }
                }
                f.WriteLine("<h1>{0}</h1>", EscapeHTML(sourName, false));

                // Add repository information
                foreach (var sourCit in fSourceRecord.RepositoryCitations) {
                    GDMRepositoryRecord repoRec = fTree.GetPtrValue<GDMRepositoryRecord>(sourCit);
                    if (repoRec != null) {
                        if (!string.IsNullOrEmpty(repoRec.RepositoryName)) {
                            f.WriteLine("<h2>{0}</h2>", EscapeHTML(repoRec.RepositoryName, false));
                        }

                        foreach (GDMNotes ns in repoRec.Notes) {
                            WriteNotes(f, ns);
                        }
                    }

                    foreach (GDMNotes ns in sourCit.Notes) {
                        WriteNotes(f, ns);
                    }
                }

                // Add Publication Information
                string pubFacts;
                if (GMConfig.Instance.ObfuscateEmails) {
                    pubFacts = ObfuscateEmail(fSourceRecord.Publication.Lines.Text);
                } else {
                    pubFacts = fSourceRecord.Publication.Lines.Text;
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

                // Collect together multimedia links
                if (GMConfig.Instance.AllowMultimedia && fSourceRecord.HasMultimediaLinks) {
                    AddMultimedia(fSourceRecord.MultimediaLinks, string.Concat(fSourceRecord.XRef, "mms"), string.Concat(fSourceRecord.XRef, "mos"), 
                                  GMConfig.Instance.MaxSourceImageWidth, GMConfig.Instance.MaxSourceImageHeight, stats);
                }

                // Add pics
                OutputMultimedia(f);

                // Add textFromSource
                string cleanText = fSourceRecord.Text.Lines.Text;
                if (GMConfig.Instance.ObfuscateEmails) {
                    cleanText = ObfuscateEmail(cleanText);
                }
                if (!string.IsNullOrEmpty(cleanText)) {
                    f.WriteLine("        <div id=\"text\">");
                    f.WriteLine("          <h1>{0}</h1>", fLangMan.LS(PLS.LSID_Text));
                    f.WriteLine("          <p class=\"pretext\">");
                    f.WriteLine(EscapeHTML(cleanText, false));
                    f.WriteLine("            </p>");
                    f.WriteLine("        </div> <!-- text -->");
                }

                // Add notes
                OutputNotes(f, fSourceRecord.Notes);

                if (!GMConfig.Instance.SupressBackreferences) {
                    f.WriteLine("        <div id=\"citations\">");
                    f.WriteLine("          <h1>{0}</h1>", fLangMan.LS(PLS.LSID_Citations));
                    f.WriteLine("          <ul>");

                    var htBackrefs = GMHelper.MakeBackReferences(fSourceRecord);
                    IDictionaryEnumerator enumerator = htBackrefs.GetEnumerator();
                    while (enumerator.MoveNext()) {
                        GDMIndividualRecord ir = (GDMIndividualRecord)(enumerator.Value);
                        if (ir != null && GMHelper.GetVisibility(ir)) {
                            string link = MakeLink(ir);
                            if (!string.IsNullOrEmpty(link)) {
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
            if (fMultimediaList.Count > 0) {
                f.WriteLine("        <div id=\"sourcePics\">");
                foreach (Multimedia iMultimedia in fMultimediaList) {
                    string nonPicMainFilename = "multimedia/" + GMHelper.NonPicFilename(iMultimedia.Format, false, GMConfig.Instance.LinkOriginalPicture);

                    string imageTitle = "";
                    string altName = "";

                    if (iMultimedia.Title != null) {
                        imageTitle = iMultimedia.Title;
                        altName = iMultimedia.Title;
                    }

                    f.WriteLine("          <p>");

                    if (iMultimedia.Width != 0 && iMultimedia.Height != 0) {
                        // Must be a picture.
                        if (altName == "") {
                            altName = fLangMan.LS(PLS.LSID_ImageForThisSource);
                        }
                        if (iMultimedia.LargeFileName.Length > 0) {
                            f.WriteLine(string.Concat("            <a href=\"", iMultimedia.LargeFileName, "\"><img src=\"", iMultimedia.FileName, "\" alt=\"", altName, "\" /></a>")); // TODO: clip and scale properly. Use MainForm.s_config to set a max scale
                        } else {
                            f.WriteLine(string.Concat("            <img src=\"", iMultimedia.FileName, "\" alt=\"", altName, "\" />")); // TODO: clip and scale properly. Use MainForm.s_config to set a max scale
                        }
                    } else {
                        // Other multimedia
                        if (altName == "") {
                            altName = fLangMan.LS(PLS.LSID_MediaForThisSource);
                        }

                        if (GMConfig.Instance.LinkOriginalPicture) {
                            f.WriteLine(string.Concat("            <a href=\"", iMultimedia.FileName, "\"><img src=\"", nonPicMainFilename, "\" alt=\"", altName, "\" /></a>")); // TODO: clip and scale properly. Use MainForm.s_config to set a max scale
                        } else {
                            f.WriteLine(string.Concat("            <img src=\"", nonPicMainFilename, "\" alt=\"", altName, "\" />")); // TODO: clip and scale properly. Use MainForm.s_config to set a max scale
                        }
                    }
                    f.WriteLine("          </p>");
                    if (imageTitle != "") {
                        f.WriteLine("<p id=\"sourcepic_title\">{0}</p>", imageTitle);
                    }
                }
                f.WriteLine("        </div> <!-- sourcePics -->");
            }
        }
    }
}
