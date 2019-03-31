/* CCreatorFrontPage.cs
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
using System.Drawing;
using System.Globalization;
using System.IO;
using GKCommon.GEDCOM;
using GKCore.Logging;

namespace GEDmill.HTML
{
    /// <summary>
    /// Creates the HTML for the front (landing) page
    /// </summary>
    public class CreatorFrontPage : Creator
    {
        private static readonly ILogger fLogger = LogManager.GetLogger(CConfig.LOG_FILE, CConfig.LOG_LEVEL, typeof(CreatorFrontPage).Name);

        // Statistics about the website (number of files etc.)
        private Stats fStats;


        public CreatorFrontPage(GEDCOMTree gedcom, IProgressCallback progress, string w3cfile, Stats stats) : base(gedcom, progress, w3cfile)
        {
            fStats = stats;
        }

        // The main method that causes the front page to be created. 
        public void Create()
        {
            string pageDescription = "GEDmill GEDCOM to HTML family history website";
            string keywords = "family tree history " + MainForm.Config.OwnersName;
            string title = MainForm.Config.SiteTitle;

            HTMLFile f = null;
            try {
                f = new HTMLFile(MainForm.Config.FrontPageURL, title, pageDescription, keywords); // Creates a new file, and puts standard header html into it.
                f.Writer.WriteLine("  <div id=\"page\"> <!-- page -->");
                f.Writer.WriteLine("    <div id=\"cover\"> <!-- cover -->");

                f.Writer.WriteLine(String.Concat("       <h1>", EscapeHTML(title, false), "</h1>"));

                if (MainForm.Config.FrontPageImageFilename != "") {
                    Rectangle newArea = new Rectangle(0, 0, 0, 0);
                    string pictureFile = CopyMultimedia(MainForm.Config.FrontPageImageFilename, "", 0, 0, ref newArea, null);
                    if (pictureFile != null && pictureFile != "") {
                        f.Writer.WriteLine(String.Concat("       <p><img src=\"" + pictureFile + "\" alt=\"Front page image\" /></p>"));
                    }
                }

                if (MainForm.Config.CommentaryText != null && MainForm.Config.CommentaryText != "") {
                    if (MainForm.Config.CommentaryIsHtml) {
                        f.Writer.WriteLine(String.Concat("       <p>", MainForm.Config.CommentaryText, "</p>"));
                    } else {
                        f.Writer.WriteLine(String.Concat("       <p>", EscapeHTML(MainForm.Config.CommentaryText, false), "</p>"));
                    }
                }

                if (MainForm.Config.ShowFrontPageStats) {
                    string sIndividuals;
                    if (fStats.Individuals == 0) {
                        sIndividuals = "no";
                    } else {
                        sIndividuals = fStats.Individuals.ToString();
                    }
                    sIndividuals += " individual";
                    if (fStats.Individuals != 1) {
                        sIndividuals += "s";
                    }

                    string sSources;
                    if (fStats.Sources == 0) {
                        sSources = "";
                    } else {
                        sSources = String.Concat(", cross-referenced to ", fStats.Sources.ToString(), " source");
                    }
                    if (fStats.Sources > 1) {
                        sSources += "s";
                    }

                    string sMultimedia;
                    string sFileType;
                    if (fStats.NonPicturesIncluded) {
                        sFileType = "multimedia file";
                    } else {
                        sFileType = "image";
                    }
                    if (fStats.MultimediaFiles == 0) {
                        sMultimedia = "";
                    } else {
                        sMultimedia = String.Concat(". There are links to ", fStats.MultimediaFiles.ToString(), " ", sFileType);
                    }
                    if (fStats.MultimediaFiles > 1) {
                        sMultimedia += "s";
                    }

                    f.Writer.WriteLine(String.Concat("       <p>This website contains records on ", sIndividuals, sSources, sMultimedia, ".</p>"));
                }

                f.Writer.WriteLine("       <div id=\"links\"> <!-- links -->");
                f.Writer.WriteLine(String.Concat("         <p><a href=\"individuals1.", MainForm.Config.HtmlExtension, "\">", MainForm.Config.IndexTitle, "</a></p>"));
                f.Writer.WriteLine("       </div> <!-- links -->");
                if (MainForm.Config.KeyIndividuals != null && MainForm.Config.KeyIndividuals.Count > 0) {
                    // Although in theory you might want a restricted individual as a key individual, (they still form part of the tree), in practice this isn't allowed:
                    ArrayList alCensoredKeyIndividuals = new ArrayList(MainForm.Config.KeyIndividuals.Count);

                    foreach (string keyXref in MainForm.Config.KeyIndividuals) {
                        GEDCOMIndividualRecord air = fTree.XRefIndex_Find(keyXref) as GEDCOMIndividualRecord;
                        if (air != null) {
                            alCensoredKeyIndividuals.Add(MakeLink(air));
                        }
                    }

                    if (alCensoredKeyIndividuals.Count > 0) {
                        string plural = "";
                        if (alCensoredKeyIndividuals.Count > 1) {
                            plural = "s";
                        }
                        f.Writer.WriteLine("         <div id=\"keyindividuals\">");
                        f.Writer.WriteLine(String.Concat("           <p>Key Individual", plural, ":</p>"));
                        f.Writer.WriteLine("           <ul>");
                        foreach (string air_link in alCensoredKeyIndividuals) {
                            f.Writer.WriteLine(String.Concat("             <li>", air_link, "</li>"));
                        }
                        f.Writer.WriteLine("           </ul>");
                        f.Writer.WriteLine("         </div> <!-- keyindividuals -->");
                    }
                }

                string byEmail = "";
                // Email contact address
                if (MainForm.Config.UserEmailAddress != null && MainForm.Config.UserEmailAddress.Length > 0) {
                    byEmail = String.Concat(" by <a href=\"mailto:", MainForm.Config.UserEmailAddress, "\">", EscapeHTML(MainForm.Config.UserEmailAddress, false), "</a>");
                }

                // Add brand and contact label
                f.Writer.WriteLine(String.Concat("       <p>Website created", byEmail, " using GEDmill.</p>"));
                // Add last update string
                if (MainForm.Config.AddHomePageCreateTime) {
                    DateTime dt = DateTime.Now;
                    string update_date_string = dt.ToString("dd MMMM yyyy", DateTimeFormatInfo.InvariantInfo);

                    f.Writer.WriteLine(String.Concat("       <p>Created on ", update_date_string, ".</p>"));
                }

                // Add link to users main website
                if (MainForm.Config.MainWebsiteLink != "") {
                    f.Writer.WriteLine(String.Concat("    <p><a href=\"", MainForm.Config.MainWebsiteLink, "\">Return to main site</a></p>"));
                }

                f.Writer.WriteLine("    </div> <!-- cover -->");
                f.Writer.WriteLine("  </div> <!-- page -->");
            } catch (IOException e) {
                fLogger.WriteError("Caught IO Exception(7) : ", e);
            } catch (ArgumentException e) {
                fLogger.WriteError("Caught Argument Exception(7) : ", e);
            } finally {
                if (f != null) {
                    // Add standard footer to the file
                    f.Close();
                }
            }
        }
    }
}
