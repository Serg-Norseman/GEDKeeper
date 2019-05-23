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
using System.Collections.Generic;
using System.Drawing;
using System.Globalization;
using System.IO;
using GEDmill.Model;
using GDModel;
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


        public CreatorFrontPage(GDMTree gedcom, IProgressCallback progress, string w3cfile, Stats stats) : base(gedcom, progress, w3cfile)
        {
            fStats = stats;
        }

        // The main method that causes the front page to be created. 
        public void Create()
        {
            string keywords = "family tree history " + CConfig.Instance.OwnersName;
            string title = CConfig.Instance.SiteTitle;

            HTMLFile f = null;
            try {
                f = new HTMLFile(CConfig.Instance.FrontPageURL, title, PageDescription, keywords); // Creates a new file, and puts standard header html into it.
                f.WriteLine("  <div id=\"page\"> <!-- page -->");
                f.WriteLine("    <div id=\"cover\"> <!-- cover -->");

                f.WriteLine("<h1>{0}</h1>", EscapeHTML(title, false));

                if (!string.IsNullOrEmpty(CConfig.Instance.FrontPageImageFilename)) {
                    Rectangle newArea = new Rectangle(0, 0, 0, 0);
                    string pictureFile = CopyMultimedia(CConfig.Instance.FrontPageImageFilename, "", 0, 0, ref newArea, null);
                    if (!string.IsNullOrEmpty(pictureFile)) {
                        f.WriteLine("<p><img src=\"{0}\" alt=\"Front page image\" /></p>", pictureFile);
                    }
                }

                if (!string.IsNullOrEmpty(CConfig.Instance.CommentaryText)) {
                    if (CConfig.Instance.CommentaryIsHtml) {
                        f.WriteLine("<p>{0}</p>", CConfig.Instance.CommentaryText);
                    } else {
                        f.WriteLine("<p>{0}</p>", EscapeHTML(CConfig.Instance.CommentaryText, false));
                    }
                }

                if (CConfig.Instance.ShowFrontPageStats) {
                    string sIndividuals;
                    sIndividuals = fStats.Individuals == 0 ? "no" : fStats.Individuals.ToString();
                    sIndividuals += " individual";
                    if (fStats.Individuals != 1) {
                        sIndividuals += "s";
                    }

                    string sSources;
                    sSources = fStats.Sources == 0 ? "" : string.Concat(", cross-referenced to ", fStats.Sources.ToString(), " source");
                    if (fStats.Sources > 1) {
                        sSources += "s";
                    }

                    string fileType = fStats.NonPicturesIncluded ? "multimedia file" : "image";
                    string multimedia = fStats.MultimediaFiles == 0 ? "" : string.Concat(". There are links to ", fStats.MultimediaFiles.ToString(), " ", fileType);

                    if (fStats.MultimediaFiles > 1) {
                        multimedia += "s";
                    }

                    f.WriteLine(string.Concat("       <p>This website contains records on ", sIndividuals, sSources, multimedia, ".</p>"));
                }

                f.WriteLine("       <div id=\"links\"> <!-- links -->");
                f.WriteLine(string.Concat("         <p><a href=\"individuals1.", CConfig.Instance.HtmlExtension, "\">", CConfig.Instance.IndexTitle, "</a></p>"));
                f.WriteLine("       </div> <!-- links -->");
                if (CConfig.Instance.KeyIndividuals != null && CConfig.Instance.KeyIndividuals.Count > 0) {
                    // Although in theory you might want a restricted individual as a key individual, (they still form part of the tree), in practice this isn't allowed:
                    var censoredKeyIndividuals = new List<string>(CConfig.Instance.KeyIndividuals.Count);

                    foreach (string keyXref in CConfig.Instance.KeyIndividuals) {
                        GDMIndividualRecord air = fTree.XRefIndex_Find(keyXref) as GDMIndividualRecord;
                        if (air != null) {
                            censoredKeyIndividuals.Add(MakeLink(air));
                        }
                    }

                    if (censoredKeyIndividuals.Count > 0) {
                        string plural = "";
                        if (censoredKeyIndividuals.Count > 1) {
                            plural = "s";
                        }
                        f.WriteLine("<div id=\"keyindividuals\">");
                        f.WriteLine("<p>Key Individual{0}:</p>", plural);
                        f.WriteLine("<ul>");
                        foreach (string air_link in censoredKeyIndividuals) {
                            f.WriteLine("<li>{0}</li>", air_link);
                        }
                        f.WriteLine("</ul>");
                        f.WriteLine("</div> <!-- keyindividuals -->");
                    }
                }

                string byEmail = "";
                // Email contact address
                if (!string.IsNullOrEmpty(CConfig.Instance.UserEmailAddress)) {
                    byEmail = string.Concat(" by <a href=\"mailto:", CConfig.Instance.UserEmailAddress, "\">", EscapeHTML(CConfig.Instance.UserEmailAddress, false), "</a>");
                }

                // Add brand and contact label
                f.WriteLine("<p>Website created{0} using GEDmill.</p>", byEmail);
                // Add last update string
                if (CConfig.Instance.AddHomePageCreateTime) {
                    DateTime dt = DateTime.Now;
                    string update_date_string = dt.ToString("dd MMMM yyyy", DateTimeFormatInfo.InvariantInfo);
                    f.WriteLine("<p>Created on {0}.</p>", update_date_string);
                }

                // Add link to users main website
                if (!string.IsNullOrEmpty(CConfig.Instance.MainWebsiteLink)) {
                    f.WriteLine("<p><a href=\"{0}\">Return to main site</a></p>", CConfig.Instance.MainWebsiteLink);
                }

                f.WriteLine("    </div> <!-- cover -->");
                f.WriteLine("  </div> <!-- page -->");
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
