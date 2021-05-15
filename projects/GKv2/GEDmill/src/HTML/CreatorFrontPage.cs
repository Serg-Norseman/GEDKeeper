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
using GKCore.Logging;

namespace GEDmill.HTML
{
    /// <summary>
    /// Creates the HTML for the front (landing) page
    /// </summary>
    public class CreatorFrontPage : Creator
    {
        private static readonly ILogger fLogger = LogManager.GetLogger(GMConfig.LOG_FILE, GMConfig.LOG_LEVEL, typeof(CreatorFrontPage).Name);

        // Statistics about the website (number of files etc.)
        private Stats fStats;


        public CreatorFrontPage(GDMTree tree, IProgressCallback progress, string w3cfile, Stats stats) : base(tree, progress, w3cfile)
        {
            fStats = stats;
        }

        // The main method that causes the front page to be created. 
        public void Create()
        {
            string keywords = "family tree history " + GMConfig.Instance.OwnersName;
            string title = GMConfig.Instance.SiteTitle;

            HTMLFile f = null;
            try {
                f = new HTMLFile(GMConfig.Instance.FrontPageURL, title, PageDescription, keywords); // Creates a new file, and puts standard header html into it.
                f.WriteLine("  <div id=\"page\"> <!-- page -->");
                f.WriteLine("    <div id=\"cover\"> <!-- cover -->");

                f.WriteLine("<h1>{0}</h1>", EscapeHTML(title, false));

                if (!string.IsNullOrEmpty(GMConfig.Instance.FrontPageImageFilename)) {
                    Rectangle newArea = new Rectangle(0, 0, 0, 0);
                    string pictureFile = CopyMultimedia(GMConfig.Instance.FrontPageImageFilename, "", 0, 0, ref newArea, null);
                    if (!string.IsNullOrEmpty(pictureFile)) {
                        f.WriteLine("<p><img src=\"{0}\" alt=\"Front page image\" /></p>", pictureFile);
                    }
                }

                if (!string.IsNullOrEmpty(GMConfig.Instance.CommentaryText)) {
                    if (GMConfig.Instance.CommentaryIsHtml) {
                        f.WriteLine("<p>{0}</p>", GMConfig.Instance.CommentaryText);
                    } else {
                        f.WriteLine("<p>{0}</p>", EscapeHTML(GMConfig.Instance.CommentaryText, false));
                    }
                }

                if (GMConfig.Instance.ShowFrontPageStats) {
                    string individuals = fStats.Individuals == 0 ? "no" : fStats.Individuals.ToString();
                    individuals += " individual";
                    if (fStats.Individuals != 1) {
                        individuals += "s";
                    }

                    string sources = fStats.Sources == 0 ? "" : string.Concat(", cross-referenced to ", fStats.Sources.ToString(), " source");
                    if (fStats.Sources > 1) {
                        sources += "s";
                    }

                    string fileType = fStats.NonPicturesIncluded ? "multimedia file" : "image";
                    string multimedia = fStats.MultimediaFiles == 0 ? "" : string.Concat(". There are links to ", fStats.MultimediaFiles.ToString(), " ", fileType);

                    if (fStats.MultimediaFiles > 1) {
                        multimedia += "s";
                    }

                    f.WriteLine(string.Concat("       <p>This website contains records on ", individuals, sources, multimedia, ".</p>"));
                }

                f.WriteLine("       <div id=\"links\"> <!-- links -->");
                f.WriteLine(string.Concat("         <p><a href=\"individuals1.", GMConfig.Instance.HtmlExtension, "\">", GMConfig.Instance.IndexTitle, "</a></p>"));
                f.WriteLine("       </div> <!-- links -->");
                if (GMConfig.Instance.KeyIndividuals != null && GMConfig.Instance.KeyIndividuals.Count > 0) {
                    // Although in theory you might want a restricted individual as a key individual, (they still form part of the tree), in practice this isn't allowed:
                    var censoredKeyIndividuals = new List<string>(GMConfig.Instance.KeyIndividuals.Count);

                    foreach (string keyXref in GMConfig.Instance.KeyIndividuals) {
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
                if (!string.IsNullOrEmpty(GMConfig.Instance.UserEmailAddress)) {
                    byEmail = string.Concat(" by <a href=\"mailto:", GMConfig.Instance.UserEmailAddress, "\">", EscapeHTML(GMConfig.Instance.UserEmailAddress, false), "</a>");
                }

                // Add brand and contact label
                f.WriteLine("<p>Website created{0} using GEDmill.</p>", byEmail);
                // Add last update string
                if (GMConfig.Instance.AddHomePageCreateTime) {
                    f.WriteLine("<p>Created on {0}.</p>", GMHelper.GetNowDateStr());
                }

                // Add link to users main website
                if (!string.IsNullOrEmpty(GMConfig.Instance.MainWebsiteLink)) {
                    f.WriteLine("<p><a href=\"{0}\">Return to main site</a></p>", GMConfig.Instance.MainWebsiteLink);
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
