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
using System.IO;
using GDModel;
using GEDmill.Model;
using GKCore.Logging;

namespace GEDmill.HTML
{
    /// <summary>
    /// Summary description for CCreatorHelppage.
    /// </summary>
    public class CreatorHelpPage : Creator
    {
        private static readonly ILogger fLogger = LogManager.GetLogger(CConfig.LOG_FILE, CConfig.LOG_LEVEL, typeof(CreatorHelpPage).Name);

        public CreatorHelpPage(GDMTree tree, IProgressCallback progress, string w3cfile) : base(tree, progress, w3cfile)
        {
        }

        // The main method that causes the help page to be created. 
        public void Create()
        {
            FileStream helpStream = null;
            StreamReader helpReader = null;
            try {
                helpStream = new FileStream(CConfig.Instance.ApplicationPath + "\\helpsource.html", FileMode.Open);
                helpReader = new StreamReader(helpStream, System.Text.Encoding.UTF8);
            } catch (IOException e) {
                fLogger.WriteError("Caught io exception while loading help file source: {0}", e);
                helpStream = null;
                helpReader = null;
            }

            if (helpStream != null && helpReader != null) {
                string keywords = "family tree history " + CConfig.Instance.OwnersName;
                string title = CConfig.Instance.SiteTitle;

                HTMLFile f = null;
                try {
                    // Create a new file and put standard header html into it.
                    f = new HTMLFile(CConfig.Instance.HelpPageURL, title, PageDescription, keywords);

                    OutputPageHeader(f, "", "", true, false);

                    f.WriteLine("  <div id=\"page\"> <!-- page -->");

                    // Copy in the help html source
                    string helpLine;
                    while ((helpLine = helpReader.ReadLine()) != null) {
                        f.WriteLine(helpLine);
                    }

                    f.WriteLine("  </div> <!-- page -->");
                } catch (IOException e) {
                    fLogger.WriteError("Caught IO Exception : ", e);
                } catch (ArgumentException e) {
                    fLogger.WriteError("Caught Argument Exception : ", e);
                } finally {
                    if (f != null) {
                        // Add standard footer to the file
                        f.Close();
                    }
                }
            }
            if (helpReader != null) {
                helpReader.Close();
            }
            if (helpStream != null) {
                helpStream.Close();
            }
        }
    }
}
