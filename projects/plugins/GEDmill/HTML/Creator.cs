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
using System.IO;
using System.Text;
using BSLib;
using GDModel;
using GEDmill.Model;
using GKCore.Interfaces;
using GKCore.Types;
using GKL = GKCore.Logging;

namespace GEDmill.HTML
{
    /// <summary>
    /// Base class providing general functionality required by all classes that create HTML pages.
    /// </summary>
    public abstract class Creator
    {
        private const int TAB_SPACES = 4;

        private static readonly GKL.ILogger fLogger = GKL.LogManager.GetLogger(GMConfig.LOG_FILE, GMConfig.LOG_LEVEL, typeof(Creator).Name);

        protected readonly ILangMan fLangMan;

        protected IBaseContext fContext;

        // The raw data that we are turning into a website.
        protected GDMTree fTree;

        // The same multimedia file may be referenced multiple times. 
        // This hash prevents it being copied to the output directory more than once.
        private static Dictionary<string, FilenameAndSize> fCopiedFiles = new Dictionary<string, FilenameAndSize>();


        protected Creator(IBaseContext context, ILangMan langMan)
        {
            fContext = context;
            fTree = fContext.Tree;
            fLangMan = langMan;
        }

        /// <summary>
        /// This clears the static list of all multimedia files copied to the output directory (and possibly renamed).
        /// </summary>
        public static void ClearCopiedFilesList()
        {
            fCopiedFiles.Clear();
        }

        // Converts all HTML characters into their escaped versions
        // Set hardSpace to false if you want to keep first space as breakable, true if you want all nbsp.
        // TODO: Surely there is a .Net function to do this (WebUtility.HtmlEncode)?
        // TODO: Might want to preserve <a> links in the HTML in case user has specified them in their data.
        protected static string EscapeHTML(string original, bool hardSpace)
        {
            if (string.IsNullOrEmpty(original)) {
                return string.Empty;
            }

            fLogger.WriteInfo(string.Format("EscapeHTML({0})", original));

            var sb = new StringBuilder(original.Length);
            int tabPos = 0;
            bool doneCRLF = false;
            bool doneSpace = false;
            int n = 0;
            foreach (char c in original) {
                switch (c) {
                    case (char)0x91:
                    case (char)0x92:
                        sb.Append("'");
                        doneCRLF = false;
                        doneSpace = false;
                        tabPos++;
                        break;
                    case (char)0x93:
                    case (char)0x94:
                        sb.Append("\"");
                        doneCRLF = false;
                        doneSpace = false;
                        tabPos++;
                        break;
                    case '<':
                        sb.Append("&lt;");
                        doneCRLF = false;
                        doneSpace = false;
                        tabPos++;
                        break;
                    case '>':
                        sb.Append("&gt;");
                        doneCRLF = false;
                        doneSpace = false;
                        tabPos++;
                        break;
                    case '\"':
                        sb.Append("&quot;");
                        doneCRLF = false;
                        doneSpace = false;
                        tabPos++;
                        break;
                    case '&':
                        sb.Append("&amp;");
                        doneCRLF = false;
                        doneSpace = false;
                        tabPos++;
                        break;
                    case ' ':
                        if (doneSpace || hardSpace) {
                            sb.Append("&nbsp;");
                        } else {
                            sb.Append(' ');
                            doneSpace = true;
                        }
                        doneCRLF = false;
                        tabPos++;
                        break;
                    case '\n':
                        if (!doneCRLF) {
                            sb.Append("<br />");
                        }
                        doneCRLF = false; // To allow multiple CRLFs to produce multiple <BR />s
                        doneSpace = false;
                        tabPos = 0;
                        break;
                    case '\r':
                        if (!doneCRLF) {
                            sb.Append("<br />");
                            doneCRLF = true;
                        }
                        doneSpace = false;
                        tabPos = 0;
                        break;
                    case '\t':
                        do {
                            sb.Append("&nbsp;");
                            tabPos++;
                        }
                        while ((tabPos % TAB_SPACES) != 0);
                        doneSpace = true;
                        break;

                    default:
                        sb.Append(c);
                        doneCRLF = false;
                        doneSpace = false;
                        tabPos++;
                        break;
                }
                ++n;
            }
            return sb.ToString();
        }

        /// <summary>
        /// Converts all Javascript characters into their escaped versions.
        /// </summary>
        protected static string EscapeJavascript(string original)
        {
            if (original == null) {
                return "";
            }

            var sb = new StringBuilder(original.Length);
            foreach (char c in original) {
                switch (c) {
                    case '\'':
                        sb.Append("\\'");
                        break;
                    default:
                        sb.Append(c);
                        break;
                }
            }
            return sb.ToString();
        }

        /// <summary>
        /// Converts all invalid sFilename characters into underscores.
        /// </summary>
        protected static string EscapeFilename(string original)
        {
            fLogger.WriteInfo(string.Format("EscapeFilename({0})", original));

            if (original == null) {
                return "_";
            }

            const string validChars = " 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz$%'`-@{}~!#()&_^";

            var sb = new StringBuilder(original.Length);
            foreach (char c in original) {
                char cc = c;
                if (validChars.IndexOf(c) < 0) {
                    cc = '_';
                }
                sb.Append(cc);
            }
            return sb.ToString();
        }

        /// <summary>
        /// Returns a string with all email addresses replaced by value of sReplacement.
        /// </summary>
        protected static string ObfuscateEmail(string text)
        {
            if (text == null) {
                return null;
            }
            int nLength = text.Length;
            var sb = new StringBuilder(nLength);
            int i = 0;
            int nNameStart = -1;
            int nState = 0;
            const string invalidNameChars = ",\"£$^&*()+=]}[{':;,<>?/\\|`¬#~";
            const string replacement = "<email address>";

            while (i < nLength) {
                char c = text[i];

                switch (nState) {
                    case 0:
                        // Not seen anything special.
                        if (!char.IsWhiteSpace(c) && c != '@' && c != '.' && invalidNameChars.IndexOf(c) < 0) {
                            // Possible name char, remember where name starts.
                            nState = 1;
                            nNameStart = i;
                        } else {
                            // Can't be an email name. Add it verbatim.
                            sb.Append(c);
                            nState = 0;
                        }
                        break;
                    case 1:
                        // Seen one or more name chars.
                        if (c == '@') {
                            // Now looking for domain.
                            nState = 2;
                        } else if (!char.IsWhiteSpace(c) && invalidNameChars.IndexOf(c) < 0) {
                            // Continue looking through a possible name string.
                        } else {
                            // Can't be an email address. Add what we've got so far and return
                            // to hunting email addresses.
                            sb.Append(text.Substring(nNameStart, i - nNameStart));
                            sb.Append(c);
                            nState = 0;
                        }
                        break;
                    case 2:
                        // Seen at sign, now looking for domain
                        if (!char.IsWhiteSpace(c) && c != '@' && c != '.' && invalidNameChars.IndexOf(c) < 0) {
                            // Possible domain char.
                            // Now looking for dot among domain chars.
                            nState = 3;
                        } else {
                            // Can't be an email address. Add what we've got so far and return
                            // to hunting email addresses.
                            sb.Append(text.Substring(nNameStart, i - nNameStart));
                            sb.Append(c);
                            nState = 0;
                        }
                        break;
                    case 3:
                        // Looking for first dot among domain chars
                        if (c == '.') {
                            // Now looking for another domain.
                            nState = 4;
                        } else if (!char.IsWhiteSpace(c) && c != '@' && invalidNameChars.IndexOf(c) < 0) {
                            // A possible domain char, keep looking for dot.
                        } else {
                            // Can't be an email address. Add what we've got so far and return
                            // to hunting email addresses.
                            sb.Append(text.Substring(nNameStart, i - nNameStart));
                            sb.Append(c);
                            nState = 0;
                        }
                        break;
                    case 4:
                        // Looking for valid domain char to start next domain portion.
                        if (!char.IsWhiteSpace(c) && c != '@' && c != '.' && invalidNameChars.IndexOf(c) < 0) {
                            // A valid domain char. Look for another dot , or end.
                            nState = 5;
                        } else {
                            // Can't be an email address. Add what we've got so far and return
                            // to hunting email addresses.
                            sb.Append(text.Substring(nNameStart, i - nNameStart));
                            sb.Append(c);
                            nState = 0;
                        }
                        break;
                    case 5:
                        // Looking for a dot or end of domain among valid domain chars
                        if (c == '.') {
                            // Read rest of domain part.
                            nState = 6;
                        } else if (!char.IsWhiteSpace(c) && c != '@' && invalidNameChars.IndexOf(c) < 0) {
                            // Valid domain name. Keep looking for dot or end.
                        } else if (c != '@') {
                            // Found complete email address
                            sb.Append(replacement);
                            sb.Append(c);
                            nState = 0;
                        } else {
                            // Can't be an email address. Add what we've got so far and return
                            // to hunting email addresses.
                            sb.Append(text.Substring(nNameStart, i - nNameStart));
                            sb.Append(c);
                            nState = 0;
                        }
                        break;
                    case 6:
                        // Looking for valid domain char to start next domain portion, or can end here if address is (name@add.add.)
                        if (!char.IsWhiteSpace(c) && c != '@' && c != '.' && invalidNameChars.IndexOf(c) < 0) {
                            // A valid domain char. Look for another dot , or end.
                            nState = 5;
                        } else {
                            // Found complete email address (ending in a full-stop).
                            sb.Append(replacement);
                            sb.Append('.');
                            sb.Append(c);
                            nState = 0;
                        }
                        break;
                }
                ++i;
            }

            // Add anything remaining in email addr buffer.
            if (nState == 5 || nState == 6) {
                // Found complete email address.
                sb.Append(replacement);
                if (nState == 6) {
                    // We ended on a dot.
                    sb.Append('.');
                }
            } else if (nState > 0) {
                sb.Append(text.Substring(nNameStart, i - nNameStart));
            }
            return sb.ToString();
        }

        /// <summary>
        /// Generates navbar at top of page, in header div.
        /// </summary>
        protected void OutputPageHeader(HTMLFile f, string previousChildLink, string nextChildLink, bool includeIndexLink)
        {
            if (GMConfig.Instance.IncludeNavbar) {
                string frontPageLink = "";
                if (GMConfig.Instance.FrontPageFilename != "") {
                    frontPageLink += string.Concat("<a href=\"", GMConfig.Instance.FrontPageFilename, ".html", "\">", fLangMan.LS(PLS.FrontPage), "</a>");
                }
                string mainSiteLink = "";
                if (GMConfig.Instance.MainWebsiteLink != "") {
                    mainSiteLink += string.Concat("<a href=\"", GMConfig.Instance.MainWebsiteLink, "\">", fLangMan.LS(PLS.MainSite), "</a>");
                }

                bool includeNavbar = previousChildLink != "" || nextChildLink != "" || includeIndexLink || frontPageLink != "" || mainSiteLink != "";

                if (includeNavbar) {
                    f.WriteLine("    <div id=\"header\">");
                    f.WriteLine("      <ul>");

                    if (previousChildLink != "") {
                        f.WriteLine("<li>{0}</li>", previousChildLink);
                    }

                    if (nextChildLink != "") {
                        f.WriteLine("<li>{0}</li>", nextChildLink);
                    }

                    if (includeIndexLink) {
                        f.WriteLine(string.Concat("<li><a href=\"individuals1.html\">", fLangMan.LS(PLS.Index), "</a></li>"));
                    }

                    if (frontPageLink != "") {
                        f.WriteLine("<li>{0}</li>", frontPageLink);
                    }

                    if (mainSiteLink != "") {
                        f.WriteLine("<li>{0}</li>", mainSiteLink);
                    }

                    f.WriteLine("      </ul>");
                    f.WriteLine("    </div> <!-- header -->");
                    f.WriteLine("");
                }
            }
        }

        // Copies a file from the user's source directory to the website output directory, renaming and resizing as appropriate.
        // Returns the sFilename of the copy.
        // sArea is sAsid sub-part of image
        // sArea is changed to reflect new image size
        // sArea can be {0,0,0,0} meaning use whole image
        // stats can be null if we don't care about keeping count of the multimedia files.
        public static string CopyMultimedia(string fullFilename, string newFilename, int maxWidth, int maxHeight,
                                            ref ExtRect rectArea, Stats stats)
        {
            fLogger.WriteInfo(string.Format("CopyMultimedia( {0}, {1}, {2} )", fullFilename, maxWidth, maxHeight));

            if (!File.Exists(fullFilename)) {
                return string.Empty;
            }

            string result = fullFilename;

            if (string.IsNullOrEmpty(newFilename)) {
                newFilename = Path.GetFileName(fullFilename);
            }

            try {
                string asidFilename;
                if (rectArea.Width == 0) {
                    asidFilename = fullFilename;
                } else {
                    asidFilename = string.Concat(fullFilename, ".", rectArea.Left.ToString(), ",", rectArea.Top.ToString(), ",", rectArea.Width.ToString(), ",", rectArea.Height.ToString());
                }

                if (maxWidth != 0 && maxHeight != 0) {
                    asidFilename = string.Concat(asidFilename, "(", maxWidth.ToString(), "x", maxHeight.ToString(), ")");
                }

                if (fullFilename != null && !string.IsNullOrEmpty(GMConfig.Instance.OutputFolder)) {
                    // Have we already copied the sFilename?
                    if (fCopiedFiles.ContainsKey(asidFilename)) {
                        var filenameAndSize = fCopiedFiles[asidFilename];
                        result = filenameAndSize.FileName;
                        rectArea = new ExtRect(rectArea.Left, rectArea.Top, filenameAndSize.Width, filenameAndSize.Height);
                    } else {
                        // Copy file into output directory
                        if (GMConfig.Instance.CopyMultimedia) {
                            string imageFolder = GMConfig.Instance.ImageFolder;
                            string outputFolder = GMConfig.Instance.OutputFolder;

                            if (imageFolder != "") {
                                imageFolder = imageFolder + '\\';
                            }
                            if (outputFolder != "") {
                                outputFolder = outputFolder + '\\';
                            }

                            string copyFilename = string.Concat(imageFolder, newFilename);
                            string absImageFolder = string.Concat(outputFolder, imageFolder);
                            string absCopyFilename = string.Concat(absImageFolder, newFilename);

                            // If image folder doesn't exist, create it
                            if (!File.Exists(absImageFolder) && !Directory.Exists(absImageFolder)) // TODO: this returns false if it exists but you don't have permission!
                            {
                                Directory.CreateDirectory(absImageFolder); // TODO: catch failure to create, e.g. output folder not there yet
                            }

                            // If new sFilename already exists, append a number and keep trying
                            uint uCopy = 0;
                            string filePart = Path.GetFileNameWithoutExtension(copyFilename);
                            string extnPart = Path.GetExtension(copyFilename);
                            while (File.Exists(absCopyFilename)) {
                                const string sAdditionalLetters = "abcdefghijklmnopqrstuvwxyz";
                                if (GMConfig.Instance.RenameMultimedia == false) {
                                    uint nCopyPlus = uCopy + 2;
                                    copyFilename = string.Concat(imageFolder, filePart, "-", nCopyPlus.ToString(), extnPart);
                                } else if (uCopy >= sAdditionalLetters.Length) {
                                    // Once all the extra letters have been used up, put number as "-n", where n starts from 2.
                                    uint nCopyMinus = uCopy - (uint)(sAdditionalLetters.Length - 2);
                                    copyFilename = string.Concat(imageFolder, filePart, "-", nCopyMinus.ToString(), extnPart);
                                } else {
                                    copyFilename = string.Concat(imageFolder, filePart, sAdditionalLetters[(int)uCopy], extnPart);
                                }
                                uCopy++;

                                absCopyFilename = string.Concat(outputFolder, copyFilename);
                            }

                            fLogger.WriteInfo(string.Format("Copying \"{0}\" to \"{1}\"", fullFilename, absCopyFilename));

                            File.Copy(fullFilename, absCopyFilename, true);

                            File.SetAttributes(fullFilename, FileAttributes.Normal); // Make any Read-Only files read-write.
                            if (maxWidth != 0 && maxHeight != 0) {
                                // It must be a picture file
                                copyFilename = TreeDrawer.ConvertAndCropImage(outputFolder, copyFilename, ref rectArea, maxWidth, maxHeight);
                            }
                            fCopiedFiles[asidFilename] = new FilenameAndSize(copyFilename, rectArea.Width, rectArea.Height);
                            result = copyFilename;
                        } else {
                            if (GMConfig.Instance.RelativiseMultimedia) {
                                // TODO: make path of sFilename relative to MainForm.s_config.m_outputFolder
                                string sRelativeFilename = fullFilename;
                                result = sRelativeFilename;
                            }
                        }
                        if (stats != null) {
                            stats.MultimediaFiles++;
                        }
                    }
                }
                result = result.Replace('\\', '/');
            } catch (IOException e) {
                fLogger.WriteError("Caught IO Exception : ", e);
                result = "";
            } catch (ArgumentException e) {
                fLogger.WriteError("Caught Argument Exception : ", e);
                result = "";
            } catch (HTMLException e) {
                fLogger.WriteError("Caught HTML Exception : ", e);
                result = "";
            } catch (Exception e) {
                fLogger.WriteError("Caught generic exception : ", e);
                result = "";
            }

            return result;
        }

        // Creates link HTML for the individual e.g. <a href="indiI1.html">Fred Bloggs</a>
        protected static string MakeLink(GDMIndividualRecord ir)
        {
            string name;
            if (!GMHelper.GetVisibility(ir) && !GMConfig.Instance.UseWithheldNames) {
                name = GMConfig.Instance.ConcealedName;
            } else {
                string dummy;
                name = GMHelper.CapitaliseName(ir.GetPrimaryPersonalName(), out dummy, out dummy);
            }
            return MakeLink(ir, name);
        }

        protected static string MakeNote(string noteStr)
        {
            if (!string.IsNullOrEmpty(noteStr)) {
                return string.Concat("<p class=\"eventNote\">", EscapeHTML(noteStr, false), "</p>");
            }
            return string.Empty;
        }

        // Creates link HTML for the individual e.g. <a href="indiI1.html">Next Child</a>. Uses name provided by caller.
        protected static string MakeLink(GDMIndividualRecord ir, string name)
        {
            string link;
            if (!GMHelper.GetVisibility(ir)) {
                // TODO: Why are we linking to invisible people?
                link = EscapeHTML(name, true);
            } else {
                link = string.Concat("<a href=\"", GetIndividualHTMLFilename(ir), "\">", EscapeHTML(name, false), "</a>");
            }
            return link;
        }

        /// <summary>
        /// Returns a string to use as a sFilename for this individual's HTML page. The string is just the Filename, not a fully qualified path.
        /// </summary>
        protected static string GetIndividualHTMLFilename(GDMIndividualRecord ir)
        {
            string relativeFilename = string.Concat("indi", ir.XRef, ".html");
            return relativeFilename;
        }

        protected static string MakeLinkNumber(GDMSourceCitation sourCit, int sourceCount, bool hasComma)
        {
            string comma = hasComma ? "," : "";
            return string.Concat("<span class=\"reference\">", comma, sourceCount.ToString(), "</span>");
        }

        /// <summary>
        /// Returns a string to use in the list of references at the bottom of the page
        /// </summary>
        protected static string MakeLinkText(GDMTree tree, GDMSourceCitation sourCit, int sourceCount)
        {
            var sourRec = tree.GetPtrValue<GDMSourceRecord>(sourCit);
            return string.Concat(sourceCount.ToString(), ". ", /*m_sSourceDescription*/sourRec.ShortTitle);
        }

        /// <summary>
        /// Returns the name of the alternative picture file to display for non-diaplayable files of the given format
        /// </summary>
        protected static string NonPicFilename(MultimediaKind format, bool small, bool clickToDownload)
        {
            string filename;
            switch (format) {
                case MultimediaKind.mkAudio:
                    filename = small ? "gmaudio_sm.png" : clickToDownload ? "gmaudio.png" : "gmaudion.png";
                    break;
                case MultimediaKind.mkVideo:
                    filename = small ? "gmvideo_sm.png" : clickToDownload ? "gmvideo.png" : "gmvideon.png";
                    break;
                default:
                    filename = small ? "gmdoc_sm.png" : clickToDownload ? "gmdoc.png" : "gmdocn.png";
                    break;
            }
            return filename;
        }
    }
}
