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
using System.Drawing.Imaging;
using System.IO;
using System.Text;
using GDModel;
using GEDmill.Model;
using GKCore.Interfaces;
using GKCore.Logging;

namespace GEDmill.HTML
{
    /// <summary>
    /// Base class providing general functionality required by all classes that create HTML pages.
    /// </summary>
    public abstract class Creator
    {
        private static readonly ILogger fLogger = LogManager.GetLogger(GMConfig.LOG_FILE, GMConfig.LOG_LEVEL, typeof(Creator).Name);

        protected readonly ILangMan fLangMan;

        protected IBaseContext fContext;

        // The raw data that we are turning into a website.
        protected GDMTree fTree;

        // Pointer to the window showing the progress bar, so that web page creation progress can be shown to user.
        private IProgressCallback fProgressWindow;

        // The same multimedia file may be referenced multiple times. 
        // This hash prevents it being copied to the output directory more than once.
        private static Dictionary<string, FilenameAndSize> fCopiedFiles = new Dictionary<string, FilenameAndSize>();


        protected Creator(IBaseContext context, IProgressCallback progress, ILangMan langMan)
        {
            fContext = context;
            fTree = fContext.Tree;
            fProgressWindow = progress;
            fLangMan = langMan;
        }

        // This clears the static list of all multimedia files copied to the output directory (and possibly renamed).
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

            int tabSpaces = GMConfig.Instance.TabSpaces;
            var sb = new StringBuilder(original.Length);
            int tabPos = 0;
            bool doneCRLF = false;
            bool doneSpace = false;
            int length = original.Length;
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
                        while ((tabPos % tabSpaces) != 0);
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

        // Converts all Javascript characters into their escaped versions
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

        // Converts all invalid sFilename characters into underscores
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

        // Returns a string with all email addresses replaced by value of sReplacement.
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

        // Generates navbar at top of page, in header div
        protected void OutputPageHeader(HTMLFile f, string previousChildLink, string nextChildLink, bool includeIndexLink)
        {
            if (GMConfig.Instance.IncludeNavbar) {
                string frontPageLink = "";
                if (GMConfig.Instance.FrontPageFilename != "") {
                    frontPageLink += string.Concat("<a href=\"", GMConfig.Instance.FrontPageFilename, ".html", "\">", fLangMan.LS(PLS.LSID_FrontPage), "</a>");
                }
                string mainSiteLink = "";
                if (GMConfig.Instance.MainWebsiteLink != "") {
                    mainSiteLink += string.Concat("<a href=\"", GMConfig.Instance.MainWebsiteLink, "\">", fLangMan.LS(PLS.LSID_MainSite), "</a>");
                }

                bool includeNavbar = previousChildLink != ""
                  || nextChildLink != ""
                  || includeIndexLink
                  || frontPageLink != ""
                  || mainSiteLink != "";

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
                        f.WriteLine(string.Concat("<li><a href=\"individuals1.html\">", fLangMan.LS(PLS.LSID_Index), "</a></li>"));
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
                                            ref Rectangle rectArea, Stats stats)
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
                    asidFilename = string.Concat(fullFilename, ".", rectArea.X.ToString(), ",", rectArea.Y.ToString(), ",", rectArea.Width.ToString(), ",", rectArea.Height.ToString());
                }

                if (maxWidth != 0 && maxHeight != 0) {
                    asidFilename = string.Concat(asidFilename, "(", maxWidth.ToString(), "x", maxHeight.ToString(), ")");
                }

                if (fullFilename != null && !string.IsNullOrEmpty(GMConfig.Instance.OutputFolder)) {
                    // Have we already copied the sFilename?
                    if (fCopiedFiles.ContainsKey(asidFilename)) {
                        var filenameAndSize = fCopiedFiles[asidFilename];
                        result = filenameAndSize.FileName;
                        rectArea.Width = filenameAndSize.Width;
                        rectArea.Height = filenameAndSize.Height;
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
                                copyFilename = ConvertAndCropImage(outputFolder, copyFilename, ref rectArea, maxWidth, maxHeight);
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

        // Returns a string to use as a sFilename for this individual's HTML page.
        // The string is just the sFilename, not a fully qualified path.
        protected static string GetIndividualHTMLFilename(GDMIndividualRecord ir)
        {
            string relativeFilename = string.Concat("indi", ir.XRef, ".html");
            if (GMConfig.Instance.UserRecFilename && ir.HasUserReferences) {
                GDMUserReference urn = ir.UserReferences[0];
                string filenameUserRef = EscapeFilename(urn.StringValue);
                if (filenameUserRef.Length > 0) {
                    relativeFilename = string.Concat("indi", filenameUserRef, ".html");
                }
            }
            return relativeFilename;
        }

        // Crops the specified image file to the given size. Also converts non-standard formats to standard ones.
        // Returns sFilename in case extension has changed.
        // sArea is changed to reflect new image size
        private static string ConvertAndCropImage(string folder, string fileName, ref Rectangle rectArea, int maxWidth, int maxHeight)
        {
            fLogger.WriteInfo(string.Format("ConvertAndCropImage( {0}, {1} )", folder != null ? folder : "null", fileName != null ? fileName : "null"));

            string absFilename = string.Concat(folder, fileName);

            Image image = null;
            try {
                image = Image.FromFile(absFilename);
            } catch (OutOfMemoryException) {
                // Image is not a GDI compatible format
                image = null;
            }

            if (image == null) {
                throw (new HTMLException("Unknown image format for file " + absFilename)); // Let caller sort it out.
            }

            Rectangle rectNewArea;
            if (rectArea.Width <= 0 || rectArea.Height <= 0) {
                SizeF s = image.PhysicalDimension;
                if (s.Width <= maxWidth && s.Height <= maxHeight) {
                    maxWidth = (int)s.Width;
                    maxHeight = (int)s.Height;
                    // Nothing needs to be done, bitmap already correct size.
                    // Carry on with conversion.
                }
                rectNewArea = new Rectangle(0, 0, (int)s.Width, (int)s.Height);
                rectArea.X = 0;
                rectArea.Y = 0;
                rectArea.Width = rectNewArea.Width;
                rectArea.Height = rectNewArea.Height;
            } else {
                rectNewArea = new Rectangle(0, 0, rectArea.Width, rectArea.Height);
            }

            if (maxWidth != 0 && maxHeight != 0) {
                // If image is too big then shrink it. (Can't always use GetThumbnailImage because that might use embedded thumbnail).
                ScaleAreaToFit(ref rectNewArea, maxWidth, maxHeight);
            }

            Bitmap bitmapNew = new Bitmap(rectNewArea.Width, rectNewArea.Height, PixelFormat.Format24bppRgb);
            Graphics graphicsNew = Graphics.FromImage(bitmapNew);

            graphicsNew.DrawImage(image, rectNewArea, rectArea, GraphicsUnit.Pixel);
            image.Dispose();

            // Find which format to save in. TODO: There must be a more elegant way!!
            string extn = Path.GetExtension(fileName);
            string filepart = Path.GetDirectoryName(fileName);
            filepart += "\\" + Path.GetFileNameWithoutExtension(fileName);
            ImageFormat imageFormat;
            switch (extn.ToLower()) {
                case ".jpg":
                case ".jpeg":
                    extn = ".jpg";
                    imageFormat = ImageFormat.Jpeg;
                    break;
                case ".gif":
                    imageFormat = ImageFormat.Gif;
                    break;
                case ".bmp":
                    imageFormat = ImageFormat.Bmp;
                    break;
                case ".tif":
                case ".tiff":
                    // Tif's don't display in browsers, so convert to png.
                    imageFormat = ImageFormat.Png;
                    extn = ".png";
                    break;
                case ".exif":
                    imageFormat = ImageFormat.Exif;
                    break;
                case ".png":
                    imageFormat = ImageFormat.Png;
                    break;
                default:
                    imageFormat = ImageFormat.Jpeg;
                    break;
            }

            string filenameNew = filepart + extn;
            string absFilenameNew = string.Concat(folder, filenameNew);
            try {
                if (File.Exists(absFilename)) {
                    // Delete the old file (e.g. if converting from tif to png)
                    File.Delete(absFilename);
                }
            } catch (Exception e) {
                fLogger.WriteError(string.Format("Caught exception while removing old bitmap file {0}", absFilename), e);
            }
            try {
                if (File.Exists(absFilenameNew)) {
                    // Delete any existing file
                    File.SetAttributes(absFilenameNew, FileAttributes.Normal);
                    File.Delete(absFilenameNew);
                }
                bitmapNew.Save(absFilenameNew, imageFormat);
            } catch (Exception e) {
                fLogger.WriteError(string.Format("Caught exception while writing bitmap file {0}", filenameNew), e);
                filenameNew = "";
            }
            graphicsNew.Dispose();
            bitmapNew.Dispose();

            rectArea = rectNewArea;
            return filenameNew;
        }

        // Modifies rectNew to fit within the limits given, keeping its aspect ratio
        protected static void ScaleAreaToFit(ref Rectangle rectangle, int maxWidth, int maxHeight)
        {
            if (rectangle.Height > maxHeight) {
                // Image won't fit horizontally, so scale in both directions til it will
                rectangle.Width = (rectangle.Width * maxHeight) / rectangle.Height;
                rectangle.Height = maxHeight;
            }

            if (rectangle.Width > maxWidth) {
                // Image won't fit horizontally, so scale in both directions til it will
                rectangle.Height = (rectangle.Height * maxWidth) / rectangle.Width;
                rectangle.Width = maxWidth;
            }
        }

        protected static string MakeLinkNumber(GDMSourceCitation sourCit, int sourceCount, bool hasComma)
        {
            string comma = hasComma ? "," : "";
            return string.Concat("<span class=\"reference\">", comma, sourceCount.ToString(), "</span>");
        }

        // Returns a string to use in the list of references at the bottom of the page
        protected static string MakeLinkText(GDMTree tree, GDMSourceCitation sourCit, int sourceCount)
        {
            var sourRec = tree.GetPtrValue<GDMSourceRecord>(sourCit);
            return string.Concat(sourceCount.ToString(), ". ", /*m_sSourceDescription*/sourRec.ShortTitle);
        }

        // Returns the name of the alternative picture file to display for non-diaplayable files of the given format
        protected static string NonPicFilename(string format, bool small, bool clickToDownload)
        {
            string filename;
            switch (format.ToLower()) {
                case "wav":
                case "mp3":
                case "mid":
                case "midi":
                case "rmi":
                case "au":
                case "wma":
                    filename = small ? "gmaudio_sm.png" : clickToDownload ? "gmaudio.png" : "gmaudion.png";
                    break;
                case "avi":
                case "mpeg":
                case "mpg":
                case "wmv":
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
