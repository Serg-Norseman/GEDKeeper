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
using GEDmill.Exceptions;
using GEDmill.Model;
using GKCore.Logging;

namespace GEDmill.HTML
{
    /// <summary>
    /// Base class providing general functionality required by all classes that create HTML pages.
    /// </summary>
    public abstract class Creator
    {
        private static readonly ILogger fLogger = LogManager.GetLogger(CConfig.LOG_FILE, CConfig.LOG_LEVEL, typeof(Creator).Name);

        protected const string PageDescription = "GEDmill GEDCOM to HTML family history website";

        // The raw data that we are turning into a website.
        protected GDMTree fTree;

        // Pointer to the window showing the progress bar, so that web page creation progress can be shown to user.
        private IProgressCallback fProgressWindow;

        // The same multimedia file may be referenced multiple times. 
        // This hash prevents it being copied to the output directory more than once.
        private static Dictionary<string, FilenameAndSize> fCopiedFiles = new Dictionary<string, FilenameAndSize>();

        // The sFilename for the Valid XHTML sticker image.
        private string fW3CFile;


        protected Creator(GDMTree tree, IProgressCallback progress, string sW3cfile)
        {
            fTree = tree;
            fProgressWindow = progress;
            fW3CFile = sW3cfile;
        }

        // This clears the static list of all multimedia files copied to the output directory (and possibly renamed).
        public static void ClearCopiedFilesList()
        {
            fCopiedFiles.Clear();
        }

        // Converts all HTML characters into their escaped versions
        // Set bHardSpace to false if you want to keep first space as breakable, true if you want all nbsp.
        // TODO: Surely there is a .Net function to do this (WebUtility.HtmlEncode)?
        // TODO: Might want to preserve <a> links in the HTML in case user has specified them in their data.
        public static string EscapeHTML(string original, bool hardSpace)
        {
            fLogger.WriteInfo(string.Format("EscapeHTML({0})", original));

            uint tabSpaces = CConfig.Instance.TabSpaces;

            if (original == null) {
                return "&lt;null&gt;";
            }

            StringBuilder sb = new StringBuilder(original.Length);
            uint uTabPos = 0;
            bool bDoneCRLF = false;
            bool bDoneSpace = false;
            int nLength = original.Length;
            int n = 0;
            foreach (char c in original) {
                switch (c) {
                    case (char)0x91:
                    case (char)0x92:
                        sb.Append("'");
                        bDoneCRLF = false;
                        bDoneSpace = false;
                        uTabPos++;
                        break;
                    case (char)0x93:
                    case (char)0x94:
                        sb.Append("\"");
                        bDoneCRLF = false;
                        bDoneSpace = false;
                        uTabPos++;
                        break;
                    case '<':
                        sb.Append("&lt;");
                        bDoneCRLF = false;
                        bDoneSpace = false;
                        uTabPos++;
                        break;
                    case '>':
                        sb.Append("&gt;");
                        bDoneCRLF = false;
                        bDoneSpace = false;
                        uTabPos++;
                        break;
                    case '\"':
                        sb.Append("&quot;");
                        bDoneCRLF = false;
                        bDoneSpace = false;
                        uTabPos++;
                        break;
                    case '&':
                        sb.Append("&amp;");
                        bDoneCRLF = false;
                        bDoneSpace = false;
                        uTabPos++;
                        break;
                    case ' ':
                        if (bDoneSpace || hardSpace) {
                            sb.Append("&nbsp;");
                        } else {
                            sb.Append(' ');
                            bDoneSpace = true;
                        }
                        bDoneCRLF = false;
                        uTabPos++;
                        break;
                    case '\n':
                        if (!bDoneCRLF) {
                            sb.Append("<br />");
                        }
                        bDoneCRLF = false; // To allow multiple CRLFs to produce multiple <BR />s
                        bDoneSpace = false;
                        uTabPos = 0;
                        break;
                    case '\r':
                        if (!bDoneCRLF) {
                            sb.Append("<br />");
                            bDoneCRLF = true;
                        }
                        bDoneSpace = false;
                        uTabPos = 0;
                        break;
                    case '\t':
                        do {
                            sb.Append("&nbsp;");
                            uTabPos++;
                        }
                        while ((uTabPos % tabSpaces) != 0);
                        bDoneSpace = true;
                        break;

                    default:
                        sb.Append(c);
                        bDoneCRLF = false;
                        bDoneSpace = false;
                        uTabPos++;
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

            StringBuilder sb = new StringBuilder(original.Length);

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

            const string sValidChars = " 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz$%'`-@{}~!#()&_^";

            StringBuilder sb = new StringBuilder(original.Length);
            int nLength = original.Length;
            int n = 0;
            foreach (char c in original) {
                char cc = c;
                if (sValidChars.IndexOf(c) < 0) {
                    cc = '_';
                }
                sb.Append(cc);
                ++n;
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
            StringBuilder sb = new StringBuilder(nLength);
            int i = 0;
            int nNameStart = -1;
            int nState = 0;
            const string sInvalidNameChars = ",\"£$^&*()+=]}[{':;,<>?/\\|`¬#~";
            const string sReplacement = "<email address>";

            while (i < nLength) {
                char c = text[i];

                switch (nState) {
                    case 0:
                        // Not seen anything special.
                        if (!char.IsWhiteSpace(c) && c != '@' && c != '.' && sInvalidNameChars.IndexOf(c) < 0) {
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
                        } else if (!char.IsWhiteSpace(c) && sInvalidNameChars.IndexOf(c) < 0) {
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
                        if (!char.IsWhiteSpace(c) && c != '@' && c != '.' && sInvalidNameChars.IndexOf(c) < 0) {
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
                        } else if (!char.IsWhiteSpace(c) && c != '@' && sInvalidNameChars.IndexOf(c) < 0) {
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
                        if (!char.IsWhiteSpace(c) && c != '@' && c != '.' && sInvalidNameChars.IndexOf(c) < 0) {
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
                        } else if (!char.IsWhiteSpace(c) && c != '@' && sInvalidNameChars.IndexOf(c) < 0) {
                            // Valid domain name. Keep looking for dot or end.
                        } else if (c != '@') {
                            // Found complete email address
                            sb.Append(sReplacement);
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
                        if (!char.IsWhiteSpace(c) && c != '@' && c != '.' && sInvalidNameChars.IndexOf(c) < 0) {
                            // A valid domain char. Look for another dot , or end.
                            nState = 5;
                        } else {
                            // Found complete email address (ending in a full-stop).
                            sb.Append(sReplacement);
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
                sb.Append(sReplacement);
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
        protected static void OutputPageHeader(HTMLFile f, string previousChildLink, string nextChildLink,
                                               bool includeIndexLink, bool includeHelpLink)
        {
            if (CConfig.Instance.IncludeNavbar) {
                string frontPageLink = "";
                if (CConfig.Instance.FrontPageFilename != "") {
                    frontPageLink += string.Concat("<a href=\"", CConfig.Instance.FrontPageFilename, ".", CConfig.Instance.HtmlExtension, "\">front page</a>");
                }
                string mainSiteLink = "";
                if (CConfig.Instance.MainWebsiteLink != "") {
                    mainSiteLink += string.Concat("<a href=\"", CConfig.Instance.MainWebsiteLink, "\">main site</a>");
                }
                string helpPageLink = "";
                if (CConfig.Instance.IncludeHelpPage) {
                    helpPageLink += string.Concat("<a href=\"help.", CConfig.Instance.HtmlExtension, "\">help</a>");
                }
                bool includeNavbar = previousChildLink != ""
                  || nextChildLink != ""
                  || includeIndexLink
                  || frontPageLink != ""
                  || mainSiteLink != ""
                  || helpPageLink != "";

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
                        f.WriteLine(string.Concat("<li><a href=\"individuals1.", CConfig.Instance.HtmlExtension, "\">index</a></li>"));
                    }

                    if (frontPageLink != "") {
                        f.WriteLine("<li>{0}</li>", frontPageLink);
                    }

                    if (mainSiteLink != "") {
                        f.WriteLine("<li>{0}</li>", mainSiteLink);
                    }

                    if (includeHelpLink && helpPageLink != "") {
                        f.WriteLine("<li>{0}</li>", helpPageLink);
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
        public static string CopyMultimedia(string fullFilename, string newFilename, uint maxWidth, uint maxHeight,
                                            ref Rectangle rectArea, Stats stats)
        {
            fLogger.WriteInfo(string.Format("CopyMultimedia( {0}, {1}, {2} )", fullFilename, maxWidth, maxHeight));

            if (!File.Exists(fullFilename)) {
                return "";
            }

            string result = fullFilename;

            if (newFilename == "") {
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

                if (fullFilename != null && CConfig.Instance.OutputFolder != null && CConfig.Instance.OutputFolder != "") {
                    // Have we already copied the sFilename?
                    if (fCopiedFiles.ContainsKey(asidFilename)) {
                        FilenameAndSize filenameAndSize = (FilenameAndSize)fCopiedFiles[asidFilename];
                        result = filenameAndSize.FileName;
                        rectArea.Width = filenameAndSize.Width;
                        rectArea.Height = filenameAndSize.Height;
                    } else {
                        // Copy file into output directory
                        if (CConfig.Instance.CopyMultimedia) {
                            string imageFolder = CConfig.Instance.ImageFolder;
                            string outputFolder = CConfig.Instance.OutputFolder;

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
                                if (CConfig.Instance.RenameMultimedia == false) {
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
                            if (CConfig.Instance.RelativiseMultimedia) {
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

        // Outputs the HTML to display the W3C Valid XHTML image on the page.
        protected void OutputValiditySticker(HTMLFile f)
        {
            f.WriteLine("<p class=\"plain\">");
            f.WriteLine("<a href=\"http://validator.w3.org/check?uri=referer\"><img");
            f.WriteLine("src=\"" + fW3CFile + "\"");
            f.WriteLine("style=\"margin-top:4px\"");
            f.WriteLine("alt=\"Valid XHTML 1.0 Strict\" height=\"31\" width=\"88\" /></a>");
            f.WriteLine("</p>");
        }

        // Creates link HTML for the individual e.g. <a href="indiI1.html">Fred Bloggs</a>
        protected static string MakeLink(GDMIndividualRecord ir)
        {
            string name = ir.GetPrimaryFullName();
            string dummy = "";
            if (name == "") {
                name = CConfig.Instance.UnknownName;
            } else if (!ir.GetVisibility() && !CConfig.Instance.UseWithheldNames) {
                name = CConfig.Instance.ConcealedName;
            } else {
                name = CConfig.Instance.CapitaliseName(name, ref dummy, ref dummy);
            }
            return MakeLink(ir, name);
        }

        // Creates link HTML for the individual e.g. <a href="indiI1.html">Next Child</a>. Uses name provided by caller.
        protected static string MakeLink(GDMIndividualRecord ir, string name)
        {
            string link;
            if (!ir.GetVisibility()) {
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
            string relativeFilename = string.Concat("indi", ir.XRef, ".", CConfig.Instance.HtmlExtension);
            if (CConfig.Instance.UserRecFilename) {
                if (ir.UserReferences.Count > 0) {
                    GDMUserReference urn = ir.UserReferences[0];
                    string filenameUserRef = EscapeFilename(urn.StringValue);
                    if (filenameUserRef.Length > 0) {
                        relativeFilename = string.Concat("indi", filenameUserRef, ".", CConfig.Instance.HtmlExtension);
                    }
                }
            }
            return relativeFilename;
        }

        // Crops the specified image file to the given size. Also converts non-standard formats to standard ones.
        // Returns sFilename in case extension has changed.
        // sArea is changed to reflect new image size
        private static string ConvertAndCropImage(string folder, string fileName, ref Rectangle rectArea, uint maxWidth, uint maxHeight)
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
                    maxWidth = (uint)s.Width;
                    maxHeight = (uint)s.Height;
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
                GMHelper.ScaleAreaToFit(ref rectNewArea, maxWidth, maxHeight);
            }

            Bitmap bitmapNew = new Bitmap(rectNewArea.Width, rectNewArea.Height, PixelFormat.Format24bppRgb);
            Graphics graphicsNew = Graphics.FromImage(bitmapNew);

            graphicsNew.DrawImage(image, rectNewArea, rectArea, GraphicsUnit.Pixel);
            image.Dispose();

            // Find which format to save in. TODO: There must be a more elegant way!!
            string sExtn = Path.GetExtension(fileName);
            string sFilepart = Path.GetDirectoryName(fileName);
            sFilepart += "\\" + Path.GetFileNameWithoutExtension(fileName);
            System.Drawing.Imaging.ImageFormat imageFormat;
            switch (sExtn.ToLower()) {
                case ".jpg":
                case ".jpeg":
                    sExtn = ".jpg";
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
                    sExtn = ".png";
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

            string filenameNew = sFilepart + sExtn;
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
    }
}
