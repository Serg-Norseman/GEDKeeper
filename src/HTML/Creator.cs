/* CCreator.cs
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
using System.IO;
using GEDmill.Exceptions;
using GKCommon.GEDCOM;
using System.Text;
using GKCore.Logging;

namespace GEDmill.HTML
{
    /// <summary>
    /// Base class providing general functionality required by all classes that create HTML pages.
    /// </summary>
    public abstract class Creator
    {
        private static readonly ILogger fLogger = LogManager.GetLogger(CConfig.LOG_FILE, CConfig.LOG_LEVEL, typeof(Creator).Name);

        // The raw data that we are turning into a website.
        protected GEDCOMTree fTree;

        // Pointer to the window showing the progress bar, so that web page creation progress can be shown to user.
        private IProgressCallback fProgressWindow;

        // The same multimedia file may be referenced multiple times. 
        // This hash prevents it being copied to the output directory more than once.
        private static Hashtable fCopiedFiles = new Hashtable();

        // The sFilename for the Valid XHTML sticker image.
        private string fW3CFile;


        protected Creator(GEDCOMTree gedcom, IProgressCallback progress, string sW3cfile)
        {
            fTree = gedcom;
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
        // TODO: Surely there is a .Net function to do this?
        // TODO: Might want to preserve <a> links in the HTML in case user has specified them in their data.
        protected static string EscapeHTML(string original, bool hardSpace)
        {
            fLogger.WriteInfo(String.Format("EscapeHTML({0})", original));

            uint tabSpaces = MainForm.Config.TabSpaces;

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
            fLogger.WriteInfo(String.Format("EscapeFilename({0})", original));

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

        // Modifies the provided string to have its first letter capitalised and the rest unchanged.
        public static void Capitalise(ref string s)
        {
            if (!string.IsNullOrEmpty(s)) {
                s = Char.ToUpper(s[0]) + s.Substring(1);
            } else {
                s = "";
            }
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
        protected static void OutputPageHeader(StreamWriter sw, string previousChildLink, string nextChildLink,
                                               bool includeIndexLink, bool includeHelpLink)
        {
            if (MainForm.Config.IncludeNavbar) {
                string frontPageLink = "";
                if (MainForm.Config.FrontPageFilename != "") {
                    frontPageLink += String.Concat("<a href=\"", MainForm.Config.FrontPageFilename, ".", MainForm.Config.HtmlExtension, "\">front page</a>");
                }
                string mainSiteLink = "";
                if (MainForm.Config.MainWebsiteLink != "") {
                    mainSiteLink += String.Concat("<a href=\"", MainForm.Config.MainWebsiteLink, "\">main site</a>");
                }
                string helpPageLink = "";
                if (MainForm.Config.IncludeHelpPage) {
                    helpPageLink += String.Concat("<a href=\"help.", MainForm.Config.HtmlExtension, "\">help</a>");
                }
                bool includeNavbar = previousChildLink != ""
                  || nextChildLink != ""
                  || includeIndexLink
                  || frontPageLink != ""
                  || mainSiteLink != ""
                  || helpPageLink != "";

                if (includeNavbar) {
                    sw.WriteLine("    <div id=\"header\">");
                    sw.WriteLine("      <ul>");

                    if (previousChildLink != "") {
                        sw.WriteLine(String.Concat("        <li>", previousChildLink, "</li>"));
                    }

                    if (nextChildLink != "") {
                        sw.WriteLine(String.Concat("        <li>", nextChildLink, "</li>"));
                    }

                    if (includeIndexLink) {
                        sw.WriteLine(String.Concat("        <li><a href=\"individuals1.",
                          MainForm.Config.HtmlExtension,
                          "\">index</a></li>"));
                    }

                    if (frontPageLink != "") {
                        sw.WriteLine(String.Concat("        <li>", frontPageLink, "</li>"));
                    }

                    if (mainSiteLink != "") {
                        sw.WriteLine(String.Concat("        <li>", mainSiteLink, "</li>"));
                    }

                    if (includeHelpLink && helpPageLink != "") {
                        sw.WriteLine(String.Concat("        <li>", helpPageLink, "</li>"));
                    }

                    sw.WriteLine("      </ul>");
                    sw.WriteLine("    </div> <!-- header -->");
                    sw.WriteLine("");
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
            fLogger.WriteInfo(String.Format("CopyMultimedia( {0}, {1}, {2} )", fullFilename, maxWidth, maxHeight));

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
                    asidFilename = String.Concat(fullFilename, ".", rectArea.X.ToString(), ",", rectArea.Y.ToString(), ",", rectArea.Width.ToString(), ",", rectArea.Height.ToString());
                }

                if (maxWidth != 0 && maxHeight != 0) {
                    asidFilename = String.Concat(asidFilename, "(", maxWidth.ToString(), "x", maxHeight.ToString(), ")");
                }

                if (fullFilename != null && MainForm.Config.OutputFolder != null && MainForm.Config.OutputFolder != "") {
                    // Have we already copied the sFilename?
                    if (fCopiedFiles.ContainsKey(asidFilename)) {
                        FilenameAndSize filenameAndSize = (FilenameAndSize)fCopiedFiles[asidFilename];
                        result = filenameAndSize.FileName;
                        rectArea.Width = filenameAndSize.Width;
                        rectArea.Height = filenameAndSize.Height;
                    } else {
                        // Copy file into output directory
                        if (MainForm.Config.CopyMultimedia) {
                            string imageFolder = MainForm.Config.ImageFolder;
                            string outputFolder = MainForm.Config.OutputFolder;

                            if (imageFolder != "") {
                                imageFolder = imageFolder + '\\';
                            }
                            if (outputFolder != "") {
                                outputFolder = outputFolder + '\\';
                            }

                            string copyFilename = String.Concat(imageFolder, newFilename);
                            string absImageFolder = String.Concat(outputFolder, imageFolder);
                            string absCopyFilename = String.Concat(absImageFolder, newFilename);

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
                                if (MainForm.Config.RenameMultimedia == false) {
                                    uint nCopyPlus = uCopy + 2;
                                    copyFilename = String.Concat(imageFolder, filePart, "-", nCopyPlus.ToString(), extnPart);
                                } else if (uCopy >= sAdditionalLetters.Length) {
                                    // Once all the extra letters have been used up, put number as "-n", where n starts from 2.
                                    uint nCopyMinus = uCopy - (uint)(sAdditionalLetters.Length - 2);
                                    copyFilename = String.Concat(imageFolder, filePart, "-", nCopyMinus.ToString(), extnPart);
                                } else {
                                    copyFilename = String.Concat(imageFolder, filePart, sAdditionalLetters[(int)uCopy], extnPart);
                                }
                                uCopy++;

                                absCopyFilename = String.Concat(outputFolder, copyFilename);
                            }

                            fLogger.WriteInfo(String.Format("Copying \"{0}\" to \"{1}\"", fullFilename, absCopyFilename));

                            File.Copy(fullFilename, absCopyFilename, true);

                            File.SetAttributes(fullFilename, System.IO.FileAttributes.Normal); // Make any Read-Only files read-write.
                            if (maxWidth != 0 && maxHeight != 0) {
                                // It must be a picture file
                                copyFilename = ConvertAndCropImage(outputFolder, copyFilename, ref rectArea, maxWidth, maxHeight);
                            }
                            fCopiedFiles[asidFilename] = new FilenameAndSize(copyFilename, rectArea.Width, rectArea.Height);
                            result = copyFilename;
                        } else {
                            if (MainForm.Config.RelativiseMultimedia) {
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
            f.Writer.WriteLine("<p class=\"plain\">");
            f.Writer.WriteLine("<a href=\"http://validator.w3.org/check?uri=referer\"><img");
            f.Writer.WriteLine("src=\"" + fW3CFile + "\"");
            f.Writer.WriteLine("style=\"margin-top:4px\"");
            f.Writer.WriteLine("alt=\"Valid XHTML 1.0 Strict\" height=\"31\" width=\"88\" /></a>");
            f.Writer.WriteLine("</p>");
        }

        // Creates link HTML for the individual e.g. <a href="indiI1.html">Fred Bloggs</a>
        protected static string MakeLink(GEDCOMIndividualRecord ir)
        {
            string name = ir.GetPrimaryFullName();
            string dummy = "";
            if (name == "") {
                name = MainForm.Config.UnknownName;
            } else if (!ir.GetVisibility() && !MainForm.Config.UseWithheldNames) {
                name = MainForm.Config.ConcealedName;
            } else {
                name = MainForm.Config.CapitaliseName(name, ref dummy, ref dummy);
            }
            return MakeLink(ir, name);
        }

        // Creates link HTML for the individual e.g. <a href="indiI1.html">Next Child</a>. Uses name provided by caller.
        protected static string MakeLink(GEDCOMIndividualRecord ir, string name)
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
        protected static string GetIndividualHTMLFilename(GEDCOMIndividualRecord ir)
        {
            string relativeFilename = string.Concat("indi", ir.XRef, ".", MainForm.Config.HtmlExtension);
            if (MainForm.Config.UserRecFilename) {
                if (ir.UserReferences.Count > 0) {
                    GEDCOMUserReference urn = ir.UserReferences[0];
                    string filenameUserRef = EscapeFilename(urn.StringValue);
                    if (filenameUserRef.Length > 0) {
                        relativeFilename = string.Concat("indi", filenameUserRef, ".", MainForm.Config.HtmlExtension);
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
            fLogger.WriteInfo(String.Format("ConvertAndCropImage( {0}, {1} )", folder != null ? folder : "null", fileName != null ? fileName : "null"));

            string absFilename = String.Concat(folder, fileName);

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
                MainForm.ScaleAreaToFit(ref rectNewArea, maxWidth, maxHeight);
            }

            Bitmap bitmapNew = new Bitmap(rectNewArea.Width, rectNewArea.Height, System.Drawing.Imaging.PixelFormat.Format24bppRgb);
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
                    imageFormat = System.Drawing.Imaging.ImageFormat.Jpeg;
                    break;
                case ".gif":
                    imageFormat = System.Drawing.Imaging.ImageFormat.Gif;
                    break;
                case ".bmp":
                    imageFormat = System.Drawing.Imaging.ImageFormat.Bmp;
                    break;
                case ".tif":
                case ".tiff":
                    // Tif's don't display in browsers, so convert to png.
                    imageFormat = System.Drawing.Imaging.ImageFormat.Png;
                    sExtn = ".png";
                    break;
                case ".exif":
                    imageFormat = System.Drawing.Imaging.ImageFormat.Exif;
                    break;
                case ".png":
                    imageFormat = System.Drawing.Imaging.ImageFormat.Png;
                    break;
                default:
                    imageFormat = System.Drawing.Imaging.ImageFormat.Jpeg;
                    break;
            }

            string filenameNew = sFilepart + sExtn;
            string absFilenameNew = String.Concat(folder, filenameNew);
            try {
                if (File.Exists(absFilename)) {
                    // Delete the old file (e.g. if converting from tif to png)
                    File.Delete(absFilename);
                }
            } catch (Exception e) {
                fLogger.WriteError(string.Format("Caught exception while removing old bitmap file {0} : {1}", absFilename), e);
            }
            try {
                if (File.Exists(absFilenameNew)) {
                    // Delete any existing file
                    File.SetAttributes(absFilenameNew, FileAttributes.Normal);
                    File.Delete(absFilenameNew);
                }
                bitmapNew.Save(absFilenameNew, imageFormat);
            } catch (Exception e) {
                fLogger.WriteError(string.Format("Caught exception while writing bitmap file {0} : {1}", filenameNew), e);
                filenameNew = "";
            }
            graphicsNew.Dispose();
            bitmapNew.Dispose();

            rectArea = rectNewArea;
            return filenameNew;
        }
    }
}
