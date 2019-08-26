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
using System.IO.IsolatedStorage;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;
using GKCore.Logging;

namespace GEDmill
{
    /// <summary>
    /// Class that contains all the user configurable settings, and serialises them into IsolatedStorage at program start and exit.
    /// Note not all settings here are presented through the UI, but most are. Originally there was a distinction her but it has become blurred.
    /// </summary>
    public class CConfig
    {
        public const string LOG_FILE = "GEDmill.log";
        public const string LOG_LEVEL = "WARN";

        // The current version of this app, for display purposes.
        public static string SoftwareVersion = "1.11.0";

        // The name of the app for display purposes
        public static string SoftwareName = "GEDmill " + SoftwareVersion;

        // Filename for the online help (as in "on the same system", as opposed to offline e.g. printed manual)
        public static string HelpFilename = "GEDmill Help.chm";

        private static readonly ILogger fLogger = LogManager.GetLogger(CConfig.LOG_FILE, CConfig.LOG_LEVEL, typeof(CConfig).Name);

        // Filename used to store users config in isolated storage.
        public string ConfigFilename;

        // Name to use where no other name is available
        public string UnknownName;

        // How to capitalise individuals' names
        public int NameCapitalisation;

        // File extension for html files
        public string HtmlExtension;

        // Absolute sFilename of image to use as background
        public string BackgroundImage;

        // Maximum allowed width of individual image
        public uint MaxImageWidth;

        // Maximum allowed height of individual image
        public uint MaxImageHeight;

        // Age at which defining occupation is chosen
        public int AgeForOccupation;

        // Maximum allowed width of source image
        public uint MaxSourceImageWidth;

        // Maximum allowed height of source image
        public uint MaxSourceImageHeight;

        // Maximum allowed width of thumbnail image
        public uint MaxThumbnailImageWidth;

        // Maximum allowed height of thumbnail image
        public uint MaxThumbnailImageHeight;

        // Filename of image if any to use on front webpage.
        public string FrontPageImageFilename;

        // Filename of front webpage.
        public string FrontPageFilename;

        // Number of spaces between each tab stop, when converting \t characters to html.
        public uint TabSpaces;

        // True if event descriptions to start with capital letter, e.g. "Born" as opposed to "born".
        public bool CapitaliseEventDescriptions;

        // True if to include text about website statistics on front page.
        public bool ShowFrontPageStats;

        // Extra text to include on title page.
        public string CommentaryText;

        // Name of font to render tree diagrams with
        public string TreeFontName;

        // Size of font to render tree diagrams with
        public float TreeFontSize;

        // Width to try not to exceed when creating mini tree diagrams
        public int TargetTreeWidth;

        // True if mini tree diagrams are to be added to pages
        public bool ShowMiniTrees;

        // User's email address to include on webpages
        public string UserEmailAddress;

        // Allow multiple multimedia files per individual. Enables m_nMaxNumberMultimediaFiles.
        public bool AllowMultipleImages;

        // If true will not overwrite existing front page file.
        public bool PreserveFrontPage;

        // If true will not overwrite existing style sheet css file.
        public bool PreserveStylesheet;

        // Whether to create autorun.inf and autoplay.exe files
        public bool CreateCDROMFiles;

        // Whether to include files that aren't pictures as multimedia objects
        public bool AllowNonPictures;

        // Link to users main website, put at top of each indi page.
        public string MainWebsiteLink;

        // Name to use for stylesheet css file.
        public string StylesheetFilename;

        // Name to user for individuals who appear but have their information withheld
        public string ConcealedName;

        // Word used to prefix locations, e.g. "at" or "in".
        public string PlaceWord;

        // String to use (in index) for names with no surname
        public string NoSurname;

        // True if all sources mentioned by a restricted individual should also be restricted (i.e. omitted from website)
        public bool RestrictAssociatedSources;

        // True if all multimedia files are to be renamed to a standard naming system. Now generally superceded by m_renameOriginalPicture
        public bool RenameMultimedia;

        // True to generate a separate html page for each letter of names index. Useful if index would otherwise be massive.
        public bool IndexLetterPerPage;

        // HTML string representing colour to draw connecting lines in mini trees
        public string MiniTreeColourBranch;

        // HTML string representing colour to draw outline of individual boxes in mini trees
        public string MiniTreeColourIndiBorder;

        // HTML string representing colour to fill normal individual boxes in mini trees
        public string MiniTreeColourIndiBackground;

        // HTML string representing colour to fill selected individual boxes in mini trees
        public string MiniTreeColourIndiHighlight;

        // HTML string representing colour to fill boxes for individuals marked as concealed in mini trees.
        public string MiniTreeColourIndiBgConcealed;

        // HTML string representing colour to write text for individuals marked as concealed in mini trees.
        public string MiniTreeColourIndiFgConcealed;

        // HTML string representing colour to fill shaded individual boxes in mini trees
        public string MiniTreeColourIndiShade;

        // HTML string representing colour to draw text in individual boxes in mini trees
        public string MiniTreeColourIndiText;

        // HTML string representing colour to draw linkable text in individual boxes in mini trees
        public string MiniTreeColourIndiLink;

        // HTML string representing colour to fill entire background of mini tree diagrams
        public string MiniTreeColourBackground;

        // Whether to restrict records with RESN set to confidential
        public bool RestrictConfidential;

        // Whether to restrict records with RESN set to privacy
        public bool RestrictPrivacy;

        // If true, multimedia files are copied into m_imageFolder.
        public bool CopyMultimedia;

        // Which folder to copy multimedia into. Ends up as subdirectory to m_outputFolder;
        public string ImageFolder;

        // If true, links to multimedia are altered to be relative to output folder (e.g. ..\..\files\file.zzz). If false, they are left as absolute (e.g. D:\files\file.zzz)
        public bool RelativiseMultimedia;

        // Maximum number of multimedia files per individual page
        public int MaxNumberMultimediaFiles;

        // Title to use for index page
        public string IndexTitle;

        // Folder in which to dump all the html output
        public string OutputFolder;

        // Which version number of the software this config file is for
        public string Version;

        // Name of owner used in titles, descriptions of pages.
        public string OwnersName;

        // Xref of root individual in tree, for link on front page.
        public string FirstRecordXRef;

        // Title to put on front page of website
        public string SiteTitle;

        // Filename of gedcom file to parse
        public string InputFilename;

        // Path to folder containing this application.
        public string ApplicationPath;

        // Address of remote FTP server in which to put the files
        public string FtpServer;

        // Username on remote server
        public string FtpUsername;

        // Password for remote server
        public string FtpPassword;

        // Folder on remote server in which to put the files
        public string FtpUploadFolder;

        // String indicating image format to use to store mini tree file
        public string MiniTreeImageFormat;

        // If true, the background image will be used to fill in the background of mini trees, giving the effect that they are transparent.
        public bool FakeMiniTreeTransparency;

        // List of Xrefs for individuals to be mentioned on front page.
        public List<string> KeyIndividuals;

        // Set true if indexes are allowed to span multiple html files
        public bool MultiPageIndexes;

        // Number of individuals to aim to list per index page.
        public uint IndividualsPerIndexPage;

        // If true, website pages are opened in web browser once app exits with Finish button.
        public bool OpenWebsiteOnExit;

        // If true, instead of pretending restricted individuals don't exist, they are shown as black boxes.
        public bool OnlyConceal;

        // If true, clicking the picture will link to the original large picture.
        public bool LinkOriginalPicture;

        // If true, the original picture will be renamed as it is copied. Now takes on role of m_renameMultimedia.
        public bool RenameOriginalPicture;

        // Directory in which list of excluded individuals is stored
        public string ExcludeFileDir;

        // Name of file in which list of excluded individuals is stored
        public string ExcludeFileName;

        // Directory in which pictures were added from for an individual
        public string LastPictureAddedDir;

        // If true, email addresses won't appear in web pages in harvestable form.
        public bool ObfuscateEmails;

        // If true, "Last updated <date>" appears on home page.
        public bool AddHomePageCreateTime;

        // If true, pages get "Valid XHTML" validator link at bottom.
        public bool IncludeValiditySticker;

        // If true, nickName else otherName appears in brackets in individual's index entry.
        public bool IncludeNickNamesInIndex;

        // Optional text user can have displayed in each page footer.
        public string CustomFooter;

        // If true, the user's custom individual reference number will appear in the index.
        public bool IncludeUserRefInIndex;

        // If true, include the individuals (most appropriate, best) occupation in page header.
        public bool OccupationHeadline;

        // True indicates that text in Commentary box (settings pane) should not be escaped. (i.e. HTML is preserved)
        public bool CommentaryIsHtml;

        // True indicates that text in Footer box (settings pane) should not be escaped. (i.e. HTML is preserved)
        public bool FooterIsHtml;

        // True means when naming html files, to use user record number if present for sFilename.
        public bool UserRecFilename;

        // If false, doesn't include the navigation bar at the top of each page.
        public bool IncludeNavbar;

        // If true, withheld records will use the indivdual's name in minitrees, rather than m_concealedName.
        public bool UseWithheldNames;

        // If true, first names are put on a different line to surnames in minitree individual boxes.
        public bool ConserveTreeWidth;

        // If true, the mini tree has siblings of the selected individuals ordered as they are in the GEDCOM. False means sort them by date. Significant when some siblings have no recorded birth date
        public bool KeepSiblingOrder;

        // If false, no multimedia pics, images, files etc. will appear in HTML.
        public bool AllowMultimedia;

        // If true, the list of "Citations" on source records will not be generated.
        public bool SupressBackreferences;

        // If true a help page will be included in the output, with a link at the top of each record page.
        public bool IncludeHelpPage;


        private static CConfig fInstance;

        public static CConfig Instance
        {
            get {
                if (fInstance == null) {
                    fInstance = new CConfig();
                }
                return fInstance;
            }
        }


        // Constructor, sets default values for the config
        private CConfig()
        {
            ConfigFilename = "GEDmill Config";
            RestrictConfidential = false;
            RestrictPrivacy = false;
            OutputFolder = "";
            UnknownName = "<unknown>"; // "no name" implied we knew them and they had no name.
            NameCapitalisation = 1;
            Version = "1.11.0";
            HtmlExtension = "html";
            CopyMultimedia = true;
            ImageFolder = "multimedia";
            RelativiseMultimedia = false;
            ApplicationPath = GMHelper.GetAppPath();
            BackgroundImage = ApplicationPath + "\\bg-gedmill.jpg";
            MaxImageWidth = 160;
            MaxImageHeight = 160;
            MaxNumberMultimediaFiles = 32;
            AgeForOccupation = 50;
            OwnersName = Environment.UserName;
            NoSurname = "No Surname";
            IndexTitle = "Index Of Names";
            MaxSourceImageWidth = 800;
            MaxSourceImageHeight = 800;
            MaxThumbnailImageWidth = 45;
            MaxThumbnailImageHeight = 45;
            FirstRecordXRef = "";
            SiteTitle = "Family history";
            if (OwnersName != null && OwnersName != "")
                SiteTitle += " of " + OwnersName;
            InputFilename = "";
            FrontPageImageFilename = ApplicationPath + "\\gedmill.jpg";
            TabSpaces = 8;
            PlaceWord = "in";
            CapitaliseEventDescriptions = true;
            RestrictAssociatedSources = true;
            RenameMultimedia = true;
            IndexLetterPerPage = false;
            ShowFrontPageStats = true;
            CommentaryText = "";
            FtpServer = "";
            FtpUsername = "";
            FtpPassword = "";
            FtpUploadFolder = "/";

            // Reset those settings that can be modified by the user on the config screen.
            ResetUserSettings();
        }

        // Capitalises an individual's name according to config setting
        public string CapitaliseName(string name, ref string firstName, ref string surname)
        {
            if (name == null) {
                if (surname != null) {
                    surname = UnknownName;
                }
                return UnknownName;
            }

            string newName = "";
            switch (NameCapitalisation) {
                case 1:
                case 0:
                    // capitalise surname (the bit in //s)
                    bool bSeenSlash = false;
                    bool bFirstName = true;
                    char oldc = '\0';
                    foreach (char c in name) {
                        if (c == '/') {
                            bSeenSlash = !bSeenSlash;
                            if (bFirstName && oldc != ' ' && newName.Length > 0) {
                                // Ensure there is a space between first and last names (e.g. from "Fred/Bloggs/")
                                newName += ' ';
                                oldc = ' '; // To make oldc set to space too.
                            } else {
                                oldc = c;
                            }
                            bFirstName = false;
                        } else if (bSeenSlash) {
                            char cc = c;
                            if (NameCapitalisation == 1) {
                                cc = char.ToUpper(cc);
                            }
                            newName += cc;
                            if (surname != null) {
                                surname += cc;
                            }
                            oldc = c;
                        } else {
                            newName += c;

                            // Collapse multiple spaces into one
                            if (oldc != ' ' || c != ' ') {
                                if (bFirstName && firstName != null) {
                                    firstName += c;
                                } else if (!bFirstName && surname != null) {
                                    surname += c;
                                }
                            }
                            oldc = c;
                        }
                    }
                    break;
                default:
                    newName = name;
                    break;
            }

            return newName;
        }

        // Construct the HTTP URL for the created site's landing page
        public string FrontPageURL
        {
            get {
                return string.Concat(OutputFolder, "\\", FrontPageFilename, ".", HtmlExtension);
            }
        }

        // Construct the HTTP URL for the created site's help page
        public string HelpPageURL
        {
            get {
                return string.Concat(OutputFolder, "\\", "help.", HtmlExtension);
            }
        }

        // Serialise all the config settings into isolated storage.
        public void StoreSettings()
        {
            // Open the stream from the IsolatedStorage.
            IsolatedStorageFile isoStore = IsolatedStorageFile.GetStore(IsolatedStorageScope.User | IsolatedStorageScope.Assembly, null, null);
            Stream stream = new IsolatedStorageFileStream(ConfigFilename, FileMode.Create, isoStore);

            if (stream == null) {
                return;
            }
            try {
                // Serialize the configuration into the IsolatedStorage.
                IFormatter formatter = new BinaryFormatter();

                formatter.Serialize(stream, Version);
                formatter.Serialize(stream, RestrictConfidential);
                formatter.Serialize(stream, RestrictPrivacy);
                formatter.Serialize(stream, OutputFolder);
                formatter.Serialize(stream, UnknownName);
                formatter.Serialize(stream, NameCapitalisation);
                formatter.Serialize(stream, HtmlExtension);
                formatter.Serialize(stream, CopyMultimedia);
                formatter.Serialize(stream, ImageFolder);
                formatter.Serialize(stream, RelativiseMultimedia);
                formatter.Serialize(stream, BackgroundImage);
                formatter.Serialize(stream, MaxImageWidth);
                formatter.Serialize(stream, MaxImageHeight);
                formatter.Serialize(stream, MaxNumberMultimediaFiles);
                formatter.Serialize(stream, AgeForOccupation);
                formatter.Serialize(stream, OwnersName);
                formatter.Serialize(stream, NoSurname);
                formatter.Serialize(stream, IndexTitle);
                formatter.Serialize(stream, MaxSourceImageWidth);
                formatter.Serialize(stream, MaxSourceImageHeight);
                formatter.Serialize(stream, FirstRecordXRef);
                formatter.Serialize(stream, SiteTitle);
                formatter.Serialize(stream, InputFilename);
                formatter.Serialize(stream, ApplicationPath);
                formatter.Serialize(stream, FrontPageImageFilename);
                formatter.Serialize(stream, TabSpaces);
                formatter.Serialize(stream, PlaceWord);
                formatter.Serialize(stream, CapitaliseEventDescriptions);
                formatter.Serialize(stream, RestrictAssociatedSources);
                formatter.Serialize(stream, RenameMultimedia);
                formatter.Serialize(stream, IndexLetterPerPage);
                formatter.Serialize(stream, MiniTreeColourBranch);
                formatter.Serialize(stream, MiniTreeColourIndiBorder);
                formatter.Serialize(stream, MiniTreeColourIndiBackground);
                formatter.Serialize(stream, MiniTreeColourIndiHighlight);
                formatter.Serialize(stream, MiniTreeColourIndiShade);
                formatter.Serialize(stream, ShowFrontPageStats);
                formatter.Serialize(stream, CommentaryText);
                formatter.Serialize(stream, FtpServer);
                formatter.Serialize(stream, FtpUsername);
                formatter.Serialize(stream, FtpPassword);
                formatter.Serialize(stream, FtpUploadFolder);
                formatter.Serialize(stream, TreeFontName);
                formatter.Serialize(stream, TreeFontSize);
                formatter.Serialize(stream, TargetTreeWidth);
                formatter.Serialize(stream, MiniTreeImageFormat);
                formatter.Serialize(stream, MiniTreeColourIndiText);
                formatter.Serialize(stream, MiniTreeColourIndiLink);
                formatter.Serialize(stream, MiniTreeColourBackground);
                formatter.Serialize(stream, ShowMiniTrees);
                formatter.Serialize(stream, UserEmailAddress);
                formatter.Serialize(stream, FakeMiniTreeTransparency);
                int nKeyIndividuals = 0;
                if (KeyIndividuals != null) {
                    nKeyIndividuals = KeyIndividuals.Count;
                }
                formatter.Serialize(stream, nKeyIndividuals);
                if (KeyIndividuals != null) {
                    foreach (string keyXref in KeyIndividuals) {
                        formatter.Serialize(stream, keyXref);
                    }
                }
                formatter.Serialize(stream, MultiPageIndexes);
                formatter.Serialize(stream, IndividualsPerIndexPage);
                formatter.Serialize(stream, OpenWebsiteOnExit);
                formatter.Serialize(stream, FrontPageFilename);
                formatter.Serialize(stream, CreateCDROMFiles);
                formatter.Serialize(stream, MaxNumberMultimediaFiles);
                formatter.Serialize(stream, AllowMultipleImages);
                formatter.Serialize(stream, AllowNonPictures);
                formatter.Serialize(stream, MaxThumbnailImageWidth);
                formatter.Serialize(stream, MaxThumbnailImageHeight);
                formatter.Serialize(stream, MainWebsiteLink);
                formatter.Serialize(stream, PreserveFrontPage);
                formatter.Serialize(stream, PreserveStylesheet);
                formatter.Serialize(stream, StylesheetFilename);
                formatter.Serialize(stream, MiniTreeColourIndiBgConcealed);
                formatter.Serialize(stream, OnlyConceal);
                formatter.Serialize(stream, ConcealedName);
                formatter.Serialize(stream, MiniTreeColourIndiFgConcealed);
                formatter.Serialize(stream, LinkOriginalPicture);
                formatter.Serialize(stream, RenameOriginalPicture);
                formatter.Serialize(stream, ExcludeFileDir);
                formatter.Serialize(stream, ExcludeFileName);
                formatter.Serialize(stream, LastPictureAddedDir);
                formatter.Serialize(stream, ObfuscateEmails);
                formatter.Serialize(stream, AddHomePageCreateTime);
                formatter.Serialize(stream, IncludeValiditySticker);
                formatter.Serialize(stream, IncludeNickNamesInIndex);
                formatter.Serialize(stream, CustomFooter);
                formatter.Serialize(stream, IncludeUserRefInIndex);
                formatter.Serialize(stream, OccupationHeadline);
                formatter.Serialize(stream, CommentaryIsHtml);
                formatter.Serialize(stream, FooterIsHtml);
                formatter.Serialize(stream, UserRecFilename);
                formatter.Serialize(stream, IncludeNavbar);
                formatter.Serialize(stream, UseWithheldNames);
                uint uVersionMajor = 1, uVersionMinor = 11, uVersionBuild = 0; // 1.9.2, 1.10.0, 1.10.1, 1.10.2, 1.10.3, 1.10.4, 1.11.0
                formatter.Serialize(stream, uVersionMajor);
                formatter.Serialize(stream, uVersionMinor);
                formatter.Serialize(stream, uVersionBuild);
                formatter.Serialize(stream, ConserveTreeWidth);
                formatter.Serialize(stream, AllowMultimedia);
                formatter.Serialize(stream, SupressBackreferences);
                formatter.Serialize(stream, KeepSiblingOrder);
                formatter.Serialize(stream, IncludeHelpPage);
            } catch (Exception e) {
                fLogger.WriteError("Exception caught while writing MainForm.s_config : ", e);
            } finally {
                stream.Close();
            }
        }

        // Deserialise all the settings from isolated storage.
        public void RecoverSettings()
        {
            uint uVersionMajor = 0, uVersionMinor = 0, uVersionBuild = 0;

            IsolatedStorageFile isoStore = IsolatedStorageFile.GetStore(IsolatedStorageScope.User | IsolatedStorageScope.Assembly, null, null);
            if (isoStore.GetFileNames(ConfigFilename).Length == 0) {
                // File doesn't exist. Leave defaults in place.
                return;
            }
            // Read the stream from Isolated Storage.
            Stream stream = new IsolatedStorageFileStream(ConfigFilename, FileMode.OpenOrCreate, isoStore);
            if (stream == null) {
                return;
            }

            try {
                // DeSerialize the Hashtable from stream.
                IFormatter formatter = new BinaryFormatter();

                Version = (string)formatter.Deserialize(stream);
                RestrictConfidential = (bool)formatter.Deserialize(stream);
                RestrictPrivacy = (bool)formatter.Deserialize(stream);
                OutputFolder = (string)formatter.Deserialize(stream);
                UnknownName = (string)formatter.Deserialize(stream);
                NameCapitalisation = (int)formatter.Deserialize(stream);
                HtmlExtension = (string)formatter.Deserialize(stream);
                CopyMultimedia = (bool)formatter.Deserialize(stream);
                ImageFolder = (string)formatter.Deserialize(stream);
                RelativiseMultimedia = (bool)formatter.Deserialize(stream);
                BackgroundImage = (string)formatter.Deserialize(stream);
                MaxImageWidth = (uint)formatter.Deserialize(stream);
                MaxImageHeight = (uint)formatter.Deserialize(stream);
                int deprecated_m_nMaxNumberMultimediaFiles = (int)formatter.Deserialize(stream);
                AgeForOccupation = (int)formatter.Deserialize(stream);
                OwnersName = (string)formatter.Deserialize(stream);
                NoSurname = (string)formatter.Deserialize(stream);
                IndexTitle = (string)formatter.Deserialize(stream);
                MaxSourceImageWidth = (uint)formatter.Deserialize(stream);
                MaxSourceImageHeight = (uint)formatter.Deserialize(stream);
                FirstRecordXRef = (string)formatter.Deserialize(stream);
                SiteTitle = (string)formatter.Deserialize(stream);
                InputFilename = (string)formatter.Deserialize(stream);
                ApplicationPath = (string)formatter.Deserialize(stream);
                FrontPageImageFilename = (string)formatter.Deserialize(stream);
                TabSpaces = (uint)formatter.Deserialize(stream);
                PlaceWord = (string)formatter.Deserialize(stream);
                CapitaliseEventDescriptions = (bool)formatter.Deserialize(stream);
                RestrictAssociatedSources = (bool)formatter.Deserialize(stream);
                RenameMultimedia = (bool)formatter.Deserialize(stream);
                IndexLetterPerPage = (bool)formatter.Deserialize(stream);
                MiniTreeColourBranch = (string)formatter.Deserialize(stream);
                MiniTreeColourIndiBorder = (string)formatter.Deserialize(stream);
                MiniTreeColourIndiBackground = (string)formatter.Deserialize(stream);
                MiniTreeColourIndiHighlight = (string)formatter.Deserialize(stream);
                MiniTreeColourIndiShade = (string)formatter.Deserialize(stream);
                ShowFrontPageStats = (bool)formatter.Deserialize(stream);
                CommentaryText = (string)formatter.Deserialize(stream);
                FtpServer = (string)formatter.Deserialize(stream);
                FtpUsername = (string)formatter.Deserialize(stream);
                FtpPassword = (string)formatter.Deserialize(stream);
                FtpUploadFolder = (string)formatter.Deserialize(stream);
                TreeFontName = (string)formatter.Deserialize(stream);
                TreeFontSize = (float)formatter.Deserialize(stream);
                TargetTreeWidth = (int)formatter.Deserialize(stream);
                MiniTreeImageFormat = (string)formatter.Deserialize(stream);
                MiniTreeColourIndiText = (string)formatter.Deserialize(stream);
                MiniTreeColourIndiLink = (string)formatter.Deserialize(stream);
                MiniTreeColourBackground = (string)formatter.Deserialize(stream);
                ShowMiniTrees = (bool)formatter.Deserialize(stream);
                UserEmailAddress = (string)formatter.Deserialize(stream);
                FakeMiniTreeTransparency = (bool)formatter.Deserialize(stream);
                int nKeyIndividuals = (int)formatter.Deserialize(stream);
                KeyIndividuals = new List<string>();
                while (nKeyIndividuals-- > 0) {
                    string keyXref = (string)formatter.Deserialize(stream);
                    KeyIndividuals.Add(keyXref);
                }
                MultiPageIndexes = (bool)formatter.Deserialize(stream);
                IndividualsPerIndexPage = (uint)formatter.Deserialize(stream);
                OpenWebsiteOnExit = (bool)formatter.Deserialize(stream);
                FrontPageFilename = (string)formatter.Deserialize(stream);
                CreateCDROMFiles = (bool)formatter.Deserialize(stream);
                MaxNumberMultimediaFiles = (int)formatter.Deserialize(stream);
                AllowMultipleImages = (bool)formatter.Deserialize(stream);
                AllowNonPictures = (bool)formatter.Deserialize(stream);
                MaxThumbnailImageWidth = (uint)formatter.Deserialize(stream);
                MaxThumbnailImageHeight = (uint)formatter.Deserialize(stream);
                MainWebsiteLink = (string)formatter.Deserialize(stream);
                PreserveFrontPage = (bool)formatter.Deserialize(stream);
                PreserveStylesheet = (bool)formatter.Deserialize(stream);
                StylesheetFilename = (string)formatter.Deserialize(stream);
                MiniTreeColourIndiBgConcealed = (string)formatter.Deserialize(stream);
                OnlyConceal = (bool)formatter.Deserialize(stream);
                ConcealedName = (string)formatter.Deserialize(stream);
                MiniTreeColourIndiFgConcealed = (string)formatter.Deserialize(stream);
                LinkOriginalPicture = (bool)formatter.Deserialize(stream);
                RenameOriginalPicture = (bool)formatter.Deserialize(stream);
                ExcludeFileDir = (string)formatter.Deserialize(stream);
                ExcludeFileName = (string)formatter.Deserialize(stream);
                LastPictureAddedDir = (string)formatter.Deserialize(stream);
                ObfuscateEmails = (bool)formatter.Deserialize(stream);
                AddHomePageCreateTime = (bool)formatter.Deserialize(stream);
                IncludeValiditySticker = (bool)formatter.Deserialize(stream);
                IncludeNickNamesInIndex = (bool)formatter.Deserialize(stream);
                CustomFooter = (string)formatter.Deserialize(stream);
                IncludeUserRefInIndex = (bool)formatter.Deserialize(stream);
                OccupationHeadline = (bool)formatter.Deserialize(stream);
                CommentaryIsHtml = (bool)formatter.Deserialize(stream);
                FooterIsHtml = (bool)formatter.Deserialize(stream);
                UserRecFilename = (bool)formatter.Deserialize(stream);
                IncludeNavbar = (bool)formatter.Deserialize(stream);
                UseWithheldNames = (bool)formatter.Deserialize(stream);
                uVersionMajor = (uint)formatter.Deserialize(stream);
                uVersionMinor = (uint)formatter.Deserialize(stream);
                uVersionBuild = (uint)formatter.Deserialize(stream);
                ConserveTreeWidth = (bool)formatter.Deserialize(stream);
                AllowMultimedia = (bool)formatter.Deserialize(stream);
                SupressBackreferences = (bool)formatter.Deserialize(stream);
                KeepSiblingOrder = (bool)formatter.Deserialize(stream);
                IncludeHelpPage = (bool)formatter.Deserialize(stream);
            } catch (Exception e) {
                fLogger.WriteError("Exception caught while reading MainForm.s_config : ", e);
            } finally {
                // We are done with it.
                stream.Close();
            }

            if (!(uVersionMajor > 1 || uVersionMinor > 9 || (uVersionMinor == 9 && uVersionBuild > 0))) {
                // This config came from a version less than 1.9.1
                // Need to mangle m_mainWebsiteLink, because it now must include the http:// prefix.
                if (MainWebsiteLink.Length > 0) {
                    MainWebsiteLink = "http://" + MainWebsiteLink;
                }
            }
        }

        // Reset those settings that can be modified by the user on the config screen.
        public void ResetUserSettings()
        {
            TreeFontName = "Arial";
            TreeFontSize = 7.2f;
            TargetTreeWidth = 800;
            MiniTreeImageFormat = "gif";
            MiniTreeColourBranch = "#000000";
            MiniTreeColourIndiBorder = "#000000";
            MiniTreeColourIndiBackground = "#ffffd2";
            MiniTreeColourIndiHighlight = "#ffffff";
            MiniTreeColourIndiBgConcealed = "#cccccc";
            MiniTreeColourIndiFgConcealed = "#000000";
            MiniTreeColourIndiShade = "#ffffd2";
            MiniTreeColourIndiText = "#000000";
            MiniTreeColourIndiLink = "#3333ff";
            MiniTreeColourBackground = "#aaaaaa";
            FakeMiniTreeTransparency = false;
            ShowMiniTrees = true;
            UserEmailAddress = "";
            KeyIndividuals = new List<string>();
            MultiPageIndexes = true;
            IndividualsPerIndexPage = 1000;
            OpenWebsiteOnExit = true;
            FrontPageFilename = "home";
            AllowMultipleImages = false;
            CreateCDROMFiles = false;
            AllowNonPictures = true;
            MainWebsiteLink = "";
            PreserveFrontPage = false;
            PreserveStylesheet = false;
            StylesheetFilename = "gedmill-style";
            OnlyConceal = false;
            ConcealedName = "Private Record";
            LinkOriginalPicture = false;
            RenameOriginalPicture = false;
            ExcludeFileDir = "";
            ExcludeFileName = "";
            LastPictureAddedDir = "";
            ObfuscateEmails = false;
            AddHomePageCreateTime = true;
            IncludeValiditySticker = false;
            IncludeNickNamesInIndex = true;
            CustomFooter = "";
            IncludeUserRefInIndex = false;
            OccupationHeadline = true;
            CommentaryIsHtml = false;
            FooterIsHtml = false;
            UserRecFilename = false;
            IncludeNavbar = true;
            UseWithheldNames = false;
            ConserveTreeWidth = false;
            KeepSiblingOrder = false;
            AllowMultimedia = true;
            SupressBackreferences = false;
            IncludeHelpPage = true;
        }
    }
}
