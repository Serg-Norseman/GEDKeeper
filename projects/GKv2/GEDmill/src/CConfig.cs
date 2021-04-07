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
using BSLib;
using GKCore;
using GKCL = GKCore.Logging;

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

        private static readonly GKCL.ILogger fLogger = GKCL.LogManager.GetLogger(CConfig.LOG_FILE, CConfig.LOG_LEVEL, typeof(CConfig).Name);

        // Filename used to store users config
        public const string ConfigFilename = "GEDmillPlugin.ini";

        // Name to use where no other name is available
        public string UnknownName;

        // How to capitalise individuals' names
        public int NameCapitalisation;

        // File extension for html files
        public string HtmlExtension;

        // Absolute sFilename of image to use as background
        public string BackgroundImage;

        // Maximum allowed width of individual image
        public int MaxImageWidth;

        // Maximum allowed height of individual image
        public int MaxImageHeight;

        // Age at which defining occupation is chosen
        public int AgeForOccupation;

        // Maximum allowed width of source image
        public int MaxSourceImageWidth;

        // Maximum allowed height of source image
        public int MaxSourceImageHeight;

        // Maximum allowed width of thumbnail image
        public int MaxThumbnailImageWidth;

        // Maximum allowed height of thumbnail image
        public int MaxThumbnailImageHeight;

        // Filename of image if any to use on front webpage.
        public string FrontPageImageFilename;

        // Filename of front webpage.
        public string FrontPageFilename;

        // Number of spaces between each tab stop, when converting \t characters to html.
        public int TabSpaces;

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

        // Path to application's data
        public string AppDataPath;

        // String indicating image format to use to store mini tree file
        public string MiniTreeImageFormat;

        // If true, the background image will be used to fill in the background of mini trees, giving the effect that they are transparent.
        public bool FakeMiniTreeTransparency;

        // List of Xrefs for individuals to be mentioned on front page.
        public List<string> KeyIndividuals;

        // Set true if indexes are allowed to span multiple html files
        public bool MultiPageIndexes;

        // Number of individuals to aim to list per index page.
        public int IndividualsPerIndexPage;

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
            ApplicationPath = GMHelper.GetAppPath();
            AppDataPath = AppHost.GetAppDataPathStatic();
            BackgroundImage = ApplicationPath + "\\bg-gedmill.jpg";
            FrontPageImageFilename = ApplicationPath + "\\gedmill.jpg";

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
            MaxImageWidth = 160;
            MaxImageHeight = 160;
            MaxNumberMultimediaFiles = 32;
            AgeForOccupation = 50;
            NoSurname = "No Surname";
            IndexTitle = "Index Of Names";
            MaxSourceImageWidth = 800;
            MaxSourceImageHeight = 800;
            MaxThumbnailImageWidth = 45;
            MaxThumbnailImageHeight = 45;
            FirstRecordXRef = "";
            InputFilename = "";
            TabSpaces = 8;
            PlaceWord = "in";
            CapitaliseEventDescriptions = true;
            RestrictAssociatedSources = true;
            RenameMultimedia = true;
            IndexLetterPerPage = false;
            ShowFrontPageStats = true;
            CommentaryText = "";

            OwnersName = Environment.UserName;

            SiteTitle = "Family history";
            if (!string.IsNullOrEmpty(OwnersName))
                SiteTitle += " of " + OwnersName;

            // Reset those settings that can be modified by the user on the config screen.
            Reset();
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

        // Serialise all the config settings
        public void Save()
        {
            try {
                using (var ini = new IniFile(AppDataPath + ConfigFilename)) {
                    ini.WriteString("Common", "Version", Version);
                    ini.WriteBool("Common", "RestrictConfidential", RestrictConfidential);
                    ini.WriteBool("Common", "RestrictPrivacy", RestrictPrivacy);
                    ini.WriteString("Common", "OutputFolder", OutputFolder);
                    ini.WriteString("Common", "UnknownName", UnknownName);
                    ini.WriteInteger("Common", "NameCapitalisation", NameCapitalisation);
                    ini.WriteString("Common", "HtmlExtension", HtmlExtension);
                    ini.WriteBool("Common", "CopyMultimedia", CopyMultimedia);
                    ini.WriteString("Common", "ImageFolder", ImageFolder);
                    ini.WriteBool("Common", "RelativiseMultimedia", RelativiseMultimedia);
                    ini.WriteString("Common", "BackgroundImage", BackgroundImage);
                    ini.WriteInteger("Common", "MaxImageWidth", MaxImageWidth);
                    ini.WriteInteger("Common", "MaxImageHeight", MaxImageHeight);
                    ini.WriteInteger("Common", "MaxNumberMultimediaFiles", MaxNumberMultimediaFiles);
                    ini.WriteInteger("Common", "AgeForOccupation", AgeForOccupation);
                    ini.WriteString("Common", "OwnersName", OwnersName);
                    ini.WriteString("Common", "NoSurname", NoSurname);
                    ini.WriteString("Common", "IndexTitle", IndexTitle);
                    ini.WriteInteger("Common", "MaxSourceImageWidth", MaxSourceImageWidth);
                    ini.WriteInteger("Common", "MaxSourceImageHeight", MaxSourceImageHeight);
                    ini.WriteString("Common", "FirstRecordXRef", FirstRecordXRef);
                    ini.WriteString("Common", "SiteTitle", SiteTitle);
                    ini.WriteString("Common", "InputFilename", InputFilename);
                    ini.WriteString("Common", "ApplicationPath", ApplicationPath);
                    ini.WriteString("Common", "FrontPageImageFilename", FrontPageImageFilename);
                    ini.WriteInteger("Common", "TabSpaces", TabSpaces);
                    ini.WriteString("Common", "PlaceWord", PlaceWord);
                    ini.WriteBool("Common", "CapitaliseEventDescriptions", CapitaliseEventDescriptions);
                    ini.WriteBool("Common", "RestrictAssociatedSources", RestrictAssociatedSources);
                    ini.WriteBool("Common", "RenameMultimedia", RenameMultimedia);
                    ini.WriteBool("Common", "IndexLetterPerPage", IndexLetterPerPage);
                    ini.WriteString("Common", "MiniTreeColourBranch", MiniTreeColourBranch);
                    ini.WriteString("Common", "MiniTreeColourIndiBorder", MiniTreeColourIndiBorder);
                    ini.WriteString("Common", "MiniTreeColourIndiBackground", MiniTreeColourIndiBackground);
                    ini.WriteString("Common", "MiniTreeColourIndiHighlight", MiniTreeColourIndiHighlight);
                    ini.WriteString("Common", "MiniTreeColourIndiShade", MiniTreeColourIndiShade);
                    ini.WriteBool("Common", "ShowFrontPageStats", ShowFrontPageStats);
                    ini.WriteString("Common", "CommentaryText", CommentaryText);
                    ini.WriteString("Common", "TreeFontName", TreeFontName);
                    ini.WriteFloat("Common", "TreeFontSize", TreeFontSize);
                    ini.WriteInteger("Common", "TargetTreeWidth", TargetTreeWidth);
                    ini.WriteString("Common", "MiniTreeImageFormat", MiniTreeImageFormat);
                    ini.WriteString("Common", "MiniTreeColourIndiText", MiniTreeColourIndiText);
                    ini.WriteString("Common", "MiniTreeColourIndiLink", MiniTreeColourIndiLink);
                    ini.WriteString("Common", "MiniTreeColourBackground", MiniTreeColourBackground);
                    ini.WriteBool("Common", "ShowMiniTrees", ShowMiniTrees);
                    ini.WriteString("Common", "UserEmailAddress", UserEmailAddress);
                    ini.WriteBool("Common", "FakeMiniTreeTransparency", FakeMiniTreeTransparency);

                    int nKeyIndividuals = 0;
                    if (KeyIndividuals != null) {
                        nKeyIndividuals = KeyIndividuals.Count;
                        ini.WriteInteger("Individuals", "Count", nKeyIndividuals);
                        for (int i = 0; i < nKeyIndividuals; i++) {
                            string keyXref = KeyIndividuals[i];
                            ini.WriteString("Individuals", "I_" + i, keyXref);
                        }
                    }

                    ini.WriteBool("Common", "MultiPageIndexes", MultiPageIndexes);
                    ini.WriteInteger("Common", "IndividualsPerIndexPage", IndividualsPerIndexPage);
                    ini.WriteBool("Common", "OpenWebsiteOnExit", OpenWebsiteOnExit);
                    ini.WriteString("Common", "FrontPageFilename", FrontPageFilename);
                    ini.WriteBool("Common", "CreateCDROMFiles", CreateCDROMFiles);
                    ini.WriteBool("Common", "AllowMultipleImages", AllowMultipleImages);
                    ini.WriteBool("Common", "AllowNonPictures", AllowNonPictures);
                    ini.WriteInteger("Common", "MaxThumbnailImageWidth", MaxThumbnailImageWidth);
                    ini.WriteInteger("Common", "MaxThumbnailImageHeight", MaxThumbnailImageHeight);
                    ini.WriteString("Common", "MainWebsiteLink", MainWebsiteLink);
                    ini.WriteBool("Common", "PreserveFrontPage", PreserveFrontPage);
                    ini.WriteBool("Common", "PreserveStylesheet", PreserveStylesheet);
                    ini.WriteString("Common", "StylesheetFilename", StylesheetFilename);
                    ini.WriteString("Common", "MiniTreeColourIndiBgConcealed", MiniTreeColourIndiBgConcealed);
                    ini.WriteBool("Common", "OnlyConceal", OnlyConceal);
                    ini.WriteString("Common", "ConcealedName", ConcealedName);
                    ini.WriteString("Common", "MiniTreeColourIndiFgConcealed", MiniTreeColourIndiFgConcealed);
                    ini.WriteBool("Common", "LinkOriginalPicture", LinkOriginalPicture);
                    ini.WriteBool("Common", "RenameOriginalPicture", RenameOriginalPicture);
                    ini.WriteString("Common", "ExcludeFileDir", ExcludeFileDir);
                    ini.WriteString("Common", "ExcludeFileName", ExcludeFileName);
                    ini.WriteBool("Common", "ObfuscateEmails", ObfuscateEmails);
                    ini.WriteBool("Common", "AddHomePageCreateTime", AddHomePageCreateTime);
                    ini.WriteBool("Common", "IncludeValiditySticker", IncludeValiditySticker);
                    ini.WriteBool("Common", "IncludeNickNamesInIndex", IncludeNickNamesInIndex);
                    ini.WriteString("Common", "CustomFooter", CustomFooter);
                    ini.WriteBool("Common", "IncludeUserRefInIndex", IncludeUserRefInIndex);
                    ini.WriteBool("Common", "OccupationHeadline", OccupationHeadline);
                    ini.WriteBool("Common", "CommentaryIsHtml", CommentaryIsHtml);
                    ini.WriteBool("Common", "FooterIsHtml", FooterIsHtml);
                    ini.WriteBool("Common", "UserRecFilename", UserRecFilename);
                    ini.WriteBool("Common", "IncludeNavbar", IncludeNavbar);
                    ini.WriteBool("Common", "UseWithheldNames", UseWithheldNames);
                    ini.WriteBool("Common", "ConserveTreeWidth", ConserveTreeWidth);
                    ini.WriteBool("Common", "AllowMultimedia", AllowMultimedia);
                    ini.WriteBool("Common", "SupressBackreferences", SupressBackreferences);
                    ini.WriteBool("Common", "KeepSiblingOrder", KeepSiblingOrder);
                    ini.WriteBool("Common", "IncludeHelpPage", IncludeHelpPage);

                    int uVersionMajor = 1, uVersionMinor = 11, uVersionBuild = 0;
                    ini.WriteInteger("Common", "VersionMajor", uVersionMajor);
                    ini.WriteInteger("Common", "VersionMinor", uVersionMinor);
                    ini.WriteInteger("Common", "VersionBuild", uVersionBuild);
                }
            } catch (Exception ex) {
                fLogger.WriteError("CConfig.StoreSettings()", ex);
            }
        }

        // Deserialise all the settings
        public void Load()
        {
            int uVersionMajor = 0, uVersionMinor = 0, uVersionBuild = 0;

            try {
                using (var ini = new IniFile(AppDataPath + ConfigFilename)) {
                    Version = ini.ReadString("Common", "Version", "");
                    RestrictConfidential = ini.ReadBool("Common", "RestrictConfidential", false);
                    RestrictPrivacy = ini.ReadBool("Common", "RestrictPrivacy", false);
                    OutputFolder = ini.ReadString("Common", "OutputFolder", "");
                    UnknownName = ini.ReadString("Common", "UnknownName", "<unknown>");
                    NameCapitalisation = ini.ReadInteger("Common", "NameCapitalisation", 1);
                    HtmlExtension = ini.ReadString("Common", "HtmlExtension", "html");
                    CopyMultimedia = ini.ReadBool("Common", "CopyMultimedia", true);
                    ImageFolder = ini.ReadString("Common", "ImageFolder", "multimedia");
                    RelativiseMultimedia = ini.ReadBool("Common", "RelativiseMultimedia", false);
                    BackgroundImage = ini.ReadString("Common", "BackgroundImage", "");
                    MaxImageWidth = ini.ReadInteger("Common", "MaxImageWidth", 160);
                    MaxImageHeight = ini.ReadInteger("Common", "MaxImageHeight", 160);
                    MaxNumberMultimediaFiles = ini.ReadInteger("Common", "MaxNumberMultimediaFiles", 32);
                    AgeForOccupation = ini.ReadInteger("Common", "AgeForOccupation", 50);
                    OwnersName = ini.ReadString("Common", "OwnersName", "");
                    NoSurname = ini.ReadString("Common", "NoSurname", "No Surname");
                    IndexTitle = ini.ReadString("Common", "IndexTitle", "Index Of Names");
                    MaxSourceImageWidth = ini.ReadInteger("Common", "MaxSourceImageWidth", 800);
                    MaxSourceImageHeight = ini.ReadInteger("Common", "MaxSourceImageHeight", 800);
                    FirstRecordXRef = ini.ReadString("Common", "FirstRecordXRef", "");
                    SiteTitle = ini.ReadString("Common", "SiteTitle", "Family history");
                    InputFilename = ini.ReadString("Common", "InputFilename", "");
                    ApplicationPath = ini.ReadString("Common", "ApplicationPath", "");
                    FrontPageImageFilename = ini.ReadString("Common", "FrontPageImageFilename", "");
                    TabSpaces = ini.ReadInteger("Common", "TabSpaces", 8);
                    PlaceWord = ini.ReadString("Common", "PlaceWord", "in");
                    CapitaliseEventDescriptions = ini.ReadBool("Common", "CapitaliseEventDescriptions", true);
                    RestrictAssociatedSources = ini.ReadBool("Common", "RestrictAssociatedSources", true);
                    RenameMultimedia = ini.ReadBool("Common", "RenameMultimedia", true);
                    IndexLetterPerPage = ini.ReadBool("Common", "IndexLetterPerPage", false);
                    MiniTreeColourBranch = ini.ReadString("Common", "MiniTreeColourBranch", "#000000");
                    MiniTreeColourIndiBorder = ini.ReadString("Common", "MiniTreeColourIndiBorder", "#000000");
                    MiniTreeColourIndiBackground = ini.ReadString("Common", "MiniTreeColourIndiBackground", "#ffffd2");
                    MiniTreeColourIndiHighlight = ini.ReadString("Common", "MiniTreeColourIndiHighlight", "#ffffff");
                    MiniTreeColourIndiShade = ini.ReadString("Common", "MiniTreeColourIndiShade", "#ffffd2");
                    ShowFrontPageStats = ini.ReadBool("Common", "ShowFrontPageStats", true);
                    CommentaryText = ini.ReadString("Common", "CommentaryText", "");
                    TreeFontName = ini.ReadString("Common", "TreeFontName", "Arial");
                    TreeFontSize = (float)ini.ReadFloat("Common", "TreeFontSize", 7.2f);
                    TargetTreeWidth = ini.ReadInteger("Common", "TargetTreeWidth", 800);
                    MiniTreeImageFormat = ini.ReadString("Common", "MiniTreeImageFormat", "gif");
                    MiniTreeColourIndiText = ini.ReadString("Common", "MiniTreeColourIndiText", "#000000");
                    MiniTreeColourIndiLink = ini.ReadString("Common", "MiniTreeColourIndiLink", "#3333ff");
                    MiniTreeColourBackground = ini.ReadString("Common", "MiniTreeColourBackground", "#aaaaaa");
                    ShowMiniTrees = ini.ReadBool("Common", "ShowMiniTrees", true);
                    UserEmailAddress = ini.ReadString("Common", "UserEmailAddress", "");
                    FakeMiniTreeTransparency = ini.ReadBool("Common", "FakeMiniTreeTransparency", false);

                    int nKeyIndividuals = ini.ReadInteger("Individuals", "Count", 0);
                    KeyIndividuals = new List<string>();
                    for (int i = 0; i < nKeyIndividuals; i++) {
                        string keyXref = ini.ReadString("Individuals", "I_" + i, "");
                        KeyIndividuals.Add(keyXref);
                    }

                    MultiPageIndexes = ini.ReadBool("Common", "MultiPageIndexes", true);
                    IndividualsPerIndexPage = ini.ReadInteger("Common", "IndividualsPerIndexPage", 1000);
                    OpenWebsiteOnExit = ini.ReadBool("Common", "OpenWebsiteOnExit", true);
                    FrontPageFilename = ini.ReadString("Common", "FrontPageFilename", "home");
                    CreateCDROMFiles = ini.ReadBool("Common", "CreateCDROMFiles", false);
                    AllowMultipleImages = ini.ReadBool("Common", "AllowMultipleImages", false);
                    AllowNonPictures = ini.ReadBool("Common", "AllowNonPictures", true);
                    MaxThumbnailImageWidth = ini.ReadInteger("Common", "MaxThumbnailImageWidth", 45);
                    MaxThumbnailImageHeight = ini.ReadInteger("Common", "MaxThumbnailImageHeight", 45);
                    MainWebsiteLink = ini.ReadString("Common", "MainWebsiteLink", "");
                    PreserveFrontPage = ini.ReadBool("Common", "PreserveFrontPage", false);
                    PreserveStylesheet = ini.ReadBool("Common", "PreserveStylesheet", false);
                    StylesheetFilename = ini.ReadString("Common", "StylesheetFilename", "gedmill-style");
                    MiniTreeColourIndiBgConcealed = ini.ReadString("Common", "MiniTreeColourIndiBgConcealed", "#cccccc");
                    OnlyConceal = ini.ReadBool("Common", "OnlyConceal", false);
                    ConcealedName = ini.ReadString("Common", "ConcealedName", "Private Record");
                    MiniTreeColourIndiFgConcealed = ini.ReadString("Common", "MiniTreeColourIndiFgConcealed", "#000000");
                    LinkOriginalPicture = ini.ReadBool("Common", "LinkOriginalPicture", false);
                    RenameOriginalPicture = ini.ReadBool("Common", "RenameOriginalPicture", false);
                    ExcludeFileDir = ini.ReadString("Common", "ExcludeFileDir", "");
                    ExcludeFileName = ini.ReadString("Common", "ExcludeFileName", "");
                    ObfuscateEmails = ini.ReadBool("Common", "ObfuscateEmails", false);
                    AddHomePageCreateTime = ini.ReadBool("Common", "AddHomePageCreateTime", true);
                    IncludeValiditySticker = ini.ReadBool("Common", "IncludeValiditySticker", false);
                    IncludeNickNamesInIndex = ini.ReadBool("Common", "IncludeNickNamesInIndex", true);
                    CustomFooter = ini.ReadString("Common", "CustomFooter", "");
                    IncludeUserRefInIndex = ini.ReadBool("Common", "IncludeUserRefInIndex", false);
                    OccupationHeadline = ini.ReadBool("Common", "OccupationHeadline", true);
                    CommentaryIsHtml = ini.ReadBool("Common", "CommentaryIsHtml", false);
                    FooterIsHtml = ini.ReadBool("Common", "FooterIsHtml", false);
                    UserRecFilename = ini.ReadBool("Common", "UserRecFilename", false);
                    IncludeNavbar = ini.ReadBool("Common", "IncludeNavbar", true);
                    UseWithheldNames = ini.ReadBool("Common", "UseWithheldNames", false);
                    ConserveTreeWidth = ini.ReadBool("Common", "ConserveTreeWidth", false);
                    AllowMultimedia = ini.ReadBool("Common", "AllowMultimedia", true);
                    SupressBackreferences = ini.ReadBool("Common", "SupressBackreferences", false);
                    KeepSiblingOrder = ini.ReadBool("Common", "KeepSiblingOrder", false);
                    IncludeHelpPage = ini.ReadBool("Common", "IncludeHelpPage", true);

                    uVersionMajor = ini.ReadInteger("Common", "VersionMajor", 0);
                    uVersionMinor = ini.ReadInteger("Common", "VersionMinor", 0);
                    uVersionBuild = ini.ReadInteger("Common", "VersionBuild", 0);
                }
            } catch (Exception ex) {
                fLogger.WriteError("CConfig.Load()", ex);
            }
        }

        // Reset those settings that can be modified by the user on the config screen.
        public void Reset()
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
