/* CConfig.cs
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
using System.IO;
using System.IO.IsolatedStorage;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;

namespace GEDmill
{
    // Class that contains all the user configurable settings, and serialises them into IsolatedStorage at program start and exit.
    // Note not all settings here are presented through the UI, but most are. Originally there was a distinction her but it has become blurred.
    public class CConfig
    {
        // Filename used to store users config in isolated storage.
        public string m_configFilename;

        // Name to use where no other name is available
        public string m_sUnknownName;

        // How to capitalise individuals' names
        public int m_nNameCapitalisation;

        // File extension for html files
        public string m_sHtmlExtension;

        // Absolute sFilename of image to use as background
        public string m_sBackgroundImage;

        // Maximum allowed width of individual image
        public uint m_uMaxImageWidth;

        // Maximum allowed height of individual image
        public uint m_uMaxImageHeight;

        // Age at which defining occupation is chosen
        public int m_nAgeForOccupation;

        // Maximum allowed width of source image
        public uint m_uMaxSourceImageWidth;

        // Maximum allowed height of source image
        public uint m_uMaxSourceImageHeight;

        // Maximum allowed width of thumbnail image
        public uint m_uMaxThumbnailImageWidth;

        // Maximum allowed height of thumbnail image
        public uint m_uMaxThumbnailImageHeight;

        // Filename of image if any to use on front webpage.
        public string m_sFrontPageImageFilename;

        // Filename of front webpage.
        public string m_sFrontPageFilename;

        // Number of spaces between each tab stop, when converting \t characters to html.
        public uint m_uTabSpaces;

        // True if event descriptions to start with capital letter, e.g. "Born" as opposed to "born".
        public bool m_bCapitaliseEventDescriptions;

        // True if to include text about website statistics on front page.
        public bool m_bFrontPageStats;

        // Extra text to include on title page.
        public string m_sCommentaryText;

        // Name of font to render tree diagrams with
        public string m_sTreeFontName;

        // Size of font to render tree diagrams with
        public float m_fTreeFontSize;

        // Width to try not to exceed when creating mini tree diagrams
        public int m_nTargetTreeWidth;

        // True if mini tree diagrams are to be added to pages
        public bool m_bShowMiniTrees;

        // User's email address to include on webpages
        public string m_sUserEmailAddress;

        // Character set to encode output html files
        public ECharset m_ecHtmlCharset;

        // Allow multiple multimedia files per individual. Enables m_nMaxNumberMultimediaFiles.
        public bool m_bAllowMultipleImages;

        // If true will not overwrite existing front page file.
        public bool m_bPreserveFrontPage;

        // If true will not overwrite existing style sheet css file.
        public bool m_bPreserveStylesheet;

        // Whether to create autorun.inf and autoplay.exe files
        public bool m_bCreateCDROMFiles;

        // Whether to include files that aren't pictures as multimedia objects
        public bool m_bAllowNonPictures;

        // Link to users main website, put at top of each indi page.
        public string m_sMainWebsiteLink;

        // Name to use for stylesheet css file.
        public string m_sStylesheetFilename;

        // Name to user for individuals who appear but have their information withheld
        public string m_sConcealedName;

        // Word used to prefix locations, e.g. "at" or "in".
        public string m_sPlaceWord;

        // String to use (in index) for names with no surname
        public string m_sNoSurname;

        // True and parser won't strip leading whitespace from gedcomLine value fields.
        public bool m_bDataMayStartWithWhitespace;

        // True and parser won't strip trailing whitespace from gedcomLine value fields.
        public bool m_bDataMayEndWithWhitespace;

        // True if all sources mentioned by a restricted individual should also be restricted (i.e. omitted from website)
        public bool m_bRestrictAssociatedSources;

        // True if all multimedia files are to be renamed to a standard naming system. Now generally superceded by m_renameOriginalPicture
        public bool m_bRenameMultimedia;

        // True to generate a separate html page for each letter of names index. Useful if index would otherwise be massive.
        public bool m_bIndexLetterPerPage;

        // HTML string representing colour to draw connecting lines in mini trees
        public string m_sMiniTreeColourBranch;

        // HTML string representing colour to draw outline of individual boxes in mini trees
        public string m_sMiniTreeColourIndiBorder;

        // HTML string representing colour to fill normal individual boxes in mini trees
        public string m_sMiniTreeColourIndiBackground;

        // HTML string representing colour to fill selected individual boxes in mini trees
        public string m_sMiniTreeColourIndiHighlight;

        // HTML string representing colour to fill boxes for individuals marked as concealed in mini trees.
        public string m_sMiniTreeColourIndiBgConcealed;

        // HTML string representing colour to write text for individuals marked as concealed in mini trees.
        public string m_sMiniTreeColourIndiFgConcealed;

        // HTML string representing colour to fill shaded individual boxes in mini trees
        public string m_sMiniTreeColourIndiShade;

        // HTML string representing colour to draw text in individual boxes in mini trees
        public string m_sMiniTreeColourIndiText;

        // HTML string representing colour to draw linkable text in individual boxes in mini trees
        public string m_sMiniTreeColourIndiLink;

        // HTML string representing colour to fill entire background of mini tree diagrams
        public string m_sMiniTreeColourBackground;

        // Whether to restrict records with RESN set to confidential
        public bool m_bRestrictConfidential;

        // Whether to restrict records with RESN set to privacy
        public bool m_bRestrictPrivacy;

        // If true, multimedia files are copied into m_imageFolder.
        public bool m_bCopyMultimedia;

        // Which folder to copy multimedia into. Ends up as subdirectory to m_outputFolder;
        public string m_sImageFolder;

        // If true, links to multimedia are altered to be relative to output folder (e.g. ..\..\files\file.zzz). If false, they are left as absolute (e.g. D:\files\file.zzz)
        public bool m_bRelativiseMultimedia;

        // Maximum number of multimedia files per individual page
        public int m_nMaxNumberMultimediaFiles;

        // Title to use for index page
        public string m_sIndexTitle;

        // Folder in which to dump all the html output
        public string m_sOutputFolder;

        // Which version number of the software this config file is for
        public string m_sVersion;

        // Name of owner used in titles, descriptions of pages.
        public string m_sOwnersName;

        // Xref of root individual in tree, for link on front page.
        public string m_sFirstRecordXref;

        // Title to put on front page of website
        public string m_sTitle;

        // Filename of gedcom file to parse
        public string m_sInputFilename;

        // Path to folder containing this application.
        public string m_sApplicationPath;

        // Address of remote FTP server in which to put the files
        public string m_sFtpServer;

        // Username on remote server
        public string m_sFtpUsername;

        // Password for remote server
        public string m_sFtpPassword;

        // Folder on remote server in which to put the files
        public string m_sFtpUploadFolder;

        // String indicating image format to use to store mini tree file
        public string m_sMiniTreeImageFormat;

        // If true, the background image will be used to fill in the background of mini trees, giving the effect that they are transparent.
        public bool m_bFakeMiniTreeTransparency;

        // List of Xrefs for individuals to be mentioned on front page.
        public ArrayList m_alKeyIndividuals;

        // Set true if indexes are allowed to span multiple html files
        public bool m_bMultiPageIndexes;

        // Number of individuals to aim to list per index page.
        public uint m_uIndividualsPerIndexPage;

        // If true, website pages are opened in web browser once app exits with Finish button.
        public bool m_bOpenWebsiteOnExit;

        // If true, instead of pretending restricted individuals don't exist, they are shown as black boxes.
        public bool m_bOnlyConceal;

        // If true, clicking the picture will link to the original large picture.
        public bool m_bLinkOriginalPicture;

        // If true, the original picture will be renamed as it is copied. Now takes on role of m_renameMultimedia.
        public bool m_bRenameOriginalPicture;

        // Directory in which list of excluded individuals is stored
        public string m_sExcludeFileDir;

        // Name of file in which list of excluded individuals is stored
        public string m_sExcludeFileName;

        // Directory in which pictures were added from for an individual
        public string m_sLastPictureAddedDir;

        // If true, email addresses won't appear in web pages in harvestable form.
        public bool m_bObfuscateEmails;

        // If true, "Last updated <date>" appears on home page.
        public bool m_bAddHomePageCreateTime;

        // If true, pages get "Valid XHTML" validator link at bottom.
        public bool m_bIncludeValiditySticker;

        // If true, nickName else otherName appears in brackets in individual's index entry.
        public bool m_bIncludeNickNamesInIndex;

        // Optional text user can have displayed in each page footer.
        public string m_sCustomFooter;

        // If true, the user's custom individual reference number will appear in the index.
        public bool m_bIncludeUserRefInIndex;

        // If true, include the individuals (most appropriate, best) occupation in page header.
        public bool m_bOccupationHeadline;

        // True indicates that text in Commentary box (settings pane) should not be escaped. (i.e. HTML is preserved)
        public bool m_bCommentaryIsHtml;

        // True indicates that text in Footer box (settings pane) should not be escaped. (i.e. HTML is preserved)
        public bool m_bFooterIsHtml;

        // True means when naming html files, to use user record number if present for sFilename.
        public bool m_bUserRecFilename;

        // If false, doesn't include the navigation bar at the top of each page.
        public bool m_bIncludeNavbar;

        // If true, withheld records will use the indivdual's name in minitrees, rather than m_concealedName.
        public bool m_bUseWithheldNames;

        // If true, first names are put on a different gedcomLine to surnames in minitree individual boxes.
        public bool m_bConserveTreeWidth;

        // If true, the mini tree has siblings of the selected individuals ordered as they are in the GEDCOM. False means sort them by date. Significant when some siblings have no recorded birth date
        public bool m_bKeepSiblingOrder;

        // If false, no multimedia pics, images, files etc. will appear in HTML.
        public bool m_bAllowMultimedia;

        // If false, no Byte-Order-Mark will be output at the start of any UTF8 files generated.
        public bool m_bUseBom;

        // If true, the list of "Citations" on source records will not be generated.
        public bool m_bSupressBackreferences;

        // If true a help page will be included in the output, with a link at the top of each record page.
        public bool m_bIncludeHelppage; 

        
        // Constructor, sets default values for the config
        public CConfig()
        {
            m_configFilename = "GEDmill Config";
            m_bRestrictConfidential = false;
            m_bRestrictPrivacy = false;
            m_sOutputFolder = "";
            m_sUnknownName = "<unknown>"; // "no name" implied we knew them and they had no name.
            m_nNameCapitalisation = 1;
            m_sVersion = "1.11.0";
            m_sHtmlExtension = "html";
            m_bCopyMultimedia = true;
            m_sImageFolder = "multimedia";
            m_bRelativiseMultimedia = false;
            m_sApplicationPath = System.IO.Path.GetDirectoryName( System.Reflection.Assembly.GetExecutingAssembly().Location );
            m_sBackgroundImage = m_sApplicationPath + "\\bg-gedmill.jpg";
            m_uMaxImageWidth = 160; 
            m_uMaxImageHeight = 160;
            m_nMaxNumberMultimediaFiles = 32;
            m_nAgeForOccupation = 50;
            m_sOwnersName = Environment.UserName;
            m_sNoSurname = "No Surname";
            m_sIndexTitle = "Index Of Names";
            m_uMaxSourceImageWidth = 800;
            m_uMaxSourceImageHeight = 800;
            m_uMaxThumbnailImageWidth = 45;
            m_uMaxThumbnailImageHeight = 45;
            m_sFirstRecordXref = "";
            m_sTitle = "Family history";
            if( m_sOwnersName != null && m_sOwnersName != "" )
                m_sTitle += " of " + m_sOwnersName;
            m_sInputFilename = "";
            m_sFrontPageImageFilename = m_sApplicationPath + "\\gedmill.jpg";
            m_uTabSpaces = 8;
            m_sPlaceWord = "in";
            m_bCapitaliseEventDescriptions = true;
            m_bDataMayStartWithWhitespace = true;
            m_bRestrictAssociatedSources = true;
            m_bRenameMultimedia = true;
            m_bIndexLetterPerPage = false;
            m_bFrontPageStats = true;
            m_sCommentaryText = "";
            m_sFtpServer = "";
            m_sFtpUsername = "";
            m_sFtpPassword = "";
            m_sFtpUploadFolder = "/";
            m_ecHtmlCharset = ECharset.UTF8;

            // Reset those settings that can be modified by the user on the config screen.
            ResetUserSettings(); 
        }

        // Capitalises an individual's name according to config setting
        public string CapitaliseName( string name, ref string firstName, ref string surname )
        {
            if( name == null )
            {
                if (surname != null)
                {
                    surname = m_sUnknownName;
                }
                return m_sUnknownName;
            }

            string newName = "";
            switch( m_nNameCapitalisation )
            {
                case 1:
                case 0:
                    // capitalise surname (the bit in //s)
                    bool bSeenSlash = false;
                    bool bFirstName = true;
                    char oldc = '\0';
                    foreach( char c in name )
                    {
                        if( c == '/' )
                        {
                            bSeenSlash = !bSeenSlash;
                            if( bFirstName && oldc != ' ' && newName.Length > 0 )
                            {
                                // Ensure there is a space between first and last names (e.g. from "Fred/Bloggs/")
                                newName += ' ';
                                oldc = ' '; // To make oldc set to space too.
                            }
                            else
                            {
                                oldc = c;
                            }
                            bFirstName = false;
                        }
                        else if( bSeenSlash )
                        {
                            char cc = c;
                            if( m_nNameCapitalisation == 1 )
                            {
                                cc = char.ToUpper(cc);
                            }
                            newName += cc;
                            if (surname != null)
                            {
                                surname += cc;
                            }
                            oldc = c;
                        }
                        else
                        {
                            newName += c;

                            // Collapse multiple spaces into one
                            if( oldc != ' ' || c != ' ' )
                            {
                                if (bFirstName && firstName != null)
                                {
                                    firstName += c;
                                }
                                else if (!bFirstName && surname != null)
                                {
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

            // Strip trailing spaces from newName (happens if surname is blank)
            CGedcom.StripTrailingWhitespace( ref newName );

            // Strip trailing spaces from firstName (e.g. from "Fred /Bloggs/")
            CGedcom.StripTrailingWhitespace( ref firstName );

            return newName;
        }

        // Construct the HTTP URL for the created site's landing page
        public string FrontPageURL
        {
            get
            {
                return String.Concat( m_sOutputFolder, "\\", m_sFrontPageFilename, ".", m_sHtmlExtension );
            }
        }

        // Construct the HTTP URL for the created site's help page
        public string HelppageURL
        {
          get
          {
            return String.Concat( m_sOutputFolder, "\\", "help.", m_sHtmlExtension );
          }
        }

        // Serialise all the config settings into isolated storage.
        public void StoreSettings()
        {
            // Open the stream from the IsolatedStorage.
            IsolatedStorageFile isoStore = IsolatedStorageFile.GetStore( IsolatedStorageScope.User | IsolatedStorageScope.Assembly, null, null );
            Stream stream = new IsolatedStorageFileStream( m_configFilename, FileMode.Create, isoStore );

            if ( stream == null )
            {
                return;
            }
            try
            {
                // Serialize the configuration into the IsolatedStorage.
                IFormatter formatter = new BinaryFormatter();

                formatter.Serialize( stream, m_sVersion );
                formatter.Serialize( stream, m_bRestrictConfidential );
                formatter.Serialize( stream, m_bRestrictPrivacy );
                formatter.Serialize( stream, m_sOutputFolder );
                formatter.Serialize( stream, m_sUnknownName );
                formatter.Serialize( stream, m_nNameCapitalisation );
                formatter.Serialize( stream, m_sHtmlExtension );
                formatter.Serialize( stream, m_bCopyMultimedia );
                formatter.Serialize( stream, m_sImageFolder );
                formatter.Serialize( stream, m_bRelativiseMultimedia );
                formatter.Serialize( stream, m_sBackgroundImage );
                formatter.Serialize( stream, m_uMaxImageWidth );
                formatter.Serialize( stream, m_uMaxImageHeight );
                formatter.Serialize( stream, m_nMaxNumberMultimediaFiles );
                formatter.Serialize( stream, m_nAgeForOccupation );
                formatter.Serialize( stream, m_sOwnersName );
                formatter.Serialize( stream, m_sNoSurname );
                formatter.Serialize( stream, m_sIndexTitle );
                formatter.Serialize( stream, m_uMaxSourceImageWidth );
                formatter.Serialize( stream, m_uMaxSourceImageHeight );
                formatter.Serialize( stream, m_sFirstRecordXref );
                formatter.Serialize( stream, m_sTitle );
                formatter.Serialize( stream, m_sInputFilename );
                formatter.Serialize( stream, m_sApplicationPath );
                formatter.Serialize( stream, m_sFrontPageImageFilename );
                formatter.Serialize( stream, m_uTabSpaces );
                formatter.Serialize( stream, m_sPlaceWord );
                formatter.Serialize( stream, m_bCapitaliseEventDescriptions );
                formatter.Serialize( stream, m_bDataMayStartWithWhitespace );
                formatter.Serialize( stream, m_bRestrictAssociatedSources );
                formatter.Serialize( stream, m_bRenameMultimedia );
                formatter.Serialize( stream, m_bIndexLetterPerPage );
                formatter.Serialize( stream, m_sMiniTreeColourBranch );
                formatter.Serialize( stream, m_sMiniTreeColourIndiBorder );
                formatter.Serialize( stream, m_sMiniTreeColourIndiBackground );
                formatter.Serialize( stream, m_sMiniTreeColourIndiHighlight );
                formatter.Serialize( stream, m_sMiniTreeColourIndiShade );
                formatter.Serialize( stream, m_bFrontPageStats );
                formatter.Serialize( stream, m_sCommentaryText );
                formatter.Serialize( stream, m_sFtpServer );
                formatter.Serialize( stream, m_sFtpUsername );
                formatter.Serialize( stream, m_sFtpPassword );
                formatter.Serialize( stream, m_sFtpUploadFolder );
                formatter.Serialize( stream, m_sTreeFontName );
                formatter.Serialize( stream, m_fTreeFontSize );
                formatter.Serialize( stream, m_nTargetTreeWidth );
                formatter.Serialize( stream, m_sMiniTreeImageFormat );
                formatter.Serialize( stream, m_sMiniTreeColourIndiText );
                formatter.Serialize( stream, m_sMiniTreeColourIndiLink );
                formatter.Serialize( stream, m_sMiniTreeColourBackground );
                formatter.Serialize( stream, m_bShowMiniTrees );
                formatter.Serialize( stream, m_sUserEmailAddress );
                formatter.Serialize( stream, m_bFakeMiniTreeTransparency );
                int nKeyIndividuals = 0;
                if( m_alKeyIndividuals != null )
                {
                    nKeyIndividuals = m_alKeyIndividuals.Count;
                }
                formatter.Serialize( stream, nKeyIndividuals );
                if( m_alKeyIndividuals != null )
                {
                    foreach( string keyXref in m_alKeyIndividuals )
                    {
                        formatter.Serialize( stream, keyXref );
                    }
                }
                formatter.Serialize( stream, m_bMultiPageIndexes );
                formatter.Serialize( stream, m_uIndividualsPerIndexPage );
                formatter.Serialize( stream, m_bOpenWebsiteOnExit );
                formatter.Serialize( stream, m_sFrontPageFilename );
                formatter.Serialize( stream, m_ecHtmlCharset );
                formatter.Serialize( stream, m_bCreateCDROMFiles );
                formatter.Serialize( stream, m_nMaxNumberMultimediaFiles );
                formatter.Serialize( stream, m_bAllowMultipleImages );
                formatter.Serialize( stream, m_bAllowNonPictures );
                formatter.Serialize( stream, m_uMaxThumbnailImageWidth );
                formatter.Serialize( stream, m_uMaxThumbnailImageHeight );
                formatter.Serialize( stream, m_sMainWebsiteLink );
                formatter.Serialize( stream, m_bPreserveFrontPage );
                formatter.Serialize( stream, m_bPreserveStylesheet );
                formatter.Serialize( stream, m_sStylesheetFilename );
                formatter.Serialize( stream, m_sMiniTreeColourIndiBgConcealed );
                formatter.Serialize( stream, m_bOnlyConceal );
                formatter.Serialize( stream, m_sConcealedName );
                formatter.Serialize( stream, m_sMiniTreeColourIndiFgConcealed );
                formatter.Serialize( stream, m_bLinkOriginalPicture );
                formatter.Serialize( stream, m_bRenameOriginalPicture );
                formatter.Serialize( stream, m_sExcludeFileDir );
                formatter.Serialize( stream, m_sExcludeFileName );
                formatter.Serialize( stream, m_sLastPictureAddedDir );
                formatter.Serialize( stream, m_bObfuscateEmails );
                formatter.Serialize( stream, m_bAddHomePageCreateTime );
                formatter.Serialize( stream, m_bIncludeValiditySticker );
                formatter.Serialize( stream, m_bIncludeNickNamesInIndex );
                formatter.Serialize( stream, m_sCustomFooter );
                formatter.Serialize( stream, m_bIncludeUserRefInIndex );
                formatter.Serialize( stream, m_bOccupationHeadline );
                formatter.Serialize( stream, m_bCommentaryIsHtml );
                formatter.Serialize( stream, m_bFooterIsHtml );
                formatter.Serialize( stream, m_bUserRecFilename );
                formatter.Serialize( stream, m_bIncludeNavbar );
                formatter.Serialize( stream, m_bUseWithheldNames );
                uint uVersionMajor=1, uVersionMinor=11, uVersionBuild=0; // 1.9.2, 1.10.0, 1.10.1, 1.10.2, 1.10.3, 1.10.4, 1.11.0
                formatter.Serialize( stream, uVersionMajor );
                formatter.Serialize( stream, uVersionMinor );
                formatter.Serialize( stream, uVersionBuild );
                formatter.Serialize( stream, m_bConserveTreeWidth );
                formatter.Serialize( stream, m_bAllowMultimedia );
                formatter.Serialize( stream, m_bUseBom );
                formatter.Serialize( stream, m_bSupressBackreferences );
                formatter.Serialize( stream, m_bDataMayEndWithWhitespace );
                formatter.Serialize( stream, m_bKeepSiblingOrder );
                formatter.Serialize( stream, m_bIncludeHelppage );
            }
            catch( Exception e )
            {
                LogFile.TheLogFile.WriteLine( LogFile.DT_CONFIG, LogFile.EDebugLevel.Error, "Exception caught while writing MainForm.s_config : " + e.ToString() );
            }
            finally
            {
                stream.Close();
            }
        }

        // Deserialise all the settings from isolated storage.
        public void RecoverSettings()
        {
            uint uVersionMajor=0, uVersionMinor=0, uVersionBuild=0;

            IsolatedStorageFile isoStore = IsolatedStorageFile.GetStore( IsolatedStorageScope.User | IsolatedStorageScope.Assembly, null, null );
            if( isoStore.GetFileNames( m_configFilename ).Length == 0 )
            {
                // File doesn't exist. Leave defaults in place.
                return;
            }
            // Read the stream from Isolated Storage.
            Stream stream = new IsolatedStorageFileStream(m_configFilename, FileMode.OpenOrCreate, isoStore );
            if ( stream == null )
            {
                return;
            }

            try
            {
                // DeSerialize the Hashtable from stream.
                IFormatter formatter = new BinaryFormatter();

                m_sVersion = ( string )formatter.Deserialize(stream);
                m_bRestrictConfidential = ( bool )formatter.Deserialize(stream);
                m_bRestrictPrivacy = ( bool )formatter.Deserialize(stream);
                m_sOutputFolder = ( string )formatter.Deserialize(stream);
                m_sUnknownName = ( string )formatter.Deserialize(stream);
                m_nNameCapitalisation = ( int )formatter.Deserialize(stream);
                m_sHtmlExtension = ( string )formatter.Deserialize(stream);
                m_bCopyMultimedia = ( bool )formatter.Deserialize(stream);
                m_sImageFolder = ( string )formatter.Deserialize(stream);
                m_bRelativiseMultimedia = ( bool )formatter.Deserialize(stream);
                m_sBackgroundImage = ( string )formatter.Deserialize(stream);
                m_uMaxImageWidth = ( uint )formatter.Deserialize(stream);
                m_uMaxImageHeight = ( uint )formatter.Deserialize(stream);
                int deprecated_m_nMaxNumberMultimediaFiles = ( int )formatter.Deserialize(stream);
                m_nAgeForOccupation = ( int )formatter.Deserialize(stream);
                m_sOwnersName = ( string )formatter.Deserialize(stream);
                m_sNoSurname = ( string )formatter.Deserialize(stream);
                m_sIndexTitle = ( string )formatter.Deserialize(stream);
                m_uMaxSourceImageWidth = ( uint )formatter.Deserialize(stream);
                m_uMaxSourceImageHeight = ( uint )formatter.Deserialize(stream);
                m_sFirstRecordXref = ( string )formatter.Deserialize(stream);
                m_sTitle = ( string )formatter.Deserialize(stream);
                m_sInputFilename = ( string )formatter.Deserialize(stream);
                m_sApplicationPath = ( string )formatter.Deserialize(stream);
                m_sFrontPageImageFilename = ( string )formatter.Deserialize(stream);
                m_uTabSpaces = ( uint )formatter.Deserialize(stream);
                m_sPlaceWord = (string)formatter.Deserialize(stream);
                m_bCapitaliseEventDescriptions = (bool)formatter.Deserialize(stream);
                m_bDataMayStartWithWhitespace = (bool)formatter.Deserialize(stream);
                m_bRestrictAssociatedSources = (bool)formatter.Deserialize(stream);
                m_bRenameMultimedia = (bool)formatter.Deserialize(stream);
                m_bIndexLetterPerPage = (bool)formatter.Deserialize(stream);
                m_sMiniTreeColourBranch = (string)formatter.Deserialize(stream);
                m_sMiniTreeColourIndiBorder = (string)formatter.Deserialize(stream);
                m_sMiniTreeColourIndiBackground = (string)formatter.Deserialize(stream);
                m_sMiniTreeColourIndiHighlight = (string)formatter.Deserialize(stream);
                m_sMiniTreeColourIndiShade = (string)formatter.Deserialize(stream);
                m_bFrontPageStats = (bool)formatter.Deserialize(stream);
                m_sCommentaryText = (string)formatter.Deserialize(stream);
                m_sFtpServer = (string)formatter.Deserialize(stream);
                m_sFtpUsername = (string)formatter.Deserialize(stream);
                m_sFtpPassword = (string)formatter.Deserialize(stream);
                m_sFtpUploadFolder = (string)formatter.Deserialize(stream);
                m_sTreeFontName = (string)formatter.Deserialize(stream);
                m_fTreeFontSize = (float)formatter.Deserialize(stream);
                m_nTargetTreeWidth = (int)formatter.Deserialize(stream);
                m_sMiniTreeImageFormat = (string)formatter.Deserialize(stream);
                m_sMiniTreeColourIndiText = (string)formatter.Deserialize(stream);
                m_sMiniTreeColourIndiLink = (string)formatter.Deserialize(stream);
                m_sMiniTreeColourBackground = (string)formatter.Deserialize(stream);
                m_bShowMiniTrees = (bool)formatter.Deserialize(stream);
                m_sUserEmailAddress = (string)formatter.Deserialize(stream);
                m_bFakeMiniTreeTransparency = (bool)formatter.Deserialize(stream);
                int nKeyIndividuals = (int)formatter.Deserialize(stream);
                m_alKeyIndividuals = new ArrayList();
                while( nKeyIndividuals-- > 0 )
                {
                    string keyXref = (string)formatter.Deserialize(stream);
                    m_alKeyIndividuals.Add( keyXref );
                }
                m_bMultiPageIndexes = (bool)formatter.Deserialize(stream);
                m_uIndividualsPerIndexPage = (uint)formatter.Deserialize(stream);
                m_bOpenWebsiteOnExit = (bool)formatter.Deserialize(stream);
                m_sFrontPageFilename = (string)formatter.Deserialize(stream);
                m_ecHtmlCharset = (ECharset)formatter.Deserialize(stream);
                m_bCreateCDROMFiles = (bool)formatter.Deserialize(stream);
                m_nMaxNumberMultimediaFiles = ( int )formatter.Deserialize(stream);
                m_bAllowMultipleImages = (bool)formatter.Deserialize(stream);
                m_bAllowNonPictures = (bool)formatter.Deserialize(stream);
                m_uMaxThumbnailImageWidth = ( uint )formatter.Deserialize(stream);
                m_uMaxThumbnailImageHeight = ( uint )formatter.Deserialize(stream);
                m_sMainWebsiteLink = (string)formatter.Deserialize(stream);
                m_bPreserveFrontPage = (bool)formatter.Deserialize(stream);
                m_bPreserveStylesheet = (bool)formatter.Deserialize(stream);
                m_sStylesheetFilename = (string)formatter.Deserialize(stream);
                m_sMiniTreeColourIndiBgConcealed = (string)formatter.Deserialize(stream);
                m_bOnlyConceal = (bool)formatter.Deserialize(stream);
                m_sConcealedName = (string)formatter.Deserialize(stream);
                m_sMiniTreeColourIndiFgConcealed = (string)formatter.Deserialize(stream);
                m_bLinkOriginalPicture = (bool)formatter.Deserialize(stream);
                m_bRenameOriginalPicture = (bool)formatter.Deserialize(stream);
                m_sExcludeFileDir = (string)formatter.Deserialize(stream);
                m_sExcludeFileName = (string)formatter.Deserialize(stream);
                m_sLastPictureAddedDir = (string)formatter.Deserialize(stream);
                m_bObfuscateEmails = (bool)formatter.Deserialize(stream);
                m_bAddHomePageCreateTime = (bool)formatter.Deserialize(stream);
                m_bIncludeValiditySticker = (bool)formatter.Deserialize(stream);
                m_bIncludeNickNamesInIndex = (bool)formatter.Deserialize(stream);
                m_sCustomFooter = (string)formatter.Deserialize(stream);
                m_bIncludeUserRefInIndex = (bool)formatter.Deserialize(stream);
                m_bOccupationHeadline = (bool)formatter.Deserialize(stream);
                m_bCommentaryIsHtml = (bool)formatter.Deserialize(stream);
                m_bFooterIsHtml = (bool)formatter.Deserialize(stream);
                m_bUserRecFilename = (bool)formatter.Deserialize( stream );
                m_bIncludeNavbar = (bool)formatter.Deserialize( stream );
                m_bUseWithheldNames = (bool)formatter.Deserialize( stream );
                uVersionMajor = (uint)formatter.Deserialize( stream );
                uVersionMinor = (uint)formatter.Deserialize( stream );
                uVersionBuild = (uint)formatter.Deserialize( stream );
                m_bConserveTreeWidth = (bool)formatter.Deserialize( stream );
                m_bAllowMultimedia = (bool)formatter.Deserialize( stream );
                m_bUseBom = (bool)formatter.Deserialize( stream );
                m_bSupressBackreferences = (bool)formatter.Deserialize( stream );
                m_bDataMayEndWithWhitespace = (bool)formatter.Deserialize(stream);
                m_bKeepSiblingOrder = (bool)formatter.Deserialize( stream );
                m_bIncludeHelppage = (bool)formatter.Deserialize( stream );
            }
            catch( Exception e )
            {
                LogFile.TheLogFile.WriteLine( LogFile.DT_CONFIG, LogFile.EDebugLevel.Error, "Exception caught while reading MainForm.s_config : " + e.ToString() );
            }
            finally
            {
                // We are done with it.
                stream.Close();
            }

            if( !(uVersionMajor > 1 || uVersionMinor > 9 || (uVersionMinor == 9 && uVersionBuild > 0) ) )
            {
                // This config came from a version less than 1.9.1
                // Need to mangle m_mainWebsiteLink, because it now must include the http:// prefix.
                if( m_sMainWebsiteLink.Length > 0 )
                {
                    m_sMainWebsiteLink = "http://" + m_sMainWebsiteLink;
                }
            }
        }

        // Reset those settings that can be modified by the user on the config screen.
        public void ResetUserSettings()
        {
            m_sTreeFontName = "Arial";
            m_fTreeFontSize = 7.2f;
            m_nTargetTreeWidth = 800;
            m_sMiniTreeImageFormat = "gif";
            m_sMiniTreeColourBranch = "#000000";
            m_sMiniTreeColourIndiBorder = "#000000";
            m_sMiniTreeColourIndiBackground = "#ffffd2";
            m_sMiniTreeColourIndiHighlight = "#ffffff";
            m_sMiniTreeColourIndiBgConcealed = "#cccccc";
            m_sMiniTreeColourIndiFgConcealed = "#000000";
            m_sMiniTreeColourIndiShade = "#ffffd2";
            m_sMiniTreeColourIndiText = "#000000";
            m_sMiniTreeColourIndiLink = "#3333ff";
            m_sMiniTreeColourBackground = "#aaaaaa";
            m_bFakeMiniTreeTransparency = false;
            m_bShowMiniTrees = true;
            m_sUserEmailAddress = "";
            m_alKeyIndividuals = new ArrayList();
            m_bMultiPageIndexes = true;
            m_uIndividualsPerIndexPage = 1000;
            m_bOpenWebsiteOnExit = true;
            m_sFrontPageFilename = "home";
            m_bAllowMultipleImages = false;
            m_bCreateCDROMFiles = false;
            m_bAllowNonPictures = true;
            m_sMainWebsiteLink = "";
            m_bPreserveFrontPage = false;
            m_bPreserveStylesheet = false;
            m_sStylesheetFilename = "gedmill-style";
            m_bOnlyConceal = false;
            m_sConcealedName = "Private Record";
            m_bLinkOriginalPicture = false;
            m_bRenameOriginalPicture = false;
            m_sExcludeFileDir = "";
            m_sExcludeFileName = "";
            m_sLastPictureAddedDir = "";
            m_bObfuscateEmails = false;
            m_bAddHomePageCreateTime = true;
            m_bIncludeValiditySticker = false;
            m_bIncludeNickNamesInIndex = true;
            m_sCustomFooter = "";
            m_bIncludeUserRefInIndex = false;
            m_bOccupationHeadline = true;
            m_bCommentaryIsHtml = false;
            m_bFooterIsHtml = false;
            m_bUserRecFilename = false;
            m_bIncludeNavbar = true;
            m_bUseWithheldNames = false;
            m_bConserveTreeWidth = false;
            m_bKeepSiblingOrder = false;
            m_bAllowMultimedia = true;
            m_bUseBom = false;
            m_bSupressBackreferences = false;
            m_bDataMayEndWithWhitespace = false;
            m_bIncludeHelppage = true;
        }
    }
}
