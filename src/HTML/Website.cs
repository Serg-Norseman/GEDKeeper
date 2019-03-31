/* CWebsite.cs
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
using System.Drawing;
using System.IO;
using GEDmill.Exceptions;
using GEDmill.MiniTree;
using GKCommon.GEDCOM;
using GKCore.Logging;

namespace GEDmill.HTML
{
    /// <summary>
    /// This is a single class that has the sole purpose of creating the website by creating and calling the 
    /// various other creators (individuals, sources, front page, index, css).
    /// It also copies site-wide multimedia files, javascript and CD ROM autorun loader.
    /// </summary>
    public class Website
    {
        private static readonly ILogger fLogger = LogManager.GetLogger(CConfig.LOG_FILE, CConfig.LOG_LEVEL, typeof(Website).Name);

        // The raw data that we are turning into a website.
        protected GEDCOMTree fTree;

        // Pointer to the window showing the progress bar, so that web page creation progress can be shown to user.
        protected IProgressCallback fProgressWindow;


        public Website(GEDCOMTree gedcom, IProgressCallback progress)
        {
            fTree = gedcom;
            fProgressWindow = progress;
        }

        // The heart of GEDmill is here.
        public void Create()
        {
            fLogger.WriteInfo("CWebsite::Create()");

            // 1 means the process was aborted, for signalling back to calling thread. 2 means file nError.      
            ThreadError threaderror = new ThreadError(1, "No error");

            try {
                // The value to indicate in the progress bar to show how much of the website creation is complete.
                int nProgress = 0;

                int[] gfstats = fTree.GetRecordStats();

                // The maximum value of the progress bar, i.e. when website creation is fully complete.
                int nProgressMax =
                    1  // Site-wide multimedia files
                  + 1  // W3C Sticker
                  + 1  // Background image
                  + 1  // Style sheet
                  + gfstats[(int)GEDCOMRecordType.rtIndividual]
                  + 1  // Individuals Index
                  + gfstats[(int)GEDCOMRecordType.rtSource]
                  + 1  // Front page
                  + 1  // Help page
                  + 1  // CD ROM (Doesn't matter here that CD ROM autorun might not be included.)
                  + 1; // Scripts (Doesn't matter here that scripts might not be included.)

                // The paintbox with which to draw the mini tree
                Paintbox paintbox = new Paintbox(MainForm.Config);
                paintbox.SetBackgroundImage(MainForm.Config.BackgroundImage);

                // Object to keep count of number of files created etc.
                Stats stats = new Stats();

                // Here goes....

                // Start the progress indicator.
                fProgressWindow.Begin(0, nProgressMax);
                if (fProgressWindow.IsAborting) {
                    return;
                }
                // Copy the images to use in place of non-pic multimedia files.
                fProgressWindow.SetText("Copying multimedia");
                CopyIcons();
                if (fProgressWindow.IsAborting) {
                    return;
                }
                fProgressWindow.StepTo(++nProgress);

                // Copy the W3C sticker file.
                fProgressWindow.SetText("Copying W3C sticker");
                string sW3CFilename = "";
                if (MainForm.Config.IncludeValiditySticker) {
                    sW3CFilename = CopyW3CSticker();
                }
                if (fProgressWindow.IsAborting) {
                    return;
                }
                fProgressWindow.StepTo(++nProgress);

                // Create the index creator for use by the individuals records creator.
                CreatorIndexIndividuals indiIndexCreator = new CreatorIndexIndividuals(fTree, fProgressWindow, sW3CFilename);

                // Copy the image for the background of the webpages.
                fProgressWindow.SetText("Copying background image");
                string backgroundImageFilename = CopyBackgroundImage();
                if (fProgressWindow.IsAborting) {
                    return;
                }
                fProgressWindow.StepTo(++nProgress);

                // Create the style sheet
                fProgressWindow.SetText("Creating style sheet");
                string cssFilename = String.Concat(MainForm.Config.OutputFolder, "\\", MainForm.Config.StylesheetFilename, ".css");
                if (MainForm.Config.StylesheetFilename.Length > 0 && (!MainForm.Config.PreserveStylesheet || !File.Exists(cssFilename))) {
                    CreatorStylesheet csc = new CreatorStylesheet(fTree, fProgressWindow, sW3CFilename, cssFilename, backgroundImageFilename);
                    csc.Create();
                }

                if (fProgressWindow.IsAborting) {
                    return;
                }

                fProgressWindow.StepTo(++nProgress);

                // Create the pages for the individual records.
                fProgressWindow.SetText("Creating individual pages");
                var indiList = fTree.GetRecords<GEDCOMIndividualRecord>();
                foreach (GEDCOMIndividualRecord ir in indiList) {
                    CreatorRecordIndividual ipc = new CreatorRecordIndividual(fTree, fProgressWindow, sW3CFilename, ir, indiIndexCreator, paintbox);
                    if (ipc.Create(stats)) {
                        stats.Individuals++;
                    }
                    if (fProgressWindow.IsAborting) {
                        return;
                    }

                    fProgressWindow.StepTo(++nProgress);
                }

                // Create the index for the individual records pages.
                fProgressWindow.SetText("Creating individuals index");
                indiIndexCreator.Create();
                if (fProgressWindow.IsAborting) {
                    return;
                }

                fProgressWindow.StepTo(++nProgress);

                // Clear list of copied files, so that source images get copied afresh
                // and so get resized differently to any indi images based on the same file.
                Creator.ClearCopiedFilesList();

                // Create the pages for the source records.
                fProgressWindow.SetText("Creating source pages");
                var sourList = fTree.GetRecords<GEDCOMSourceRecord>();
                foreach (GEDCOMSourceRecord sr in sourList) {
                    CreatorRecordSource spc = new CreatorRecordSource(fTree, fProgressWindow, sW3CFilename, sr);
                    if (spc.Create(stats)) {
                        stats.Sources++;
                    }

                    if (fProgressWindow.IsAborting) {
                        return;
                    }

                    fProgressWindow.StepTo(++nProgress);
                }

                if (fProgressWindow.IsAborting) {
                    return;
                }

                // Create the front page
                fProgressWindow.SetText("Creating front page");
                string front_page_filename = String.Concat(MainForm.Config.OutputFolder, "\\", MainForm.Config.FrontPageFilename, ".", MainForm.Config.HtmlExtension);
                if (MainForm.Config.FrontPageFilename.Length > 0 && (!MainForm.Config.PreserveFrontPage || !File.Exists(front_page_filename))) {
                    CreatorFrontPage fpc = new CreatorFrontPage(fTree, fProgressWindow, sW3CFilename, stats);
                    fpc.Create();
                }
                fProgressWindow.StepTo(++nProgress);
                if (fProgressWindow.IsAborting) {
                    return;
                }


                // Create the help page
                fProgressWindow.SetText("Creating help page");
                string help_page_filename = String.Concat(MainForm.Config.OutputFolder, "\\", "help.", MainForm.Config.HtmlExtension);
                if (MainForm.Config.IncludeHelpPage) {
                    CreatorHelpPage hpc = new CreatorHelpPage(fTree, fProgressWindow, sW3CFilename);
                    hpc.Create();
                }
                fProgressWindow.StepTo(++nProgress);
                if (fProgressWindow.IsAborting) {
                    return;
                }
                
                // Copy the CD ROM autorun file
                fProgressWindow.SetText("Creating CD-ROM files");
                if (MainForm.Config.CreateCDROMFiles) {
                    CreateCDROMFiles(fTree);
                }
                if (fProgressWindow.IsAborting) {
                    return;
                }

                fProgressWindow.StepTo(++nProgress);

                // Copy the Javascript
                fProgressWindow.SetText("Creating Javascript file");
                if (MainForm.Config.AllowMultipleImages) // Currently (10Dec08) the only thing that uses javascript is the multiple images feature.
                {
                    CreateJavascriptFiles(fTree);
                }
                if (fProgressWindow.IsAborting) {
                    return;
                }

                fProgressWindow.StepTo(++nProgress);

                // Done
                fLogger.WriteInfo("Finished");
                fProgressWindow.SetText("Done");
                threaderror.Error = 0;
                threaderror.Message = "";
            } catch (ArgumentException e) {
                fLogger.WriteError("Caught Argument Exception : ", e);
                threaderror.Error = 2; // 2 => abnormal abort.
                threaderror.Message = "";
            } catch (IOException e) {
                fLogger.WriteError("Caught IO Exception : ", e);
                threaderror.Error = 2; // 2 => abnormal abort.
                threaderror.Message = "";
            } catch (NullReferenceException e) {
                fLogger.WriteError("Caught NullReference Exception : ", e);
                threaderror.Error = 2; // 2 => abnormal abort.
                threaderror.Message = "";
            } catch (HTMLException e) {
                threaderror.Error = 2; // 2 => abnormal abort.
                threaderror.Message = e.Message;
            } catch (Exception e) {
                fLogger.WriteError("Caught Exception : ", e);
                threaderror.Error = 2; // 2 => abnormal abort.
                threaderror.Message = "";
            } finally {
                fLogger.WriteInfo("Thread ending...");
                if (fProgressWindow != null) {
                    fProgressWindow.End(threaderror);
                }
            }
        }

        // Copy the files for the images to use in place of non-picture multimedia files.
        private void CopyIcons()
        {
            fLogger.WriteInfo("Copying icon files...");
            try {
                // uint num_copied = 0;
                Rectangle rectNewArea = new Rectangle(0, 0, 0, 0);
                Creator.CopyMultimedia(MainForm.Config.ApplicationPath + "\\gmaudio.png", "", MainForm.Config.MaxImageWidth, MainForm.Config.MaxImageHeight, ref rectNewArea, null);
                rectNewArea = new Rectangle(0, 0, 0, 0);
                Creator.CopyMultimedia(MainForm.Config.ApplicationPath + "\\gmaudio_sm.png", "", MainForm.Config.MaxImageWidth, MainForm.Config.MaxImageHeight, ref rectNewArea, null);
                rectNewArea = new Rectangle(0, 0, 0, 0);
                Creator.CopyMultimedia(MainForm.Config.ApplicationPath + "\\gmaudion.png", "", MainForm.Config.MaxImageWidth, MainForm.Config.MaxImageHeight, ref rectNewArea, null);
                rectNewArea = new Rectangle(0, 0, 0, 0);
                Creator.CopyMultimedia(MainForm.Config.ApplicationPath + "\\gmvideo.png", "", MainForm.Config.MaxImageWidth, MainForm.Config.MaxImageHeight, ref rectNewArea, null);
                rectNewArea = new Rectangle(0, 0, 0, 0);
                Creator.CopyMultimedia(MainForm.Config.ApplicationPath + "\\gmvideo_sm.png", "", MainForm.Config.MaxImageWidth, MainForm.Config.MaxImageHeight, ref rectNewArea, null);
                rectNewArea = new Rectangle(0, 0, 0, 0);
                Creator.CopyMultimedia(MainForm.Config.ApplicationPath + "\\gmvideon.png", "", MainForm.Config.MaxImageWidth, MainForm.Config.MaxImageHeight, ref rectNewArea, null);
                rectNewArea = new Rectangle(0, 0, 0, 0);
                Creator.CopyMultimedia(MainForm.Config.ApplicationPath + "\\gmdoc.png", "", MainForm.Config.MaxImageWidth, MainForm.Config.MaxImageHeight, ref rectNewArea, null);
                rectNewArea = new Rectangle(0, 0, 0, 0);
                Creator.CopyMultimedia(MainForm.Config.ApplicationPath + "\\gmdoc_sm.png", "", MainForm.Config.MaxImageWidth, MainForm.Config.MaxImageHeight, ref rectNewArea, null);
                rectNewArea = new Rectangle(0, 0, 0, 0);
                Creator.CopyMultimedia(MainForm.Config.ApplicationPath + "\\gmdocn.png", "", MainForm.Config.MaxImageWidth, MainForm.Config.MaxImageHeight, ref rectNewArea, null);
            } catch (IOException e) {
                fLogger.WriteError("Caught io exception while copying nonpic images: {0}", e);
            } catch (ArgumentException e) {
                fLogger.WriteError("Caught argument exception while copying nonpic images: {0}", e);
            }
        }

        // Copy valid XHTML sticker file.
        private string CopyW3CSticker()
        {
            string sW3CFile = "";
            fLogger.WriteInfo("Copying W3C sticker ...");
            try {
                Rectangle rectNewArea = new Rectangle(0, 0, 0, 0);
                sW3CFile = Creator.CopyMultimedia(MainForm.Config.ApplicationPath + "\\valid-xhtml10.png", "", 0, 0, ref rectNewArea, null);
            } catch (IOException e) {
                fLogger.WriteError("Caught io exception while copying W3C sticker: {0}", e);
            } catch (ArgumentException e) {
                fLogger.WriteError("Caught argument exception while copying W3C sticker: {0}", e);
            }
            return sW3CFile;
        }

        // Copies the image for the background of the webpages.
        // Returns the sFilename of the copy.
        private string CopyBackgroundImage()
        {
            string sBackgroundImage = "";
            if (MainForm.Config.BackgroundImage != null && MainForm.Config.BackgroundImage.Length > 0) {
                try {
                    Rectangle newArea = new Rectangle(0, 0, 0, 0);
                    sBackgroundImage = Creator.CopyMultimedia(MainForm.Config.BackgroundImage, "", 0, 0, ref newArea, null);
                } catch (IOException e) {
                    fLogger.WriteError("Caught io exception while copying background image: {0}", e);
                    sBackgroundImage = "";
                } catch (ArgumentException e) {
                    fLogger.WriteError("Caught argument exception while copying background image: {0}", e);
                    sBackgroundImage = "";
                }
            }
            return sBackgroundImage;
        }

        // Copy the CD ROM autorun loader program.
        private void CreateCDROMFiles(GEDCOMTree gedcom)
        {
            string sHomepageUrl = String.Concat(MainForm.Config.FrontPageFilename, ".", MainForm.Config.HtmlExtension);

            if (sHomepageUrl != null && MainForm.Config.OutputFolder != null && MainForm.Config.OutputFolder != "") {
                string sFileopenSrc = MainForm.Config.ApplicationPath + "\\fileopen.exe";
                string sFileopenDest = MainForm.Config.OutputFolder + "\\fileopen.exe";
                string sAutorunDest = MainForm.Config.OutputFolder + "\\autorun.inf";

                // Copy fileopen.exe into output folder
                if (File.Exists(sFileopenDest)) {
                    File.SetAttributes(sFileopenDest, FileAttributes.Normal);
                    File.Delete(sFileopenDest);
                }
                fLogger.WriteInfo("Copying fileopen.exe");
                File.Copy(sFileopenSrc, sFileopenDest, true);


                // Create autorun.inf
                if (File.Exists(sAutorunDest)) {
                    File.SetAttributes(sAutorunDest, FileAttributes.Normal);
                    File.Delete(sAutorunDest);
                }
                fLogger.WriteInfo("Creating autorun.inf");

                FileStream fs = new FileStream(sAutorunDest, FileMode.Create);
                StreamWriter sw = new StreamWriter(fs, System.Text.Encoding.ASCII);

                sw.WriteLine("[AUTORUN]");
                sw.WriteLine(String.Concat("open=fileopen.exe \"", sHomepageUrl, "\""));
                sw.WriteLine("");

                sw.Close();
            }
        }

        // Copy the javascript picture-selection script.
        private void CreateJavascriptFiles(GEDCOMTree gedcom)
        {
            if (MainForm.Config.OutputFolder != null && MainForm.Config.OutputFolder != "") {
                string sJavascriptSrc = MainForm.Config.ApplicationPath + "\\gedmill.js";
                string sJavascriptDest = MainForm.Config.OutputFolder + "\\gedmill.js";
                if (File.Exists(sJavascriptSrc)) {
                    // Copy gedmill.js into output folder
                    if (File.Exists(sJavascriptDest)) {
                        File.SetAttributes(sJavascriptDest, FileAttributes.Normal);
                        File.Delete(sJavascriptDest);
                    }
                    fLogger.WriteInfo("Copying gedmill.js");
                    File.Copy(sJavascriptSrc, sJavascriptDest, true);
                } else {
                    fLogger.WriteError(string.Format("{0} not found. No Javascript.", sJavascriptSrc));
                }
            }
        }
    }
}
