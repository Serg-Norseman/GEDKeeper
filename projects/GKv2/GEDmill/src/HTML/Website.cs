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
using System.Drawing;
using System.IO;
using GDModel;
using GEDmill.MiniTree;
using GEDmill.Model;
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
        protected GDMTree fTree;

        // Pointer to the window showing the progress bar, so that web page creation progress can be shown to user.
        protected IProgressCallback fProgressWindow;


        public Website(GDMTree tree, IProgressCallback progress)
        {
            fTree = tree;
            fProgressWindow = progress;
        }

        // The heart of GEDmill is here.
        public void Create()
        {
            fLogger.WriteInfo("Website::Create()");

            // 1 means the process was aborted, for signalling back to calling thread. 2 means file nError.      
            ThreadError threaderror = new ThreadError(1, "No error");

            try {
                // The value to indicate in the progress bar to show how much of the website creation is complete.
                int progress = 0;

                int[] gfstats = fTree.GetRecordStats();

                // The maximum value of the progress bar, i.e. when website creation is fully complete.
                int progressMax =
                    1  // Site-wide multimedia files
                  + 1  // W3C Sticker
                  + 1  // Background image
                  + 1  // Style sheet
                  + gfstats[(int)GDMRecordType.rtIndividual]
                  + 1  // Individuals Index
                  + gfstats[(int)GDMRecordType.rtSource]
                  + 1  // Front page
                  + 1  // Help page
                  + 1  // CD ROM (Doesn't matter here that CD ROM autorun might not be included.)
                  + 1; // Scripts (Doesn't matter here that scripts might not be included.)

                // The paintbox with which to draw the mini tree
                Paintbox paintbox = new Paintbox(CConfig.Instance);
                paintbox.SetBackgroundImage(CConfig.Instance.BackgroundImage);

                // Object to keep count of number of files created etc.
                Stats stats = new Stats();

                // Here goes....

                // Start the progress indicator.
                fProgressWindow.Begin(0, progressMax);
                if (fProgressWindow.IsAborting) {
                    return;
                }
                // Copy the images to use in place of non-pic multimedia files.
                fProgressWindow.SetText("Copying multimedia");
                CopyIcons();
                if (fProgressWindow.IsAborting) {
                    return;
                }
                fProgressWindow.StepTo(++progress);

                // Copy the W3C sticker file.
                fProgressWindow.SetText("Copying W3C sticker");
                string sW3CFilename = "";
                if (CConfig.Instance.IncludeValiditySticker) {
                    sW3CFilename = CopyW3CSticker();
                }
                if (fProgressWindow.IsAborting) {
                    return;
                }
                fProgressWindow.StepTo(++progress);

                // Create the index creator for use by the individuals records creator.
                var indiIndexCreator = new CreatorIndexIndividuals(fTree, fProgressWindow, sW3CFilename);

                // Copy the image for the background of the webpages.
                fProgressWindow.SetText("Copying background image");
                string backgroundImageFilename = CopyBackgroundImage();
                if (fProgressWindow.IsAborting) {
                    return;
                }
                fProgressWindow.StepTo(++progress);

                // Create the style sheet
                fProgressWindow.SetText("Creating style sheet");
                string cssFilename = string.Concat(CConfig.Instance.OutputFolder, "\\", CConfig.Instance.StylesheetFilename, ".css");
                if (CConfig.Instance.StylesheetFilename.Length > 0 && (!CConfig.Instance.PreserveStylesheet || !File.Exists(cssFilename))) {
                    var csc = new CreatorStylesheet(fTree, fProgressWindow, sW3CFilename, cssFilename, backgroundImageFilename);
                    csc.Create();
                }

                if (fProgressWindow.IsAborting) {
                    return;
                }

                fProgressWindow.StepTo(++progress);

                // Create the pages for the individual records.
                fProgressWindow.SetText("Creating individual pages");
                var indiList = fTree.GetRecords<GDMIndividualRecord>();
                foreach (GDMIndividualRecord ir in indiList) {
                    var ipc = new CreatorRecordIndividual(fTree, fProgressWindow, sW3CFilename, ir, indiIndexCreator, paintbox);
                    if (ipc.Create(stats)) {
                        stats.Individuals++;
                    }
                    if (fProgressWindow.IsAborting) {
                        return;
                    }

                    fProgressWindow.StepTo(++progress);
                }

                // Create the index for the individual records pages.
                fProgressWindow.SetText("Creating individuals index");
                indiIndexCreator.Create();
                if (fProgressWindow.IsAborting) {
                    return;
                }

                fProgressWindow.StepTo(++progress);

                // Clear list of copied files, so that source images get copied afresh
                // and so get resized differently to any indi images based on the same file.
                Creator.ClearCopiedFilesList();

                // Create the pages for the source records.
                fProgressWindow.SetText("Creating source pages");
                var sourList = fTree.GetRecords<GDMSourceRecord>();
                foreach (GDMSourceRecord sr in sourList) {
                    var spc = new CreatorRecordSource(fTree, fProgressWindow, sW3CFilename, sr);
                    if (spc.Create(stats)) {
                        stats.Sources++;
                    }

                    if (fProgressWindow.IsAborting) {
                        return;
                    }

                    fProgressWindow.StepTo(++progress);
                }

                if (fProgressWindow.IsAborting) {
                    return;
                }

                // Create the front page
                fProgressWindow.SetText("Creating front page");
                string front_page_filename = string.Concat(CConfig.Instance.OutputFolder, "\\", CConfig.Instance.FrontPageFilename, ".", CConfig.Instance.HtmlExtension);
                if (CConfig.Instance.FrontPageFilename.Length > 0 && (!CConfig.Instance.PreserveFrontPage || !File.Exists(front_page_filename))) {
                    CreatorFrontPage fpc = new CreatorFrontPage(fTree, fProgressWindow, sW3CFilename, stats);
                    fpc.Create();
                }
                fProgressWindow.StepTo(++progress);
                if (fProgressWindow.IsAborting) {
                    return;
                }

                // Create the help page
                fProgressWindow.SetText("Creating help page");
                string help_page_filename = string.Concat(CConfig.Instance.OutputFolder, "\\", "help.", CConfig.Instance.HtmlExtension);
                if (CConfig.Instance.IncludeHelpPage) {
                    var hpc = new CreatorHelpPage(fTree, fProgressWindow, sW3CFilename);
                    hpc.Create();
                }
                fProgressWindow.StepTo(++progress);
                if (fProgressWindow.IsAborting) {
                    return;
                }
                
                // Copy the CD ROM autorun file
                fProgressWindow.SetText("Creating CD-ROM files");
                if (CConfig.Instance.CreateCDROMFiles) {
                    CreateCDROMFiles();
                }
                if (fProgressWindow.IsAborting) {
                    return;
                }

                fProgressWindow.StepTo(++progress);

                // Copy the Javascript
                fProgressWindow.SetText("Creating Javascript file");
                // Currently (10Dec08) the only thing that uses javascript is the multiple images feature.
                if (CConfig.Instance.AllowMultipleImages) {
                    CreateJavascriptFiles();
                }
                if (fProgressWindow.IsAborting) {
                    return;
                }

                fProgressWindow.StepTo(++progress);

                // Done
                fLogger.WriteInfo("Website::CreateFinished");
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
                Creator.CopyMultimedia(CConfig.Instance.ApplicationPath + "\\gmaudio.png", "", CConfig.Instance.MaxImageWidth, CConfig.Instance.MaxImageHeight, ref rectNewArea, null);
                rectNewArea = new Rectangle(0, 0, 0, 0);
                Creator.CopyMultimedia(CConfig.Instance.ApplicationPath + "\\gmaudio_sm.png", "", CConfig.Instance.MaxImageWidth, CConfig.Instance.MaxImageHeight, ref rectNewArea, null);
                rectNewArea = new Rectangle(0, 0, 0, 0);
                Creator.CopyMultimedia(CConfig.Instance.ApplicationPath + "\\gmaudion.png", "", CConfig.Instance.MaxImageWidth, CConfig.Instance.MaxImageHeight, ref rectNewArea, null);
                rectNewArea = new Rectangle(0, 0, 0, 0);
                Creator.CopyMultimedia(CConfig.Instance.ApplicationPath + "\\gmvideo.png", "", CConfig.Instance.MaxImageWidth, CConfig.Instance.MaxImageHeight, ref rectNewArea, null);
                rectNewArea = new Rectangle(0, 0, 0, 0);
                Creator.CopyMultimedia(CConfig.Instance.ApplicationPath + "\\gmvideo_sm.png", "", CConfig.Instance.MaxImageWidth, CConfig.Instance.MaxImageHeight, ref rectNewArea, null);
                rectNewArea = new Rectangle(0, 0, 0, 0);
                Creator.CopyMultimedia(CConfig.Instance.ApplicationPath + "\\gmvideon.png", "", CConfig.Instance.MaxImageWidth, CConfig.Instance.MaxImageHeight, ref rectNewArea, null);
                rectNewArea = new Rectangle(0, 0, 0, 0);
                Creator.CopyMultimedia(CConfig.Instance.ApplicationPath + "\\gmdoc.png", "", CConfig.Instance.MaxImageWidth, CConfig.Instance.MaxImageHeight, ref rectNewArea, null);
                rectNewArea = new Rectangle(0, 0, 0, 0);
                Creator.CopyMultimedia(CConfig.Instance.ApplicationPath + "\\gmdoc_sm.png", "", CConfig.Instance.MaxImageWidth, CConfig.Instance.MaxImageHeight, ref rectNewArea, null);
                rectNewArea = new Rectangle(0, 0, 0, 0);
                Creator.CopyMultimedia(CConfig.Instance.ApplicationPath + "\\gmdocn.png", "", CConfig.Instance.MaxImageWidth, CConfig.Instance.MaxImageHeight, ref rectNewArea, null);
            } catch (IOException e) {
                fLogger.WriteError("Caught io exception while copying nonpic images: {0}", e);
            } catch (ArgumentException e) {
                fLogger.WriteError("Caught argument exception while copying nonpic images: {0}", e);
            }
        }

        // Copy valid XHTML sticker file.
        private string CopyW3CSticker()
        {
            string stickerFile = "";
            fLogger.WriteInfo("Copying W3C sticker ...");
            try {
                Rectangle rectNewArea = new Rectangle(0, 0, 0, 0);
                stickerFile = Creator.CopyMultimedia(CConfig.Instance.ApplicationPath + "\\valid-xhtml10.png", "", 0, 0, ref rectNewArea, null);
            } catch (IOException e) {
                fLogger.WriteError("Caught io exception while copying W3C sticker: {0}", e);
            } catch (ArgumentException e) {
                fLogger.WriteError("Caught argument exception while copying W3C sticker: {0}", e);
            }
            return stickerFile;
        }

        // Copies the image for the background of the webpages.
        // Returns the sFilename of the copy.
        private string CopyBackgroundImage()
        {
            string backgroundImage = "";
            if (!string.IsNullOrEmpty(CConfig.Instance.BackgroundImage)) {
                try {
                    Rectangle newArea = new Rectangle(0, 0, 0, 0);
                    backgroundImage = Creator.CopyMultimedia(CConfig.Instance.BackgroundImage, "", 0, 0, ref newArea, null);
                } catch (IOException e) {
                    fLogger.WriteError("Caught io exception while copying background image: {0}", e);
                    backgroundImage = "";
                } catch (ArgumentException e) {
                    fLogger.WriteError("Caught argument exception while copying background image: {0}", e);
                    backgroundImage = "";
                }
            }
            return backgroundImage;
        }

        // Copy the CD ROM autorun loader program.
        private void CreateCDROMFiles()
        {
            string homepageUrl = string.Concat(CConfig.Instance.FrontPageFilename, ".", CConfig.Instance.HtmlExtension);

            if (homepageUrl != null && !string.IsNullOrEmpty(CConfig.Instance.OutputFolder)) {
                string fileopenSrc = CConfig.Instance.ApplicationPath + "\\fileopen.exe";
                string fileopenDest = CConfig.Instance.OutputFolder + "\\fileopen.exe";
                string autorunDest = CConfig.Instance.OutputFolder + "\\autorun.inf";

                // Copy fileopen.exe into output folder
                if (File.Exists(fileopenDest)) {
                    File.SetAttributes(fileopenDest, FileAttributes.Normal);
                    File.Delete(fileopenDest);
                }
                fLogger.WriteInfo("Copying fileopen.exe");
                File.Copy(fileopenSrc, fileopenDest, true);

                // Create autorun.inf
                if (File.Exists(autorunDest)) {
                    File.SetAttributes(autorunDest, FileAttributes.Normal);
                    File.Delete(autorunDest);
                }
                fLogger.WriteInfo("Creating autorun.inf");

                FileStream fs = new FileStream(autorunDest, FileMode.Create);
                StreamWriter sw = new StreamWriter(fs, System.Text.Encoding.ASCII);

                sw.WriteLine("[AUTORUN]");
                sw.WriteLine(string.Concat("open=fileopen.exe \"", homepageUrl, "\""));
                sw.WriteLine("");

                sw.Close();
            }
        }

        // Copy the javascript picture-selection script.
        private void CreateJavascriptFiles()
        {
            if (!string.IsNullOrEmpty(CConfig.Instance.OutputFolder)) {
                string jsSrc = CConfig.Instance.ApplicationPath + "\\gedmill.js";
                string jsDest = CConfig.Instance.OutputFolder + "\\gedmill.js";
                if (File.Exists(jsSrc)) {
                    // Copy gedmill.js into output folder
                    if (File.Exists(jsDest)) {
                        File.SetAttributes(jsDest, FileAttributes.Normal);
                        File.Delete(jsDest);
                    }
                    fLogger.WriteInfo("Copying gedmill.js");
                    File.Copy(jsSrc, jsDest, true);
                } else {
                    fLogger.WriteError(string.Format("{0} not found. No Javascript.", jsSrc));
                }
            }
        }
    }
}
