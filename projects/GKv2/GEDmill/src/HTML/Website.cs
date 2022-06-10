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
using GKCore.Interfaces;
using GKCore.Logging;
using GKUI.Forms;

namespace GEDmill.HTML
{
    /// <summary>
    /// This is a single class that has the sole purpose of creating the website by creating and calling the 
    /// various other creators (individuals, sources, front page, index, css).
    /// It also copies site-wide multimedia files, javascript and CD ROM autorun loader.
    /// </summary>
    public class Website
    {
        private static readonly ILogger fLogger = LogManager.GetLogger(GMConfig.LOG_FILE, GMConfig.LOG_LEVEL, typeof(Website).Name);

        private readonly IBaseContext fContext;
        private readonly ILangMan fLangMan;
        private readonly string fOutputFolder;

        // The raw data that we are turning into a website.
        private readonly GDMTree fTree;


        public Website(IBaseContext context, string outputFolder, ILangMan langMan)
        {
            fContext = context;
            fTree = fContext.Tree;
            fOutputFolder = outputFolder;
            fLangMan = langMan;
        }

        // The heart of GEDmill is here.
        public void Create(IProgressController progressWnd)
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
                  + 1  // Background image
                  + 1  // Style sheet
                  + gfstats[(int)GDMRecordType.rtIndividual]
                  + 1  // Individuals Index
                  + gfstats[(int)GDMRecordType.rtSource]
                  + 1  // Front page
                  + 1  // CD ROM (Doesn't matter here that CD ROM autorun might not be included.)
                  + 1; // Scripts (Doesn't matter here that scripts might not be included.)

                // The paintbox with which to draw the mini tree
                var paintbox = new Paintbox(GMConfig.Instance);
                paintbox.SetBackgroundImage(GMConfig.Instance.BackgroundImage);

                // Object to keep count of number of files created etc.
                var stats = new Stats();

                // Here goes....

                // Start the progress indicator.
                progressWnd.Begin(progressMax, true);
                if (progressWnd.IsCanceled) {
                    return;
                }
                // Copy the images to use in place of non-pic multimedia files.
                progressWnd.SetText(fLangMan.LS(PLS.LSID_CopyingMultimedia));
                CopyIcons();
                if (progressWnd.IsCanceled) {
                    return;
                }
                progressWnd.StepTo(++progress);

                // Create the index creator for use by the individuals records creator.
                var indiIndexCreator = new CreatorIndexIndividuals(fContext, fLangMan);

                // Copy the image for the background of the webpages.
                progressWnd.SetText(fLangMan.LS(PLS.LSID_CopyingBackground));
                string backgroundImageFilename = CopyBackgroundImage();
                if (progressWnd.IsCanceled) {
                    return;
                }
                progressWnd.StepTo(++progress);

                // Create the style sheet
                progressWnd.SetText(fLangMan.LS(PLS.LSID_CreatingStyleSheet));
                string cssFilename = string.Concat(fOutputFolder, "\\", GMConfig.StylesheetFilename);
                if (GMConfig.StylesheetFilename.Length > 0) {
                    var csc = new CreatorStylesheet(fContext, fLangMan, cssFilename, backgroundImageFilename);
                    csc.Create();
                }

                if (progressWnd.IsCanceled) {
                    return;
                }

                progressWnd.StepTo(++progress);

                // Create the pages for the individual records.
                progressWnd.SetText(fLangMan.LS(PLS.LSID_CreatingIndividualPages));
                var indiList = fTree.GetRecords<GDMIndividualRecord>();
                foreach (GDMIndividualRecord ir in indiList) {
                    var ipc = new CreatorRecordIndividual(fContext, fLangMan, ir, indiIndexCreator, paintbox);
                    if (ipc.Create(stats)) {
                        stats.Individuals++;
                    }
                    if (progressWnd.IsCanceled) {
                        return;
                    }

                    progressWnd.StepTo(++progress);
                }

                // Create the index for the individual records pages.
                progressWnd.SetText(fLangMan.LS(PLS.LSID_CreatingIndividualsIndex));
                indiIndexCreator.Create();
                if (progressWnd.IsCanceled) {
                    return;
                }

                progressWnd.StepTo(++progress);

                // Clear list of copied files, so that source images get copied afresh
                // and so get resized differently to any indi images based on the same file.
                Creator.ClearCopiedFilesList();

                // Create the pages for the source records.
                progressWnd.SetText(fLangMan.LS(PLS.LSID_CreatingSourcePages));
                var sourList = fTree.GetRecords<GDMSourceRecord>();
                foreach (GDMSourceRecord sr in sourList) {
                    var spc = new CreatorRecordSource(fContext, fLangMan, sr);
                    if (spc.Create(stats)) {
                        stats.Sources++;
                    }

                    if (progressWnd.IsCanceled) {
                        return;
                    }

                    progressWnd.StepTo(++progress);
                }

                if (progressWnd.IsCanceled) {
                    return;
                }

                // Create the front page
                progressWnd.SetText(fLangMan.LS(PLS.LSID_CreatingFrontPage));
                if (GMConfig.Instance.FrontPageFilename.Length > 0) {
                    CreatorFrontPage fpc = new CreatorFrontPage(fContext, fLangMan, stats);
                    fpc.Create();
                }
                progressWnd.StepTo(++progress);
                if (progressWnd.IsCanceled) {
                    return;
                }
                
                // Copy the CD ROM autorun file
                progressWnd.SetText(fLangMan.LS(PLS.LSID_CreatingCDROMFiles));
                if (GMConfig.Instance.CreateCDROMFiles) {
                    CreateCDROMFiles();
                }
                if (progressWnd.IsCanceled) {
                    return;
                }

                progressWnd.StepTo(++progress);

                // Copy the Javascript
                progressWnd.SetText(fLangMan.LS(PLS.LSID_CreatingJSFile));
                // Currently (10Dec08) the only thing that uses javascript is the multiple images feature.
                if (GMConfig.Instance.AllowMultipleImages) {
                    CreateJavascriptFiles();
                }
                if (progressWnd.IsCanceled) {
                    return;
                }

                progressWnd.StepTo(++progress);

                // Done
                fLogger.WriteInfo("Website::CreateFinished");
                progressWnd.SetText(fLangMan.LS(PLS.LSID_Done));
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
                if (progressWnd != null) {
                    progressWnd.End(threaderror);
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
                Creator.CopyMultimedia(GMConfig.Instance.ApplicationPath + "\\gmaudio.png", "", GMConfig.Instance.MaxImageWidth, GMConfig.Instance.MaxImageHeight, ref rectNewArea, null);
                rectNewArea = new Rectangle(0, 0, 0, 0);
                Creator.CopyMultimedia(GMConfig.Instance.ApplicationPath + "\\gmaudio_sm.png", "", GMConfig.Instance.MaxImageWidth, GMConfig.Instance.MaxImageHeight, ref rectNewArea, null);
                rectNewArea = new Rectangle(0, 0, 0, 0);
                Creator.CopyMultimedia(GMConfig.Instance.ApplicationPath + "\\gmaudion.png", "", GMConfig.Instance.MaxImageWidth, GMConfig.Instance.MaxImageHeight, ref rectNewArea, null);
                rectNewArea = new Rectangle(0, 0, 0, 0);
                Creator.CopyMultimedia(GMConfig.Instance.ApplicationPath + "\\gmvideo.png", "", GMConfig.Instance.MaxImageWidth, GMConfig.Instance.MaxImageHeight, ref rectNewArea, null);
                rectNewArea = new Rectangle(0, 0, 0, 0);
                Creator.CopyMultimedia(GMConfig.Instance.ApplicationPath + "\\gmvideo_sm.png", "", GMConfig.Instance.MaxImageWidth, GMConfig.Instance.MaxImageHeight, ref rectNewArea, null);
                rectNewArea = new Rectangle(0, 0, 0, 0);
                Creator.CopyMultimedia(GMConfig.Instance.ApplicationPath + "\\gmvideon.png", "", GMConfig.Instance.MaxImageWidth, GMConfig.Instance.MaxImageHeight, ref rectNewArea, null);
                rectNewArea = new Rectangle(0, 0, 0, 0);
                Creator.CopyMultimedia(GMConfig.Instance.ApplicationPath + "\\gmdoc.png", "", GMConfig.Instance.MaxImageWidth, GMConfig.Instance.MaxImageHeight, ref rectNewArea, null);
                rectNewArea = new Rectangle(0, 0, 0, 0);
                Creator.CopyMultimedia(GMConfig.Instance.ApplicationPath + "\\gmdoc_sm.png", "", GMConfig.Instance.MaxImageWidth, GMConfig.Instance.MaxImageHeight, ref rectNewArea, null);
                rectNewArea = new Rectangle(0, 0, 0, 0);
                Creator.CopyMultimedia(GMConfig.Instance.ApplicationPath + "\\gmdocn.png", "", GMConfig.Instance.MaxImageWidth, GMConfig.Instance.MaxImageHeight, ref rectNewArea, null);
            } catch (IOException e) {
                fLogger.WriteError("Caught io exception while copying nonpic images: {0}", e);
            } catch (ArgumentException e) {
                fLogger.WriteError("Caught argument exception while copying nonpic images: {0}", e);
            }
        }

        // Copies the image for the background of the webpages.
        // Returns the sFilename of the copy.
        private string CopyBackgroundImage()
        {
            string backgroundImage = "";
            if (!string.IsNullOrEmpty(GMConfig.Instance.BackgroundImage)) {
                try {
                    Rectangle newArea = new Rectangle(0, 0, 0, 0);
                    backgroundImage = Creator.CopyMultimedia(GMConfig.Instance.BackgroundImage, "", 0, 0, ref newArea, null);
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
            string homepageUrl = string.Concat(GMConfig.Instance.FrontPageFilename, ".html");

            if (homepageUrl != null && !string.IsNullOrEmpty(fOutputFolder)) {
                string fileopenSrc = GMConfig.Instance.ApplicationPath + "\\fileopen.exe";
                string fileopenDest = fOutputFolder + "\\fileopen.exe";
                string autorunDest = fOutputFolder + "\\autorun.inf";

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

                FileStream fs = new FileStream(autorunDest, FileMode.Create, FileAccess.Write);
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
            if (!string.IsNullOrEmpty(fOutputFolder)) {
                string jsSrc = GMConfig.Instance.ApplicationPath + "\\gedmill.js";
                string jsDest = fOutputFolder + "\\gedmill.js";
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
