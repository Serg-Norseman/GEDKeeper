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
using GEDmill.LLClasses;
using System.Collections;
using System.IO;
using System.Globalization;
using System.Drawing;
using System.Threading;
using GEDmill.ImageClasses;

namespace GEDmill.HTMLClasses
{
  // This is a single class that has the sole purpose of creating the website by creating and calling the 
  // various other creators (individuals, sources, front page, index, css).
  // It also copies site-wide multimedia files, javascript and CD ROM autorun loader.
  public class CWebsite
  {
    // The raw data that we are turning into a website.
    protected CGedcom m_gedcom;

    // Pointer to the window showing the progress bar, so that web page creation progress can be shown to user.
    protected IProgressCallback m_progressWindow;
    
    // Constructor
    public CWebsite( CGedcom gedcom, IProgressCallback progress )
    {
      m_gedcom = gedcom;
      m_progressWindow = progress;      
    }

    // The heart of GEDmill is here.
    public void Create()
    {
      LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Note, "CWebsite::Create()" );

      // 1 means the process was aborted, for signalling back to calling thread. 2 means file nError.      
      CThreadError threaderror = new CThreadError( 1, "No error" ); 

      try
      {
        // The value to indicate in the progress bar to show how much of the website creation is complete.
        int nProgress = 0;

        // The maximum value of the progress bar, i.e. when website creation is fully complete.
        int nProgressMax =
            1  // Site-wide multimedia files
          + 1  // W3C Sticker
          + 1  // Background image
          + 1  // Style sheet
          + m_gedcom.CountIndividuals 
          + 1  // Individuals Index
          + m_gedcom.CountSources
          + 1  // Front page
          + 1  // Help page
          + 1  // CD ROM (Doesn't matter here that CD ROM autorun might not be included.)
          + 1; // Scripts (Doesn't matter here that scripts might not be included.)

        // The paintbox with which to draw the mini tree
        CPaintbox paintbox = new CPaintbox(MainForm.s_config);
        paintbox.SetBackgroundImage(MainForm.s_config.m_sBackgroundImage);

        // Object to keep count of number of files created etc.
        CStats stats = new CStats();

        // Here goes....

        // Start the progress indicator.
        m_progressWindow.Begin( 0, nProgressMax );
        if (m_progressWindow.IsAborting)
        {
            return;
        }
        // Copy the images to use in place of non-pic multimedia files.
        m_progressWindow.SetText("Copying multimedia");
        CopyIcons();
        if (m_progressWindow.IsAborting)
        {
            return;
        } m_progressWindow.StepTo(++nProgress);

        // Copy the W3C sticker file.
        m_progressWindow.SetText("Copying W3C sticker");
        string sW3CFilename = "";
        if (MainForm.s_config.m_bIncludeValiditySticker)
        {
          sW3CFilename = CopyW3CSticker();
        }
        if (m_progressWindow.IsAborting)
        {
            return;
        } m_progressWindow.StepTo(++nProgress);

        // Create the index creator for use by the individuals records creator.
        CCreatorIndexIndividuals indiIndexCreator = new CCreatorIndexIndividuals( m_gedcom, m_progressWindow, sW3CFilename );

        // Copy the image for the background of the webpages.
        m_progressWindow.SetText("Copying background image");
        string sBackgroundImageFilename = CopyBackgroundImage();
        if (m_progressWindow.IsAborting)
        {
            return;
        }
        m_progressWindow.StepTo( ++nProgress );

        // Create the style sheet
        m_progressWindow.SetText("Creating style sheet");
        string cssFilename = String.Concat( MainForm.s_config.m_sOutputFolder, "\\", MainForm.s_config.m_sStylesheetFilename, ".css" );
        if( MainForm.s_config.m_sStylesheetFilename.Length>0 && (!MainForm.s_config.m_bPreserveStylesheet || !File.Exists( cssFilename ) ) )
        {
          CCreatorStylesheet csc = new CCreatorStylesheet( m_gedcom, m_progressWindow, sW3CFilename, cssFilename, sBackgroundImageFilename );
          csc.Create();
        }

        if (m_progressWindow.IsAborting)
        {
            return;
        }          
          
        m_progressWindow.StepTo( ++nProgress );

        // Create the pages for the individual records.
        m_progressWindow.SetText("Creating individual pages");
        foreach (CIndividualRecord ir in m_gedcom.m_alIndividualRecords)
        {         
          CCreatorRecordIndividual ipc = new CCreatorRecordIndividual( m_gedcom, m_progressWindow, sW3CFilename, ir, indiIndexCreator, paintbox );        
          if (ipc.Create( stats ))
          {          
            stats.m_unIndividuals++;
          }
          if (m_progressWindow.IsAborting)
          {
              return;
          }

          m_progressWindow.StepTo(++nProgress);
        }

        // Create the index for the individual records pages.
        m_progressWindow.SetText("Creating individuals index");
        indiIndexCreator.Create();
        if (m_progressWindow.IsAborting)
        {
            return;
        }

        m_progressWindow.StepTo(++nProgress);

        // Clear list of copied files, so that source images get copied afresh
        // and so get resized differently to any indi images based on the same file.
        CCreator.ClearCopiedFilesList(); 

        // Create the pages for the source records.
        m_progressWindow.SetText( "Creating source pages" );
        foreach( CSourceRecord sr in m_gedcom.m_alSourceRecords )
        {
          if( sr != null && sr.ContainsAnyInformation() )
          {
            CCreatorRecordSource spc = new CCreatorRecordSource( m_gedcom, m_progressWindow, sW3CFilename, sr );
            if( spc.Create( stats ) )
            {
              stats.m_unSources++;
            }
          }
          if (m_progressWindow.IsAborting)
          {
              return;
          }

          m_progressWindow.StepTo(++nProgress);
        }
        if (m_progressWindow.IsAborting)
        {
            return;
        }          
          

        // Create the front page
        m_progressWindow.SetText("Creating front page");
        string front_page_filename = String.Concat(MainForm.s_config.m_sOutputFolder, "\\", MainForm.s_config.m_sFrontPageFilename, ".", MainForm.s_config.m_sHtmlExtension);
        if( MainForm.s_config.m_sFrontPageFilename.Length>0 && (!MainForm.s_config.m_bPreserveFrontPage || !File.Exists( front_page_filename ) ) )
        {
          CCreatorFrontPage fpc = new CCreatorFrontPage( m_gedcom, m_progressWindow, sW3CFilename, stats );
          fpc.Create();
        }
        m_progressWindow.StepTo( ++nProgress );
        if (m_progressWindow.IsAborting)
        {
            return;
        }          
          

        // Create the help page
        m_progressWindow.SetText("Creating help page");
        string help_page_filename = String.Concat(MainForm.s_config.m_sOutputFolder, "\\", "help.", MainForm.s_config.m_sHtmlExtension);
        if( MainForm.s_config.m_bIncludeHelppage )
        {
          CCreatorHelppage hpc = new CCreatorHelppage( m_gedcom, m_progressWindow, sW3CFilename );
          hpc.Create();
        }
        m_progressWindow.StepTo( ++nProgress );
        if (m_progressWindow.IsAborting)
        {
            return;
        }          
          

        // Copy the CD ROM autorun file
        m_progressWindow.SetText( "Creating CD-ROM files" );
        if( MainForm.s_config.m_bCreateCDROMFiles )
        {
          CreateCDROMFiles( m_gedcom );
        }
        if (m_progressWindow.IsAborting)
        {
            return;
        }

        m_progressWindow.StepTo(++nProgress);

        // Copy the Javascript
        m_progressWindow.SetText( "Creating Javascript file" );
        if( MainForm.s_config.m_bAllowMultipleImages ) // Currently (10Dec08) the only thing that uses javascript is the multiple images feature.
        {
          CreateJavascriptFiles( m_gedcom );
        }
        if (m_progressWindow.IsAborting)
        {
            return;
        }

        m_progressWindow.StepTo(++nProgress);

        // Done
        LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Note, "Finished" );
        m_progressWindow.SetText( "Done" );
        threaderror.m_nError = 0;
        threaderror.m_sMessage = "";
      }
      catch( ArgumentException e )
      {
        LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Error, "Caught Argument Exception : " + e.ToString() );

        threaderror.m_nError = 2; // 2 => abnormal abort.
        threaderror.m_sMessage = "";
      }
      catch( IOException e )
      {
        LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Error, "Caught IO Exception : " + e.ToString() );

        threaderror.m_nError = 2; // 2 => abnormal abort.
        threaderror.m_sMessage = "";
      }
      catch( NullReferenceException e )
      {
        LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Error, "Caught NullReference Exception : " + e.ToString() );

        threaderror.m_nError = 2; // 2 => abnormal abort.
        threaderror.m_sMessage = "";
      }
      catch( CHTMLException e )
      {
        threaderror.m_nError = 2; // 2 => abnormal abort.
        threaderror.m_sMessage = e.Message;
      }
      catch( Exception e )
      {
        LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Error, "Caught Exception : " + e.ToString() );

        threaderror.m_nError = 2; // 2 => abnormal abort.
        threaderror.m_sMessage = "";
      }
      finally
      {
        LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Note, "Thread ending..." );

        if( m_progressWindow != null )
        {
          m_progressWindow.End( threaderror );
        }
      }
    }

    // Copy the files for the images to use in place of non-picture multimedia files.
    private void CopyIcons()
    {
      LogFile.TheLogFile.WriteLine(LogFile.DT_HTML, LogFile.EDebugLevel.Note, "Copying icon files...");
      try
      {
       // uint num_copied = 0;
        Rectangle rectNewArea = new Rectangle(0, 0, 0, 0);
        CCreator.CopyMultimedia(MainForm.s_config.m_sApplicationPath + "\\gmaudio.png", "", MainForm.s_config.m_uMaxImageWidth, MainForm.s_config.m_uMaxImageHeight, ref rectNewArea, null);
        rectNewArea = new Rectangle(0, 0, 0, 0);
        CCreator.CopyMultimedia(MainForm.s_config.m_sApplicationPath + "\\gmaudio_sm.png", "", MainForm.s_config.m_uMaxImageWidth, MainForm.s_config.m_uMaxImageHeight, ref rectNewArea, null);
        rectNewArea = new Rectangle(0, 0, 0, 0);
        CCreator.CopyMultimedia(MainForm.s_config.m_sApplicationPath + "\\gmaudion.png", "", MainForm.s_config.m_uMaxImageWidth, MainForm.s_config.m_uMaxImageHeight, ref rectNewArea, null);
        rectNewArea = new Rectangle(0, 0, 0, 0);
        CCreator.CopyMultimedia(MainForm.s_config.m_sApplicationPath + "\\gmvideo.png", "", MainForm.s_config.m_uMaxImageWidth, MainForm.s_config.m_uMaxImageHeight, ref rectNewArea, null);
        rectNewArea = new Rectangle(0, 0, 0, 0);
        CCreator.CopyMultimedia(MainForm.s_config.m_sApplicationPath + "\\gmvideo_sm.png", "", MainForm.s_config.m_uMaxImageWidth, MainForm.s_config.m_uMaxImageHeight, ref rectNewArea, null);
        rectNewArea = new Rectangle(0, 0, 0, 0);
        CCreator.CopyMultimedia(MainForm.s_config.m_sApplicationPath + "\\gmvideon.png", "", MainForm.s_config.m_uMaxImageWidth, MainForm.s_config.m_uMaxImageHeight, ref rectNewArea, null);
        rectNewArea = new Rectangle(0, 0, 0, 0);
        CCreator.CopyMultimedia(MainForm.s_config.m_sApplicationPath + "\\gmdoc.png", "", MainForm.s_config.m_uMaxImageWidth, MainForm.s_config.m_uMaxImageHeight, ref rectNewArea, null);
        rectNewArea = new Rectangle(0, 0, 0, 0);
        CCreator.CopyMultimedia(MainForm.s_config.m_sApplicationPath + "\\gmdoc_sm.png", "", MainForm.s_config.m_uMaxImageWidth, MainForm.s_config.m_uMaxImageHeight, ref rectNewArea, null);
        rectNewArea = new Rectangle(0, 0, 0, 0);
        CCreator.CopyMultimedia(MainForm.s_config.m_sApplicationPath + "\\gmdocn.png", "", MainForm.s_config.m_uMaxImageWidth, MainForm.s_config.m_uMaxImageHeight, ref rectNewArea, null);
      }
      catch( IOException e )
      {
        LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Error, String.Format("Caught io exception while copying nonpic images: {0}", e.ToString() ) );
      }
      catch( ArgumentException e )
      {
        LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Error, String.Format("Caught argument exception while copying nonpic images: {0}", e.ToString() ) );
      }
    }

    // Copy valid XHTML sticker file.
    private string CopyW3CSticker()
    {
      string sW3CFile = "";
      LogFile.TheLogFile.WriteLine(LogFile.DT_HTML, LogFile.EDebugLevel.Note, "Copying W3C sticker ...");
      try
      {
        Rectangle rectNewArea = new Rectangle(0, 0, 0, 0);
        sW3CFile = CCreator.CopyMultimedia(MainForm.s_config.m_sApplicationPath + "\\valid-xhtml10.png", "", 0, 0, ref rectNewArea, null);
      }
      catch( IOException e )
      {
        LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Error, String.Format("Caught io exception while copying W3C sticker: {0}", e.ToString() ) );
      }
      catch( ArgumentException e )
      {
        LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Error, String.Format("Caught argument exception while copying W3C sticker: {0}", e.ToString() ) );
      }
      return sW3CFile;
    }
  
    // Copies the image for the background of the webpages.
    // Returns the sFilename of the copy.
    private string CopyBackgroundImage()
    {
      string sBackgroundImage = "";
      if( MainForm.s_config.m_sBackgroundImage != null && MainForm.s_config.m_sBackgroundImage.Length > 0 )
      {
        try
        {
          Rectangle newArea = new Rectangle( 0,0,0,0 );
          sBackgroundImage = CCreator.CopyMultimedia( MainForm.s_config.m_sBackgroundImage, "", 0, 0, ref newArea, null );
        }
        catch( IOException e )
        {
          LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Error, String.Format("Caught io exception while copying background image: {0}", e.ToString() ) );

          sBackgroundImage = "";
        }
        catch( ArgumentException e )
        {
          LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Error, String.Format("Caught argument exception while copying background image: {0}", e.ToString() ) );

          sBackgroundImage = "";
        }
      }
      return sBackgroundImage;
    }

    // Copy the CD ROM autorun loader program.
    private void CreateCDROMFiles( CGedcom gedcom )
    {
      string sHomepageUrl = String.Concat( MainForm.s_config.m_sFrontPageFilename, ".", MainForm.s_config.m_sHtmlExtension );

      if( sHomepageUrl != null && MainForm.s_config.m_sOutputFolder != null && MainForm.s_config.m_sOutputFolder != "" )
      {
        string sFileopenSrc = MainForm.s_config.m_sApplicationPath + "\\fileopen.exe";
        string sFileopenDest = MainForm.s_config.m_sOutputFolder + "\\fileopen.exe";
        string sAutorunDest = MainForm.s_config.m_sOutputFolder + "\\autorun.inf";

        // Copy fileopen.exe into output folder
        if( File.Exists( sFileopenDest ) )
        {
          File.SetAttributes( sFileopenDest, FileAttributes.Normal );
          File.Delete( sFileopenDest );
        }
        LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Note, "Copying fileopen.exe" );
        File.Copy( sFileopenSrc, sFileopenDest, true );


        // Create autorun.inf
        if( File.Exists( sAutorunDest ) )
        {
          File.SetAttributes( sAutorunDest, FileAttributes.Normal );
          File.Delete( sAutorunDest );
        }
        LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Note, "Creating autorun.inf" );

        FileStream fs = new FileStream( sAutorunDest, FileMode.Create );
        StreamWriter sw = new StreamWriter( fs, System.Text.Encoding.ASCII );

        sw.WriteLine("[AUTORUN]");
        sw.WriteLine( String.Concat( "open=fileopen.exe \"", sHomepageUrl, "\"" ) );
        sw.WriteLine("");

        sw.Close();
      }

    }

    // Copy the javascript picture-selection script.
    private void CreateJavascriptFiles( CGedcom gedcom )
    {
      if( MainForm.s_config.m_sOutputFolder != null && MainForm.s_config.m_sOutputFolder != "" )
      {
        string sJavascriptSrc = MainForm.s_config.m_sApplicationPath + "\\gedmill.js";
        string sJavascriptDest = MainForm.s_config.m_sOutputFolder + "\\gedmill.js";
        if( File.Exists( sJavascriptSrc ) )
        {
          // Copy gedmill.js into output folder
          if( File.Exists( sJavascriptDest ) )
          {
            File.SetAttributes( sJavascriptDest, FileAttributes.Normal );
            File.Delete( sJavascriptDest );
          }
          LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Note, "Copying gedmill.js" );
          File.Copy( sJavascriptSrc, sJavascriptDest, true );
        }
        else
        {
          LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Error, String.Format("{0} not found. No Javascript.", sJavascriptSrc ) );
        }
      }

    }
  } // end of class
} // end of namespace


