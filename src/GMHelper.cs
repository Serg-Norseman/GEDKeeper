/* GMHelper.cs
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
 */

using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using GKCore.Logging;

namespace GEDmill
{
    /// <summary>
    /// Utility functions.
    /// </summary>
    public static class GMHelper
    {
        private static readonly ILogger fLogger = LogManager.GetLogger(CConfig.LOG_FILE, CConfig.LOG_LEVEL, typeof(GMHelper).Name);


        // Modifies rectNew to fit within the limits given, keeping its aspect ratio
        public static void ScaleAreaToFit(ref Rectangle rectNew, uint uMaxWidth, uint uMaxHeight)
        {
            if (rectNew.Height > uMaxHeight) {
                // Image won't fit horizontally, so scale in both directions til it will
                rectNew.Width = (rectNew.Width * (int)uMaxHeight) / rectNew.Height;
                rectNew.Height = (int)uMaxHeight;
            }

            if (rectNew.Width > uMaxWidth) {
                // Image won't fit horizontally, so scale in both directions til it will
                rectNew.Height = (rectNew.Height * (int)uMaxWidth) / rectNew.Width;
                rectNew.Width = (int)uMaxWidth;
            }
        }

        // Presents a file selection dialog and returns the selecetd file name and path
        public static bool SelectFile(ref string fileDir, ref string fileName, string title, string defaultName,
                                      bool loadNotSave, string filterName, List<string> filterExtensions)
        {
            bool fileSelected = false;

            FileDialog fileDialog;
            if (loadNotSave) {
                fileDialog = new OpenFileDialog();
            } else {
                fileDialog = new SaveFileDialog();
            }

            if (fileDir.Length > 0) {
                fileDialog.InitialDirectory = fileDir;
            } else {
                fileDialog.InitialDirectory = Environment.GetFolderPath(System.Environment.SpecialFolder.Personal);
            }

            if (fileName.Length > 0) {
                fileDialog.FileName = fileName;
            } else {
                fileDialog.FileName = defaultName;
            }

            string sFilterString = "";
            int nFilterAllIndex = 1;
            if (filterExtensions.Count > 0) {
                nFilterAllIndex++;
                string sFilterCode = "";
                bool bFirst = true;
                //"Picture files (*.jpg; *.gif)|*.jpg;*.gif|All files (*.*)|*.*";
                foreach (string sFilterExtn in filterExtensions) {
                    if (!bFirst) {
                        sFilterCode += ";";
                    } else {
                        bFirst = false;
                    }
                    sFilterCode += "*" + sFilterExtn;
                }
                sFilterString = filterName + " (" + sFilterCode + ")|" + sFilterCode + "|";
            }
            sFilterString += "All files (*.*)|*.*";
            fileDialog.Filter = sFilterString;
            fileDialog.FilterIndex = 1;
            string sExtn = Path.GetExtension(fileDialog.FileName);

            // Check whether selected file matches given filter
            bool bValidExtn = true;
            if (fileDialog.FileName.Length > 0) {
                bValidExtn = false;
                string sExtnFromDlg = Path.GetExtension(fileDialog.FileName).ToUpper();
                foreach (string sFilterExtn in filterExtensions) {
                    if (sExtnFromDlg == sFilterExtn.ToUpper()) {
                        bValidExtn = true;
                        break;
                    }
                }
            }

            if (!bValidExtn) {
                // Use *.* filter if default file isn't a .txt file.
                fileDialog.FilterIndex = nFilterAllIndex;
            }
            fileDialog.RestoreDirectory = true;
            fileDialog.Title = title;

            if (fileDialog.ShowDialog() == DialogResult.OK) {
                fileSelected = true;
                fileDir = Path.GetDirectoryName(fileDialog.FileName);
                fileName = Path.GetFileName(fileDialog.FileName);
            }
            fLogger.WriteInfo("Selected file : " + fileDir + "\\" + fileName);

            return (fileSelected);
        }

        // Used to display the finished website. Uses whatever app the user has assigned to open HTML files.
        public static void OpenURL(string sURL)
        {
            try {
                System.Diagnostics.Process.Start(sURL);
            } catch (Exception e2) {
                fLogger.WriteError("Caught exception while opening finished webpages : {0}", e2);
            }
        }

        public static string GetAppPath()
        {
            return Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location);
        }

        // Returns true if the given sFilename would exist on the Windows Desktop.
        // This is a hack ( because I don't know the official way to find the path of the user's Desktop ).
        public static bool IsDesktop(string filename)
        {
            string path_part = Path.GetDirectoryName(filename);
            // Strip trailing slashes
            while (path_part.Length > 0 && path_part.Substring(path_part.Length - 1, 1) == "\\") {
                path_part = path_part.Substring(0, path_part.Length - 1);
            }
            int folder_index = path_part.LastIndexOf('\\');
            if (folder_index > 0 && (folder_index + 1) < path_part.Length) {
                string folder_name = path_part.Substring(folder_index + 1);
                if (folder_name == "Desktop") {
                    return (true);
                }
            }
            return (false);
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

        // Returns the name of the alternative picture file to display for non-diaplayable files of the given format
        public static string NonPicFilename(string format, bool small, bool clickToDownload)
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
