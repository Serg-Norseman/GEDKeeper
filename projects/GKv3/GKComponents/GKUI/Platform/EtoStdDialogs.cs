/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.IO;
using System.Threading.Tasks;
using Eto.Drawing;
using Eto.Forms;
using GKCore;
using GKCore.Design.Graphics;
using GKCore.Design.Views;
using GKUI.Components;
using GKUI.Platform.Handlers;

namespace GKUI.Platform
{
    /// <summary>
    /// The implementation of the contract for working with EtoForms dialogs.
    /// </summary>
    public sealed class EtoStdDialogs : IStdDialogs
    {
        public EtoStdDialogs()
        {
        }

        public IColor SelectColor(IColor color)
        {
            using (var clrDlg = new ColorDialog()) {
                if (color != null) {
                    Color sdColor = ((ColorHandler)color).Handle;
                    clrDlg.Color = sdColor;
                }

                if (clrDlg.ShowDialog(null) == DialogResult.Ok) {
                    return new ColorHandler(clrDlg.Color);
                } else {
                    return color;
                }
            }
        }

        public IFont SelectFont(IFont font)
        {
            Font sdFont = ((FontHandler)font).Handle;

            using (FontDialog fontDlg = new FontDialog()) {
                fontDlg.Font = sdFont;
                Font selectedFont = null;
                fontDlg.FontChanged += delegate {
                    // need to handle this event for OS X, where the dialog is a floating window
                    selectedFont = fontDlg.Font;
                };
                // do not get the font here, it may return immediately with a result of DialogResult.None on certain platforms
                return (fontDlg.ShowDialog(null) != DialogResult.Ok) ? null : new FontHandler(selectedFont);
            }
        }

        public string GetOpenFile(string title, string context, string filter,
                                  int filterIndex, string defaultExt)
        {
            filter = filter.Replace(',', ';');
            using (OpenFileDialog ofd = CreateOpenFileDialog(title, context, filter, filterIndex, defaultExt, false)) {
                var dlgRes = ofd.ShowDialog(null);
                if (dlgRes == DialogResult.Ok) {
                    return ofd.FileName;
                } else {
                    return string.Empty;
                }
            }
        }

        public Task<string> GetOpenFileAsync(string title, string context, string filter, int filterIndex, string defaultExt)
        {
            throw new System.NotImplementedException();
        }

        private static OpenFileDialog CreateOpenFileDialog(string title, string context, string filter,
                                                           int filterIndex, string defaultExt, bool multiSelect)
        {
            OpenFileDialog ofd = new OpenFileDialog();

            if (!string.IsNullOrEmpty(title))
                ofd.Title = title;

            if (!string.IsNullOrEmpty(context))
                ofd.Directory = new Uri(context);

            if (!string.IsNullOrEmpty(filter)) {
                ConvertFileDialogFilters(ofd, filter);
                if (filterIndex > 0) ofd.CurrentFilterIndex = filterIndex - 1;
            }

            ofd.MultiSelect = multiSelect;

            return ofd;
        }

        public string GetSaveFile(string filter)
        {
            return GetSaveFile("", "", filter, 1, "", "");
        }

        public string GetSaveFile(string context, string filter)
        {
            return GetSaveFile("", context, filter, 1, "", "");
        }

        public string GetSaveFile(string title, string context, string filter, int filterIndex, string defaultExt,
                                  string suggestedFileName, bool overwritePrompt = true)
        {
            using (SaveFileDialog sfd = CreateSaveFileDialog(title, context, filter, filterIndex, defaultExt, suggestedFileName)) {
                // OverwritePrompt is not supported
                if (sfd.ShowDialog(null) == DialogResult.Ok) {
                    string fileName = sfd.FileName;

                    if (!Path.HasExtension(fileName)) {
                        // FIXME: replace to use CurrentFilter, but in Eto 2.7.4 it's return null!
                        fileName = Path.ChangeExtension(fileName, defaultExt);
                    }

                    return fileName;
                } else {
                    return string.Empty;
                }
            }
        }

        private static SaveFileDialog CreateSaveFileDialog(string title, string context, string filter,
                                                           int filterIndex, string defaultExt, string suggestedFileName)
        {
            SaveFileDialog sfd = new SaveFileDialog();

            if (!string.IsNullOrEmpty(title))
                sfd.Title = title;

            if (!string.IsNullOrEmpty(context))
                sfd.Directory = new Uri(context);

            if (!string.IsNullOrEmpty(filter)) {
                ConvertFileDialogFilters(sfd, filter);

                if (filterIndex > 0) sfd.CurrentFilterIndex = filterIndex - 1;
            }

            if (!string.IsNullOrEmpty(suggestedFileName))
                sfd.FileName = suggestedFileName;

            return sfd;
        }

        private static void ConvertFileDialogFilters(FileDialog fileDlg, string filter)
        {
            if (fileDlg == null)
                throw new ArgumentNullException("fileDlg");

            var filterParts = filter.Split('|');
            int filtersNum = filterParts.Length / 2;
            for (int i = 0; i < filtersNum; i++) {
                int idx = i * 2;
                string name = filterParts[idx];
                string exts = filterParts[idx + 1];

                string[] extensions = exts.Split(new char[] { ',', ';' });
                for (int k = 0; k < extensions.Length; k++) {
                    string ext = extensions[k];
                    if (ext.Length > 0 && ext[0] == '*') {
                        extensions[k] = ext.Substring(1);
                    }
                }

                fileDlg.Filters.Add(new FileFilter(name, extensions));
            }
        }


        public void ShowAlert(string msg, string title = "")
        {
            if (string.IsNullOrEmpty(title)) {
                title = GKData.APP_TITLE;
            }

            MessageBox.Show(msg, title, MessageBoxButtons.OK, MessageBoxType.Warning);
        }

        public void ShowMessage(string msg, string title = "")
        {
            if (string.IsNullOrEmpty(title)) {
                title = GKData.APP_TITLE;
            }

            MessageBox.Show(msg, title, MessageBoxButtons.OK, MessageBoxType.Information);
        }

        public void ShowError(string msg, string title = "")
        {
            if (string.IsNullOrEmpty(title)) {
                title = GKData.APP_TITLE;
            }

            MessageBox.Show(msg, title, MessageBoxButtons.OK, MessageBoxType.Error);
        }

        public bool ShowQuestion(string msg, string title = "")
        {
            if (string.IsNullOrEmpty(title)) {
                title = GKData.APP_TITLE;
            }

            return MessageBox.Show(msg, title, MessageBoxButtons.YesNo, MessageBoxType.Question) == DialogResult.Yes;
        }

        public Task<bool> ShowQuestionAsync(string msg, string title = "")
        {
            throw new System.NotImplementedException();
        }

        public void ShowWarning(string msg, string title = "")
        {
            if (string.IsNullOrEmpty(title)) {
                title = GKData.APP_TITLE;
            }

            MessageBox.Show(msg, title, MessageBoxButtons.OK, MessageBoxType.Warning);
        }


        public bool GetInput(object owner, string prompt, ref string value)
        {
            bool res = GKInputBox.QueryText(owner, GKData.APP_TITLE, prompt, ref value);
            return res && !string.IsNullOrEmpty(value);
        }

        public Task<string> GetInputAsync(object owner, string prompt)
        {
            throw new System.NotImplementedException();
        }

        public bool GetPassword(string prompt, ref string value)
        {
            bool res = GKInputBox.QueryPassword(GKData.APP_TITLE, prompt, ref value);
            return res && !string.IsNullOrEmpty(value);
        }
    }
}
