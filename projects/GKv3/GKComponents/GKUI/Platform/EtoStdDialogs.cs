/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using System.Linq;
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

        public async Task<IColor> SelectColor(IColor color)
        {
            using (var clrDlg = new ColorDialog()) {
                if (color != null) {
                    Color sdColor = ((ColorHandler)color).Handle;
                    clrDlg.Color = sdColor;
                }

                var retVal = (clrDlg.ShowDialog(null) == DialogResult.Ok) ? new ColorHandler(clrDlg.Color) : color;
                return await Task.FromResult(retVal);
            }
        }

        public async Task<IFont> SelectFont(IFont font)
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
                var retVal = (fontDlg.ShowDialog(null) == DialogResult.Ok) ? new FontHandler(selectedFont) : null;
                return await Task.FromResult(retVal);
            }
        }

        public async Task<string> GetOpenFile(string title, string context, string filter, int filterIndex, string defaultExt)
        {
            filter = filter.Replace(',', ';');
            using (OpenFileDialog ofd = CreateOpenFileDialog(title, context, filter, filterIndex, defaultExt, false)) {
                var dlgRes = ofd.ShowDialog(null);

                string retStr = (dlgRes == DialogResult.Ok) ? ofd.FileName : string.Empty;
                return await Task.FromResult(retStr);
            }
        }

        public async Task<string[]> GetOpenFiles(string title, string context, string filter, int filterIndex, string defaultExt)
        {
            filter = filter.Replace(',', ';');
            using (OpenFileDialog ofd = CreateOpenFileDialog(title, context, filter, filterIndex, defaultExt, true)) {
                var dlgRes = ofd.ShowDialog(null);

                string[] retStr = (dlgRes == DialogResult.Ok) ? ofd.Filenames.ToArray() : new string[0];
                return await Task.FromResult(retStr);
            }
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

        public async Task<string> GetSaveFile(string filter)
        {
            return await GetSaveFile("", "", filter, 1, "", "");
        }

        public async Task<string> GetSaveFile(string context, string filter)
        {
            return await GetSaveFile("", context, filter, 1, "", "");
        }

        public async Task<string> GetSaveFile(string title, string context, string filter, int filterIndex, string defaultExt,
                                              string suggestedFileName, bool overwritePrompt = true)
        {
            using (SaveFileDialog sfd = CreateSaveFileDialog(title, context, filter, filterIndex, defaultExt, suggestedFileName)) {
                // OverwritePrompt is not supported
                string retStr;
                if (sfd.ShowDialog(null) == DialogResult.Ok) {
                    string fileName = sfd.FileName;

                    if (!Path.HasExtension(fileName)) {
                        // FIXME: replace to use CurrentFilter, but in Eto 2.7.4 it's return null!
                        fileName = Path.ChangeExtension(fileName, defaultExt);
                    }

                    retStr = fileName;
                } else {
                    retStr = string.Empty;
                }

                return await Task.FromResult(retStr);
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

        public async Task<bool> ShowQuestion(string msg, string title = "")
        {
            if (string.IsNullOrEmpty(title)) {
                title = GKData.APP_TITLE;
            }

            bool retVal = MessageBox.Show(msg, title, MessageBoxButtons.YesNo, MessageBoxType.Question) == DialogResult.Yes;
            return await Task.FromResult(retVal);
        }

        public void ShowWarning(string msg, string title = "")
        {
            if (string.IsNullOrEmpty(title)) {
                title = GKData.APP_TITLE;
            }

            MessageBox.Show(msg, title, MessageBoxButtons.OK, MessageBoxType.Warning);
        }

        public async Task<string> GetInput(object owner, string prompt, string value)
        {
            bool res = GKInputBox.QueryText(owner, GKData.APP_TITLE, prompt, ref value);
            string retVal = (res && !string.IsNullOrEmpty(value)) ? value : string.Empty;
            return await Task.FromResult(retVal);
        }

        public async Task<string> GetPassword(string prompt)
        {
            string value = string.Empty;
            bool res = GKInputBox.QueryPassword(GKData.APP_TITLE, prompt, ref value);
            string retVal = (res && !string.IsNullOrEmpty(value)) ? value : string.Empty;
            return await Task.FromResult(retVal);
        }
    }
}
