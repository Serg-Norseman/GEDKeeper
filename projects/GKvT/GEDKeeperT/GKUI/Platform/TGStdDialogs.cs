/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using System.Threading.Tasks;
using GKCore;
using GKCore.Design.Graphics;
using GKCore.Design.Views;
using NStack;
using Terminal.Gui;

namespace GKUI.Platform
{
    /// <summary>
    /// The implementation of the contract for working with Terminal.Gui dialogs.
    /// </summary>
    public sealed class TGStdDialogs : IStdDialogs
    {
        public TGStdDialogs()
        {
        }

        public async Task<IColor> SelectColor(IColor color)
        {
            /*using (var clrDlg = new ColorDialog()) {
                if (color != null) {
                    Color sdColor = ((ColorHandler)color).Handle;
                    clrDlg.Color = sdColor;
                }

                if (clrDlg.ShowDialog(null) == DialogResult.Ok) {
                    return new ColorHandler(clrDlg.Color);
                } else {
                    return color;
                }
            }*/
            return null;
        }

        public async Task<IFont> SelectFont(IFont font)
        {
            /*Font sdFont = ((FontHandler)font).Handle;

            using (FontDialog fontDlg = new FontDialog())
            {
                fontDlg.Font = sdFont;
                return (fontDlg.ShowDialog(null) != DialogResult.Ok) ? null : new FontHandler(fontDlg.Font);
            }*/
            return null;
        }

        public async Task<string> SelectFolder(string folderPath)
        {
            return string.Empty;
        }

        public async Task<string> GetOpenFile(string title, string context, string filter, int filterIndex, string defaultExt)
        {
            var dlg = new OpenDialog() {
                Title = title,
                DirectoryPath = context,
                AllowedFileTypes = GetFilterFileTypes(filter),
                AllowsMultipleSelection = false,
            };
            Application.Run(dlg);
            if (!dlg.Canceled) {
                return dlg.FilePaths[0];
            } else {
                return string.Empty;
            }
        }

        public async Task<string[]> GetOpenFiles(string title, string context, string filter, int filterIndex, string defaultExt)
        {
            throw new System.NotImplementedException();
        }

        private string[] GetFilterFileTypes(string filter)
        {
            var filterParts = filter.Split('|');
            int filtersNum = filterParts.Length / 2;
            var result = new List<string>();
            for (int i = 0; i < filtersNum; i++) {
                int idx = i * 2;
                //string name = filterParts[idx];
                string exts = filterParts[idx + 1];
                string[] extensions = exts.Split(',');

                for (int k = 0; k < extensions.Length; k++) {
                    var ext = extensions[k];
                    if (ext[0] == '*')
                        ext = ext.Substring(1);
                    result.Add(ext);
                }
            }
            return result.ToArray();
        }

        public async Task<string> GetSaveFile(string context, string filter)
        {
            return GetSaveFile("", "", filter, 1, "", "").Result;
        }

        public async Task<string> GetSaveFile(string title, string context, string filter, int filterIndex, string defaultExt,
                                              string suggestedFileName, bool overwritePrompt = true)
        {
            return string.Empty;
        }


        public void ShowAlert(string msg, string title = "")
        {
            if (string.IsNullOrEmpty(title)) {
                title = GKData.APP_TITLE;
            }

            MessageBox.Query(title, msg, "OK");
        }

        public void ShowMessage(string msg, string title = "")
        {
            if (string.IsNullOrEmpty(title)) {
                title = GKData.APP_TITLE;
            }

            MessageBox.Query(title, msg, "OK");
        }

        public void ShowError(string msg, string title = "")
        {
            if (string.IsNullOrEmpty(title)) {
                title = GKData.APP_TITLE;
            }

            MessageBox.Query(title, msg, "OK");
        }

        public async Task<bool> ShowQuestion(string msg, string title = "")
        {
            if (string.IsNullOrEmpty(title)) {
                title = GKData.APP_TITLE;
            }

            return MessageBox.Query(title, msg, new ustring[] { "Yes", "No" }) == 0;
        }

        public void ShowWarning(string msg, string title = "")
        {
            if (string.IsNullOrEmpty(title)) {
                title = GKData.APP_TITLE;
            }

            MessageBox.Query(title, msg, "OK");
        }

        public async Task<string> GetInput(object owner, string prompt, string value)
        {
            /*bool res = GKInputBox.QueryText(owner, GKData.APP_TITLE, prompt, ref value);
            return res && !string.IsNullOrEmpty(value);*/
            return string.Empty;
        }

        public async Task<string> GetPassword(string prompt)
        {
            /*bool res = GKInputBox.QueryPassword(GKData.APP_TITLE, prompt, ref value);
            return res && !string.IsNullOrEmpty(value);*/
            return string.Empty;
        }
    }
}
