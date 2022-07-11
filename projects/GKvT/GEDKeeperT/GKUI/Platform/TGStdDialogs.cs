/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using System.Collections.Generic;
using BSLib.Design.Graphics;
using GKCore;
using GKCore.MVP.Views;
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

        public IColor SelectColor(IColor color)
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

        public IFont SelectFont(IFont font)
        {
            /*Font sdFont = ((FontHandler)font).Handle;

            using (FontDialog fontDlg = new FontDialog())
            {
                fontDlg.Font = sdFont;
                return (fontDlg.ShowDialog(null) != DialogResult.Ok) ? null : new FontHandler(fontDlg.Font);
            }*/
            return null;
        }

        public string GetOpenFile(string title, string context, string filter,
                                  int filterIndex, string defaultExt)
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

        public string GetSaveFile(string filter)
        {
            return GetSaveFile("", "", filter, 1, "", "");
        }

        public string GetSaveFile(string title, string context, string filter, int filterIndex, string defaultExt,
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

        public bool ShowQuestionYN(string msg, string title = "")
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


        public bool GetInput(object owner, string prompt, ref string value)
        {
            /*bool res = GKInputBox.QueryText(owner, GKData.APP_TITLE, prompt, ref value);
            return res && !string.IsNullOrEmpty(value);*/
            return false;
        }

        public bool GetPassword(string prompt, ref string value)
        {
            /*bool res = GKInputBox.QueryPassword(GKData.APP_TITLE, prompt, ref value);
            return res && !string.IsNullOrEmpty(value);*/
            return false;
        }
    }
}
