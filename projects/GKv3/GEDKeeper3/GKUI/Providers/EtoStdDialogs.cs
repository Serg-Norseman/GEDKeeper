/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using Eto.Drawing;
using Eto.Forms;
using GKCore;
using GKCore.Interfaces;
using GKCore.MVP.Views;
using GKUI.Components;

namespace GKUI.Providers
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

            using (FontDialog fontDlg = new FontDialog())
            {
                fontDlg.Font = sdFont;
                return (fontDlg.ShowDialog(null) != DialogResult.Ok) ? null : new FontHandler(fontDlg.Font);
            }
        }

        public string GetOpenFile(string title, string context, string filter,
                                  int filterIndex, string defaultExt)
        {
            using (OpenFileDialog ofd = CreateOpenFileDialog(title, context, filter, filterIndex, defaultExt, false))
            {
                if (ofd.ShowDialog(null) == DialogResult.Ok) {
                    return ofd.FileName;
                } else {
                    return string.Empty;
                }
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

            if (!string.IsNullOrEmpty(filter))
            {
                UIHelper.ConvertFileDialogFilters(ofd, filter);

                //if (filterIndex > 0) ofd.FilterIndex = filterIndex;
            }

            ofd.MultiSelect = multiSelect;

            return ofd;
        }

        public string GetSaveFile(string filter)
        {
            return GetSaveFile("", "", filter, 1, "", "");
        }

        public string GetSaveFile(string title, string context, string filter, int filterIndex, string defaultExt,
                                  string suggestedFileName, bool overwritePrompt = true)
        {
            using (SaveFileDialog sfd = CreateSaveFileDialog(title, context, filter, filterIndex, defaultExt, suggestedFileName))
            {
                //sfd.OverwritePrompt = overwritePrompt;
                if (sfd.ShowDialog(null) == DialogResult.Ok) {
                    return sfd.FileName;
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

            if (!string.IsNullOrEmpty(filter))
            {
                UIHelper.ConvertFileDialogFilters(sfd, filter);

                //if (filterIndex > 0) sfd.FilterIndex = filterIndex;
            }

            if (!string.IsNullOrEmpty(suggestedFileName))
                sfd.FileName = suggestedFileName;

            return sfd;
        }


        public void ShowMessage(string msg)
        {
            MessageBox.Show(msg, GKData.APP_TITLE, MessageBoxButtons.OK, MessageBoxType.Information);
        }

        public void ShowError(string msg)
        {
            MessageBox.Show(msg, GKData.APP_TITLE, MessageBoxButtons.OK, MessageBoxType.Error);
        }

        public bool ShowQuestionYN(string msg)
        {
            return MessageBox.Show(msg, GKData.APP_TITLE, MessageBoxButtons.YesNo, MessageBoxType.Question) == DialogResult.Yes;
        }

        public void ShowWarning(string msg)
        {
            MessageBox.Show(msg, GKData.APP_TITLE, MessageBoxButtons.OK, MessageBoxType.Warning);
        }


        public bool GetInput(string prompt, ref string value)
        {
            bool res = GKInputBox.QueryText(GKData.APP_TITLE, prompt, ref value);
            return res && !string.IsNullOrEmpty(value);
        }

        public bool GetPassword(string prompt, ref string value)
        {
            bool res = GKInputBox.QueryPassword(GKData.APP_TITLE, prompt, ref value);
            return res && !string.IsNullOrEmpty(value);
        }
    }
}
