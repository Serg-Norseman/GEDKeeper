/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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

using System.Drawing;
using System.Windows.Forms;
using BSLib.Design.Graphics;
using BSLib.Design.Handlers;
using GKCore;
using GKCore.MVP.Views;

namespace GKUI.Components
{
    /// <summary>
    /// The implementation of the contract for working with WinForms dialogs.
    /// </summary>
    public sealed class WFStdDialogs : IStdDialogs
    {
        public WFStdDialogs()
        {
        }

        public IColor SelectColor(IColor color)
        {
            using (var clrDlg = new ColorDialog()) {
                clrDlg.FullOpen = true;
                clrDlg.SolidColorOnly = true;

                if (color != null) {
                    Color sdColor = ((ColorHandler)color).Handle;
                    clrDlg.Color = sdColor;
                }

                if (clrDlg.ShowDialog() == DialogResult.OK) {
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
                return (fontDlg.ShowDialog() != DialogResult.OK) ? null : new FontHandler(fontDlg.Font);
            }
        }

        public string GetOpenFile(string title, string context, string filter,
                                  int filterIndex, string defaultExt)
        {
            using (OpenFileDialog ofd = CreateOpenFileDialog(title, context, filter, filterIndex, defaultExt, false))
            {
                if (ofd.ShowDialog() == DialogResult.OK) {
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
                ofd.InitialDirectory = context;

            if (!string.IsNullOrEmpty(filter))
            {
                ofd.Filter = filter;

                if (filterIndex > 0) ofd.FilterIndex = filterIndex;
            }

            if (!string.IsNullOrEmpty(defaultExt))
                ofd.DefaultExt = defaultExt;

            ofd.Multiselect = multiSelect;

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
                sfd.OverwritePrompt = overwritePrompt;
                if (sfd.ShowDialog() == DialogResult.OK) {
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
                sfd.InitialDirectory = context;

            if (!string.IsNullOrEmpty(filter))
            {
                sfd.Filter = filter;

                if (filterIndex > 0) sfd.FilterIndex = filterIndex;
            }

            if (!string.IsNullOrEmpty(defaultExt))
                sfd.DefaultExt = defaultExt;

            if (!string.IsNullOrEmpty(suggestedFileName))
                sfd.FileName = suggestedFileName;

            return sfd;
        }


        public void ShowAlert(string msg, string title = "")
        {
            if (string.IsNullOrEmpty(title)) {
                title = GKData.APP_TITLE;
            }

            MessageBox.Show(msg, title, MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
        }

        public void ShowMessage(string msg, string title = "")
        {
            if (string.IsNullOrEmpty(title)) {
                title = GKData.APP_TITLE;
            }

            MessageBox.Show(msg, title, MessageBoxButtons.OK, MessageBoxIcon.Asterisk);
        }

        public void ShowError(string msg, string title = "")
        {
            if (string.IsNullOrEmpty(title)) {
                title = GKData.APP_TITLE;
            }

            MessageBox.Show(msg, title, MessageBoxButtons.OK, MessageBoxIcon.Hand);
        }

        public bool ShowQuestionYN(string msg, string title = "")
        {
            if (string.IsNullOrEmpty(title)) {
                title = GKData.APP_TITLE;
            }

            return MessageBox.Show(msg, title, MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes;
        }

        public void ShowWarning(string msg, string title = "")
        {
            if (string.IsNullOrEmpty(title)) {
                title = GKData.APP_TITLE;
            }

            MessageBox.Show(msg, title, MessageBoxButtons.OK, MessageBoxIcon.Warning);
        }


        public bool GetInput(object owner, string prompt, ref string value)
        {
            bool res = GKInputBox.QueryText(owner, GKData.APP_TITLE, prompt, ref value);
            return res && !string.IsNullOrEmpty(value);
        }

        public bool GetPassword(string prompt, ref string value)
        {
            bool res = GKInputBox.QueryPassword(GKData.APP_TITLE, prompt, ref value);
            return res && !string.IsNullOrEmpty(value);
        }
    }
}
