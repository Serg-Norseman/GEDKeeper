/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Drawing;
using System.Threading.Tasks;
using System.Windows.Forms;
using GKCore;
using GKCore.Design.Graphics;
using GKCore.Design.Views;
using GKUI.Components;
using GKUI.Platform.Handlers;

namespace GKUI.Platform
{
    /// <summary>
    /// The implementation of the contract for working with WinForms dialogs.
    /// </summary>
    public sealed class WFStdDialogs : IStdDialogs
    {
        public WFStdDialogs()
        {
        }

        public async Task<IColor> SelectColor(IColor color)
        {
            using (var clrDlg = new ColorDialog()) {
                clrDlg.FullOpen = true;
                clrDlg.SolidColorOnly = true;

                if (color != null) {
                    Color sdColor = ((ColorHandler)color).Handle;
                    clrDlg.Color = sdColor;
                }

                var retVal = (clrDlg.ShowDialog() == DialogResult.OK) ? new ColorHandler(clrDlg.Color) : color;
                return await Task.FromResult(retVal);
            }
        }

        public async Task<IFont> SelectFont(IFont font)
        {
            Font sdFont = ((FontHandler)font).Handle;

            using (FontDialog fontDlg = new FontDialog()) {
                fontDlg.Font = sdFont;
                var retVal = (fontDlg.ShowDialog() == DialogResult.OK) ? new FontHandler(fontDlg.Font) : null;
                return await Task.FromResult(retVal);
            }
        }

        public async Task<string> GetOpenFile(string title, string context, string filter, int filterIndex, string defaultExt)
        {
            filter = filter.Replace(',', ';');
            using (OpenFileDialog ofd = CreateOpenFileDialog(title, context, filter, filterIndex, defaultExt, false)) {
                string retStr = (ofd.ShowDialog() == DialogResult.OK) ? ofd.FileName : string.Empty;
                return await Task.FromResult(retStr);
            }
        }

        public async Task<string[]> GetOpenFiles(string title, string context, string filter, int filterIndex, string defaultExt)
        {
            filter = filter.Replace(',', ';');
            using (OpenFileDialog ofd = CreateOpenFileDialog(title, context, filter, filterIndex, defaultExt, true)) {
                string[] retStr = (ofd.ShowDialog() == DialogResult.OK) ? ofd.FileNames : new string[0];
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
                ofd.InitialDirectory = context;

            if (!string.IsNullOrEmpty(filter)) {
                ofd.Filter = filter;

                if (filterIndex > 0) ofd.FilterIndex = filterIndex;
            }

            if (!string.IsNullOrEmpty(defaultExt))
                ofd.DefaultExt = defaultExt;

            ofd.Multiselect = multiSelect;

            return ofd;
        }

        /*public async Task<string> GetSaveFile(string filter)
        {
            return await GetSaveFile("", "", filter, 1, "", "");
        }*/

        public async Task<string> GetSaveFile(string context, string filter)
        {
            return await GetSaveFile("", context, filter, 1, "", "");
        }

        public async Task<string> GetSaveFile(string title, string context, string filter, int filterIndex, string defaultExt,
                                              string suggestedFileName, bool overwritePrompt = true)
        {
            using (SaveFileDialog sfd = CreateSaveFileDialog(title, context, filter, filterIndex, defaultExt, suggestedFileName)) {
                sfd.OverwritePrompt = overwritePrompt;
                string retStr = (sfd.ShowDialog() == DialogResult.OK) ? sfd.FileName : string.Empty;
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
                sfd.InitialDirectory = context;

            if (!string.IsNullOrEmpty(filter)) {
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

        public async Task<bool> ShowQuestion(string msg, string title = "")
        {
            if (string.IsNullOrEmpty(title)) {
                title = GKData.APP_TITLE;
            }

            bool retVal = MessageBox.Show(msg, title, MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes;
            return await Task.FromResult(retVal);
        }

        public void ShowWarning(string msg, string title = "")
        {
            if (string.IsNullOrEmpty(title)) {
                title = GKData.APP_TITLE;
            }

            MessageBox.Show(msg, title, MessageBoxButtons.OK, MessageBoxIcon.Warning);
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
