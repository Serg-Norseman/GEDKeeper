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
using Terminal.Gui.App;
using Terminal.Gui.Views;

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

        public Task<IColor> SelectColor(IColor color)
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

        public Task<IFont> SelectFont(IFont font)
        {
            /*Font sdFont = ((FontHandler)font).Handle;

            using (FontDialog fontDlg = new FontDialog())
            {
                fontDlg.Font = sdFont;
                return (fontDlg.ShowDialog(null) != DialogResult.Ok) ? null : new FontHandler(fontDlg.Font);
            }*/
            return null;
        }

        public Task<string> GetOpenFile(string title, string context, string filter, int filterIndex, string defaultExt)
        {
            var dlg = new OpenDialog() {
                Title = title,
                Path = context,
                AllowsMultipleSelection = false,
            };
            // FIXME: dont work!
            GetFilterFileTypes(filter, dlg.AllowedTypes);
            Application.Run(dlg);
            if (!dlg.Canceled) {
                return Task.FromResult(dlg.FilePaths[0]);
            } else {
                return Task.FromResult(string.Empty);
            }
        }

        private void GetFilterFileTypes(string filter, List<IAllowedType> allowedTypes)
        {
            var filterParts = filter.Split('|');
            int filtersNum = filterParts.Length / 2;
            var result = new List<string>();
            for (int i = 0; i < filtersNum; i++) {
                int idx = i * 2;
                string name = filterParts[idx];
                string exts = filterParts[idx + 1];
                string[] extensions = exts.Split(',');

                result.Clear();
                for (int k = 0; k < extensions.Length; k++) {
                    var ext = extensions[k];
                    if (ext[0] == '*')
                        ext = ext.Substring(1);
                    result.Add(ext);
                }

                var tgExts = result.ToArray();
                allowedTypes.Add(new AllowedType(name, tgExts));
            }
        }

        public Task<string> GetSaveFile(string context, string filter)
        {
            return GetSaveFile("", "", filter, 1, "", "");
        }

        public Task<string> GetSaveFile(string title, string context, string filter, int filterIndex, string defaultExt,
                                 string suggestedFileName, bool overwritePrompt = true)
        {
            return Task.FromResult(string.Empty);
        }


        public void ShowAlert(string msg, string title = "")
        {
            if (string.IsNullOrEmpty(title)) {
                title = GKData.APP_TITLE;
            }

            MessageBox.Query(Application.Instance, title, msg, "OK");
        }

        public void ShowMessage(string msg, string title = "")
        {
            if (string.IsNullOrEmpty(title)) {
                title = GKData.APP_TITLE;
            }

            MessageBox.Query(Application.Instance, title, msg, "OK");
        }

        public void ShowError(string msg, string title = "")
        {
            if (string.IsNullOrEmpty(title)) {
                title = GKData.APP_TITLE;
            }

            MessageBox.Query(Application.Instance, title, msg, "OK");
        }

        public Task<bool> ShowQuestion(string msg, string title = "")
        {
            if (string.IsNullOrEmpty(title)) {
                title = GKData.APP_TITLE;
            }

            return Task.FromResult(MessageBox.Query(Application.Instance, title, msg, new string[] { "Yes", "No" }) == 0);
        }

        public void ShowWarning(string msg, string title = "")
        {
            if (string.IsNullOrEmpty(title)) {
                title = GKData.APP_TITLE;
            }

            MessageBox.Query(Application.Instance, title, msg, "OK");
        }


        public Task<string> GetInput(object owner, string prompt, string value)
        {
            /*bool res = GKInputBox.QueryText(owner, GKData.APP_TITLE, prompt, ref value);
            return res && !string.IsNullOrEmpty(value);*/
            return Task.FromResult(string.Empty);
        }

        public Task<string> GetPassword(string prompt)
        {
            /*bool res = GKInputBox.QueryPassword(GKData.APP_TITLE, prompt, ref value);
            return res && !string.IsNullOrEmpty(value);*/
            return Task.FromResult(string.Empty);
        }

        public Task<string[]> GetOpenFiles(string title, string context, string filter, int filterIndex, string defaultExt)
        {
            throw new System.NotImplementedException();
        }
    }
}
