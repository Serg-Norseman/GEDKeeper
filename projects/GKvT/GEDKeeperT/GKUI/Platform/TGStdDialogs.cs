/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using GKCore;
using GKCore.Design.Graphics;
using GKCore.Design.Views;
using GKCore.Locales;
using GKUI.Platform.Handlers;
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
            var dlg = new ColorDialog() {
                //Title = title,
            };
            if (color != null) {
                Color sdColor = ((ColorHandler)color).Handle;
                dlg.Color = sdColor;
            }

            Application.Run(dlg);
            if (!dlg.Canceled) {
                return new ColorHandler(dlg.Color);
            } else {
                return color;
            }
        }

        public async Task<IFont> SelectFont(IFont font)
        {
            return font;
        }

        public async Task<string> SelectFolder(string folderPath)
        {
            return folderPath;
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
            return Array.Empty<string>();
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

            return MessageBox.Query(title, msg, new string[] { "Yes", "No" }) == 0;
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
            var x = new InputDialog(GKData.APP_TITLE, prompt, value, false);
            Application.Run(x);
            return x.InputValue;
        }

        public async Task<string> GetPassword(string prompt)
        {
            var x = new InputDialog(GKData.APP_TITLE, prompt, string.Empty, true);
            Application.Run(x);
            return x.InputValue;
        }


        private class InputDialog : Dialog
        {
            private readonly TextField fInputField;
            private readonly Label fPromptLabel;

            public string InputValue { get; private set; } = string.Empty;
            public bool Cancelled { get; private set; } = true;

            public InputDialog(string title, string prompt, string defaultValue = "", bool isPassword = false) : base(title, 60, 8)
            {
                fPromptLabel = new Label(prompt) {
                    X = 1,
                    Y = 1,
                    Width = Dim.Fill() - 1
                };
                Add(fPromptLabel);

                fInputField = new TextField(defaultValue) {
                    X = 1,
                    Y = 2,
                    Width = Dim.Fill() - 1,
                    Secret = isPassword
                };
                Add(fInputField);

                var acceptButton = new Button(LangMan.LS(LSID.DlgAccept));
                acceptButton.Clicked += (s, e) => {
                    InputValue = fInputField.Text.ToString() ?? string.Empty;
                    Cancelled = false;
                    Application.RequestStop();
                };

                var cancelButton = new Button(LangMan.LS(LSID.DlgCancel));
                cancelButton.Clicked += (s, e) => {
                    Cancelled = true;
                    Application.RequestStop();
                };

                ButtonAlignment = ButtonAlignments.Right;
                AddButton(acceptButton);
                AddButton(cancelButton);

                fInputField.SetFocus();
            }

            /*public bool ShowDialog()
            {
                Application.Run(this);
                return result;
            }*/
        }
    }
}
