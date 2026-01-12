/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Threading.Tasks;
using GKCore.Design.Graphics;
using GKCore.Design.Views;

namespace GKTests.Stubs
{
    public class StdDialogsStub : IStdDialogs
    {
        private static string fStrResult = string.Empty;

        public static void SetStrInputResult(string value)
        {
            fStrResult = value;
        }


        public async Task<string> GetInput(object owner, string prompt, string value)
        {
            return await Task.FromResult(fStrResult);
        }

        private static string fOpenedFileName;

        public static void SetOpenedFile(string fileName)
        {
            fOpenedFileName = fileName;
        }

        public async Task<string> GetOpenFile(string title, string context, string filter, int filterIndex, string defaultExt)
        {
            string result = fOpenedFileName;
            fOpenedFileName = string.Empty;
            return await Task.FromResult(result);
        }

        public async Task<string[]> GetOpenFiles(string title, string context, string filter, int filterIndex, string defaultExt)
        {
            return await Task.FromResult(new string[0]);
        }

        public async Task<string> GetPassword(string prompt)
        {
            return await Task.FromResult(string.Empty);
        }

        /*public async Task<string> GetSaveFile(string filter)
        {
            return await Task.FromResult(string.Empty);
        }*/

        public async Task<string> GetSaveFile(string context, string filter)
        {
            return await Task.FromResult(string.Empty);
        }

        public async Task<string> GetSaveFile(string title, string context, string filter, int filterIndex, string defaultExt, string suggestedFileName, bool overwritePrompt = true)
        {
            return await Task.FromResult(string.Empty);
        }

        public async Task<IColor> SelectColor(IColor color)
        {
            return await Task.FromResult(color);
        }

        public async Task<IFont> SelectFont(IFont font)
        {
            return await Task.FromResult(font);
        }

        public void ShowAlert(string msg, string title = "")
        {
        }

        public void ShowError(string msg, string title = "")
        {
        }

        public void ShowMessage(string msg, string title = "")
        {
        }

        public async Task<bool> ShowQuestion(string msg, string title = "")
        {
            return await Task.FromResult(fQuestionResult);
        }

        private static bool fQuestionResult;

        public static void SetQuestionResult(bool value)
        {
            fQuestionResult = value;
        }

        public void ShowWarning(string msg, string title = "")
        {
        }
    }
}
