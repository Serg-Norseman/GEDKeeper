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

using System.Threading.Tasks;
using GKCore.Design.Graphics;
using GKCore.Design.Views;

namespace GKTests.Stubs
{
    public class StdDialogsStub : IStdDialogs
    {
        public async Task<string> GetInput(object owner, string prompt, string value)
        {
            return await Task.FromResult(string.Empty);
        }

        public async Task<string> GetOpenFile(string title, string context, string filter, int filterIndex, string defaultExt)
        {
            return await Task.FromResult(string.Empty);
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
            return await Task.FromResult(false);
        }

        public void ShowWarning(string msg, string title = "")
        {
        }
    }
}
