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

using System.Threading.Tasks;
using GKCore.Design.Graphics;

namespace GKCore.Design.Views
{
    /// <summary>
    /// The interface of the class for working with WinForms dialogs.
    /// </summary>
    public interface IStdDialogs
    {
        Task<string> GetInput(object owner, string prompt, string value);

        Task<string> GetOpenFile(string title, string context, string filter, int filterIndex, string defaultExt);

        Task<string[]> GetOpenFiles(string title, string context, string filter, int filterIndex, string defaultExt);

        Task<string> GetPassword(string prompt);

        //Task<string> GetSaveFile(string filter);

        Task<string> GetSaveFile(string context, string filter);

        Task<string> GetSaveFile(string title, string context, string filter, int filterIndex, string defaultExt,
                                 string suggestedFileName, bool overwritePrompt = true);

        Task<IColor> SelectColor(IColor color);

        Task<IFont> SelectFont(IFont font);

        void ShowAlert(string msg, string title = "");

        void ShowError(string msg, string title = "");

        void ShowMessage(string msg, string title = "");

        Task<bool> ShowQuestion(string msg, string title = "");

        void ShowWarning(string msg, string title = "");
    }
}
