/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
