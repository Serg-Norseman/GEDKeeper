/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design.Controls;
using GKCore.Locales;

namespace GKCore.Design.Views
{
    public interface IScriptConsole : IView
    {
        void print(string text);
    }


    public interface IScriptEditWin : ICommonDialog, ILocalizable, IScriptConsole
    {
        ITextBox ScriptText { get; }
        ITextBox DebugOutput { get; }

        string FileName { get; set; }
        bool Modified { get; set; }

        bool CheckModified();
    }
}
