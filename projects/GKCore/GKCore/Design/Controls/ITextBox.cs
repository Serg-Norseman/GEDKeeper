/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

namespace GKCore.Design.Controls
{
    public interface ITextOutput
    {
        void AppendText(string text);
        void Clear();
    }

    public interface ITextBox : ITextContainer, ITextOutput
    {
        string[] Lines { get; }
        bool ReadOnly { get; set; }
        string SelectedText { get; set; }

        void Copy();
        void SelectAll();
    }
}
