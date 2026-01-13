/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

namespace GKCore.Design.Controls
{
    public interface IDateBox : IBaseControl
    {
        string NormalizeDate { get; set; }
        bool ReadOnly { get; set; }
        string SelectedText { get; set; }
        string Text { get; set; }

        void Clear();
        void Copy();
        void SelectAll();
    }
}
