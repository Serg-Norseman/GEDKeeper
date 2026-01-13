/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design.Controls;
using GKCore.Names;

namespace GKCore.Design.Views
{
    public interface INameEditDlg : ICommonDialog
    {
        NameEntry IName { get; set; }

        ITextBox Name { get; }
        ITextBox FPatr { get; }
        ITextBox MPatr { get; }
        IComboBox SexCombo { get; }
    }
}
