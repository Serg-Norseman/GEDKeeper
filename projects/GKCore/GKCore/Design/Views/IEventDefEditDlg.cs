/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design.Controls;
using GKCore.Events;

namespace GKCore.Design.Views
{
    public interface IEventDefEditDlg : ICommonDialog
    {
        EventDef EventDef { get; set; }

        ITextBox NameText { get; }
        IComboBox TagCombo { get; }
        ITextBox TypeText { get; }
        ICheckBox EnabledCheck { get; }
        ITextBox DescText { get; }
    }
}
