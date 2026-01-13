/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GDModel;
using GKCore.Design.Controls;

namespace GKCore.Design.Views
{
    public interface IAgeEditDlg : ICommonDialog
    {
        GDMCustomEvent Event { get; set; }

        IComboBox RelativeCombo1 { get; }

        ITextBox ValueText1 { get; }

        IComboBox RelativeCombo2 { get; }

        ITextBox ValueText2 { get; }
    }
}
