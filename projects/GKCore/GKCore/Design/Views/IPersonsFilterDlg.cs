/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design.Controls;

namespace GKCore.Design.Views
{
    public interface IPersonsFilterDlg : ICommonDialog
    {
        IComboBox SourceCombo { get; }
        IComboBox GroupCombo { get; }
        ITextBox AliveBeforeDate { get; }
        ICheckBox OnlyPatriarchsCheck { get; }
        IComboBox EventValCombo { get; }
        IComboBox ResidenceCombo { get; }
        IComboBox NameCombo { get; }

        void SetLifeRadio(int lifeSel);
        void SetSexRadio(int sexSel);
        int GetLifeRadio();
        int GetSexRadio();
        void SetLifeEnabled(bool value);
    }
}
