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
    public interface IPersonalNameEditDlg : ICommonDialog
    {
        GDMIndividualRecord IndividualRecord { get; set; }
        GDMPersonalName PersonalName { get; set; }

        ILabel SurnameLabel { get; }
        ITextBox Surname { get; }
        ITextBox Name { get; }
        ITextBox Patronymic { get; }
        IComboBox NameType { get; }
        ITextBox NamePrefix { get; }
        ITextBox Nickname { get; }
        ITextBox SurnamePrefix { get; }
        ITextBox NameSuffix { get; }
        ITextBox MarriedSurname { get; }
        IComboBox Language { get; }
    }
}
