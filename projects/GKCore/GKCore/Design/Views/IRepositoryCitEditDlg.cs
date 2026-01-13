/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GDModel;
using GKCore.Design.Controls;
using GKCore.Lists;

namespace GKCore.Design.Views
{
    public interface IRepositoryCitEditDlg : ICommonDialog
    {
        GDMRepositoryCitation RepositoryCitation { get; set; }

        IComboBox RepositoryCombo { get; }

        ISheetList CallNumbersList { get; }
    }
}
