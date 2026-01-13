/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Lists;
using GKCore.Options;

namespace GKCore.Design.Views
{
    public interface IOptionsDlg : ICommonDialog
    {
        ISheetList EventTypesList { get; }

        void AcceptCircleChartsOptions();
        void UpdateCircleChartsOptions();
        void SetPage(OptionsPage page);
    }
}
