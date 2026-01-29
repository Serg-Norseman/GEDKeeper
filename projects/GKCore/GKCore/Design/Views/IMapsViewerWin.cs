/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

#if !TERM

using System.Collections.Generic;
using GKCore.Design.Controls;
using GKCore.Maps;

namespace GKCore.Design.Views
{
    public interface IMapsViewerWin : IWindow
    {
        IMapBrowser MapBrowser { get; }
        IComboBox PersonsCombo { get; }
        ITreeView PlacesTree { get; }
        IButton SelectPlacesBtn { get; }
        ICheckBox BirthCheck { get; }
        ICheckBox DeathCheck { get; }
        ICheckBox ResidenceCheck { get; }
        ICheckBox LinesVisibleCheck { get; }
        IRadioButton TotalRadio { get; }
        IRadioButton SelectedRadio { get; }

        ITVNode FindTreeNode(string place);

        void ShowFixedPoints(IEnumerable<GeoPoint> points);
    }
}

#endif
