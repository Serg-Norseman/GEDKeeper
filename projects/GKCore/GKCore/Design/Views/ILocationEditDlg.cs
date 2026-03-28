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
    public interface ILocationEditDlg : ICommonDialog
    {
        GDMLocationRecord LocationRecord { get; set; }

        IMapBrowser MapBrowser { get; }
        ISheetList MediaList { get; }
        ISheetList NotesList { get; }
        ISheetList NamesList { get; }
        ISheetList LinksList { get; }
        IListView GeoCoordsList { get; }
        ITextBox Name { get; }
        ITextBox Latitude { get; }
        ITextBox Longitude { get; }
    }
}
