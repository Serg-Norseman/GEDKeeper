/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GDModel;
using GKCore.Design;
using GKCore.Filters;
using GKCore.Lists;

namespace GKCore.Plugins
{
    /// <summary>
    /// Interface for a widgets with support of notifications about records changes.
    /// </summary>
    public interface ISubscriber
    {
        void NotifyRecord(IBaseWindow baseWin, object record, RecordAction action);
        void NotifyFilter(IBaseWindow baseWin, GDMRecordType recType, IListSource listSource, ListFilter filter);
    }
}
