/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Filters;
using GKUI.Themes;

namespace GKCore.Design.Controls
{
    public interface IFilterGridView : IBaseControl, IThemedView
    {
        int Count { get; }
        ColumnConditionExpression this[int index] { get; }

        void AddCondition(ColumnConditionExpression fcond);
        void Clear();
        void RemoveCondition(int index);
    }
}
