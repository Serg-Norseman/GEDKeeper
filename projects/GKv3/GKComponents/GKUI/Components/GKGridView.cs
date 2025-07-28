/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using Eto.Drawing;
using Eto.Forms;
using GKCore;
using GKCore.Types;

namespace GKUI.Components
{
    public class GKGridView : GridView
    {
        protected static readonly bool HasGridCellFormat = AppHost.Instance.HasFeatureSupport(Feature.GridCellFormat);

        public Color TextColor { get; set; }

        public GKGridView()
        {
            TextColor = Colors.Black;
        }

        protected override void OnCellFormatting(GridCellFormatEventArgs e)
        {
            base.OnCellFormatting(e);

            // According to the profiler, this method has high overhead
            // (in descending order: setForegroundColor, getSelectedRow, HasFeatureSupport, getRow).

            if (!HasGridCellFormat) return;

            // FIXME: doesn't work correctly because selection changes don't call this method (Eto <= 2.8.3)
            // This method only works with OnSelectionChanged -> ReloadData(SelectedRow)
            // [Wpf]GridView.ReloadData(...) is very slow (Eto <= 2.8.3 #2245)
            /*if (e.Row == base.SelectedRow) {
                e.BackgroundColor = SystemColors.Selection;
                e.ForegroundColor = SystemColors.SelectionText;
            } else {
                e.ForegroundColor = this.TextColor;
            }*/

            e.ForegroundColor = this.TextColor;
        }
    }
}
