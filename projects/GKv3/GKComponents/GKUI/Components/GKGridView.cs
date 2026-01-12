/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using Eto.Drawing;
using Eto.Forms;
using GKCore;

namespace GKUI.Components
{
    public class GKGridView : GridView, IContextMenuHost
    {
        protected static readonly bool HasGridCellFormat = AppHost.Instance.HasFeatureSupport(Feature.GridCellFormat);

        public Color TextColor { get; set; }

        public Font Font { get; set; }

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

            if (Font != null) e.Font = Font;
            e.ForegroundColor = this.TextColor;
        }
    }
}
