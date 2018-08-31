/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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

using Eto.Forms;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public class StatusForm : CommonForm
    {
        public sealed class StatusLinesEx
        {
            private readonly StatusForm fForm;

            public string this[int index]
            {
                get { return fForm.GetStatusLine(index); }
                set { fForm.SetStatusLine(index, value); }
            }

            internal StatusLinesEx(StatusForm form)
            {
                fForm = form;
            }
        }

        private readonly TableRow fContentRow;
        private readonly TableLayout fStatusBar;
        private readonly TableRow fStatusRow;
        private readonly StatusLinesEx fStatusLines;

        public StatusLinesEx StatusLines
        {
            get { return fStatusLines; }
        }

        public new Control Content
        {
            get {
                return (fContentRow.Cells.Count > 0) ? fContentRow.Cells[0].Control : null;
            }
            set {
                if (fContentRow.Cells.Count > 0) {
                    fContentRow.Cells[0].Control = value;
                } else {
                    fContentRow.Cells.Add(value);
                }
            }
        }

        public StatusForm() : base()
        {
            fStatusRow = new TableRow() {
                Cells = { null }
            };

            fStatusBar = new TableLayout() {
                Rows = {
                    fStatusRow
                }
            };

            fContentRow = new TableRow() {
                ScaleHeight = true,
                Cells = { null }
            };

            base.Content = new TableLayout() {
                Rows = {
                    fContentRow,
                    fStatusBar
                }
            };

            fStatusLines = new StatusLinesEx(this);
        }

        protected string GetStatusLine(int index)
        {
            if (index < 0 || index >= fStatusRow.Cells.Count) {
                return string.Empty;
            } else {
                return ((Label)fStatusRow.Cells[index].Control).Text;
            }
        }

        protected void SetStatusLine(int index, string value)
        {
            fStatusBar.SuspendLayout();

            Label panel = null;
            if (index < 0) {
                return;
            } else if (index >= fStatusRow.Cells.Count) {
                while (index >= fStatusRow.Cells.Count) {
                    panel = new Label();
                    fStatusRow.Cells.Add(panel);
                }
            }

            panel = (Label)fStatusRow.Cells[index].Control;
            if (panel == null) {
                panel = new Label();
                fStatusRow.Cells[index].Control = panel;
            }
            panel.Text = value;

            for (int i = 0; i < fStatusRow.Cells.Count; i++) {
                fStatusRow.Cells[i].ScaleWidth = true;
            }
            //fStatusBar.Panels[fStatusBar.Panels.Count - 1].AutoSize = StatusBarPanelAutoSize.Spring;

            fStatusBar.ResumeLayout();
        }
    }
}
