/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using Eto.Forms;
using GKCore.Design.Views;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public class StatusForm : CommonWindow, IStatusForm
    {
        public sealed class StatusLinesEx : IStatusLines
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

        public IStatusLines StatusLines
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

        public StatusForm()
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
