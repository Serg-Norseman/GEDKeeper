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

using System.Windows.Forms;
using GKCore.MVP.Views;

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

        private readonly StatusStrip fStatusBar;
        private readonly StatusLinesEx fStatusLines;

        public IStatusLines StatusLines
        {
            get { return fStatusLines; }
        }

        public StatusForm()
        {
            fStatusBar = new StatusStrip();
            fStatusBar.Margin = new Padding(2);
            Controls.Add(fStatusBar);

            fStatusLines = new StatusLinesEx(this);
        }

        protected string GetStatusLine(int index)
        {
            if (index < 0 || index >= fStatusBar.Items.Count) {
                return string.Empty;
            } else {
                return fStatusBar.Items[index].Text;
            }
        }

        protected void SetStatusLine(int index, string value)
        {
            if (index < 0) return;

            fStatusBar.SuspendLayout();

            ToolStripStatusLabel panel;
            while (index >= fStatusBar.Items.Count) {
                panel = new ToolStripStatusLabel();
                panel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
                panel.BorderSides = ToolStripStatusLabelBorderSides.All;
                panel.BorderStyle = Border3DStyle.Sunken;
                fStatusBar.Items.Add(panel);
            }

            panel = (ToolStripStatusLabel)fStatusBar.Items[index];
            panel.Text = value;

            /*ToolStripStatusLabel last = null;
            for (int i = 0; i < fStatusBar.Items.Count; i++) {
                last = (ToolStripStatusLabel)fStatusBar.Items[i];
                last.Spring = false;
            }
            if (last != null) {
                last.Spring = true;
            }*/

            fStatusBar.ResumeLayout(false);
            fStatusBar.PerformLayout();
        }
    }
}
