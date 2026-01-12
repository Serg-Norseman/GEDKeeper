/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Windows.Forms;
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
