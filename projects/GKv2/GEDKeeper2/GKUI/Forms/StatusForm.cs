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

using System;
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

        private readonly StatusBar fStatusBar;
        private readonly StatusLinesEx fStatusLines;

        public IStatusLines StatusLines
        {
            get { return fStatusLines; }
        }

        public StatusForm() : base()
        {
            fStatusBar = new StatusBar();
            fStatusBar.Margin = new Padding(2);
            fStatusBar.ShowPanels = true;
            Controls.Add(fStatusBar);

            fStatusLines = new StatusLinesEx(this);
        }

        protected string GetStatusLine(int index)
        {
            if (index < 0 || index >= fStatusBar.Panels.Count) {
                return string.Empty;
            } else {
                return fStatusBar.Panels[index].Text;
            }
        }

        protected void SetStatusLine(int index, string value)
        {
            StatusBarPanel panel = null;
            if (index < 0) {
                return;
            } else if (index >= fStatusBar.Panels.Count) {
                while (index >= fStatusBar.Panels.Count) {
                    panel = new StatusBarPanel();
                    fStatusBar.Panels.Add(panel);
                }
            }

            panel = fStatusBar.Panels[index];
            panel.Text = value;

            for (int i = 0; i < fStatusBar.Panels.Count; i++) {
                fStatusBar.Panels[i].AutoSize = StatusBarPanelAutoSize.Contents;
            }
            fStatusBar.Panels[fStatusBar.Panels.Count - 1].AutoSize = StatusBarPanelAutoSize.Spring;
        }
    }
}
