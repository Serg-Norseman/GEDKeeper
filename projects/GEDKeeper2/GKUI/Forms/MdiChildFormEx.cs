/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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

using System.Drawing;
using System.Windows.Forms;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public class MdiChildFormEx : Form
    {
        protected override void WndProc(ref Message m)
        {
            FormWindowState prevState = WindowState;

            base.WndProc(ref m);

            if (WindowState != prevState)
                OnFormWindowStateChanged(prevState, WindowState);
        }

        protected virtual void OnFormWindowStateChanged(FormWindowState oldState, FormWindowState newState)
        {
            if (oldState == FormWindowState.Maximized && newState == FormWindowState.Normal) {
                AdjustMdiChild();
            }
        }

        /// <summary>
        /// It's a Windows-specific hack for bypass problems with the restoration
        /// of the MdiChild window from maximized state to normal (creates
        /// the dimensions that are included in visible borders of MdiParent
        /// to avoid appear of scrollbars).
        /// </summary>
        private void AdjustMdiChild()
        {
            // Is it MdiChild form?
            Form mdiParent = MdiParent;
            if (mdiParent == null) return;

            MdiClient client = null;
            foreach (Control ctl in mdiParent.Controls) {
                if (ctl is MdiClient) {
                    client = ctl as MdiClient;
                    break;
                }
            }
            if (client == null) return;

            Rectangle formRect = Bounds;
            formRect.Intersect(client.ClientRectangle);
            SetBounds(formRect.Left, formRect.Top, formRect.Width, formRect.Height, BoundsSpecified.All);
        }
    }
}
