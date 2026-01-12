/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Security.Permissions;
using System.Windows.Forms;

namespace GKUI.Components
{
    /// <summary>
    /// Analogue of TabControl (or Notebook component) with support for hiding tabs.
    /// </summary>
    public class WizardPages : TabControl
    {
        [SecurityPermission(SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.UnmanagedCode), SecurityPermission(SecurityAction.InheritanceDemand, Flags = SecurityPermissionFlag.UnmanagedCode)]
        protected override void WndProc(ref Message m)
        {
            // Hide tabs by trapping the TCM_ADJUSTRECT message
            if (m.Msg == 0x1328 && !DesignMode) {
                m.Result = (IntPtr)1;
            } else {
                base.WndProc(ref m);
            }
        }
    }
}
