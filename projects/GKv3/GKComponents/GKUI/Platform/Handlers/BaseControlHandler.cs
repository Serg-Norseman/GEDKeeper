/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using Eto.Drawing;
using Eto.Forms;
using GKCore.Design;

namespace GKUI.Platform.Handlers
{
    public abstract class BaseControlHandler<T, TThis> : ControlHandler<T, TThis>, IBaseControl
        where T : Control
        where TThis : ControlHandler<T, TThis>
    {
        protected BaseControlHandler(T control) : base(control)
        {
        }

        public bool Enabled
        {
            get { return Control.Enabled; }
            set { Control.Enabled = value; }
        }

        public bool Visible
        {
            get { return Control.Visible; }
            set { Control.Visible = value; }
        }

        public void Activate()
        {
            Control.Focus();
        }

        protected void SetAccessible(bool value)
        {
#if OS_LINUX
            // is not required and works strangely: when you change the BackgroundColor, the background color of the selected text becomes the same
            //Control.BackgroundColor = value ? SystemColors.ControlBackground : SystemColors.WindowBackground;
#else
            Control.BackgroundColor = value ? SystemColors.WindowBackground : SystemColors.Control; // win ok, mac ?
#endif
        }
    }
}
