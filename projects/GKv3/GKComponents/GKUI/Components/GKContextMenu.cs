/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using Eto;
using Eto.Drawing;
using Eto.Forms;

namespace GKUI.Platform
{
    [Handler(typeof(GKContextMenu.IHandler))]
    public class GKContextMenu : ContextMenu
    {
        public Font Font
        {
            get { return Handler.Font; }
            set { Handler.Font = value; }
        }

        public GKContextMenu()
        {
        }

        new IHandler Handler { get { return (IHandler)base.Handler; } }

        public new interface IHandler : ContextMenu.IHandler
        {
            Font Font { get; set; }
        }
    }
}
