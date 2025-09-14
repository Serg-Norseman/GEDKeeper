/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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

using Eto;
using Eto.Drawing;
using Eto.Forms;

namespace GKUI.Platform
{
    [Handler(typeof(GKMenuBar.IHandler))]
    public class GKMenuBar : MenuBar
    {
        public Color BackgroundColor
        {
            get { return Handler.BackgroundColor; }
            set { Handler.BackgroundColor = value; }
        }

        public Font Font
        {
            get { return Handler.Font; }
            set { Handler.Font = value; }
        }

        public Color TextColor
        {
            get { return Handler.TextColor; }
            set { Handler.TextColor = value; }
        }

        public GKMenuBar()
        {
        }

        new IHandler Handler { get { return (IHandler)base.Handler; } }

        public new interface IHandler : MenuBar.IHandler
        {
            Color BackgroundColor { get; set; }
            Font Font { get; set; }
            Color TextColor { get; set; }
        }
    }


    [Handler(typeof(GKToolBar.IHandler))]
    public class GKToolBar : ToolBar
    {
        public Color BackgroundColor
        {
            get { return Handler.BackgroundColor; }
            set { Handler.BackgroundColor = value; }
        }

        public Font Font
        {
            get { return Handler.Font; }
            set { Handler.Font = value; }
        }

        public Color TextColor
        {
            get { return Handler.TextColor; }
            set { Handler.TextColor = value; }
        }

        public GKToolBar()
        {
        }

        new IHandler Handler { get { return (IHandler)base.Handler; } }

        public new interface IHandler : ToolBar.IHandler
        {
            Color BackgroundColor { get; set; }
            Font Font { get; set; }
            Color TextColor { get; set; }
        }
    }
}
