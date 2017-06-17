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

using System;
using Eto.Drawing;
using Eto.Forms;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public class CustomPanel : Scrollable
    {
        private readonly Drawable fCanvas;
        private Font fFont;
        private Color fTextColor;


        public Font Font
        {
            get { return fFont; }
            set { fFont = value; }
        }

        public Color TextColor
        {
            get { return fTextColor; }
            set { fTextColor = value; }
        }


        public CustomPanel()
        {
            fCanvas = new Drawable();
            fCanvas.Paint += PaintHandler;
            fCanvas.CanFocus = true;
            Content = fCanvas;

            fFont = SystemFonts.Label();
            fTextColor = Colors.Black;
        }

        private void PaintHandler(object sender, PaintEventArgs e)
        {
            OnPaint(e);
        }

        protected virtual void OnPaint(PaintEventArgs e)
        {
        }

        protected override void OnSizeChanged(EventArgs e)
        {
            fCanvas.Size = this.ClientSize;
            base.OnSizeChanged(e);
        }

        public Graphics CreateGraphics()
        {
            if (fCanvas.SupportsCreateGraphics) {
                return fCanvas.CreateGraphics();
            } else {
                return null;
            }
        }
    }
}
