/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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

using BSLib;

namespace GKCore.Charts
{
    /// <summary>
    /// 
    /// </summary>
    public abstract class ChartModel : BaseObject
    {
        protected int fImageHeight;
        protected int fImageWidth;
        protected int fOffsetX;
        protected int fOffsetY;
        protected ChartRenderer fRenderer;

        public int ImageHeight
        {
            get { return fImageHeight; }
        }

        public int ImageWidth
        {
            get { return fImageWidth; }
        }

        public ExtSize ImageSize
        {
            get { return new ExtSize(fImageWidth, fImageHeight); }
        }

        public ChartRenderer Renderer
        {
            get { return fRenderer; }
        }

        protected ChartModel()
        {
        }

        public virtual void SetRenderer(ChartRenderer renderer)
        {
            fRenderer = renderer;
        }

        public ExtPoint GetOffsets()
        {
            return new ExtPoint(fOffsetX, fOffsetY);
        }

        /// <summary>
        /// Sets the offset for the start of drawing if the image is smaller than the drawing canvas (the tree is displayed in the center,
        /// and the canvas is not virtual and occupies the entire area of ​​the scrolled component).
        /// </summary>
        public void SetOffsets(int offsetX, int offsetY)
        {
            fOffsetX = offsetX;
            fOffsetY = offsetY;
        }
    }
}
