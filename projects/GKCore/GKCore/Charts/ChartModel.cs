/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
        protected IChartLayout fLayout;
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

        public IChartLayout Layout
        {
            get { return fLayout; }
        }

        public ChartRenderer Renderer
        {
            get { return fRenderer; }
        }

        protected ChartModel()
        {
        }

        public virtual void SetLayout(IChartLayout layout)
        {
            fLayout = layout;
            layout.SetModel(this);
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
