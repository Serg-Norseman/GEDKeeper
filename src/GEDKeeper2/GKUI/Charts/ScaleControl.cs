using System.Drawing;
using System.Drawing.Drawing2D;

namespace GKUI.Charts
{
	/// <summary>
	/// 
	/// </summary>
	public sealed class ScaleControl : ITreeControl
    {
        #region Private fields
		
        private static Rectangle ControlsScaleRect = new Rectangle(0, 0, 26, 320);
        private static Rectangle ControlsThumbRect = new Rectangle(0, 322, 26, 11);

        private const int ScaleY1 = 22;
        private const int ScaleY2 = 297;

        private readonly Bitmap fControlsImage;
        private readonly TreeChartBox fChart;

        private int fDCount = 11;
        private int fThumbPos = 5;
        private bool fVisible;
        private bool fThumbCaptured;
        private string fTip;
        private Rectangle fDestRect;

        #endregion
		
        #region Public properties
		
        public int Width
        {
            get { return 26; }
        }

        public int Height
        {
            get { return 320; }
        }

        public int DCount
        {
            get { return this.fDCount; }
            set { this.fDCount = value; }
        }

        public int ThumbPos
        {
            get { return this.fThumbPos; }
            set { this.fThumbPos = value; }
        }
		
        public string Tip
        {
            get { return this.fTip; }
            set { this.fTip = value; }
        }
		
        public bool Visible
        {
            get { return this.fVisible; }
            set { 
                this.fVisible = value;
                this.fChart.Invalidate();
            }
        }

        #endregion
		
        public ScaleControl(TreeChartBox chart)
        {
            this.fChart = chart;
            this.fControlsImage = GKResources.iChartControls;
        }

        public void Dispose()
        {
            // dummy
        }

        public void Update()
        {
            Rectangle cr = fChart.ClientRectangle;
            this.fDestRect = new Rectangle(cr.Right - (10 + this.Width), 10, this.Width, this.Height);
        }

        public void Draw(Graphics gfx)
        {
            if (gfx == null) return;

            gfx.InterpolationMode = InterpolationMode.HighQualityBicubic;
            gfx.SmoothingMode = SmoothingMode.HighQuality;
            gfx.PixelOffsetMode = PixelOffsetMode.HighQuality;
            gfx.CompositingQuality = CompositingQuality.HighQuality;
            gfx.DrawImage(fControlsImage, fDestRect, ControlsScaleRect, GraphicsUnit.Pixel);

            if (this.fDCount == 0) return;
            gfx.DrawImage(fControlsImage, this.GetDRect(fThumbPos), ControlsThumbRect, GraphicsUnit.Pixel);
        }

        private Rectangle GetDRect(int d)
        {
            int dH = ((ScaleY2 - ScaleY1) - ControlsThumbRect.Height) / (this.fDCount - 1);
            int thumbY = fDestRect.Top + ScaleY1 + (d - 1) * dH;
            return new Rectangle(fDestRect.Left, thumbY, fDestRect.Width, ControlsThumbRect.Height);
        }

        public bool Contains(int X, int Y)
        {
            return fDestRect.Contains(X, Y);
        }

        public void MouseDown(int X, int Y)
        {
            fThumbCaptured = (this.GetDRect(fThumbPos).Contains(X, Y) && !fThumbCaptured);
        }

        public void MouseMove(int X, int Y, ThumbMoved thumbMoved)
        {
            if (!fThumbCaptured) return;
			
            for (int i = 1; i <= fDCount; i++) {
                Rectangle r = GetDRect(i);
                if (r.Contains(X, Y)) {
                    fThumbPos = i;
                    fChart.Invalidate();
                    if (thumbMoved != null) thumbMoved(i);
                    break;
                }
            }
        }

        public void MouseUp(int X, int Y)
        {
            if (fThumbCaptured) fThumbCaptured = false;
        }
    }
}