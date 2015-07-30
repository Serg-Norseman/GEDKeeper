using System;
using System.Drawing;
using System.Drawing.Drawing2D;
using GKCommon;

namespace GKUI.Charts
{
	public class PersonControl : ITreeControl
	{
		private readonly TreeChartBox fChart;
		private bool fVisible;
		private TreeChartPerson fPerson;
		private Pen fCtlPen;
		private Brush fCtlBrush;
		private Rectangle fDestRect;
		
        public bool Visible
        {
            get { return this.fVisible; }
            set { 
                this.fVisible = value;
                this.fChart.Invalidate();
            }
        }
		
        public PersonControl(TreeChartBox chart)
        {
            this.fChart = chart;
            this.fCtlPen = new Pen(Color.Black, 2.0f);
            this.fCtlBrush = new SolidBrush(Color.FromArgb(128, 128, 128, 128));
        }

        public void SetPerson(TreeChartPerson person)
        {
        	this.fPerson = person;
        	
            ExtRect rt = this.fPerson.Rect;
            rt = rt.GetOffset(this.fChart.fSPX, this.fChart.fSPY);
            Rectangle rect = rt.ToRectangle();
            
            rect.X = rect.Right;
            rect.Width = 40;
            
        	this.fDestRect = rect;
        }
        
        public void Update()
		{
		}

        public void Draw(Graphics gfx)
		{
            if (gfx == null) return;

            /*ExtRect rt = this.fPerson.Rect;
            rt = rt.GetOffset(this.fChart.fSPX, this.fChart.fSPY);
            Rectangle rect = rt.ToRectangle();
            
            //rect.Top = rect.Top;
            //rect.Bottom = rect.Bottom;
            rect.X = rect.Right;
            rect.Width = 40;*/
            
            gfx.InterpolationMode = InterpolationMode.HighQualityBicubic;
            gfx.SmoothingMode = SmoothingMode.HighQuality;
            gfx.PixelOffsetMode = PixelOffsetMode.HighQuality;
            gfx.CompositingQuality = CompositingQuality.HighQuality;
            
            gfx.FillRectangle(this.fCtlBrush, this.fDestRect);
            gfx.DrawRectangle(this.fCtlPen, this.fDestRect);
		}

        public bool Contains(int X, int Y)
		{
        	return fDestRect.Contains(X, Y);
		}

        public void MouseDown(int X, int Y)
		{
		}

        public void MouseMove(int X, int Y, ThumbMoved thumbMoved)
		{
		}

        public void MouseUp(int X, int Y)
		{
		}
	}
}
