
using System;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using System.Windows.Forms;

namespace GKUI.Controls
{
	public sealed class GKLogChart : Panel
	{
		private class Fragment
		{
			public int srcval;

			public double val;
			public double log;
			public double percent;

			public int x;
			public int width;
			
			public Rectangle rect;
		}

		private readonly Brush FRAG_BRUSH = new SolidBrush(Color.Green);
		private readonly Brush EMPTY_BRUSH = new SolidBrush(Color.Gray);
		
		private readonly List<Fragment> fList;
		private readonly System.Windows.Forms.ToolTip fToolTip;
		private string fHint;

		public GKLogChart()
		{
            this.fList = new List<Fragment>();

			this.fToolTip = new System.Windows.Forms.ToolTip();
			this.fToolTip.AutoPopDelay = 5000;
			this.fToolTip.InitialDelay = 250;
			this.fToolTip.ReshowDelay = 50;
			this.fToolTip.ShowAlways = true;
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
			}
			base.Dispose(disposing);
		}

		public void Clear()
		{
			fList.Clear();
			this.UpdateContents();
		}

		public void AddFragment(int val)
		{
			if (val < 1) return;
			
			Fragment frag = new Fragment();

			frag.srcval = val;
			if (val == 1) val++;
			frag.val = (double)val;

			fList.Add(frag);

			this.UpdateContents();
		}

		private void UpdateContents()
		{
			int count = fList.Count;
			if (count == 0) return;
			
			int wid = this.Width - (count - 1);

			Fragment frag;

			double sum = 0.0;
			for (int i = 0; i < count; i++) {
				frag = fList[i];

				sum = sum + frag.val;
			}

			double logSum = 0.0;
			for (int i = 0; i < count; i++) {
				frag = fList[i];
				frag.log = Math.Log(frag.val, sum);

				logSum = logSum + frag.log;
			}

			int resWidth = 0;
			for (int i = 0; i < count; i++) {
				frag = fList[i];
				frag.percent = frag.log / logSum;
				frag.width = (int)((double)wid * frag.percent);

				resWidth = resWidth + frag.width;
			}

			// arrange delta
			int d = wid - resWidth;
			if (d > 0) {
				var list = fList.OrderByDescending(z => z.width).ToList();
				int idx = 0;
				while (d > 0) {
					frag = list[idx];
					frag.width = frag.width + 1;

					if (idx == count - 1) {
						idx = 0;
					} else idx++;
					
					d--;
				}
			}

			int x = 0;
			for (int i = 0; i < count; i++) {
				frag = fList[i];

				frag.x = x;
				frag.rect = new Rectangle(x, 0, frag.width, this.Height);
				x = x + (frag.width + 1);
			}

			base.Invalidate();
		}

		protected override void OnPaint(PaintEventArgs e)
		{
			base.OnPaint(e);
			
			if (this.Width <= 0 || this.Height <= 0) return;

			Graphics gfx = e.Graphics;

			int count = fList.Count;
			if (count > 0) {
				for (int i = 0; i < count; i++) {
					Fragment frag = fList[i];
					this.DrawRect(gfx, frag.x, frag.width, FRAG_BRUSH);
				}
			} else {
				this.DrawRect(gfx, 0, this.Width, EMPTY_BRUSH);
			}
		}

		private void DrawRect(Graphics gfx, int x, int width, Brush lb)
		{
			gfx.FillRectangle(lb, x, 0, width, this.Height);
		}

		protected override void OnMouseMove(MouseEventArgs e)
		{
			base.OnMouseMove(e);

			string hint = "";
			int count = fList.Count;
			for (int i = 0; i < count; i++) {
				Fragment frag = fList[i];

				if (frag.rect.Contains(e.X, e.Y)) {
					string st = (i + 1).ToString();
					hint = "Фрагмент: " + st + ", размер = " + frag.srcval.ToString();
					break;
				}
			}

			if (this.fHint != hint) {
				this.fHint = hint;
				fToolTip.Show(hint, this, e.X, e.Y, 3000);
			}
		}

	}
}
