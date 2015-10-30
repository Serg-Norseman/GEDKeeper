using System;
using System.Collections.Generic;
using System.Drawing;
using System.Windows.Forms;

namespace GKUI.Controls
{
	/// <summary>
	/// 
	/// </summary>
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
			if (disposing) {
                FRAG_BRUSH.Dispose();
                EMPTY_BRUSH.Dispose();
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

			// расчет простой суммы фрагментов
			double sum = 0.0;
			for (int i = 0; i < count; i++) {
				frag = fList[i];
				sum = sum + frag.val;
			}

			// расчет логарифма величины фрагмента и суммы логарифмов
			double logSum = 0.0;
			for (int i = 0; i < count; i++) {
				frag = fList[i];
				frag.log = Math.Log(frag.val, sum);

				logSum = logSum + frag.log;
			}

			// расчет визуальной ширины фрагментов и их суммы
			int resWidth = 0;
			for (int i = 0; i < count; i++) {
				frag = fList[i];
				frag.percent = frag.log / logSum;
				frag.width = (int)((double)wid * frag.percent);

				resWidth = resWidth + frag.width;
			}

			// распределить разницу между реальной шириной компонента и суммой ширины фрагментов
			int d = wid - resWidth;
			if (d > 0) {
				// разницу распределяем между наибольшими фрагментами
				List<Fragment> ordList = new List<Fragment>(this.fList);
				ordList.Sort(new FragmentComparer());

				int idx = 0;
				while (d > 0) {
					frag = ordList[idx];
					frag.width = frag.width + 1;

					if (idx == count - 1) {
						idx = 0;
					} else idx++;
					
					d--;
				}
			}

			// подготовить области отрисовки фрагментов
			int x = 0;
			for (int i = 0; i < count; i++) {
				frag = fList[i];

				frag.x = x;
				frag.rect = new Rectangle(x, 0, frag.width, this.Height);
				x = x + (frag.width + 1);
			}

			base.Invalidate();
		}

		private class FragmentComparer: IComparer<Fragment>
		{
			public int Compare(Fragment x, Fragment y)
			{
				return -x.width.CompareTo(y.width);
			}
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
