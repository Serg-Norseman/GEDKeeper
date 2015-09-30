using System;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Windows.Forms;

using GKCommon.GEDCOM;

namespace GKAncestorsCirclePlugin
{
	public class AncestorsCircleOptions
	{
		public Color[] BrushColor = new Color[11];
		public SolidBrush[] CircleBrushes = new SolidBrush[11];
		public bool CircularLines = true;
		public bool UseFillColor = true;

		public AncestorsCircleOptions()
		{
			this.CircularLines = true;

			this.BrushColor[ 0] = Color.Coral;
			this.BrushColor[ 1] = Color.CadetBlue;
			this.BrushColor[ 2] = Color.DarkGray;
			this.BrushColor[ 3] = Color.Khaki;
			this.BrushColor[ 4] = Color./*CadetBlue;*/LawnGreen;
			this.BrushColor[ 5] = Color./*DarkGray;*/Khaki;
			this.BrushColor[ 6] = Color./*Khaki;*/HotPink;
			this.BrushColor[ 7] = Color./*CadetBlue;*/Ivory;

			this.BrushColor[ 8] = Color.Black; // text
			this.BrushColor[ 9] = Color.Moccasin; // background
			this.BrushColor[10] = Color.Black; // lines

			CreateBrushes();
		}

		public void CreateBrushes()
		{
			for (int i = 0; i < this.BrushColor.Length; i++)
			{
				this.CircleBrushes[i] = new SolidBrush(this.BrushColor[i]);
			}
		}
	}


	public class AncestorsCircle : UserControl
	{
		private GEDCOMIndividualRecord fBack;
		private AncestorsCircleOptions fOptions;
		private GEDCOMIndividualRecord fRootPerson;
		private GEDCOMTree fTree;

		private double PI = 3.1415926535897931;

		private int xStart = 0;
		private int yStart = 0;
		private int gen;
		private int g;
		private int sRot = 90;
		private Point Center;
		private float ang;
		private bool mOpt = true;
		private Font mFont;

		public AncestorsCircleOptions Options
		{
			get { return this.fOptions; }
		}

		public AncestorsCircle(GEDCOMTree tree, GEDCOMIndividualRecord rootPerson)
		{
			this.fTree = tree;
			this.fRootPerson = rootPerson;
			this.fOptions = new AncestorsCircleOptions();

			this.DoubleBuffered = true;
			this.BackColor = this.fOptions.BrushColor[9];
		}

		#region Drawing

		private void InternalDraw(Graphics gfx)
		{
			int mWidth = base.Width;
			int mHeight = base.Height;

			Pen pen = new Pen(this.Options.BrushColor[10]);
			this.gen = 1;
			int num = 430;
			int num2 = 10;
			int num3 = 10;
			this.Center.X = mWidth / 2 + this.xStart - 6;
			this.Center.Y = mHeight / 2 + this.yStart - 17;
			gfx.TranslateTransform((float)(this.Center.X + num2), (float)(this.Center.Y + num3));

			int num4;
			if (this.fOptions.UseFillColor)
			{
				for (int i = 7; i >= 0; i--)
				{
					GraphicsPath graphicsPath = new GraphicsPath();
					num4 = this.sRot + i * 60 + num2;
					graphicsPath.StartFigure();
					graphicsPath.AddEllipse(-num4 - num2, -num4 - num3, 2 * num4, 2 * num4);
					graphicsPath.CloseFigure();
					gfx.FillPath(this.Options.CircleBrushes[i], graphicsPath);
					if (this.Options.CircularLines)
					{
						gfx.DrawEllipse(pen, -num4 - num2, -num4 - num3, 2 * num4, 2 * num4);
					}
				}
			}
			else
			{
				for (int j = 7; j >= 0; j--)
				{
					num4 = this.sRot + j * 60 + num2;
					if (this.Options.CircularLines)
					{
						gfx.DrawEllipse(pen, -num4 - num2, -num4 - num3, 2 * num4, 2 * num4);
					}
				}
			}
			num4 = this.sRot + -60 + num2;
			if (num4 > 0)
			{
				gfx.DrawArc(pen, -num4 - num2, -num4 - num3, 2 * num4, 2 * num4, -150, 120);
				gfx.DrawArc(pen, -num4 - num2, -num4 - num3, 2 * num4, 2 * num4, 30, 120);
			}
			gfx.ResetTransform();
			gfx.DrawLine(pen, this.Center.X + this.sRot + 10 - 30, this.Center.Y, this.Center.X + num + this.sRot, this.Center.Y);
			gfx.DrawLine(pen, this.Center.X - this.sRot - 10 + 30, this.Center.Y, this.Center.X - num - this.sRot, this.Center.Y);
			gfx.DrawLine(pen, this.Center.X, 0, this.Center.X, this.Center.Y - this.sRot - 10);
			gfx.DrawLine(pen, this.Center.X, mHeight, this.Center.X, this.Center.Y + this.sRot + 9);
			Point pt = default(Point);
			Point pt2 = default(Point);
			this.g = -1;
			for (int k = 1; k <= 4; k++)
			{
				this.g += 2;
				double num5 = (double)(this.g * 45);
				pt.X = this.Center.X - 1 - (int)((double)(this.sRot + 70) * Math.Sin(num5 * this.PI / 180.0));
				pt.Y = this.Center.Y - 1 - (int)((double)(this.sRot + 70) * Math.Cos(num5 * this.PI / 180.0));
				pt2.X = this.Center.X - 1 - (int)((double)(this.sRot + num) * Math.Sin(num5 * this.PI / 180.0));
				pt2.Y = this.Center.Y - 1 - (int)((double)(this.sRot + num) * Math.Cos(num5 * this.PI / 180.0));
				gfx.DrawLine(pen, pt, pt2);
			}
			this.g = -1;
			for (int l = 1; l <= 8; l++)
			{
				this.g += 2;
				double num5 = (double)this.g * 22.5;
				pt.X = this.Center.X - 1 - (int)((double)(this.sRot + 130) * Math.Sin(num5 * this.PI / 180.0));
				pt.Y = this.Center.Y - 1 - (int)((double)(this.sRot + 130) * Math.Cos(num5 * this.PI / 180.0));
				pt2.X = this.Center.X - 1 - (int)((double)(this.sRot + num) * Math.Sin(num5 * this.PI / 180.0));
				pt2.Y = this.Center.Y - 1 - (int)((double)(this.sRot + num) * Math.Cos(num5 * this.PI / 180.0));
				gfx.DrawLine(pen, pt, pt2);
			}
			this.g = -1;
			for (int m = 1; m <= 16; m++)
			{
				this.g += 2;
				double num5 = (double)this.g * 11.25;
				pt.X = this.Center.X - 1 - (int)((double)(this.sRot + 190) * Math.Sin(num5 * this.PI / 180.0));
				pt.Y = this.Center.Y - 1 - (int)((double)(this.sRot + 190) * Math.Cos(num5 * this.PI / 180.0));
				pt2.X = this.Center.X - 1 - (int)((double)(this.sRot + num) * Math.Sin(num5 * this.PI / 180.0));
				pt2.Y = this.Center.Y - 1 - (int)((double)(this.sRot + num) * Math.Cos(num5 * this.PI / 180.0));
				gfx.DrawLine(pen, pt, pt2);
			}
			this.g = -1;
			for (int n = 1; n <= 32; n++)
			{
				this.g += 2;
				double num5 = (double)this.g * 5.625;
				pt.X = this.Center.X - (int)((double)(this.sRot + 250) * Math.Sin(num5 * this.PI / 180.0));
				pt.Y = this.Center.Y - (int)((double)(this.sRot + 250) * Math.Cos(num5 * this.PI / 180.0));
				pt2.X = this.Center.X - (int)((double)(this.sRot + num) * Math.Sin(num5 * this.PI / 180.0));
				pt2.Y = this.Center.Y - (int)((double)(this.sRot + num) * Math.Cos(num5 * this.PI / 180.0));
				gfx.DrawLine(pen, pt, pt2);
			}
			this.g = -1;
			for (int num6 = 1; num6 <= 64; num6++)
			{
				this.g += 2;
				double num5 = (double)this.g * 2.8125;
				pt.X = this.Center.X - (int)((double)(this.sRot + 309) * Math.Sin(num5 * this.PI / 180.0));
				pt.Y = this.Center.Y - (int)((double)(this.sRot + 309) * Math.Cos(num5 * this.PI / 180.0));
				pt2.X = this.Center.X - (int)((double)(this.sRot + num) * Math.Sin(num5 * this.PI / 180.0));
				pt2.Y = this.Center.Y - (int)((double)(this.sRot + num) * Math.Cos(num5 * this.PI / 180.0));
				gfx.DrawLine(pen, pt, pt2);
			}
			this.g = -1;
			for (int num7 = 1; num7 <= 128; num7++)
			{
				this.g += 2;
				double num5 = (double)this.g * 1.40625;
				pt.X = this.Center.X - (int)((double)(this.sRot + 370) * Math.Sin(num5 * this.PI / 180.0));
				pt.Y = this.Center.Y - (int)((double)(this.sRot + 370) * Math.Cos(num5 * this.PI / 180.0));
				pt2.X = this.Center.X - (int)((double)(this.sRot + num) * Math.Sin(num5 * this.PI / 180.0));
				pt2.Y = this.Center.Y - (int)((double)(this.sRot + num) * Math.Cos(num5 * this.PI / 180.0));
				gfx.DrawLine(pen, pt, pt2);
			}
			this.mFont = new Font("Arial", 10f);
			string text;
			SizeF sizeF;
			if (this.fRootPerson == null)
			{
				text = "Choose subject";
				sizeF = gfx.MeasureString(text, this.mFont);
				gfx.ResetTransform();
				gfx.TranslateTransform((float)this.Center.X, (float)this.Center.Y);
				gfx.DrawString(text, this.mFont, this.Options.CircleBrushes[8], -sizeF.Width / 2f, -sizeF.Height / 2f);
				return;
			}

			string FirstName, LastName, dummy;
			this.fRootPerson.GetNameParts(out LastName, out FirstName, out dummy);

			text = FirstName;
			sizeF = gfx.MeasureString(text, this.mFont);
			gfx.ResetTransform();
			gfx.TranslateTransform((float)this.Center.X, (float)this.Center.Y);
			gfx.DrawString(text, this.mFont, this.Options.CircleBrushes[8], -sizeF.Width / 2f, -sizeF.Height / 2f - sizeF.Height / 2f);
			text = LastName;
			sizeF = gfx.MeasureString(text, this.mFont);
			gfx.DrawString(text, this.mFont, this.Options.CircleBrushes[8], -sizeF.Width / 2f, 0f);

			GEDCOMIndividualRecord father, mother;
			this.fRootPerson.GetParents(out father, out mother);

			if (this.mOpt)
			{
				this.g = 1;
				float v = 0f;
				if (father != null)
				{
					this.TraverseAncestors(gfx, father, v, this.g, this.sRot, 91);
				}

				this.g = 1;
				v = 180f;
				if (mother != null)
				{
					this.TraverseAncestors(gfx, mother, v, this.g, this.sRot, 91);
					return;
				}
			}
			else
			{
				this.g = 1;
				float v2 = 0f;
				if (mother != null)
				{
					this.TraverseAncestors(gfx, mother, v2, this.g, this.sRot, 91);
				}

				this.g = 1;
				v2 = 180f;
				if (father != null)
				{
					this.TraverseAncestors(gfx, father, v2, this.g, this.sRot, 91);
				}
			}
		}

		private void TraverseAncestors(Graphics gfx, GEDCOMIndividualRecord p, float v, int g, int r, int ro)
		{
			string FirstName, LastName, dummy;
			p.GetNameParts(out LastName, out FirstName, out dummy);

			this.ShowAncestorCircle(gfx, r, g, v, FirstName, LastName);

			GEDCOMIndividualRecord father, mother;
			p.GetParents(out father, out mother);

			if (father != null && this.gen < 8)
			{
				this.gen++;
				float num = (float)Math.Abs(360.0 / Math.Pow(2.0, (double)g) - (double)ro);
				v -= num / 2f;
				this.TraverseAncestors(gfx, father, v, g + 1, r + 60, ro / 2);
				this.gen--;
			}

			if (mother != null && this.gen < 8)
			{
				this.gen++;
				float num2 = (float)(360.0 / Math.Pow(2.0, (double)g));
				v += num2 / 2f;
				this.TraverseAncestors(gfx, mother, v, g + 1, r + 60, ro / 2);
				this.gen--;
			}
		}

		private void ShowAncestorCircle(Graphics gfx, int radius, int g, float v, string s1, string s2)
		{
			radius -= 20;
			switch (g)
			{
				case 1:
					{
						SizeF sizeF = gfx.MeasureString(s1, this.mFont);
						double num = Math.Sin(PI * (double)v / 180.0) * (double)radius;
						double num2 = Math.Cos(PI * (double)v / 180.0) * (double)radius;
						gfx.ResetTransform();
						gfx.TranslateTransform((float)this.Center.X + (float)num, (float)this.Center.Y - (float)num2);
						gfx.RotateTransform(v);
						gfx.DrawString(s1, this.mFont, this.Options.CircleBrushes[8], -sizeF.Width / 2f, -sizeF.Height / 2f);
						sizeF = gfx.MeasureString(s2, this.mFont);
						gfx.DrawString(s2, this.mFont, this.Options.CircleBrushes[8], -sizeF.Width / 2f, -sizeF.Height / 2f + sizeF.Height);
						break;
					}
				case 2:
				case 3:
				case 4:
				case 5:
					{
						SizeF sizeF2 = gfx.MeasureString(s1, this.mFont);
						double num3 = Math.Sin(PI * (double)v / 180.0) * (double)radius;
						double num4 = Math.Cos(PI * (double)v / 180.0) * (double)radius;
						gfx.ResetTransform();
						gfx.TranslateTransform((float)this.Center.X + (float)num3, (float)this.Center.Y - (float)num4);
						gfx.RotateTransform(v);
						gfx.DrawString(s1, this.mFont, this.Options.CircleBrushes[8], -sizeF2.Width / 2f, -sizeF2.Height / 2f);
						sizeF2 = gfx.MeasureString(s2, this.mFont);
						num3 = Math.Sin(PI * (double)v / 180.0) * (double)((float)radius - sizeF2.Height);
						num4 = Math.Cos(PI * (double)v / 180.0) * (double)((float)radius - sizeF2.Height);
						gfx.ResetTransform();
						gfx.TranslateTransform((float)this.Center.X + (float)num3, (float)this.Center.Y - (float)num4);
						gfx.RotateTransform(v);
						gfx.DrawString(s2, this.mFont, this.Options.CircleBrushes[8], -sizeF2.Width / 2f, -sizeF2.Height / 2f);
						break;
					}
				case 6:
					/*{
					SizeF sizeF3 = gfx.MeasureString(s1, this.mFont);
					double num5 = Math.Sin(PI * (double)(v - 1f) / 180.0) * (double)radius;
					double num6 = Math.Cos(PI * (double)(v - 1f) / 180.0) * (double)radius;
					gfx.ResetTransform();
					gfx.TranslateTransform((float)this.Center.X + (float)num5, (float)this.Center.Y - (float)num6);
					gfx.RotateTransform(v - 1f - 90f);
					gfx.DrawString(s1, this.mFont, this.CircleStyle.CircleBrushes[8], -sizeF3.Width / 2f, -sizeF3.Height / 2f);
					sizeF3 = gfx.MeasureString(s2, this.mFont);
					num5 = Math.Sin(PI * (double)(v + 1f) / 180.0) * (double)radius;
					num6 = Math.Cos(PI * (double)(v + 1f) / 180.0) * (double)radius;
					gfx.ResetTransform();
					gfx.TranslateTransform((float)this.Center.X + (float)num5, (float)this.Center.Y - (float)num6);
					gfx.RotateTransform(v + 1f - 90f);
					gfx.DrawString(s2, this.mFont, this.CircleStyle.CircleBrushes[8], -sizeF3.Width / 2f, -sizeF3.Height / 2f);
					break;
				}*/
				case 7:
				case 8:
					{
						SizeF sizeF4 = gfx.MeasureString(s1, this.mFont);
						double num7 = Math.Sin(PI * ((double)v + 0.6) / 180.0) * (double)radius;
						double num8 = Math.Cos(PI * ((double)v + 0.6) / 180.0) * (double)radius;
						gfx.ResetTransform();
						gfx.TranslateTransform((float)this.Center.X + (float)num7, (float)this.Center.Y - (float)num8);
						gfx.RotateTransform((float)((double)v + 0.6 - 90.0));
						gfx.DrawString(s1, this.mFont, this.Options.CircleBrushes[8], -sizeF4.Width / 2f, -sizeF4.Height / 2f);
						break;
					}
			}
		}

		#endregion

		#region Protected methods

		protected override void OnKeyDown(KeyEventArgs e)
		{
			base.OnKeyDown(e);

			e.Handled = false;
			switch (e.KeyCode) {
				case Keys.Subtract:
					this.sRot -= 10;
					break;

				case Keys.Add:
					this.sRot += 10;
					break;

				case Keys.T:
					this.ang += 45f;
					break;

				case Keys.G:
					this.ang -= 45f;
					break;

				case Keys.Space:
					this.mOpt = !this.mOpt;
					break;

				case Keys.Left:
					{
						GEDCOMIndividualRecord father, mother;
						this.fRootPerson.GetParents(out father, out mother);

						if (this.fRootPerson != null && father != null) {
							this.fBack = this.fRootPerson;
							this.fRootPerson = father;
						}
						break;
					}

				case Keys.Right:
					{
						GEDCOMIndividualRecord father, mother;
						this.fRootPerson.GetParents(out father, out mother);

						if (this.fRootPerson != null && mother != null) {
							this.fBack = this.fRootPerson;
							this.fRootPerson = mother;
						}
						break;
					}

				case Keys.Down:
					if (this.fBack != null) {
						this.fRootPerson = this.fBack;
					}
					return;

				default:
					e.Handled = true;
					break;
			}

			if (!e.Handled) this.Invalidate();
		}

		protected override void OnPaint(PaintEventArgs e)
		{
			this.InternalDraw(e.Graphics);
		}

		protected override void OnResize(EventArgs e)
		{
			this.Invalidate();

			base.OnResize(e);
		}

		#endregion
	}
}
