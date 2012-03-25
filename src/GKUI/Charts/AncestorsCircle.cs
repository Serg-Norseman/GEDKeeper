using System;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Windows.Forms;

using GedCom551;
using GKCore;

/// <summary>
/// Localization: clean
/// CodeTransformation: need
/// </summary>

namespace GKUI.Charts
{
	public class AncestorsCircle
	{
		private TGEDCOMTree doc;
		private Graphics mGraphics;
		private int mWidth;
		private int mHeight;
		private int xStart = 0;
		private int yStart = 0;
		private int gen;
		private int g;
		private int sRot = 90;
		private double PI = 3.1415926535897931;
		private Point Center;
		private float ang;
		private bool mOpt = true;
		private Font mFont;
		private AncestorsCircleOptions options;
		private TGEDCOMIndividualRecord mPerson;

		public bool UseFillColor = true;

		public AncestorsCircleOptions CircleStyle
		{
			get { return this.options; }
			set { this.options = value; }
		}

		public AncestorsCircle(TGEDCOMTree adoc, TGEDCOMIndividualRecord p)
		{
			this.doc = adoc;
			this.mPerson = p;
		}

		public void Show(Graphics grap, int width, int height)
		{
			this.mGraphics = grap;
			this.mWidth = width;
			this.mHeight = height;
			this.Prepare();
		}

		private void Prepare()
		{
			Pen pen = new Pen(this.CircleStyle.BrushColor[10]);
			this.gen = 1;
			int num = 430;
			int num2 = 10;
			int num3 = 10;
			this.Center.X = this.mWidth / 2 + this.xStart - 6;
			this.Center.Y = this.mHeight / 2 + this.yStart - 17;
			this.mGraphics.TranslateTransform((float)(this.Center.X + num2), (float)(this.Center.Y + num3));
			int num4;
			if (this.UseFillColor)
			{
				for (int i = 7; i >= 0; i--)
				{
					GraphicsPath graphicsPath = new GraphicsPath();
					num4 = this.sRot + i * 60 + num2;
					graphicsPath.StartFigure();
					graphicsPath.AddEllipse(-num4 - num2, -num4 - num3, 2 * num4, 2 * num4);
					graphicsPath.CloseFigure();
					this.mGraphics.FillPath(this.CircleStyle.CircleBrushes[i], graphicsPath);
					if (this.CircleStyle.CircularLines)
					{
						this.mGraphics.DrawEllipse(pen, -num4 - num2, -num4 - num3, 2 * num4, 2 * num4);
					}
				}
			}
			else
			{
				for (int j = 7; j >= 0; j--)
				{
					num4 = this.sRot + j * 60 + num2;
					if (this.CircleStyle.CircularLines)
					{
						this.mGraphics.DrawEllipse(pen, -num4 - num2, -num4 - num3, 2 * num4, 2 * num4);
					}
				}
			}
			num4 = this.sRot + -60 + num2;
			if (num4 > 0)
			{
				this.mGraphics.DrawArc(pen, -num4 - num2, -num4 - num3, 2 * num4, 2 * num4, -150, 120);
				this.mGraphics.DrawArc(pen, -num4 - num2, -num4 - num3, 2 * num4, 2 * num4, 30, 120);
			}
			this.mGraphics.ResetTransform();
			this.mGraphics.DrawLine(pen, this.Center.X + this.sRot + 10 - 30, this.Center.Y, this.Center.X + num + this.sRot, this.Center.Y);
			this.mGraphics.DrawLine(pen, this.Center.X - this.sRot - 10 + 30, this.Center.Y, this.Center.X - num - this.sRot, this.Center.Y);
			this.mGraphics.DrawLine(pen, this.Center.X, 0, this.Center.X, this.Center.Y - this.sRot - 10);
			this.mGraphics.DrawLine(pen, this.Center.X, this.mHeight, this.Center.X, this.Center.Y + this.sRot + 9);
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
				this.mGraphics.DrawLine(pen, pt, pt2);
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
				this.mGraphics.DrawLine(pen, pt, pt2);
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
				this.mGraphics.DrawLine(pen, pt, pt2);
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
				this.mGraphics.DrawLine(pen, pt, pt2);
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
				this.mGraphics.DrawLine(pen, pt, pt2);
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
				this.mGraphics.DrawLine(pen, pt, pt2);
			}
			this.mFont = new Font("Arial", 10f);
			string text;
			SizeF sizeF;
			if (this.mPerson == null)
			{
				text = "Choose subject";
				sizeF = this.mGraphics.MeasureString(text, this.mFont);
				this.mGraphics.ResetTransform();
				this.mGraphics.TranslateTransform((float)this.Center.X, (float)this.Center.Y);
				this.mGraphics.DrawString(text, this.mFont, this.CircleStyle.CircleBrushes[8], -sizeF.Width / 2f, -sizeF.Height / 2f);
				return;
			}

			string FirstName, LastName, dummy;
			this.mPerson.aux_GetNameParts(out LastName, out FirstName, out dummy);

			text = FirstName;
			sizeF = this.mGraphics.MeasureString(text, this.mFont);
			this.mGraphics.ResetTransform();
			this.mGraphics.TranslateTransform((float)this.Center.X, (float)this.Center.Y);
			this.mGraphics.DrawString(text, this.mFont, this.CircleStyle.CircleBrushes[8], -sizeF.Width / 2f, -sizeF.Height / 2f - sizeF.Height / 2f);
			text = LastName;
			sizeF = this.mGraphics.MeasureString(text, this.mFont);
			this.mGraphics.DrawString(text, this.mFont, this.CircleStyle.CircleBrushes[8], -sizeF.Width / 2f, 0f);

			TGEDCOMIndividualRecord father, mother;
			this.mPerson.aux_GetParents(out father, out mother);

			if (this.mOpt)
			{
				this.g = 1;
				float v = 0f;
				if (father != null)
				{
					this.TraverseAncestors(father, v, this.g, this.sRot, 91);
				}

				this.g = 1;
				v = 180f;
				if (mother != null)
				{
					this.TraverseAncestors(mother, v, this.g, this.sRot, 91);
					return;
				}
			}
			else
			{
				this.g = 1;
				float v2 = 0f;
				if (mother != null)
				{
					this.TraverseAncestors(mother, v2, this.g, this.sRot, 91);
				}

				this.g = 1;
				v2 = 180f;
				if (father != null)
				{
					this.TraverseAncestors(father, v2, this.g, this.sRot, 91);
				}
			}
		}

		private void TraverseAncestors(TGEDCOMIndividualRecord p, float v, int g, int r, int ro)
		{
			string FirstName, LastName, dummy;
			p.aux_GetNameParts(out LastName, out FirstName, out dummy);

			this.ShowAncestorCircle(r, g, v, FirstName, LastName);

			TGEDCOMIndividualRecord father, mother;
			p.aux_GetParents(out father, out mother);

			if (father != null && this.gen < 8)
			{
				this.gen++;
				float num = (float)Math.Abs(360.0 / Math.Pow(2.0, (double)g) - (double)ro);
				v -= num / 2f;
				this.TraverseAncestors(father, v, g + 1, r + 60, ro / 2);
				this.gen--;
			}

			if (mother != null && this.gen < 8)
			{
				this.gen++;
				float num2 = (float)(360.0 / Math.Pow(2.0, (double)g));
				v += num2 / 2f;
				this.TraverseAncestors(mother, v, g + 1, r + 60, ro / 2);
				this.gen--;
			}
		}

		private void ShowAncestorCircle(int radius, int g, float v, string s1, string s2)
		{
			radius -= 20;
			switch (g)
			{
				case 1:
				{
					SizeF sizeF = this.mGraphics.MeasureString(s1, this.mFont);
					double num = Math.Sin(PI * (double)v / 180.0) * (double)radius;
					double num2 = Math.Cos(PI * (double)v / 180.0) * (double)radius;
					this.mGraphics.ResetTransform();
					this.mGraphics.TranslateTransform((float)this.Center.X + (float)num, (float)this.Center.Y - (float)num2);
					this.mGraphics.RotateTransform(v);
					this.mGraphics.DrawString(s1, this.mFont, this.CircleStyle.CircleBrushes[8], -sizeF.Width / 2f, -sizeF.Height / 2f);
					sizeF = this.mGraphics.MeasureString(s2, this.mFont);
					this.mGraphics.DrawString(s2, this.mFont, this.CircleStyle.CircleBrushes[8], -sizeF.Width / 2f, -sizeF.Height / 2f + sizeF.Height);
					break;
				}
				case 2:
				case 3:
				case 4:
				case 5:
				{
					SizeF sizeF2 = this.mGraphics.MeasureString(s1, this.mFont);
					double num3 = Math.Sin(PI * (double)v / 180.0) * (double)radius;
					double num4 = Math.Cos(PI * (double)v / 180.0) * (double)radius;
					this.mGraphics.ResetTransform();
					this.mGraphics.TranslateTransform((float)this.Center.X + (float)num3, (float)this.Center.Y - (float)num4);
					this.mGraphics.RotateTransform(v);
					this.mGraphics.DrawString(s1, this.mFont, this.CircleStyle.CircleBrushes[8], -sizeF2.Width / 2f, -sizeF2.Height / 2f);
					sizeF2 = this.mGraphics.MeasureString(s2, this.mFont);
					num3 = Math.Sin(PI * (double)v / 180.0) * (double)((float)radius - sizeF2.Height);
					num4 = Math.Cos(PI * (double)v / 180.0) * (double)((float)radius - sizeF2.Height);
					this.mGraphics.ResetTransform();
					this.mGraphics.TranslateTransform((float)this.Center.X + (float)num3, (float)this.Center.Y - (float)num4);
					this.mGraphics.RotateTransform(v);
					this.mGraphics.DrawString(s2, this.mFont, this.CircleStyle.CircleBrushes[8], -sizeF2.Width / 2f, -sizeF2.Height / 2f);
					break;
				}
				case 6:
				/*{
					SizeF sizeF3 = this.mGraphics.MeasureString(s1, this.mFont);
					double num5 = Math.Sin(PI * (double)(v - 1f) / 180.0) * (double)radius;
					double num6 = Math.Cos(PI * (double)(v - 1f) / 180.0) * (double)radius;
					this.mGraphics.ResetTransform();
					this.mGraphics.TranslateTransform((float)this.Center.X + (float)num5, (float)this.Center.Y - (float)num6);
					this.mGraphics.RotateTransform(v - 1f - 90f);
					this.mGraphics.DrawString(s1, this.mFont, this.CircleStyle.CircleBrushes[8], -sizeF3.Width / 2f, -sizeF3.Height / 2f);
					sizeF3 = this.mGraphics.MeasureString(s2, this.mFont);
					num5 = Math.Sin(PI * (double)(v + 1f) / 180.0) * (double)radius;
					num6 = Math.Cos(PI * (double)(v + 1f) / 180.0) * (double)radius;
					this.mGraphics.ResetTransform();
					this.mGraphics.TranslateTransform((float)this.Center.X + (float)num5, (float)this.Center.Y - (float)num6);
					this.mGraphics.RotateTransform(v + 1f - 90f);
					this.mGraphics.DrawString(s2, this.mFont, this.CircleStyle.CircleBrushes[8], -sizeF3.Width / 2f, -sizeF3.Height / 2f);
					break;
				}*/
				case 7:
				case 8:
				{
					SizeF sizeF4 = this.mGraphics.MeasureString(s1, this.mFont);
					double num7 = Math.Sin(PI * ((double)v + 0.6) / 180.0) * (double)radius;
					double num8 = Math.Cos(PI * ((double)v + 0.6) / 180.0) * (double)radius;
					this.mGraphics.ResetTransform();
					this.mGraphics.TranslateTransform((float)this.Center.X + (float)num7, (float)this.Center.Y - (float)num8);
					this.mGraphics.RotateTransform((float)((double)v + 0.6 - 90.0));
					this.mGraphics.DrawString(s1, this.mFont, this.CircleStyle.CircleBrushes[8], -sizeF4.Width / 2f, -sizeF4.Height / 2f);
					break;
				}
			}
		}

		public void OnKeyDown(KeyEventArgs e)
		{
			if (e.KeyCode == Keys.Subtract)
			{
				this.sRot -= 10;
				return;
			}
			if (e.KeyCode == Keys.Add)
			{
				this.sRot += 10;
				return;
			}
			if (e.KeyCode == Keys.T)
			{
				this.ang += 45f;
				return;
			}
			if (e.KeyCode == Keys.G)
			{
				this.ang -= 45f;
				return;
			}
			if (e.KeyCode == Keys.Space)
			{
				this.mOpt = !this.mOpt;
				return;
			}

			TGEDCOMIndividualRecord father, mother;
			this.mPerson.aux_GetParents(out father, out mother);

			/*if (e.KeyCode == Keys.Left)
			{
				if (this.mPerson != null && father != null)
				{
					TGEDCOMIndividualRecord person = father;
					person.mBack = this.mPerson;
					this.mPerson = person;
					return;
				}
			}
			else
			{
				if (e.KeyCode == Keys.Right)
				{
					if (this.mPerson != null && mother != null)
					{
						TGEDCOMIndividualRecord person2 = mother;
						person2.mBack = this.mPerson;
						this.mPerson = person2;
						return;
					}
				}
				else
				{
					if (e.KeyCode == Keys.Down)
					{
						if (this.mPerson != null && this.mPerson.mBack != null)
						{
							this.mPerson = this.mPerson.mBack;
							return;
						}
					}
					else
					{
						Keys arg_15E_0 = e.KeyCode;
					}
				}
			}*/
		}

	}
}
