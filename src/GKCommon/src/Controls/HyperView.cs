using System;
using System.Collections.Generic;
using System.Drawing;
using System.Windows.Forms;

namespace GKCommon.Controls
{
    /// <summary>
    /// 
    /// </summary>
    public class HyperView : Panel
	{
		public delegate void LinkEventHandler(object sender, string linkName);

		private class HyperLink
		{
			public readonly string Name;
            public readonly ExtRect Rect;

			public HyperLink(string name, int x, int y, int w, int h)
			{
				this.Name = name;
				this.Rect.Left = x;
				this.Rect.Top = y;
				this.Rect.Right = x + w;
				this.Rect.Bottom = y + h;
			}

			public bool HasCoord(int x, int y, int y_offset)
			{
				return x >= this.Rect.Left && x <= this.Rect.Right && y >= this.Rect.Top + y_offset && y <= this.Rect.Bottom + y_offset;
			}
		}

		/*public enum TRuleStyle
		{
			rsLowered,
			rsRaised
		}*/

		private bool FAcceptFontChange;
		private int FBorderWidth;
		private Color FColor;
		private SolidBrush FDefBrush;
		private int FHeightCount;
		private int[] FHeights;
		private int FLeftPos;
		private readonly StringList FLines;
        private readonly List<HyperLink> FLinks;
        private int FLink;
		private Color FLinkColor;
		private LinkEventHandler fOnLink;
		private int FPageHeight;
		private int FPageWidth;
		private Point FRange;
		private Font FTextFont;
		private int FTopPos;

		//private TRuleStyle FRuleStyle;
		//private Color FDwnColor;
		//private Color FUpColor;

		public event LinkEventHandler OnLink
		{
			add { this.fOnLink = value; }
			remove {
				if (this.fOnLink == value)
				{
					this.fOnLink = null;
				}
			}
		}

		public int LeftPos
		{
			get { return this.FLeftPos; }
			set { this.SetLeftPos(value); }
		}

		public int TopPos
		{
			get { return this.FTopPos; }
			set { this.SetTopPos(value); }
		}

		public int BorderWidth
		{
			get { return this.FBorderWidth; }
			set { this.SetBorderWidth(value); }
		}

		public Color Color
		{
			get { return this.FColor; }
			set { this.SetColor(value); }
		}

		public StringList Lines
		{
			get { return this.FLines; }
			set { this.SetLines(value); }
		}

		/*public TRuleStyle RuleStyle
		{
			get { return this.FRuleStyle; }
			set { this.SetRuleStyle(value); }
		}*/

		public HyperView()
		{
			base.TabStop = true;
			base.BorderStyle = BorderStyle.Fixed3D;

			this.FHeightCount = 0;
			this.FAcceptFontChange = false;
			this.FLines = new StringList();
			this.FLines.OnChange += this.LinesChanged;
			this.FHeights = new int[0];
			this.FTopPos = 0;
			this.FLeftPos = 0;
			this.FColor = SystemColors.Control;
			this.FLinks = new List<HyperLink>();
			this.FLinkColor = Color.Blue;
			this.FLink = -1;
			
			//this.FUpColor = Color.Gray;
			//this.FDwnColor = Color.White;
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				this.ClearLinks();
				//this.FLinks.Dispose();
				this.FHeights = null;
                this.FLines.Dispose();

                if (FDefBrush != null) FDefBrush.Dispose();
                if (FTextFont != null) FTextFont.Dispose();
			}
			base.Dispose(disposing);
		}

		/*private new void FontChanged(object sender)
		{
			if (this.FAcceptFontChange)
			{
				this.ArrangeText();
				base.Invalidate();
			}
		}*/

		private void LinesChanged(object sender)
		{
			this.ArrangeText();
			this.TopPos = 0;
			this.LeftPos = 0;
			base.Invalidate();
		}

		private void SetBorderWidth(int value)
		{
			if (this.FBorderWidth != value)
			{
				this.FBorderWidth = value;
				base.Invalidate();
			}
		}

		private void SetColor(Color value)
		{
			if (this.FColor != value)
			{
				this.FColor = value;
				base.Invalidate();
			}
		}

		/*private void SetFont(Font value)
		{
			this.ArrangeText();
			base.Invalidate();
		}*/

		private void SetLines(StringList value)
		{
			this.FLines.Assign(value);
		}

		/*private void SetRuleStyle(TRuleStyle value)
		{
			if (this.FRuleStyle != value)
			{
				this.FRuleStyle = value;
				if (this.FRuleStyle == TRuleStyle.rsRaised)
				{
					this.FUpColor = Color.White;
					this.FDwnColor = Color.Gray;
				}
				else
				{
					this.FUpColor = Color.Gray;
					this.FDwnColor = Color.White;
				}
				base.Invalidate();
			}
		}*/

		private void ArrangeText()
		{
			this.FTextFont = (base.Parent.Font.Clone() as Font);
			this.FDefBrush = new SolidBrush(Color.Black);

			this.PrepareText();
			this.DoPaint();

			this.ScrollRange();
		}

		private void ClearLinks()
		{
			this.FLinks.Clear();
		}

		private int GetFontSize(string s, ref int i)
		{
			int result = 0;

			while (true)
			{
				char c = s[i];
				if (c < '0' || c > '9') break;

				i++;
				result = 10 * result + (int)s[i - 1] - 48;
			}

			return result;
		}

		/*private Color GetFontColor(string s, ref int i)
		{
			string ss = new string(s[i - 1], 1);
			while (s[i] != '~')
			{
				i++;
				ss += s[i - 1];
			}
			return Color.FromName(ss);
		}*/

		private void MeasureText(Graphics grx, string ss, ref int xPos, ref int yPos, ref int hMax, ref int xMax)
		{
			if (yPos >= -hMax && ss != "") {
				Size str_size = grx.MeasureString(ss, this.FTextFont).ToSize();
				xPos += str_size.Width;

				if (xPos > xMax) xMax = xPos;
				int h = str_size.Height;
				if (h > hMax) hMax = h;
			}
		}

		private void OutText(Graphics grx, string ss, ref int xPos, ref int yPos, ref int hMax)
		{
			if (yPos >= -hMax && ss != "") {
				grx.DrawString(ss, this.FTextFont, this.FDefBrush, (float)xPos, (float)yPos);

				Size str_size = grx.MeasureString(ss, this.FTextFont).ToSize();
				xPos += str_size.Width;
			}
		}

		private void PrepareText()
		{
			this.FHeightCount = this.FLines.Count;
			this.FHeights = new int[this.FHeightCount];

			this.FAcceptFontChange = false;
			Graphics grx = base.CreateGraphics();
			try
			{
				this.ClearLinks();

				int y_pos = 0;
				int xMax = 0;

				int num = this.FLines.Count - 1;
				for (int Line = 0; Line <= num; Line++)
				{
						int x_pos = 0;
						int line_height = grx.MeasureString("A", this.FTextFont).ToSize().Height;

						string s = this.FLines[Line];

						int i = 1;
						string ss = "";
						while (i <= s.Length)
						{
							if (s[i - 1] == '~')
							{
								if (s[i] == '~') {
									ss += "~";
								}

								this.MeasureText(grx, ss, ref x_pos, ref y_pos, ref line_height, ref xMax);
								i++;

								while (s[i - 1] != '~')
								{
									char c = char.ToUpper(s[i - 1]);

									switch (c)
									{
										case '+':
											this.SetFontSize(((double)this.FTextFont.Size + (double)this.GetFontSize(s, ref i)));
											break;

										case '-':
											this.SetFontSize(((double)this.FTextFont.Size - (double)this.GetFontSize(s, ref i)));
											break;

										case '0':
											this.FTextFont = (base.Parent.Font.Clone() as Font);
											break;

										case 'B':
											this.SetFontStyle(FontStyle.Bold);
											break;

										case 'I':
											this.SetFontStyle(FontStyle.Italic);
											break;

										case 'R':
											// dummy
											break;

										case 'S':
											this.SetFontStyle(FontStyle.Strikeout);
											break;

										case 'U':
											this.SetFontStyle(FontStyle.Underline);
											break;

										case '^':
											{
												string sn = "";
												while (s[i] != ':') {
													i++;
													sn += s[i - 1];
												}
												i++;
												ss = "";
												while (s[i] != '~') {
													i++;
													ss += s[i - 1];
												}

												int ss_width = grx.MeasureString(ss, this.FTextFont).ToSize().Width;
												this.FLinks.Add(new HyperLink(sn, x_pos, y_pos, ss_width, line_height));
												this.MeasureText(grx, ss, ref x_pos, ref y_pos, ref line_height, ref xMax);

												break;
											}

										default:
											while (s[i] != '~') i++;
											break;
									}

									i++;
								}
								ss = "";
							}
							else
							{
								ss += s[i - 1];
							}

							i++;
						}

						this.MeasureText(grx, ss, ref x_pos, ref y_pos, ref line_height, ref xMax);
						y_pos += line_height;
						this.FHeights[Line] = line_height;
				}

				this.FPageWidth = xMax + 2 * this.FBorderWidth;
				this.FPageHeight = y_pos + 2 * this.FBorderWidth;
			}
			finally
			{
				grx.Dispose();
				this.FAcceptFontChange = true;
			}
		}

		private void DoPaint()
		{
			if (FHeights.Length != FLines.Count) return;

			this.FAcceptFontChange = false;
			Graphics grx = base.CreateGraphics();
			try
			{
				grx.FillRectangle(new SolidBrush(SystemColors.Control), base.ClientRectangle);

				int y_pos = this.FBorderWidth - this.FTopPos;

				int num = this.FLines.Count - 1;
				for (int Line = 0; Line <= num; Line++)
				{
						int x_pos = this.FBorderWidth - this.FLeftPos;
						int line_height = this.FHeights[Line];

						string s = this.FLines[Line];

						int i = 1;
						string ss = "";
						while (i <= s.Length)
						{
							if (s[i - 1] == '~')
							{
								if (s[i] == '~') {
									ss += "~";
								}

								this.OutText(grx, ss, ref x_pos, ref y_pos, ref line_height);
								i++;

								while (s[i - 1] != '~')
								{
									char c = char.ToUpper(s[i - 1]);

									switch (c)
									{
										case '+':
											this.SetFontSize(((double)this.FTextFont.Size + (double)this.GetFontSize(s, ref i)));
											break;

										case '-':
											this.SetFontSize(((double)this.FTextFont.Size - (double)this.GetFontSize(s, ref i)));
											break;

										case '0':
											this.FTextFont = (base.Parent.Font.Clone() as Font);
											break;

										case 'B':
											this.SetFontStyle(FontStyle.Bold);
											break;

										case 'I':
											this.SetFontStyle(FontStyle.Italic);
											break;

										case 'R':
											// need to realize
											break;

										case 'S':
											this.SetFontStyle(FontStyle.Strikeout);
											break;

										case 'U':
											this.SetFontStyle(FontStyle.Underline);
											break;

										case '^':
											{
												string sn = "";
												while (s[i] != ':') {
													i++;
													sn += s[i - 1];
												}
												i++;
												ss = "";
												while (s[i] != '~') {
													i++;
													ss += s[i - 1];
												}

												Color save_color = this.FDefBrush.Color;
												this.FDefBrush.Color = this.FLinkColor;
												this.SetFontStyle(FontStyle.Underline);

												this.OutText(grx, ss, ref x_pos, ref y_pos, ref line_height);

												this.FDefBrush.Color = save_color;
												this.SetFontStyle(FontStyle.Underline);

												break;
											}

										default:
											while (s[i] != '~') i++;
											break;
									}

									i++;
								}
								ss = "";
							}
							else
							{
								ss += s[i - 1];
							}

							i++;
						}

						this.OutText(grx, ss, ref x_pos, ref y_pos, ref line_height);
						y_pos += line_height;
				}
			}
			finally
			{
				grx.Dispose();
				this.FAcceptFontChange = true;
			}
		}

		private void SetFontSize(double size)
		{
			this.FTextFont = new Font(this.FTextFont.Name, ((float)size), this.FTextFont.Style, this.FTextFont.Unit, this.FTextFont.GdiCharSet, this.FTextFont.GdiVerticalFont);
		}

		private void SetFontStyle(FontStyle style)
		{
			FontStyle fontStyle = this.FTextFont.Style;
			if ((fontStyle & style) == FontStyle.Regular)
			{
				fontStyle |= style;
			}
			else
			{
				fontStyle &= ~style;
			}
			this.FTextFont = new Font(this.FTextFont, fontStyle);
		}

		private void GotoLink(int linkIndex)
		{
			HyperLink hLink = this.FLinks[linkIndex];
			string lnk = "~@" + hLink.Name + "~";
			int h = this.FBorderWidth;

			int num = this.FLines.Count - 1;
			for (int j = 0; j <= num; j++)
			{
				if (this.FLines[j].IndexOf(lnk) >= 0)
				{
					this.TopPos = h;
					return;
				}

				h += this.FHeights[j];
			}

			if (this.fOnLink != null) this.fOnLink(this, hLink.Name);
		}

		private void ScrollRange()
		{
			Rectangle CR = base.ClientRectangle;

			if (this.FPageWidth < CR.Width) {
				this.FRange.X = 0;
				this.LeftPos = 0;
			} else {
				this.FRange.X = (this.FPageWidth - CR.Width);
			}

			if (this.FPageHeight < CR.Height) {
				this.FRange.Y = 0;
				this.TopPos = 0;
			} else {
				this.FRange.Y = (this.FPageHeight - CR.Height);
			}

            Win32Native.SetScrollRange(this.Handle, Win32Native.SB_HORZ, 0, this.FRange.X, false);
            Win32Native.SetScrollRange(this.Handle, Win32Native.SB_VERT, 0, this.FRange.Y, false);
		}

		private void SetLeftPos(int value)
		{
			if (value < 0) value = 0;
			if (value > this.FRange.X) value = this.FRange.X;

			if (this.FLeftPos != value)
			{
				ExtRect dummy = ExtRect.Empty();
				ExtRect R;
                Win32Native.ScrollWindowEx(this.Handle, this.FLeftPos - value, 0, ref dummy, ref dummy, 0, out R, 0u);
                Win32Native.SetScrollPos(this.Handle, 0, this.FLeftPos, true);
				base.Invalidate();
				this.FLeftPos = value;
			}
		}

		private void SetTopPos(int value)
		{
			if (value < 0) value = 0;
			if (value > this.FRange.Y) value = this.FRange.Y;

			if (this.FTopPos != value)
			{
				ExtRect dummy = ExtRect.Empty();
				ExtRect R;
                Win32Native.ScrollWindowEx(this.Handle, 0, this.FTopPos - value, ref dummy, ref dummy, 0, out R, 0u);
                Win32Native.SetScrollPos(this.Handle, 1, this.FTopPos, true);
				base.Invalidate();
				this.FTopPos = value;
			}
		}

		#region Protected methods

		protected override void OnKeyDown(KeyEventArgs e)
		{
			base.OnKeyDown(e);
			switch (e.KeyCode)
			{
				case Keys.Prior:
					this.TopPos -= base.ClientRectangle.Height / 2;
					break;

				case Keys.Next:
					this.TopPos += base.ClientRectangle.Height / 2;
					break;

				case Keys.End:
					this.TopPos = this.FPageHeight - base.ClientRectangle.Height + 2 * this.FBorderWidth;
					this.LeftPos = this.FPageWidth - base.ClientRectangle.Width;
					break;

				case Keys.Home:
					this.TopPos = 0;
					this.LeftPos = 0;
					break;

				case Keys.Left:
					this.LeftPos -= base.ClientRectangle.Width / 20;
					break;

				case Keys.Up:
					this.TopPos -= base.ClientRectangle.Height / 20;
					break;

				case Keys.Right:
					this.LeftPos += base.ClientRectangle.Width / 20;
					break;

				case Keys.Down:
					this.TopPos += base.ClientRectangle.Height / 20;
					break;
			}
		}

		protected override void OnMouseDown(MouseEventArgs e)
		{
			base.OnMouseDown(e);

			if (!this.Focused) base.Focus();

			if (this.FLink >= 0) this.GotoLink(this.FLink);
		}

		protected override void OnMouseMove(MouseEventArgs e)
		{
			base.OnMouseMove(e);

			int y_offset = (this.FBorderWidth - this.FTopPos);
			
			int num = this.FLinks.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				if (this.FLinks[i].HasCoord(e.X, e.Y, y_offset))
				{
					this.FLink = i;
					this.Cursor = Cursors.Hand;
					return;
				}
			}

			if (this.FLink >= 0)
			{
				this.Cursor = Cursors.Default;
				this.FLink = -1;
			}
		}

		protected override void OnMouseWheel(MouseEventArgs e)
		{
			base.OnMouseWheel(e);

			if (FHeights.Length > 0)
			{
				if (e.Delta > 0)
				{
					this.TopPos -= FHeights[0];
				}
				else
				{
					this.TopPos += FHeights[0];
				}
			}
		}

		protected override void OnPaint(PaintEventArgs e)
		{
			this.DoPaint();
		}

		protected override void WndProc(ref Message m)
		{
			base.WndProc(ref m);

			if (m.Msg == Win32Native.WM_SIZE)
			{
				this.ScrollRange();
			}
			else if (m.Msg == Win32Native.WM_GETDLGCODE)
			{
				m.Result = (IntPtr)(m.Result.ToInt32() | Win32Native.DLGC_WANTARROWS | Win32Native.DLGC_WANTTAB | Win32Native.DLGC_WANTCHARS);
			}
			else if (m.Msg == Win32Native.WM_HSCROLL)
			{
				uint wParam = (uint)m.WParam.ToInt32();
				int new_pos = SysUtils.DoScroll(this.Handle, wParam, 0, this.LeftPos, 0, this.FPageWidth,
				                                base.ClientRectangle.Width / 20, base.ClientRectangle.Width / 2);
				this.SetLeftPos(new_pos);
			}
			else if (m.Msg == Win32Native.WM_VSCROLL)
			{
				uint wParam = (uint)m.WParam.ToInt32();
				int new_pos = SysUtils.DoScroll(this.Handle, wParam, 1, this.TopPos, 0, this.FPageHeight, 
				                                base.ClientRectangle.Height / 20, base.ClientRectangle.Height / 2);
				this.SetTopPos(new_pos);
			}
		}

		#endregion
		
	}
}
