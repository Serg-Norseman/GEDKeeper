using System;
using System.Collections.Generic;
using System.Drawing;
using System.Windows.Forms;

using Ext.Utils;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Controls
{
	public class GKHyperView : Panel, IDisposable
	{
		public delegate void TLinkEvent(object sender, string linkName);

		private class HyperLink
		{
			public string Name;
			public TRect Rect;

			public HyperLink(string aName, int x, int y, int w, int h)
			{
				this.Name = aName;
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

		public enum TRuleStyle : byte
		{
			rsLowered,
			rsRaised
		}

		private bool FAcceptFontChange;
		private int FBorderWidth;
		private Color FColor;
		private SolidBrush FDefBrush;
		private Color FDwnColor;
		private int FHeightCount;
		private int[] FHeights;
		private int FLeftPos;
		private StringList FLines;
		private int FLink;
		private Color FLinkColor;
		private List<HyperLink> FLinks;
		private GKHyperView.TLinkEvent EOnLink;
		private int FPageHeight;
		private int FPageWidth;
		private Point FRange;
		private GKHyperView.TRuleStyle FRuleStyle;
		private Font FTextFont;
		private int FTopPos;
		private Color FUpColor;

		public event GKHyperView.TLinkEvent OnLink
		{
			add { this.EOnLink = value; }
			remove {
				if (this.EOnLink == value)
				{
					this.EOnLink = null;
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

		public GKHyperView.TRuleStyle RuleStyle
		{
			get { return this.FRuleStyle; }
			set { this.SetRuleStyle(value); }
		}

		public GKHyperView()
		{
			base.TabStop = true;
			base.BorderStyle = BorderStyle.Fixed3D;
			this.FHeightCount = 0;
			this.FAcceptFontChange = false;
			this.FLines = new StringList();
			this.FLines.OnChange += new TNotifyEvent(this.LinesChanged);
			this.FHeights = new int[0];
			this.FTopPos = 0;
			this.FLeftPos = 0;
			this.FColor = SystemColors.Control;
			this.FLinks = new List<HyperLink>();
			this.FLinkColor = Color.Blue;
			this.FLink = -1;
			this.FUpColor = Color.Gray;
			this.FDwnColor = Color.White;
		}

		protected override void Dispose(bool Disposing)
		{
			if (Disposing)
			{
				this.ClearLinks();
				//this.FLinks.Dispose();
				this.FHeights = null;
				this.FLines.Free();

                if (FDefBrush != null) FDefBrush.Dispose();
                if (FTextFont != null) FTextFont.Dispose();
			}
			base.Dispose(Disposing);
		}

		private new void FontChanged(object Sender)
		{
			if (this.FAcceptFontChange)
			{
				this.ArrangeText();
				base.Invalidate();
			}
		}

		private void LinesChanged(object Sender)
		{
			this.ArrangeText();
			this.TopPos = 0;
			this.LeftPos = 0;
			base.Invalidate();
		}

		private void SetBorderWidth(int Value)
		{
			if (this.FBorderWidth != Value)
			{
				this.FBorderWidth = Value;
				base.Invalidate();
			}
		}

		private void SetColor(Color Value)
		{
			if (this.FColor != Value)
			{
				this.FColor = Value;
				base.Invalidate();
			}
		}

		private void SetFont(Font Value)
		{
			this.ArrangeText();
			base.Invalidate();
		}

		private void SetLines(StringList Value)
		{
			this.FLines.Assign(Value);
		}

		private void SetRuleStyle(GKHyperView.TRuleStyle Value)
		{
			if (this.FRuleStyle != Value)
			{
				this.FRuleStyle = Value;
				if (this.FRuleStyle == GKHyperView.TRuleStyle.rsRaised)
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
		}

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
			int Result = 0;
			while (true)
			{
				char c = s[i];
				if (c < '0' || c >= ':')
				{
					break;
				}
				i++;
				Result = 10 * Result + (int)s[i - 1] - 48;
			}
			return Result;
		}

		private Color GetFontColor(string s, ref int i)
		{
			string ss = new string(s[i - 1], 1);
			while (s[i] != '~')
			{
				i++;
				ss += s[i - 1];
			}
			return Color.FromName(ss);
		}

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
												this.FLinks.Add(new GKHyperView.HyperLink(sn, x_pos, y_pos, ss_width, line_height));
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

		private void SetFontSize(double ASize)
		{
			this.FTextFont = new Font(this.FTextFont.Name, ((float)ASize), this.FTextFont.Style, this.FTextFont.Unit, this.FTextFont.GdiCharSet, this.FTextFont.GdiVerticalFont);
		}

		private void SetFontStyle(FontStyle AStyle)
		{
			FontStyle fontStyle = this.FTextFont.Style;
			if ((fontStyle & AStyle) == FontStyle.Regular)
			{
				fontStyle |= AStyle;
			}
			else
			{
				fontStyle &= ~AStyle;
			}
			this.FTextFont = new Font(this.FTextFont, fontStyle);
		}

		private void GotoLink(int linkIndex)
		{
			HyperLink hLink = (this.FLinks[linkIndex] as HyperLink);
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

			if (this.EOnLink != null) this.EOnLink(this, hLink.Name);
		}

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

		protected override void OnPaint(PaintEventArgs pe)
		{
			this.DoPaint();
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

            Win32Native.SetScrollRange((uint)this.Handle, 0, 0, this.FRange.X, false);
            Win32Native.SetScrollRange((uint)this.Handle, 1, 0, this.FRange.Y, false);
		}

		private void SetLeftPos(int Value)
		{
			if (Value < 0) Value = 0;
			if (Value > this.FRange.X) Value = this.FRange.X;

			if (this.FLeftPos != Value)
			{
				TRect dummy = TRect.Empty();
				TRect R = TRect.Empty();
                Win32Native.ScrollWindowEx((uint)this.Handle, this.FLeftPos - Value, 0, ref dummy, ref dummy, 0, out R, 0u);
                Win32Native.SetScrollPos((uint)this.Handle, 0, this.FLeftPos, true);
				base.Invalidate();
				this.FLeftPos = Value;
			}
		}

		private void SetTopPos(int Value)
		{
			if (Value < 0) Value = 0;
			if (Value > this.FRange.Y) Value = this.FRange.Y;

			if (this.FTopPos != Value)
			{
				TRect dummy = TRect.Empty();
				TRect R = TRect.Empty();
                Win32Native.ScrollWindowEx((uint)this.Handle, 0, this.FTopPos - Value, ref dummy, ref dummy, 0, out R, 0u);
                Win32Native.SetScrollPos((uint)this.Handle, 1, this.FTopPos, true);
				base.Invalidate();
				this.FTopPos = Value;
			}
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
				int new_pos = SysUtils.DoScroll((uint)this.Handle.ToInt32(), wParam, 0, this.LeftPos, 0, this.FPageWidth,
				                                base.ClientRectangle.Width / 20, base.ClientRectangle.Width / 2);
				this.SetLeftPos(new_pos);
			}
			else if (m.Msg == Win32Native.WM_VSCROLL)
			{
				uint wParam = (uint)m.WParam.ToInt32();
				int new_pos = SysUtils.DoScroll((uint)this.Handle.ToInt32(), wParam, 1, this.TopPos, 0, this.FPageHeight, 
				                                base.ClientRectangle.Height / 20, base.ClientRectangle.Height / 2);
				this.SetTopPos(new_pos);
			}
		}
	}
}
