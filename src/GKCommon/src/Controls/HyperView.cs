/*
 *  An HyperText (readonly) Memo for Delphi
 *    from TJumpMemo by Alexander Kuznetsov (sanhome@hotmail.com)
 *  Copyright (C) 1997 Paul Toth (TothPaul@Mygale.org)
 *  http://www.mygale.org/~tothpaul
 *  
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either version 2
 *  of the License, or (at your option) any later version.
 *  
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 *  C# implementation:
 *  Copyright (C) 2011 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
 */

using System;
using System.Collections.Generic;
using System.Drawing;
using System.Windows.Forms;
using System.Security.Permissions;

namespace GKCommon.Controls
{
	public delegate void LinkEventHandler(object sender, string linkName);

	/// <summary>
	/// 
	/// </summary>
	public class HyperView : Panel
	{
		private sealed class HyperLink
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

		//private bool fAcceptFontChange;
		private int fBorderWidth;
		private Color fColor;
		private SolidBrush fDefBrush;
		private int fHeightCount;
		private int[] fHeights;
		private int fLeftPos;
		private readonly StringList fLines;
		private readonly List<HyperLink> fLinks;
		private int fLink;
		private Color fLinkColor;
		private int fPageHeight;
		private int fPageWidth;
		private Point fRange;
		private Font fTextFont;
		private int fTopPos;

		private static readonly object EventLink;

		//private TRuleStyle FRuleStyle;
		//private Color FDwnColor;
		//private Color FUpColor;

		static HyperView()
		{
			HyperView.EventLink = new object();
		}

		public event LinkEventHandler OnLink
		{
			add { base.Events.AddHandler(HyperView.EventLink, value); }
			remove { base.Events.RemoveHandler(HyperView.EventLink, value); }
		}

		public int LeftPos
		{
			get { return this.fLeftPos; }
			set { this.SetLeftPos(value); }
		}

		public int TopPos
		{
			get { return this.fTopPos; }
			set { this.SetTopPos(value); }
		}

		public int BorderWidth
		{
			get { return this.fBorderWidth; }
			set { this.SetBorderWidth(value); }
		}

		public Color Color
		{
			get { return this.fColor; }
			set { this.SetColor(value); }
		}

		public StringList Lines
		{
			get { return this.fLines; }
			set { this.fLines.Assign(value); }
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
			base.DoubleBuffered = true;

			this.fHeightCount = 0;
			//this.fAcceptFontChange = false;
			this.fLines = new StringList();
			this.fLines.OnChange += this.LinesChanged;
			this.fHeights = new int[0];
			this.fTopPos = 0;
			this.fLeftPos = 0;
			this.fColor = SystemColors.Control;
			this.fLinks = new List<HyperLink>();
			this.fLinkColor = Color.Blue;
			this.fLink = -1;
			
			//this.FUpColor = Color.Gray;
			//this.FDwnColor = Color.White;
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				this.ClearLinks();
				//this.FLinks.Dispose();
				this.fHeights = null;
				this.fLines.Dispose();

				if (fDefBrush != null) fDefBrush.Dispose();
				if (fTextFont != null) fTextFont.Dispose();
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

		private void SetBorderWidth(int value)
		{
			if (this.fBorderWidth != value)
			{
				this.fBorderWidth = value;
				base.Invalidate();
			}
		}

		private void SetColor(Color value)
		{
			if (this.fColor != value)
			{
				this.fColor = value;
				base.Invalidate();
			}
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

        private void LinesChanged(object sender)
		{
			this.TopPos = 0;
			this.LeftPos = 0;
			this.ArrangeText();
		}

		private void ArrangeText()
		{
			this.fTextFont = (base.Parent.Font.Clone() as Font);
			this.fDefBrush = new SolidBrush(Color.Black);

			this.PrepareText();
			this.ScrollRange();

			this.Invalidate();
		}

		private void ClearLinks()
		{
			this.fLinks.Clear();
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
				Size str_size = grx.MeasureString(ss, this.fTextFont).ToSize();
				xPos += str_size.Width;

				if (xPos > xMax) xMax = xPos;
				int h = str_size.Height;
				if (h > hMax) hMax = h;
			}
		}

		private void OutText(Graphics grx, string ss, ref int xPos, ref int yPos, ref int hMax)
		{
			if (yPos >= -hMax && ss != "") {
				grx.DrawString(ss, this.fTextFont, this.fDefBrush, (float)xPos, (float)yPos);

				Size str_size = grx.MeasureString(ss, this.fTextFont).ToSize();
				xPos += str_size.Width;
			}
		}

		private void PrepareText()
		{
			this.fHeightCount = this.fLines.Count;
			this.fHeights = new int[this.fHeightCount];

			//this.fAcceptFontChange = false;
			Graphics gfx = base.CreateGraphics();
			try
			{
				this.ClearLinks();

				int y_pos = 0;
				int xMax = 0;

				int num = this.fLines.Count;
				for (int line = 0; line < num; line++)
				{
					int x_pos = 0;
					int line_height = gfx.MeasureString("A", this.fTextFont).ToSize().Height;

					string s = this.fLines[line];

					int i = 1;
					string ss = "";
					while (i <= s.Length)
					{
						if (s[i - 1] == '~')
						{
							if (s[i] == '~') {
								ss += "~";
							}

							this.MeasureText(gfx, ss, ref x_pos, ref y_pos, ref line_height, ref xMax);
							i++;

							while (s[i - 1] != '~')
							{
								char c = char.ToUpper(s[i - 1]);

								switch (c)
								{
									case '+':
										this.SetFontSize(((double)this.fTextFont.Size + (double)this.GetFontSize(s, ref i)));
										break;

									case '-':
										this.SetFontSize(((double)this.fTextFont.Size - (double)this.GetFontSize(s, ref i)));
										break;

									case '0':
										this.fTextFont = (base.Parent.Font.Clone() as Font);
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

											int ss_width = gfx.MeasureString(ss, this.fTextFont).ToSize().Width;
											this.fLinks.Add(new HyperLink(sn, x_pos, y_pos, ss_width, line_height));
											this.MeasureText(gfx, ss, ref x_pos, ref y_pos, ref line_height, ref xMax);

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

					this.MeasureText(gfx, ss, ref x_pos, ref y_pos, ref line_height, ref xMax);
					y_pos += line_height;
					this.fHeights[line] = line_height;
				}

				this.fPageWidth = xMax + 2 * this.fBorderWidth;
				this.fPageHeight = y_pos + 2 * this.fBorderWidth;
			}
			finally
			{
				gfx.Dispose();
				//this.fAcceptFontChange = true;
			}
		}

		private void DoPaint(Graphics gfx)
		{
			if (fHeights.Length != fLines.Count) return;

			//this.fAcceptFontChange = false;
			try
			{
				gfx.FillRectangle(new SolidBrush(SystemColors.Control), base.ClientRectangle);

				int y_pos = this.fBorderWidth - this.fTopPos;

				int num = this.fLines.Count;
				for (int line = 0; line < num; line++)
				{
					int x_pos = this.fBorderWidth - this.fLeftPos;
					int line_height = this.fHeights[line];

					string s = this.fLines[line];

					int i = 1;
					string ss = "";
					while (i <= s.Length)
					{
						if (s[i - 1] == '~')
						{
							if (s[i] == '~') {
								ss += "~";
							}

							this.OutText(gfx, ss, ref x_pos, ref y_pos, ref line_height);
							i++;

							while (s[i - 1] != '~')
							{
								char c = char.ToUpper(s[i - 1]);

								switch (c)
								{
									case '+':
										this.SetFontSize(((double)this.fTextFont.Size + (double)this.GetFontSize(s, ref i)));
										break;

									case '-':
										this.SetFontSize(((double)this.fTextFont.Size - (double)this.GetFontSize(s, ref i)));
										break;

									case '0':
										this.fTextFont = (base.Parent.Font.Clone() as Font);
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

											Color save_color = this.fDefBrush.Color;
											this.fDefBrush.Color = this.fLinkColor;
											this.SetFontStyle(FontStyle.Underline);

											this.OutText(gfx, ss, ref x_pos, ref y_pos, ref line_height);

											this.fDefBrush.Color = save_color;
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

					this.OutText(gfx, ss, ref x_pos, ref y_pos, ref line_height);
					y_pos += line_height;
				}
			}
			finally
			{
				//this.fAcceptFontChange = true;
			}
		}

		private void SetFontSize(double size)
		{
			this.fTextFont = new Font(this.fTextFont.Name, ((float)size), this.fTextFont.Style, this.fTextFont.Unit, this.fTextFont.GdiCharSet, this.fTextFont.GdiVerticalFont);
		}

		private void SetFontStyle(FontStyle style)
		{
			FontStyle fontStyle = this.fTextFont.Style;
			if ((fontStyle & style) == FontStyle.Regular)
			{
				fontStyle |= style;
			}
			else
			{
				fontStyle &= ~style;
			}
			this.fTextFont = new Font(this.fTextFont, fontStyle);
		}

		private void GotoLink(int linkIndex)
		{
			HyperLink hLink = this.fLinks[linkIndex];
			string lnk = "~@" + hLink.Name + "~";
			int h = this.fBorderWidth;

			int num = this.fLines.Count;
			for (int j = 0; j < num; j++) {
				if (this.fLines[j].IndexOf(lnk) >= 0) {
					this.TopPos = h;
					return;
				}

				h += this.fHeights[j];
			}

			this.DoLink(hLink.Name);
		}

		private void DoLink(string linkName)
		{
			LinkEventHandler eventHandler = (LinkEventHandler)base.Events[HyperView.EventLink];
			if (eventHandler == null) return;

			eventHandler(this, linkName);
		}
		
		private void ScrollRange()
		{
			Rectangle CR = base.ClientRectangle;

			if (this.fPageWidth < CR.Width) {
				this.fRange.X = 0;
				this.LeftPos = 0;
			} else {
				this.fRange.X = (this.fPageWidth - CR.Width);
			}

			if (this.fPageHeight < CR.Height) {
				this.fRange.Y = 0;
				this.TopPos = 0;
			} else {
				this.fRange.Y = (this.fPageHeight - CR.Height);
			}

			NativeMethods.SetScrollRange(this.Handle, NativeMethods.SB_HORZ, 0, this.fRange.X, false);
			NativeMethods.SetScrollRange(this.Handle, NativeMethods.SB_VERT, 0, this.fRange.Y, false);
		}

		private void SetLeftPos(int value)
		{
			if (value < 0) value = 0;
			if (value > this.fRange.X) value = this.fRange.X;

			if (this.fLeftPos != value)
			{
				ExtRect dummy = ExtRect.CreateEmpty();
				ExtRect R;
				NativeMethods.ScrollWindowEx(this.Handle, this.fLeftPos - value, 0, ref dummy, ref dummy, 0, out R, 0u);
				NativeMethods.SetScrollPos(this.Handle, 0, this.fLeftPos, true);
				base.Invalidate();
				this.fLeftPos = value;
			}
		}

		private void SetTopPos(int value)
		{
			if (value < 0) value = 0;
			if (value > this.fRange.Y) value = this.fRange.Y;

			if (this.fTopPos != value)
			{
				ExtRect dummy = ExtRect.CreateEmpty();
				ExtRect R;
				NativeMethods.ScrollWindowEx(this.Handle, 0, this.fTopPos - value, ref dummy, ref dummy, 0, out R, 0u);
				NativeMethods.SetScrollPos(this.Handle, 1, this.fTopPos, true);
				base.Invalidate();
				this.fTopPos = value;
			}
		}

		#region Protected methods

		protected override void OnFontChanged(EventArgs e)
		{
			this.ArrangeText();

			base.OnFontChanged(e);
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
					this.TopPos = this.fPageHeight - base.ClientRectangle.Height + 2 * this.fBorderWidth;
					this.LeftPos = this.fPageWidth - base.ClientRectangle.Width;
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

			if (this.fLink >= 0) this.GotoLink(this.fLink);
		}

		protected override void OnMouseMove(MouseEventArgs e)
		{
			base.OnMouseMove(e);

			int y_offset = (this.fBorderWidth - this.fTopPos);
			
			int num = this.fLinks.Count;
			for (int i = 0; i < num; i++)
			{
				if (this.fLinks[i].HasCoord(e.X, e.Y, y_offset))
				{
					this.fLink = i;
					this.Cursor = Cursors.Hand;
					return;
				}
			}

			if (this.fLink >= 0)
			{
				this.Cursor = Cursors.Default;
				this.fLink = -1;
			}
		}

		protected override void OnMouseWheel(MouseEventArgs e)
		{
			base.OnMouseWheel(e);

			if (fHeights.Length > 0)
			{
				if (e.Delta > 0)
				{
					this.TopPos -= fHeights[0];
				}
				else
				{
					this.TopPos += fHeights[0];
				}
			}
		}

		protected override void OnPaint(PaintEventArgs e)
		{
			this.DoPaint(e.Graphics);
		}

        [SecurityPermission(SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.UnmanagedCode), SecurityPermission(SecurityAction.InheritanceDemand, Flags = SecurityPermissionFlag.UnmanagedCode)]
        protected override void WndProc(ref Message m)
		{
			base.WndProc(ref m);

			if (m.Msg == NativeMethods.WM_SIZE)
			{
				this.ScrollRange();
			}
			else if (m.Msg == NativeMethods.WM_GETDLGCODE)
			{
				m.Result = (IntPtr)(m.Result.ToInt32() | NativeMethods.DLGC_WANTARROWS | NativeMethods.DLGC_WANTTAB | NativeMethods.DLGC_WANTCHARS);
			}
			else if (m.Msg == NativeMethods.WM_HSCROLL)
			{
				uint wParam = (uint)m.WParam.ToInt32();
				int new_pos = SysUtils.DoScroll(this.Handle, wParam, 0, this.LeftPos, 0, this.fPageWidth,
				                                base.ClientRectangle.Width / 20, base.ClientRectangle.Width / 2);
				this.SetLeftPos(new_pos);
			}
			else if (m.Msg == NativeMethods.WM_VSCROLL)
			{
				uint wParam = (uint)m.WParam.ToInt32();
				int new_pos = SysUtils.DoScroll(this.Handle, wParam, 1, this.TopPos, 0, this.fPageHeight,
				                                base.ClientRectangle.Height / 20, base.ClientRectangle.Height / 2);
				this.SetTopPos(new_pos);
			}
		}

		#endregion
		
	}
}
