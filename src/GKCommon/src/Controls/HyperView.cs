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

using BSLib;
using ExtUtils.ScrollableControls;

namespace GKCommon.Controls
{
	public delegate void LinkEventHandler(object sender, string linkName);

	/// <summary>
	/// 
	/// </summary>
	public class HyperView : VirtualScrollableControl
	{
		private sealed class HyperLink
		{
            private readonly ExtRect fRect;

            public readonly string Name;

			public HyperLink(string name, int x, int y, int w, int h)
			{
				this.Name = name;
				this.fRect = ExtRect.CreateBounds(x, y, w, h);
			}

            public bool HasCoord(int x, int y, int xOffset, int yOffset)
			{
                return x >= this.fRect.Left + xOffset && x <= this.fRect.Right + xOffset 
                    && y >= this.fRect.Top + yOffset && y <= this.fRect.Bottom + yOffset;
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
		private int[] fHeights;
		private Size fTextSize;
		private readonly StringList fLines;
		private readonly List<HyperLink> fLinks;
		private int fLink;
		private Color fLinkColor;
		private Font fTextFont;
		
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

        public Color LinkColor
        {
            get { return this.fLinkColor; }
            set { this.fLinkColor = value; }
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
			//base.SetStyle(ControlStyles.OptimizedDoubleBuffer, true); // for some reason it doesn't work

			//this.fAcceptFontChange = false;
			this.fLines = new StringList();
			this.fLines.OnChange += this.LinesChanged;
			this.fHeights = new int[0];
			this.fColor = SystemColors.Control;
			this.fLinks = new List<HyperLink>();
			this.fLinkColor = Color.Blue;
			this.fLink = -1;
			
			this.fTextSize = Size.Empty;
			this.WheelScrollsControl = true;
			
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
            this.ScrollTo(0, 0);
			this.ArrangeText();
		}

		private void ArrangeText()
		{
			this.fTextFont = (base.Parent.Font.Clone() as Font);
			this.fDefBrush = new SolidBrush(Color.Black);

			this.PrepareText();
			this.AdjustViewPort();

			this.Invalidate();
		}

		private void AdjustViewPort()
		{
			if (this.AutoScroll && !this.fTextSize.IsEmpty)
				this.AutoScrollMinSize = new Size(this.fTextSize.Width + this.Padding.Horizontal, this.fTextSize.Height + this.Padding.Vertical);
		}

		private void ClearLinks()
		{
			this.fLinks.Clear();
		}

		private static int GetFontSize(string s, ref int i)
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
				Size strSize = grx.MeasureString(ss, this.fTextFont).ToSize();
				xPos += strSize.Width;

				if (xPos > xMax) xMax = xPos;
				int h = strSize.Height;
				if (h > hMax) hMax = h;
			}
		}

		private void OutText(Graphics gfx, string ss, ref int xPos, ref int yPos, ref int hMax)
		{
			if (yPos >= -hMax && ss != "") {
				gfx.DrawString(ss, this.fTextFont, this.fDefBrush, xPos, yPos);

				Size strSize = gfx.MeasureString(ss, this.fTextFont).ToSize();
				xPos += strSize.Width;
			}
		}

		private void PrepareText()
		{
			this.fHeights = new int[this.fLines.Count];
			
			//this.fAcceptFontChange = false;
			Graphics gfx = base.CreateGraphics();
			try
			{
				this.ClearLinks();

				int yPos = 0;
				int xMax = 0;

				int num = this.fLines.Count;
				for (int line = 0; line < num; line++)
				{
					int xPos = 0;
					int lineHeight = gfx.MeasureString("A", this.fTextFont).ToSize().Height;

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

							this.MeasureText(gfx, ss, ref xPos, ref yPos, ref lineHeight, ref xMax);
							i++;

							while (s[i - 1] != '~')
							{
								char c = char.ToUpper(s[i - 1]);

								switch (c)
								{
									case '+':
										this.SetFontSize((this.fTextFont.Size + GetFontSize(s, ref i)));
										break;

									case '-':
                                        this.SetFontSize((this.fTextFont.Size - GetFontSize(s, ref i)));
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

											int ssWidth = gfx.MeasureString(ss, this.fTextFont).ToSize().Width;
											this.fLinks.Add(new HyperLink(sn, xPos, yPos, ssWidth, lineHeight));
											this.MeasureText(gfx, ss, ref xPos, ref yPos, ref lineHeight, ref xMax);

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

					this.MeasureText(gfx, ss, ref xPos, ref yPos, ref lineHeight, ref xMax);
					yPos += lineHeight;
					this.fHeights[line] = lineHeight;
				}

				int textWidth = xMax + 2 * this.fBorderWidth;
				int textHeight = yPos + 2 * this.fBorderWidth;
				this.fTextSize = new Size(textWidth, textHeight);
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

				int yOffset = this.fBorderWidth - -this.AutoScrollPosition.Y;

				int num = this.fLines.Count;
				for (int line = 0; line < num; line++)
				{
                    int xOffset = this.fBorderWidth - -this.AutoScrollPosition.X;
                    int lineHeight = this.fHeights[line];

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

							this.OutText(gfx, ss, ref xOffset, ref yOffset, ref lineHeight);
							i++;

							while (s[i - 1] != '~')
							{
								char c = char.ToUpper(s[i - 1]);

								switch (c)
								{
									case '+':
                                        this.SetFontSize((this.fTextFont.Size + GetFontSize(s, ref i)));
										break;

									case '-':
                                        this.SetFontSize((this.fTextFont.Size - GetFontSize(s, ref i)));
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

											Color saveColor = this.fDefBrush.Color;
											this.fDefBrush.Color = this.fLinkColor;
											this.SetFontStyle(FontStyle.Underline);

											this.OutText(gfx, ss, ref xOffset, ref yOffset, ref lineHeight);

											this.fDefBrush.Color = saveColor;
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

					this.OutText(gfx, ss, ref xOffset, ref yOffset, ref lineHeight);
					yOffset += lineHeight;
				}
			}
			finally
			{
				//this.fAcceptFontChange = true;
			}
		}

        private void SetFontSize(float size)
		{
			this.fTextFont = new Font(this.fTextFont.Name, size, this.fTextFont.Style, this.fTextFont.Unit, this.fTextFont.GdiCharSet, this.fTextFont.GdiVerticalFont);
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
			        this.ScrollTo(0, h);
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

		private void AdjustScroll(int x, int y)
		{
			this.AutoScrollPosition = new Point(this.HorizontalScroll.Value + x, this.VerticalScroll.Value + y);

			this.Invalidate();

			this.OnScroll(new ScrollEventArgs(ScrollEventType.EndScroll, 0));
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
					this.AdjustScroll(0, -this.VerticalScroll.LargeChange);
					break;

				case Keys.Next:
					this.AdjustScroll(0, this.VerticalScroll.LargeChange);
					break;

				case Keys.Home:
					this.AdjustScroll(-this.HorizontalScroll.Maximum, -this.VerticalScroll.Maximum);
					break;

				case Keys.End:
					this.AdjustScroll(-this.HorizontalScroll.Maximum, this.VerticalScroll.Maximum);
					break;

				case Keys.Left:
					this.AdjustScroll(-(e.Modifiers == Keys.None ? this.HorizontalScroll.SmallChange : this.HorizontalScroll.LargeChange), 0);
					break;

				case Keys.Right:
					this.AdjustScroll(e.Modifiers == Keys.None ? this.HorizontalScroll.SmallChange : this.HorizontalScroll.LargeChange, 0);
					break;

				case Keys.Up:
					this.AdjustScroll(0, -(e.Modifiers == Keys.None ? this.VerticalScroll.SmallChange : this.VerticalScroll.LargeChange));
					break;

				case Keys.Down:
					this.AdjustScroll(0, e.Modifiers == Keys.None ? this.VerticalScroll.SmallChange : this.VerticalScroll.LargeChange);
					break;
			}
		}

		protected override bool IsInputKey(Keys keyData)
		{
			bool result;

			if ((keyData & Keys.Right) == Keys.Right | (keyData & Keys.Left) == Keys.Left | (keyData & Keys.Up) == Keys.Up | (keyData & Keys.Down) == Keys.Down
			   | (keyData & Keys.Prior) == Keys.Prior | (keyData & Keys.Next) == Keys.Next | (keyData & Keys.End) == Keys.End | (keyData & Keys.Home) == Keys.Home)
				result = true;
			else
				result = base.IsInputKey(keyData);

			return result;
		}

		protected override void OnMouseDown(MouseEventArgs e)
		{
			base.OnMouseDown(e);

			if (this.fLink >= 0) this.GotoLink(this.fLink);
		}

		protected override void OnMouseMove(MouseEventArgs e)
		{
			base.OnMouseMove(e);

			int yOffset = (this.fBorderWidth - -this.AutoScrollPosition.Y);
            int xOffset = (this.fBorderWidth - -this.AutoScrollPosition.X);
			
			int num = this.fLinks.Count;
			for (int i = 0; i < num; i++)
			{
                if (this.fLinks[i].HasCoord(e.X, e.Y, xOffset, yOffset))
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

		protected override void OnPaint(PaintEventArgs e)
		{
			this.DoPaint(e.Graphics);
		}

		#endregion
		
	}
}
