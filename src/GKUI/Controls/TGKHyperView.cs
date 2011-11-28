using System;
using System.Drawing;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Windows.Forms;

using GKCore.Sys;

namespace GKUI.Controls
{
	public class TGKHyperView : Panel, IDisposable
	{
		public delegate void TLinkEvent(object Sender, string LinkName);

		private class TLink
		{
			public string Name;
			public TRect Rect;

			public TLink(string Aname, int x, int y, int w, int h)
			{
				this.Name = Aname;
				this.Rect.Left = x;
				this.Rect.Top = y;
				this.Rect.Right = x + w;
				this.Rect.Bottom = y + h;
			}

			public bool GotMouse(int x, int y)
			{
				return x >= this.Rect.Left && x <= this.Rect.Right && y >= this.Rect.Top && y <= this.Rect.Bottom;
			}

			public void Free()
			{
				TObjectHelper.Free(this);
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
		private Font FFont;
		private ushort FHeightCount;
		private int[] FHeights;
		private int FLeftPos;
		private TStringList FLines;
		private int FLink;
		private Color FLinkColor;
		private TList FLinks;
		private TGKHyperView.TLinkEvent EOnLink;
		private int FPageHeight;
		private int FPageWidth;
		private Point FRange;
		private TGKHyperView.TRuleStyle FRuleStyle;
		private Font FTextFont;
		private int FTopPos;
		private Color FUpColor;

		public event TGKHyperView.TLinkEvent OnLink
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

		public new Font Font
		{
			get { return this.FFont; }
			set { this.SetFont(value); }
		}

		public TStringList Lines
		{
			get { return this.FLines; }
			set { this.SetLines(value); }
		}

		public TGKHyperView.TRuleStyle RuleStyle
		{
			get { return this.FRuleStyle; }
			set { this.SetRuleStyle(value); }
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
			if ((int)this.FHeightCount != this.FLines.Count)
			{
				this.FHeightCount = (ushort)this.FLines.Count;
				this.FHeights = new int[this.FHeightCount];
			}

			this.ArrangeText();
			this.TopPos = 0;
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

		private void SetLines(TStringList Value)
		{
			this.FLines.Assign(Value);
		}

		private void SetRuleStyle(TGKHyperView.TRuleStyle Value)
		{
			if (this.FRuleStyle != Value)
			{
				this.FRuleStyle = Value;
				if (this.FRuleStyle == TGKHyperView.TRuleStyle.rsRaised)
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

		private int GetHeight(Graphics grx, string s)
		{
			return grx.MeasureString(s, this.FTextFont).ToSize().Height;
		}

		private int GetWidth(Graphics grx, string s)
		{
			return grx.MeasureString(s, this.FTextFont).ToSize().Width;
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

		private void OutText(Graphics grx, string ss, bool resize, ref int x, ref int y, ref int hMax, ref int xMax)
		{
			if (y >= -hMax && ss != "")
			{
				if (!resize)
				{
					grx.DrawString(ss, this.FTextFont, this.FDefBrush, (float)x, (float)y);
				}

				Size s_sizes = grx.MeasureString(ss, this.FTextFont).ToSize();

				x += s_sizes.Width;

				if (resize)
				{
					if (x > xMax) xMax = x;
					int h = s_sizes.Height;
					if (h > hMax) hMax = h;
				}
			}
		}

		private void DoPaint(bool aResize)
		{
			if (FHeights.Length != FLines.Count) return;

			Graphics grx = base.CreateGraphics();
			try
			{
				this.FAcceptFontChange = false;
				this.ClearLinks();
				if (!aResize)
				{
					grx.FillRectangle(new SolidBrush(SystemColors.Control), base.ClientRectangle);
				}

				int y;
				int xMax = 0;

				if (aResize) {
					y = 0;
					xMax = 0;
				} else {
					y = this.FBorderWidth - this.FTopPos;
				}

				int num = this.FLines.Count - 1;
				int Line = 0;
				if (num >= Line)
				{
					num++;
					while (true)
					{
						int x;
						int hMax;
						if (aResize)
						{
							x = 0;
							hMax = this.GetHeight(grx, "A");
						}
						else
						{
							x = this.FBorderWidth - this.FLeftPos;
							hMax = this.FHeights[Line];
						}

						string s = this.FLines[Line];
						int i = 1;
						string ss = "";
						while (i <= ((s != null) ? s.Length : 0))
						{
							if (s[i - 1] == '~')
							{
								if (s[i] == '~')
								{
									ss += "~";
								}
								this.OutText(grx, ss, aResize, ref x, ref y, ref hMax, ref xMax);
								i++;

								while (s[i - 1] != '~')
								{
									char c = char.ToUpper(s[i - 1]);
									if (c != '+')
									{
										if (c != '-')
										{
											if (c != '0')
											{
												if (c != 'B')
												{
													if (c != 'I')
													{
														if (c != 'R')
														{
															if (c != 'S')
															{
																if (c != 'U')
																{
																	if (c != '^')
																	{
																		while (s[i] != '~')
																		{
																			i++;
																		}
																	}
																	else
																	{
																		string sn = "";
																		while (s[i] != ':')
																		{
																			i++;
																			sn += s[i - 1];
																		}
																		i++;
																		ss = "";
																		while (s[i] != '~')
																		{
																			i++;
																			ss += s[i - 1];
																		}
																		Color SaveColor = this.FDefBrush.Color;
																		this.FDefBrush.Color = this.FLinkColor;
																		this.SetFontStyle(FontStyle.Underline);
																		if (!aResize)
																		{
																			this.FLinks.Add(new TGKHyperView.TLink(sn, x, y, this.GetWidth(grx, ss), hMax));
																		}
																		this.OutText(grx, ss, aResize, ref x, ref y, ref hMax, ref xMax);
																		this.FDefBrush.Color = SaveColor;
																		this.SetFontStyle(FontStyle.Underline);
																	}
																}
																else
																{
																	this.SetFontStyle(FontStyle.Underline);
																}
															}
															else
															{
																this.SetFontStyle(FontStyle.Strikeout);
															}
														}
														else
														{
															if (!aResize)
															{
																goto Block_21;
															}
														}
													}
													else
													{
														this.SetFontStyle(FontStyle.Italic);
													}
												}
												else
												{
													this.SetFontStyle(FontStyle.Bold);
												}
											}
											else
											{
												this.FTextFont = (base.Parent.Font.Clone() as Font);
											}
										}
										else
										{
											this.SetFontSize(((double)this.FTextFont.Size - (double)this.GetFontSize(s, ref i)));
										}
									}
									else
									{
										this.SetFontSize(((double)this.FTextFont.Size + (double)this.GetFontSize(s, ref i)));
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

						this.OutText(grx, ss, aResize, ref x, ref y, ref hMax, ref xMax);

						y += hMax;

						if (aResize)
						{
							this.FHeights[Line] = hMax;
						}

						Line++;
						if (Line == num)
						{
							goto IL_359;
						}
					}
					Block_21:
					throw new Exception("need to realize");
				}

				IL_359:

				if (aResize)
				{
					this.FPageWidth = xMax + 2 * this.FBorderWidth;
					this.FPageHeight = y + 2 * this.FBorderWidth;
				}

				this.FAcceptFontChange = true;
			}
			finally
			{
				grx.Dispose();
			}
		}

		private void ArrangeText()
		{
			this.FTextFont = (base.Parent.Font.Clone() as Font);
			this.FDefBrush = new SolidBrush(Color.Black);
			this.DoPaint(true);
			this.DoPaint(false);
			this.ScrollRange();
		}

		private void ClearLinks()
		{
			for (int i = 0; i <= this.FLinks.Count - 1; i++)
			{
				(this.FLinks[i] as TGKHyperView.TLink).Free();
			}
			this.FLinks.Clear();
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

		private void GotoLink(ushort Link)
		{
			string i = "~@" + (this.FLinks[(int)Link] as TGKHyperView.TLink).Name + "~";
			int h = this.FBorderWidth;

			int num = this.FLines.Count - 1;
			int j = 0;
			if (num >= j)
			{
				num++;
				while (SysUtils.Pos(i, this.FLines[j]) <= 0)
				{
					h += this.FHeights[j];
					j++;
					if (j == num)
					{
						goto IL_77;
					}
				}
				this.TopPos = h;
				return;
			}

			IL_77:
			if (this.EOnLink != null)
			{
				this.EOnLink(this, (this.FLinks[(int)Link] as TGKHyperView.TLink).Name);
			}
		}

		protected override void OnKeyDown(KeyEventArgs e)
		{
			base.OnKeyDown(e);
			switch (e.KeyCode)
			{
				case Keys.Prior:
				{
					this.TopPos -= base.ClientRectangle.Height / 2;
					break;
				}
				case Keys.Next:
				{
					this.TopPos += base.ClientRectangle.Height / 2;
					break;
				}
				case Keys.End:
				{
					this.TopPos = this.FPageHeight - base.ClientRectangle.Height + 2 * this.FBorderWidth;
					this.LeftPos = this.FPageWidth - base.ClientRectangle.Width;
					break;
				}
				case Keys.Home:
				{
					this.TopPos = 0;
					this.LeftPos = 0;
					break;
				}
				case Keys.Left:
				{
					this.LeftPos -= base.ClientRectangle.Width / 20;
					break;
				}
				case Keys.Up:
				{
					this.TopPos -= base.ClientRectangle.Height / 20;
					break;
				}
				case Keys.Right:
				{
					this.LeftPos += base.ClientRectangle.Width / 20;
					break;
				}
				case Keys.Down:
				{
					this.TopPos += base.ClientRectangle.Height / 20;
					break;
				}
			}
		}

		protected override void OnMouseDown(MouseEventArgs e)
		{
			base.OnMouseDown(e);

			if (!this.Focused)
			{
				base.Focus();
			}
			if (this.FLink >= 0)
			{
				this.GotoLink((ushort)this.FLink);
			}
		}

		protected override void OnMouseMove(MouseEventArgs e)
		{
			base.OnMouseMove(e);

			int num = this.FLinks.Count - 1;
			int i = 0;
			if (num >= i)
			{
				num++;
				while (!(this.FLinks[i] as TGKHyperView.TLink).GotMouse(e.X, e.Y))
				{
					i++;
					if (i == num)
					{
						goto IL_5F;
					}
				}
				this.FLink = i;
				this.Cursor = Cursors.Hand;
				return;
			}

			IL_5F:
			if (this.FLink >= 0)
			{
				this.Cursor = Cursors.Default;
				this.FLink = -1;
			}
		}

		protected override void OnMouseWheel(MouseEventArgs e)
		{
			base.OnMouseWheel(e);

			if (e.Delta > 0)
			{
				this.TopPos++;
			}
			else
			{
				this.TopPos--;
			}
		}

		protected override void OnPaint(PaintEventArgs pe)
		{
			base.OnPaint(pe);
			this.DoPaint(false);
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

			SysUtils.SetScrollRange((uint)this.Handle.ToInt32(), 0, 0, this.FRange.X, (LongBool)0);
			SysUtils.SetScrollRange((uint)this.Handle.ToInt32(), 1, 0, this.FRange.Y, (LongBool)0);
		}

		private void SetLeftPos(int Value)
		{
			if (Value < 0) Value = 0;
			if (Value > this.FRange.X) Value = this.FRange.X;

			if (this.FLeftPos != Value)
			{
				TRect dummy = TRect.Empty();
				TRect R = TRect.Empty();
				SysUtils.ScrollWindowEx((uint)this.Handle.ToInt32(), this.FLeftPos - Value, 0, ref dummy, ref dummy, 0, out R, 0u);
				SysUtils.SetScrollPos((uint)this.Handle.ToInt32(), 0, this.FLeftPos, (LongBool)(-1));
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
				SysUtils.ScrollWindowEx((uint)this.Handle.ToInt32(), 0, this.FTopPos - Value, ref dummy, ref dummy, 0, out R, 0u);
				SysUtils.SetScrollPos((uint)this.Handle.ToInt32(), 1, this.FTopPos, (LongBool)(-1));
				base.Invalidate();
				this.FTopPos = Value;
			}
		}

		protected override void WndProc(ref Message m)
		{
			base.WndProc(ref m);
			int msg = m.Msg;
			if (msg != 5)
			{
				if (msg != 135)
				{
					if (msg != 276)
					{
						if (msg == 277)
						{
							uint wParam = (uint)m.WParam.ToInt32();
							switch (SysUtils.GetScrollEventType(wParam & 65535u))
							{
								case ScrollEventType.SmallDecrement:
								{
									this.TopPos -= base.ClientRectangle.Height / 20;
									break;
								}
								case ScrollEventType.SmallIncrement:
								{
									this.TopPos += base.ClientRectangle.Height / 20;
									break;
								}
								case ScrollEventType.LargeDecrement:
								{
									this.TopPos -= base.ClientRectangle.Height / 2;
									break;
								}
								case ScrollEventType.LargeIncrement:
								{
									this.TopPos += base.ClientRectangle.Height / 2;
									break;
								}
								case ScrollEventType.ThumbPosition:
								case ScrollEventType.ThumbTrack:
								{
									TScrollInfo ScrollInfo = new TScrollInfo();
									ScrollInfo.cbSize = (uint)Marshal.SizeOf( ScrollInfo );
									ScrollInfo.fMask = 23u;
									SysUtils.GetScrollInfo((uint)this.Handle.ToInt32(), 1, ref ScrollInfo);
									this.TopPos = ScrollInfo.nTrackPos;
									break;
								}
								case ScrollEventType.First:
								{
									this.TopPos = 0;
									break;
								}
								case ScrollEventType.Last:
								{
									this.TopPos = this.FPageHeight;
									break;
								}
							}
						}
					}
					else
					{
						uint wParam = (uint)m.WParam.ToInt32();
						switch (SysUtils.GetScrollEventType(wParam & 65535u))
						{
							case ScrollEventType.SmallDecrement:
							{
								this.LeftPos -= base.ClientRectangle.Width / 20;
								break;
							}
							case ScrollEventType.SmallIncrement:
							{
								this.LeftPos += base.ClientRectangle.Width / 20;
								break;
							}
							case ScrollEventType.LargeDecrement:
							{
								this.LeftPos -= base.ClientRectangle.Width / 2;
								break;
							}
							case ScrollEventType.LargeIncrement:
							{
								this.LeftPos += base.ClientRectangle.Width / 2;
								break;
							}
							case ScrollEventType.ThumbPosition:
							case ScrollEventType.ThumbTrack:
							{
								TScrollInfo ScrollInfo = new TScrollInfo();
								ScrollInfo.cbSize = (uint)Marshal.SizeOf( ScrollInfo );
								ScrollInfo.fMask = 23u;
								SysUtils.GetScrollInfo((uint)this.Handle.ToInt32(), 0, ref ScrollInfo);
								this.LeftPos = ScrollInfo.nTrackPos;
								break;
							}
							case ScrollEventType.First:
							{
								this.LeftPos = 0;
								break;
							}
							case ScrollEventType.Last:
							{
								this.LeftPos = this.FPageWidth;
								break;
							}
						}
					}
				}
				else
				{
					m.Result = (IntPtr)(m.Result.ToInt32() | 1 | 2 | 128);
				}
			}
			else
			{
				this.ScrollRange();
			}
		}

		public TGKHyperView()
		{
			base.TabStop = true;
			base.BorderStyle = BorderStyle.Fixed3D;
			this.FHeightCount = 0;
			this.FAcceptFontChange = false;
			this.FLines = new TStringList();
			this.FLines.OnChange += new TNotifyEvent(this.LinesChanged);
			int[] fHeights = this.FHeights;
			int[] array;
			int[] expr_52 = array = new int[0];
			if (fHeights != null)
			{
				int num;
				if ((num = fHeights.Length) > 0)
				{
					num = 0;
				}
				if (num > 0)
				{
					Array.Copy(fHeights, array, num);
				}
			}
			this.FHeights = expr_52;
			this.FTopPos = 0;
			this.FLeftPos = 0;
			this.FColor = SystemColors.Control;
			this.FLinks = new TList();
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
				this.FLinks.Free();
				TObjectHelper.Free(this.FFont);
				this.FHeights = null;
				this.FLines.Free();
			}
			base.Dispose(Disposing);
		}
	}
}
