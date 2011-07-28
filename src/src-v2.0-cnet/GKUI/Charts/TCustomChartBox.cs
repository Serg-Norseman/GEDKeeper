using GedCom551;
using GKCore;
using GKSys;
using GKUI.Lists;
using System;
using System.Drawing;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace GKUI.Charts
{
	public abstract class TCustomChartBox : Panel, IDisposable
	{
		private int FBorderWidth;
		protected int FImageHeight;
		protected int FImageWidth;
		private int FLeftPos;
		private int FTopPos;
		private TPoint FRange;
		protected int FSPX;
		protected int FSPY;

		internal TGenEngine FEngine;
		internal TPersonsFilter FFilter;
		internal TChartOptions FOptions;
		internal TGenEngine.TShieldState FShieldState;
		internal TGEDCOMTree FTree;
		internal Font FDrawFont;

		public int BorderWidth
		{
			get { return this.FBorderWidth; }
			set { this.SetBorderWidth(value); }
		}

		public TGenEngine Engine
		{
			get { return this.FEngine; }
			set { this.FEngine = value; }
		}

		public TPersonsFilter Filter
		{
			get { return this.FFilter; }
			set { this.FFilter = value; }
		}

		public int LeftPos
		{
			get { return this.FLeftPos; }
			set { this.SetLeftPos(value); }
		}

		public TChartOptions Options
		{
			get { return this.FOptions; }
			set { this.FOptions = value; }
		}

		public TGenEngine.TShieldState ShieldState
		{
			get { return this.FShieldState; }
			set { this.FShieldState = value; }
		}

		public int TopPos
		{
			get { return this.FTopPos; }
			set { this.SetTopPos(value); }
		}

		public TGEDCOMTree Tree
		{
			get { return this.FTree; }
			set { this.FTree = value; }
		}

		internal int DoScroll(int nBar, int aOldPos, int aMin, int aMax, ScrollEventType scrType)
		{
			int NewPos = aOldPos;
			switch (scrType) {
				case ScrollEventType.SmallDecrement:
				{
					NewPos--;
					break;
				}
				case ScrollEventType.SmallIncrement:
				{
					NewPos++;
					break;
				}
				case ScrollEventType.LargeDecrement:
				{
					NewPos--;
					break;
				}
				case ScrollEventType.LargeIncrement:
				{
					NewPos++;
					break;
				}
				case ScrollEventType.ThumbPosition:
				case ScrollEventType.ThumbTrack:
				{
					TScrollInfo ScrollInfo = new TScrollInfo();
					ScrollInfo.cbSize = (uint)Marshal.SizeOf( ScrollInfo );
					ScrollInfo.fMask = 23u;
					VCLUtils.GetScrollInfo((uint)this.Handle.ToInt32(), nBar, ref ScrollInfo);
					NewPos = ScrollInfo.nTrackPos;
					break;
				}
				case ScrollEventType.First:
				{
					NewPos = 0;
					break;
				}
				case ScrollEventType.Last:
				{
					NewPos = aMax;
					break;
				}
			}
			if (NewPos < aMin)
			{
				NewPos = aMin;
			}
			if (NewPos > aMax)
			{
				NewPos = aMax;
			}
			return NewPos;
		}

		internal void ScrollRange()
		{
			if (this.FImageWidth < base.ClientRectangle.Width) {
				this.FRange.X = 0;
				this.LeftPos = (base.ClientRectangle.Width - this.FImageWidth) / 2;
			} else {
				this.FRange.X = this.FImageWidth - base.ClientRectangle.Width;
			}

			if (this.FImageHeight < base.ClientRectangle.Height) {
				this.FRange.Y = 0;
				this.TopPos = (base.ClientRectangle.Height - this.FImageHeight) / 2;
			} else {
				this.FRange.Y = this.FImageHeight - base.ClientRectangle.Height;
			}

			VCLUtils.SetScrollRange((uint)this.Handle.ToInt32(), 0, 0, this.FRange.X, (LongBool)0);
			VCLUtils.SetScrollRange((uint)this.Handle.ToInt32(), 1, 0, this.FRange.Y, (LongBool)0);

			base.Invalidate();
		}

		internal void SetBorderWidth(int Value)
		{
			if (this.FBorderWidth != Value)
			{
				this.FBorderWidth = Value;
				base.Invalidate();
			}
		}

		internal void SetLeftPos(int Value)
		{
			if (Value < 0)
			{
				Value = 0;
			}

			if (Value > this.FRange.X)
			{
				Value = this.FRange.X;
			}

			if (this.FLeftPos != Value)
			{
				TRect dummy = TRect.Empty();
				TRect R = TRect.Empty();
				VCLUtils.ScrollWindowEx((uint)this.Handle.ToInt32(), this.FLeftPos - Value, 0, ref dummy, ref dummy, 0, out R, 0u);
				VCLUtils.SetScrollPos((uint)this.Handle.ToInt32(), 0, this.FLeftPos, (LongBool)(-1));
				base.Invalidate();
				this.FLeftPos = Value;
			}
		}

		internal void SetTopPos(int Value)
		{
			if (Value < 0)
			{
				Value = 0;
			}

			if (Value > this.FRange.Y)
			{
				Value = this.FRange.Y;
			}

			if (this.FTopPos != Value)
			{
				TRect dummy = TRect.Empty();
				TRect R = TRect.Empty();
				VCLUtils.ScrollWindowEx((uint)this.Handle.ToInt32(), 0, this.FTopPos - Value, ref dummy, ref dummy, 0, out R, 0u);
				VCLUtils.SetScrollPos((uint)this.Handle.ToInt32(), 1, this.FTopPos, (LongBool)(-1));
				base.Invalidate();
				this.FTopPos = Value;
			}
		}

		protected override void OnMouseDown(MouseEventArgs e)
		{
			base.OnMouseDown(e);
			if (!this.Focused) base.Focus();
		}

		protected override void WndProc(ref Message m)
		{
			base.WndProc(ref m);
			if (m.Msg == 5)
			{
				this.ScrollRange();
			}
			else
			{
				if (m.Msg == 135)
				{
					m.Result = (IntPtr)(m.Result.ToInt32() | 1 | 2 | 128 | 4);
				}
				else
				{
					if (m.Msg == 276)
					{
						uint wParam = (uint)m.WParam.ToInt32();
						ScrollEventType scrType = TGKSys.GetScrollEventType(wParam & 65535u);
						int new_pos = this.DoScroll(0, this.LeftPos, 0, this.FRange.X, scrType);
						this.SetLeftPos(new_pos);
					}
					else
					{
						if (m.Msg == 277)
						{
							uint wParam = (uint)m.WParam.ToInt32();
							ScrollEventType scrType = TGKSys.GetScrollEventType(wParam & 65535u);
							int new_pos = this.DoScroll(1, this.TopPos, 0, this.FRange.Y, scrType);
							this.SetTopPos(new_pos);
						}
					}
				}
			}
		}

		public TCustomChartBox()
		{
			base.BorderStyle = BorderStyle.Fixed3D;
			base.DoubleBuffered = true;
			base.TabStop = true;
			this.TopPos = 0;
			this.LeftPos = 0;
		}

		protected override void Dispose(bool Disposing)
		{
			if (Disposing)
			{				
			}
			base.Dispose(Disposing);
		}

		protected override void OnPaint(PaintEventArgs pe)
		{
			base.OnPaint(pe);
			this.InternalDraw(pe.Graphics, true);
		}

		public virtual void InternalDraw(Graphics aCanvas, bool Default)
		{
			aCanvas.FillRectangle(new SolidBrush(Color.White), base.ClientRectangle);
			if (Default)
			{
				this.FSPX = this.FBorderWidth - this.FLeftPos;
				this.FSPY = this.FBorderWidth - this.FTopPos;
				if (this.FImageWidth < base.ClientRectangle.Width)
				{
					this.FSPX += (base.ClientRectangle.Width - this.FImageWidth) / 2;
				}
				if (this.FImageHeight < base.ClientRectangle.Height)
				{
					this.FSPY += (base.ClientRectangle.Height - this.FImageHeight) / 2;
				}
			}
			else
			{
				this.FSPX = 0;
				this.FSPY = 0;
			}
			// this overrided routines
		}
	}
}
