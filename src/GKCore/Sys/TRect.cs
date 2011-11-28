using System;
using System.Drawing;
using System.Runtime.InteropServices;

namespace GKCore.Sys
{
	[StructLayout(LayoutKind.Sequential, Pack = 1)]
	public struct TRect
	{
		public int Left;
		public int Top;
		public int Right;
		public int Bottom;

		public static TRect Create([In] int ALeft, [In] int ATop, [In] int ARight, [In] int ABottom)
		{
			TRect Result;
			Result.Left = ALeft;
			Result.Top = ATop;
			Result.Right = ARight;
			Result.Bottom = ABottom;
			return Result;
		}

		public static TRect Bounds([In] int ALeft, [In] int ATop, [In] int AWidth, [In] int AHeight)
		{
			return TRect.Create(ALeft, ATop, ALeft + AWidth, ATop + AHeight);
		}

		public static TRect Empty()
		{
			return TRect.Create(0, 0, 0, 0);
		}

		public override string ToString()
		{
			return string.Concat(new string[]
			{
				"{X=", this.Left.ToString(), 
				",Y=", this.Top.ToString(), 
				",Width=", this.GetWidth().ToString(), 
				",Height=", this.GetHeight().ToString(), 
				"}"
			});
		}

		public Rectangle ToRectangle()
		{
			return new Rectangle(this.Left, this.Top, this.Right - this.Left + 1, this.Bottom - this.Top + 1);
		}

		public int GetWidth()
		{
			return this.Right - this.Left + 1;
		}

		public int GetHeight()
		{
			return this.Bottom - this.Top + 1;
		}

		public bool IsEmpty()
		{
			return this.Right <= this.Left || this.Bottom <= this.Top;
		}

		public bool Contains([In] int X, [In] int Y)
		{
			return X >= this.Left && Y >= this.Top && X < this.Right && Y < this.Bottom;
		}

		public TRect GetOffset([In] int X, [In] int Y)
		{
			return TRect.Create(this.Left + X, this.Top + Y, this.Right + X, this.Bottom + Y);
		}
	}
}
