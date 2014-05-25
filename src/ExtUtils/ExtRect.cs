using System;
using System.Drawing;
using System.Runtime.InteropServices;

namespace ExtUtils
{
    [StructLayout(LayoutKind.Sequential, Pack = 1)]
	public struct ExtRect
	{
		public int Left;
		public int Top;
		public int Right;
		public int Bottom;

		public static ExtRect Create(int ALeft, int ATop, int ARight, int ABottom)
		{
			ExtRect result;
			result.Left = ALeft;
			result.Top = ATop;
			result.Right = ARight;
			result.Bottom = ABottom;
			return result;
		}

		public static ExtRect Bounds(int ALeft, int ATop, int AWidth, int AHeight)
		{
			return ExtRect.Create(ALeft, ATop, ALeft + AWidth, ATop + AHeight);
		}

		public static ExtRect Empty()
		{
			return ExtRect.Create(0, 0, 0, 0);
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

		public bool Contains(int X, int Y)
		{
			return X >= this.Left && Y >= this.Top && X < this.Right && Y < this.Bottom;
		}

		public ExtRect GetOffset(int X, int Y)
		{
			return ExtRect.Create(this.Left + X, this.Top + Y, this.Right + X, this.Bottom + Y);
		}
		
		public void OffsetEx(int DX, int DY)
		{
			this.Left += DX;
			this.Right -= DX;
			this.Top += DY;
			this.Bottom -= DY;
		}

	}

}
