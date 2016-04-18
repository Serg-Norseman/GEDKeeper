using System.Drawing;
using System.Runtime.InteropServices;

namespace GKCommon
{
    [StructLayout(LayoutKind.Sequential, Pack = 1)]
	public struct ExtRect
	{
	    public static readonly ExtRect Empty = default(ExtRect);
	    
		public int Left;
		public int Top;
		public int Right;
		public int Bottom;

		public static ExtRect Create(int left, int top, int right, int bottom)
		{
			ExtRect result;
			result.Left = left;
			result.Top = top;
			result.Right = right;
			result.Bottom = bottom;
			return result;
		}

		public static ExtRect CreateBounds(int left, int top, int width, int height)
		{
			return ExtRect.Create(left, top, left + width - 1, top + height - 1);
		}

		public static ExtRect CreateEmpty()
		{
			return ExtRect.Create(0, 0, 0, 0);
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

		public bool Contains(int x, int y)
		{
			return x >= this.Left && y >= this.Top && x <= this.Right && y <= this.Bottom;
		}

		public ExtRect GetOffset(int dX, int dY)
		{
			return ExtRect.Create(this.Left + dX, this.Top + dY, this.Right + dX, this.Bottom + dY);
		}

		public void Offset(int dX, int dY)
		{
			this.Left += dX;
			this.Right += dX;
			this.Top += dY;
			this.Bottom += dY;
		}

		public void Inflate(int dX, int dY)
		{
			this.Left += dX;
			this.Right -= dX;
			this.Top += dY;
			this.Bottom -= dY;
		}

		public bool IntersectsWith(ExtRect rect)
		{
			return rect.Left < this.Right && this.Left < rect.Right && rect.Top < this.Bottom && this.Top < rect.Bottom;
		}

		public override string ToString()
		{
			return string.Concat(new string[] {
				"{X=", this.Left.ToString(), ",Y=", this.Top.ToString(), 
				",Width=", this.GetWidth().ToString(), ",Height=", this.GetHeight().ToString(), "}"
			});
		}

		public Rectangle ToRectangle()
		{
			return new Rectangle(this.Left, this.Top, this.Right - this.Left + 1, this.Bottom - this.Top + 1);
		}
	}
}
