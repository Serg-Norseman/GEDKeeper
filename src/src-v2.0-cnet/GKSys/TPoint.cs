using System;
using System.Runtime.InteropServices;

namespace GKSys
{
	[StructLayout(LayoutKind.Sequential, Pack = 1)]
	public struct TPoint
	{
		public int X;
		public int Y;

		public static TPoint Create([In] int AX, [In] int AY)
		{
			TPoint Result;
			Result.X = AX;
			Result.Y = AY;
			return Result;
		}

		public static TPoint Empty()
		{
			return TPoint.Create(0, 0);
		}

		public override string ToString()
		{
			return string.Concat(new string[]
			{
				"{X=", this.X.ToString(), ",Y=", this.Y.ToString(), "}"
			});
		}

		public int Left()
		{
			return this.X;
		}

		public int Top()
		{
			return this.Y;
		}

		public bool IsEmpty()
		{
			return this.X == 0 && this.Y == 0;
		}

		public bool Equals([In] int AX, [In] int AY)
		{
			return this.X == AX && this.Y == AY;
		}

		public bool Equals([In] TPoint Value)
		{
			return this.X == Value.X && this.Y == Value.Y;
		}
	}
}
