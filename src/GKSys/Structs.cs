using System;
using System.Drawing;
using System.Runtime.InteropServices;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKSys
{
	public class TSetElementTypeAttribute : Attribute
	{
		private Type FElementType;

		public Type ElementType
		{
			get { return this.FElementType; }
		}

		public TSetElementTypeAttribute(Type AElementType)
		{
			this.FElementType = AElementType;
		}
	}

	public enum LongBool : int
	{}

	[StructLayout(LayoutKind.Sequential, Pack = 1)]
	public struct TScrollInfo
	{
		public uint cbSize;
		public uint fMask;
		public int nMin;
		public int nMax;
		public uint nPage;
		public int nPos;
		public int nTrackPos;
	}


	[StructLayout(LayoutKind.Sequential, Pack = 1, Size = 1)]
	public struct _Unnamed1
	{
		public static readonly byte[] Map;
		static _Unnamed1()
		{
			_Unnamed1.Map = new byte[]
			{
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				62, 
				0, 
				0, 
				0, 
				63, 
				52, 
				53, 
				54, 
				55, 
				56, 
				57, 
				58, 
				59, 
				60, 
				61, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				1, 
				2, 
				3, 
				4, 
				5, 
				6, 
				7, 
				8, 
				9, 
				10, 
				11, 
				12, 
				13, 
				14, 
				15, 
				16, 
				17, 
				18, 
				19, 
				20, 
				21, 
				22, 
				23, 
				24, 
				25, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				26, 
				27, 
				28, 
				29, 
				30, 
				31, 
				32, 
				33, 
				34, 
				35, 
				36, 
				37, 
				38, 
				39, 
				40, 
				41, 
				42, 
				43, 
				44, 
				45, 
				46, 
				47, 
				48, 
				49, 
				50, 
				51, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0, 
				0
			};
		}
	}


	[StructLayout(LayoutKind.Sequential, Pack = 1, Size = 1)]
	public struct _Unnamed2
	{
		public static readonly char[] Map;
		static _Unnamed2()
		{
			_Unnamed2.Map = new char[]
			{
				'A', 
				'B', 
				'C', 
				'D', 
				'E', 
				'F', 
				'G', 
				'H', 
				'I', 
				'J', 
				'K', 
				'L', 
				'M', 
				'N', 
				'O', 
				'P', 
				'Q', 
				'R', 
				'S', 
				'T', 
				'U', 
				'V', 
				'W', 
				'X', 
				'Y', 
				'Z', 
				'a', 
				'b', 
				'c', 
				'd', 
				'e', 
				'f', 
				'g', 
				'h', 
				'i', 
				'j', 
				'k', 
				'l', 
				'm', 
				'n', 
				'o', 
				'p', 
				'q', 
				'r', 
				's', 
				't', 
				'u', 
				'v', 
				'w', 
				'x', 
				'y', 
				'z', 
				'0', 
				'1', 
				'2', 
				'3', 
				'4', 
				'5', 
				'6', 
				'7', 
				'8', 
				'9', 
				'+', 
				'/'
			};
		}
	}


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


	public struct EnumSet
	{
		private uint FValue;

		public static EnumSet Create()
		{
			EnumSet Result = new EnumSet();
			Result.FValue = 0u;
			return Result;
		}

		public static EnumSet Create(params Enum[] e)
		{
			EnumSet Result = EnumSet.Create();
			Result.Include(e);
			return Result;
		}

		public void Include(params Enum[] e)
		{
			//e = (Enum[])e.Clone();
			for (int i = 0; i <= e.Length - 1; i++) {
				this.Include(e[i]);
			}
		}

		public void Include(Enum e)
		{
			unchecked
			{
				byte pos = ((IConvertible)e).ToByte(null);
				this.FValue |= (uint)(1 << (int)pos);
			}
		}

		public void Exclude(Enum e)
		{
			unchecked
			{
				byte pos = ((IConvertible)e).ToByte(null);
				this.FValue &= (uint)(1 << (int)pos ^ -1);
			}
		}

		public bool InSet(Enum e)
		{
			unchecked
			{
				byte pos = ((IConvertible)e).ToByte(null);
				uint bt = (uint)(1 << (int)pos);
				return (bt & this.FValue) > 0u;
			}
		}

		public bool IsEmpty()
		{
			return this.FValue == 0u;
		}

		public string ToString(byte B)
		{
			byte bt = 1;
			string s = "";
			int i = 1;
			do
			{
				if ((B & bt) > 0)
				{
					s = "1" + s;
				}
				else
				{
					s = "0" + s;
				}
				bt = (byte)((int)((uint)bt) << 1);
				i++;
			}
			while (i != 9);
			return s;
		}

	}
}
