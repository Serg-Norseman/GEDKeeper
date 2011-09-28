using System;

using GKCore.Sys;

namespace XLSFile
{
	public enum TCellAttribute : byte
	{
		acHiddenFormula,
		acLocked,
		acShaded,
		acBottomBorder,
		acTopBorder,
		acRightBorder,
		acLeftBorder,
		acLeft,
		acCenter,
		acRight,
		acFill
	}

	[Flags, TSetElementType(typeof(TCellAttribute))]
	public enum TCellAttributeSet : ushort
	{
		acHiddenFormula = 1,
		acLocked = 2,
		acShaded = 4,
		acBottomBorder = 8,
		acTopBorder = 16,
		acRightBorder = 32,
		acLeftBorder = 64,
		acLeft = 128,
		acCenter = 256,
		acRight = 512,
		acFill = 1024
	}

	public class TCell : TData
	{
		private byte[] FAttribute = new byte[3];
		public ushort Col;
		public ushort Row;

		public TCellAttributeSet Attribute
		{
			set { this.SetAttribute(value);	}
		}

		private void SetAttribute(TCellAttributeSet Value)
		{
			int i = 0;
			do
			{
				this.FAttribute[i] = 0;
				i++;
			}
			while (i != 3);
			if ((Value & TCellAttributeSet.acHiddenFormula) != (TCellAttributeSet)0)
			{
				this.FAttribute[0] = (byte)((uint)this.FAttribute[0] + 128u);
			}
			if ((Value & TCellAttributeSet.acLocked) != (TCellAttributeSet)0)
			{
				this.FAttribute[0] = (byte)((uint)this.FAttribute[0] + 64u);
			}
			if ((Value & TCellAttributeSet.acShaded) != (TCellAttributeSet)0)
			{
				this.FAttribute[2] = (byte)((uint)this.FAttribute[2] + 128u);
			}
			if ((Value & TCellAttributeSet.acBottomBorder) != (TCellAttributeSet)0)
			{
				this.FAttribute[2] = (byte)((uint)this.FAttribute[2] + 64u);
			}
			if ((Value & TCellAttributeSet.acTopBorder) != (TCellAttributeSet)0)
			{
				this.FAttribute[2] = (byte)((uint)this.FAttribute[2] + 32u);
			}
			if ((Value & TCellAttributeSet.acRightBorder) != (TCellAttributeSet)0)
			{
				this.FAttribute[2] = (byte)((uint)this.FAttribute[2] + 16u);
			}
			if ((Value & TCellAttributeSet.acLeftBorder) != (TCellAttributeSet)0)
			{
				this.FAttribute[2] = (byte)((uint)this.FAttribute[2] + 8u);
			}
			if ((Value & TCellAttributeSet.acLeft) != (TCellAttributeSet)0)
			{
				this.FAttribute[2] = (byte)((uint)this.FAttribute[2] + 1u);
			}
			else
			{
				if ((Value & TCellAttributeSet.acCenter) != (TCellAttributeSet)0)
				{
					this.FAttribute[2] = (byte)((uint)this.FAttribute[2] + 2u);
				}
				else
				{
					if ((Value & TCellAttributeSet.acRight) != (TCellAttributeSet)0)
					{
						this.FAttribute[2] = (byte)((uint)this.FAttribute[2] + 3u);
					}
				}
			}
			if ((Value & TCellAttributeSet.acFill) != (TCellAttributeSet)0)
			{
				this.FAttribute[2] = (byte)((uint)this.FAttribute[2] + 4u);
			}
		}

		public override void Write(TBIFFWriter W)
		{
			W.WriteWord(this.Row);
			W.WriteWord(this.Col);
			int i = 0;
			do
			{
				W.WriteByte(this.FAttribute[i]);
				i++;
			}
			while (i != 3);
		}

	}
}
