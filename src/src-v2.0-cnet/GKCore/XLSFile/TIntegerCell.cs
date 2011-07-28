using GKSys;
using System;

namespace XLSFile
{
	public class TIntegerCell : TCell
	{
		public int Value;

		public TIntegerCell()
		{
			this.opCode = 2;
		}

		public override void Write(TBIFFWriter W)
		{
			base.Write(W);
			W.WriteInt(this.Value);
		}
	}
}
