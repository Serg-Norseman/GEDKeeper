using System;

namespace Ext.XLSFile
{
	public class TDoubleCell : TCell
	{
		public double Value;

		public TDoubleCell()
		{
			this.opCode = 3;
		}

		public override void Write(TBIFFWriter W)
		{
			base.Write(W);
			W.WriteDouble(this.Value);
		}
	}
}
