using System;

namespace XLSFile
{
	public class TWordCell : TCell
	{
		public ushort Value;

		public TWordCell()
		{
			this.opCode = 2;
		}

		public override void Write(TBIFFWriter W)
		{
			base.Write(W);
			W.WriteWord(this.Value);
		}
	}
}
