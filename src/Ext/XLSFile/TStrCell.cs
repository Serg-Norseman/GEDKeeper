using System;

namespace Ext.XLSFile
{
	public class TStrCell : TCell
	{
		public string Value;

		public TStrCell()
		{
			this.opCode = 4;
		}

		public override void Write(TBIFFWriter W)
		{
			base.Write(W);
			W.WriteStr(this.Value);
		}
	}
}
