using System;

namespace XLSFile
{
	public class TBOF : TData
	{
		public TBOF()
		{
			this.opCode = 2057;
		}

		public override void Write(TBIFFWriter W)
		{
			W.WriteWord(0);
			W.WriteWord(16);
			W.WriteWord(0);
		}
	}
}
