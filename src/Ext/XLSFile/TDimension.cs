using System;

namespace XLSFile
{
	public class TDimension : TData
	{
		public ushort MinSaveRecs;
		public ushort MaxSaveRecs;
		public ushort MinSaveCols;
		public ushort MaxSaveCols;

		public TDimension()
		{
			this.opCode = 0;
			this.MinSaveRecs = 0;
			this.MaxSaveRecs = 1000;
			this.MinSaveCols = 0;
			this.MaxSaveCols = 100;
		}

		public override void Write(TBIFFWriter W)
		{
			W.WriteWord(this.MinSaveRecs);
			W.WriteWord(this.MaxSaveRecs);
			W.WriteWord(this.MinSaveCols);
			W.WriteWord(this.MaxSaveCols);
		}
	}
}
