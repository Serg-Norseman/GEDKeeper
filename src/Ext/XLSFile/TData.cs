using System;

using GKCore.Sys;

namespace XLSFile
{
	public abstract class TData
	{
		public ushort opCode;
		public abstract void Write(TBIFFWriter W);

		public void Free()
		{
			TObjectHelper.Free(this);
		}

		public TData()
		{
		}
	}
}
