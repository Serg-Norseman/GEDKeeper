using System;

using Ext.Utils;

namespace Ext.XLSFile
{
	public abstract class TData
	{
		public ushort opCode;
		public abstract void Write(TBIFFWriter W);

		public void Free()
		{
			SysUtils.Free(this);
		}

		public TData()
		{
		}
	}
}
