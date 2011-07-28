using GKSys;
using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GKSys
{
	public abstract class TCustomMemoryStream : TStream
	{
		internal long FSize;
		internal long FPosition;
		protected internal byte[] FMemory;
		[Browsable(false)]
		public byte[] Memory
		{
			get
			{
				return this.FMemory;
			}
		}
		public override int Read(ref byte[] Buffer, int Offset, int Count)
		{
			int Result;
			if (this.FPosition >= (long)((ulong)0) && Count >= 0)
			{
				Result = (int)(this.FSize - this.FPosition);
				if (Result > 0)
				{
					if (Result > Count)
					{
						Result = Count;
					}
					Array.Copy(this.FMemory, this.FPosition, Buffer, (long)Offset, (long)Result);
					this.FPosition += (long)Result;
					return Result;
				}
			}
			Result = 0;
			return Result;
		}
		public override long Seek([In] long Offset, TSeekOrigin Origin)
		{
			if (Origin != TSeekOrigin.soBeginning)
			{
				if (Origin != TSeekOrigin.soCurrent)
				{
					if (Origin == TSeekOrigin.soEnd)
					{
						this.FPosition = this.FSize + Offset;
					}
				}
				else
				{
					this.FPosition += Offset;
				}
			}
			else
			{
				this.FPosition = Offset;
			}
			return this.FPosition;
		}
		public void SaveToStream(TStream Stream)
		{
			if (this.FSize != (long)((ulong)0))
			{
				Stream.WriteBuffer(this.FMemory, (int)this.FSize);
			}
		}
		public void SaveToFile([In] string FileName)
		{
			TStream Stream = new TFileStream(FileName, 65535);
			try
			{
				this.SaveToStream(Stream);
			}
			finally
			{
				Stream.Free();
			}
		}

		public TCustomMemoryStream()
		{
		}
	}
}
