using System;
using System.ComponentModel;
using System.IO;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GKSys
{
	public class TCLRStreamWrapper : TStream, IDisposable
	{
		protected internal Stream FHandle;
		protected internal bool Disposed_;
		[Browsable(false)]
		public Stream Handle
		{
			get
			{
				return this.FHandle;
			}
		}
		protected internal override void SetSize(long NewSize)
		{
			this.FHandle.SetLength(NewSize);
		}
		public TCLRStreamWrapper(Stream AHandle)
		{
			this.FHandle = AHandle;
		}

		public void Dispose()
		{
			if (!this.Disposed_)
			{
				if (this.FHandle != null)
				{
					this.FHandle.Close();
				}
				this.Disposed_ = true;
			}
		}

		public override int Read(ref byte[] Buffer, int Offset, int Count)
		{
			return this.FHandle.Read(Buffer, Offset, Count);
		}
		public override int Write([In] byte[] Buffer, int Offset, int Count)
		{
			int Result;
			try
			{
				this.FHandle.Write(Buffer, Offset, Count);
				Result = Count;
			}
			catch (Exception E)
			{
				Result = 0;
			}
			return Result;
		}

		public override long Seek([In] long Offset, TSeekOrigin Origin)
		{
			return this.FHandle.Seek(Offset, VCLUtils.OriginMap[(int)Origin]);
		}
	}
}
