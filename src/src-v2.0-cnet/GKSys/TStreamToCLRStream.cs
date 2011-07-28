using System;
using System.ComponentModel;
using System.IO;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GKSys
{

	public class TStreamToCLRStream : Stream, IDisposable
	{
		protected internal TStream FStream;
		protected internal bool Disposed_;

		[Browsable(false)]
		public override bool CanRead
		{
			get
			{
				return true;
			}
		}
		[Browsable(false)]
		public override bool CanSeek
		{
			get
			{
				return true;
			}
		}
		[Browsable(false)]
		public override bool CanWrite
		{
			get
			{
				return true;
			}
		}
		[Browsable(false)]
		public override long Length
		{
			get
			{
				return this.FStream.Size;
			}
		}
		[Browsable(false)]
		public override long Position
		{
			get
			{
				return this.FStream.Position;
			}
			set
			{
				this.FStream.Position = value;
			}
		}
		protected internal TStreamToCLRStream(TStream Stream)
		{
			this.FStream = Stream;
		}
		public override void Close()
		{
			this.FStream.Free();
			this.FStream = null;
		}
		public override void Flush()
		{
		}
		public override int Read(byte[] Buffer, int Offset, int Count)
		{
			return this.FStream.Read(ref Buffer, Offset, Count);
		}

		public override long Seek(long Offset, SeekOrigin Origin)
		{
			TSeekOrigin LOrigin = TSeekOrigin.soCurrent;
			switch (Origin) {
				case SeekOrigin.Begin:
					LOrigin = TSeekOrigin.soBeginning;
					break;
				case SeekOrigin.Current:
					LOrigin = TSeekOrigin.soCurrent;
					break;
				case SeekOrigin.End:
					LOrigin = TSeekOrigin.soEnd;
					break;
				
			}
			return this.FStream.Seek(Offset, LOrigin);
		}

		public override void SetLength(long Value)
		{
			this.FStream.Size = Value;
		}
		public override void Write(byte[] Buffer, int Offset, int Count)
		{
			this.FStream.Write(Buffer, Offset, Count);
		}

		public void Dispose()
		{
			if (!this.Disposed_)
			{
				this.FStream.Free();
				this.Disposed_ = true;
			}
		}

		public static Stream GetStream(TStream Stream)
		{
			Stream Result;
			if (Stream is TCLRStreamWrapper)
			{
				Result = (Stream as TCLRStreamWrapper).Handle;
			}
			else
			{
				Result = new TStreamToCLRStream(Stream);
			}
			return Result;
		}
	}
}
