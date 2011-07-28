using System;
using System.ComponentModel;
using System.IO;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GKSys
{

	public enum TSeekOrigin : byte
	{
		soBeginning,
		soCurrent,
		soEnd
	}

	public class EStreamError : Exception
	{
		public EStreamError()
		{
		}
		public EStreamError(string message) : base(message)
		{
		}
		public EStreamError(string message, Exception innerException) : base(message, innerException)
		{
		}
	}

	public class EFilerError : EStreamError
	{
		public EFilerError()
		{
		}

		public EFilerError(string message) : base(message)
		{
		}

		public EFilerError(string message, Exception innerException) : base(message, innerException)
		{
		}
	}

	public class EReadError : EFilerError
	{
		public EReadError()
		{
		}

		public EReadError(string message) : base(message)
		{
		}

		public EReadError(string message, Exception innerException) : base(message, innerException)
		{
		}
	}

	public class EWriteError : EFilerError
	{
		public EWriteError()
		{
		}

		public EWriteError(string message) : base(message)
		{
		}

		public EWriteError(string message, Exception innerException) : base(message, innerException)
		{
		}
	}

	public abstract class TStream
	{
		[Browsable(false)]
		public long Position
		{
			get
			{
				return this.GetPosition();
			}
			set
			{
				this.SetPosition(value);
			}
		}
		[Browsable(false)]
		public long Size
		{
			get
			{
				return this.GetSize();
			}
			set
			{
				this.SetSize(value);
			}
		}
		internal long GetPosition()
		{
			return this.Seek((long)((ulong)0), TSeekOrigin.soCurrent);
		}
		internal void SetPosition([In] long Pos)
		{
			this.Seek(Pos, TSeekOrigin.soBeginning);
		}
		internal long GetSize()
		{
			long Pos = this.Seek((long)((ulong)0), TSeekOrigin.soCurrent);
			long Result = this.Seek((long)((ulong)0), TSeekOrigin.soEnd);
			this.Seek(Pos, TSeekOrigin.soBeginning);
			return Result;
		}
		internal int Skip(int Amount)
		{
			int P = (int)this.Position;
			return (int)(this.Seek((long)Amount, TSeekOrigin.soCurrent) - (long)P);
		}
		protected internal abstract void SetSize(long NewSize);
		public abstract int Read(ref byte[] Buffer, int Offset, int Count);
		public int Read(ref byte[] Buffer, int Count)
		{
			return this.Read(ref Buffer, 0, Count);
		}
		public int Read(ref byte Buffer)
		{
			byte[] Buf = new byte[1];
			int Result = this.Read(ref Buf, 1);
			Buffer = Buf[0];
			return Result;
		}
		public int Read(ref byte Buffer, int Count)
		{
			byte[] Buf = new byte[1];
			int Result;
			if (Count != 0)
			{
				Result = this.Read(ref Buf, 1);
				if (Count > 1)
				{
					Result += this.Skip(Count - 1);
				}
				Buffer = Buf[0];
			}
			else
			{
				Result = 0;
			}
			return Result;
		}
		public int Read(ref bool Buffer)
		{
			byte[] Buf = new byte[1];
			int Result = this.Read(ref Buf, 1);
			Buffer = (Buf[0] > 0);
			return Result;
		}
		public int Read(ref bool Buffer, int Count)
		{
			byte[] Buf = new byte[1];
			int Result;
			if (Count != 0)
			{
				Result = this.Read(ref Buf, 1);
				if (Count > 1)
				{
					Result += this.Skip(Count - 1);
				}
				Buffer = (Buf[0] > 0);
			}
			else
			{
				Result = 0;
			}
			return Result;
		}
		public int Read(ref char Buffer)
		{
			byte[] Buf = new byte[2];
			int Result = this.Read(ref Buf, 2);
			Buffer = (char)((uint)Buf[0] | (int)((uint)Buf[1]) << 8);
			return Result;
		}
		public int Read(ref char Buffer, int Count)
		{
			byte[] Buf = new byte[2];
			int S = 0;
			if (Count > 2)
			{
				S = Count - 2;
				Count = 2;
			}
			int Result;
			if (Count != 0)
			{
				Buf[1] = 0;
				Result = this.Read(ref Buf, Count);
				Buffer = (char)((uint)Buf[0] | (int)((uint)Buf[1]) << 8);
				if (S != 0)
				{
					Result += this.Skip(S);
				}
			}
			else
			{
				Result = 0;
			}
			return Result;
		}
		public int Read(ref sbyte Buffer)
		{
			byte[] Buf = new byte[1];
			int Result = this.Read(ref Buf, 1);
			Buffer = (sbyte)Buf[0];
			return Result;
		}
		public int Read(ref sbyte Buffer, int Count)
		{
			byte[] Buf = new byte[1];
			int Result;
			if (Count != 0)
			{
				Result = this.Read(ref Buf, 1);
				if (Count > 1)
				{
					Result += this.Skip(Count - 1);
				}
				Buffer = (sbyte)Buf[0];
			}
			else
			{
				Result = 0;
			}
			return Result;
		}
		public int Read(ref short Buffer)
		{
			byte[] Buf = new byte[2];
			int Result = this.Read(ref Buf, 2);
			Buffer = (short)((uint)Buf[0] | (int)((uint)Buf[1]) << 8);
			return Result;
		}
		public int Read(ref short Buffer, int Count)
		{
			byte[] Buf = new byte[2];
			int S = 0;
			if (Count > 2)
			{
				S = Count - 2;
				Count = 2;
			}
			int Result;
			if (Count != 0)
			{
				Buf[1] = 0;
				Result = this.Read(ref Buf, Count);
				Buffer = (short)((uint)Buf[0] | (int)((uint)Buf[1]) << 8);
				if (S != 0)
				{
					Result += this.Skip(S);
				}
			}
			else
			{
				Result = 0;
			}
			return Result;
		}
		public int Read(ref ushort Buffer)
		{
			byte[] Buf = new byte[2];
			int Result = this.Read(ref Buf, 2);
			Buffer = (ushort)((uint)Buf[0] | (int)((uint)Buf[1]) << 8);
			return Result;
		}
		public int Read(ref ushort Buffer, int Count)
		{
			byte[] Buf = new byte[2];
			int S = 0;
			if (Count > 2)
			{
				S = Count - 2;
				Count = 2;
			}
			int Result;
			if (Count != 0)
			{
				Buf[1] = 0;
				Result = this.Read(ref Buf, Count);
				Buffer = (ushort)((uint)Buf[0] | (int)((uint)Buf[1]) << 8);
				if (S != 0)
				{
					Result += this.Skip(S);
				}
			}
			else
			{
				Result = 0;
			}
			return Result;
		}
		public int Read(ref int Buffer)
		{
			byte[] Buf = new byte[4];
			int Result = this.Read(ref Buf, 4);
			Buffer = (int)((uint)Buf[0] | (int)((uint)Buf[1]) << 8 | (int)((uint)Buf[2]) << 16 | (int)((uint)Buf[3]) << 24);
			return Result;
		}
		public int Read(ref int Buffer, int Count)
		{
			byte[] Buf = new byte[4];
			int S = 0;
			if (Count > 4)
			{
				S = Count - 4;
				Count = 4;
			}
			int Result;
			if (Count != 0)
			{
				Buf[1] = 0;
				Buf[2] = 0;
				Buf[3] = 0;
				Result = this.Read(ref Buf, Count);
				Buffer = (int)((uint)Buf[0] | (int)((uint)Buf[1]) << 8 | (int)((uint)Buf[2]) << 16 | (int)((uint)Buf[3]) << 24);
				if (S != 0)
				{
					Result += this.Skip(S);
				}
			}
			else
			{
				Result = 0;
			}
			return Result;
		}
		public int Read(ref uint Buffer)
		{
			byte[] Buf = new byte[4];
			int Result = this.Read(ref Buf, 4);
			Buffer = (uint)(Buf[0] | (Buf[1]) << 8 | (Buf[2]) << 16 | (Buf[3]) << 24);
			return Result;
		}
		public int Read(ref uint Buffer, int Count)
		{
			byte[] Buf = new byte[4];
			int S = 0;
			if (Count > 4)
			{
				S = Count - 4;
				Count = 4;
			}
			int Result;
			if (Count != 0)
			{
				Buf[1] = 0;
				Buf[2] = 0;
				Buf[3] = 0;
				Result = this.Read(ref Buf, Count);
				Buffer = (uint)(Buf[0] | (Buf[1]) << 8 | (Buf[2]) << 16 | (Buf[3]) << 24);
				if (S != 0)
				{
					Result += this.Skip(S);
				}
			}
			else
			{
				Result = 0;
			}
			return Result;
		}
		public int Read(ref long Buffer)
		{
			byte[] Buf = new byte[8];
			int Result = this.Read(ref Buf, 8);
			Buffer = ((long)((ulong)Buf[0]) | (int)((long)((ulong)Buf[1])) << 8 | (int)((long)((ulong)Buf[2])) << 16 | (int)((long)((ulong)Buf[3])) << 24 | (int)((long)((ulong)Buf[4])) << 32 | (int)((long)((ulong)Buf[5])) << 40 | (int)((long)((ulong)Buf[6])) << 48 | (int)((long)((ulong)Buf[7])) << 56);
			return Result;
		}
		public int Read(ref long Buffer, int Count)
		{
			byte[] Buf = new byte[8];
			int S = 0;
			if (Count > 8)
			{
				S = Count - 8;
				Count = 8;
			}
			int Result;
			if (Count != 0)
			{
				Buf[1] = 0;
				Buf[2] = 0;
				Buf[3] = 0;
				Buf[4] = 0;
				Buf[5] = 0;
				Buf[6] = 0;
				Buf[7] = 0;
				Result = this.Read(ref Buf, Count);
				Buffer = ((long)((ulong)Buf[0]) | (int)((long)((ulong)Buf[1])) << 8 | (int)((long)((ulong)Buf[2])) << 16 | (int)((long)((ulong)Buf[3])) << 24 | (int)((long)((ulong)Buf[4])) << 32 | (int)((long)((ulong)Buf[5])) << 40 | (int)((long)((ulong)Buf[6])) << 48 | (int)((long)((ulong)Buf[7])) << 56);
				if (S != 0)
				{
					Result += this.Skip(S);
				}
			}
			else
			{
				Result = 0;
			}
			return Result;
		}
		public int Read(ref ulong Buffer)
		{
			byte[] Buf = new byte[8];
			int Result = this.Read(ref Buf, 8);
			Buffer = (ulong)((long)((ulong)Buf[0]) | (int)((long)((ulong)Buf[1])) << 8 | (int)((long)((ulong)Buf[2])) << 16 | (int)((long)((ulong)Buf[3])) << 24 | (int)((long)((ulong)Buf[4])) << 32 | (int)((long)((ulong)Buf[5])) << 40 | (int)((long)((ulong)Buf[6])) << 48 | (int)((long)((ulong)Buf[7])) << 56);
			return Result;
		}
		public int Read(ref ulong Buffer, int Count)
		{
			byte[] Buf = new byte[8];
			int S = 0;
			if (Count > 8)
			{
				S = Count - 8;
				Count = 8;
			}
			int Result;
			if (Count != 0)
			{
				Buf[1] = 0;
				Buf[2] = 0;
				Buf[3] = 0;
				Buf[4] = 0;
				Buf[5] = 0;
				Buf[6] = 0;
				Buf[7] = 0;
				Result = this.Read(ref Buf, Count);
				Buffer = (ulong)((long)((ulong)Buf[0]) | (int)((long)((ulong)Buf[1])) << 8 | (int)((long)((ulong)Buf[2])) << 16 | (int)((long)((ulong)Buf[3])) << 24 | (int)((long)((ulong)Buf[4])) << 32 | (int)((long)((ulong)Buf[5])) << 40 | (int)((long)((ulong)Buf[6])) << 48 | (int)((long)((ulong)Buf[7])) << 56);
				if (S != 0)
				{
					Result += this.Skip(S);
				}
			}
			else
			{
				Result = 0;
			}
			return Result;
		}
		public int Read(ref float Buffer)
		{
			byte[] Buf = new byte[4];
			int Result = this.Read(ref Buf, 4);
			Buffer = BitConverter.ToSingle(Buf, 0);
			return Result;
		}
		public int Read(ref float Buffer, int Count)
		{
			byte[] Buf = new byte[4];
			int Result;
			if (Count != 4)
			{
				Buffer = 0f;
				Result = this.Skip(Count);
			}
			else
			{
				Result = this.Read(ref Buf, 4);
				Buffer = BitConverter.ToSingle(Buf, 0);
			}
			return Result;
		}
		public int Read(ref double Buffer)
		{
			byte[] Buf = new byte[8];
			int Result = this.Read(ref Buf, 8);
			Buffer = BitConverter.ToDouble(Buf, 0);
			return Result;
		}
		public int Read(ref double Buffer, int Count)
		{
			byte[] Buf = new byte[8];
			int Result;
			if (Count == 8)
			{
				Result = this.Read(ref Buf, 8);
				Buffer = BitConverter.ToDouble(Buf, 0);
			}
			else
			{
				Buffer = 0.0;
				Result = this.Skip(Count);
			}
			return Result;
		}
		public abstract int Write([In] byte[] Buffer, int Offset, int Count);
		public int Write([In] byte[] Buffer, int Count)
		{
			return this.Write(Buffer, 0, Count);
		}
		public int Write([In] byte Buffer)
		{
			return this.Write(new byte[]
			{
				Buffer
			}, 1);
		}
		public int Write([In] byte Buffer, int Count)
		{
			byte[] Buf = new byte[1];
			int C = Count;
			if (C > 1)
			{
				C = 1;
			}
			Buf[0] = Buffer;
			int Result = this.Write(Buf, C);
			if (C < Count)
			{
				Result += this.Skip(Count - C);
			}
			return Result;
		}
		public int Write([In] bool Buffer)
		{
			return this.Write(new byte[]
			{
			                  	(byte)(Buffer ? 1 : 0)
			}, 1);
		}
		public int Write([In] bool Buffer, int Count)
		{
			byte[] Buf = new byte[1];
			int C = Count;
			if (C > 1)
			{
				C = 1;
			}
			Buf[0] = (byte)((Buffer) ? 1 : 0);
			int Result = this.Write(Buf, C);
			if (C < Count)
			{
				Result += this.Skip(Count - C);
			}
			return Result;
		}
		public int Write([In] char Buffer)
		{
			return this.Write(new byte[]
			{
				(byte)(Buffer & 'ÿ'), 
				(byte)((uint)Buffer >> 8 & 255u)
			}, 2);
		}
		public int Write([In] char Buffer, int Count)
		{
			byte[] Buf = new byte[2];
			int C = Count;
			if (C > 2)
			{
				C = 2;
			}
			Buf[0] = (byte)(Buffer & 'ÿ');
			Buf[1] = (byte)((uint)Buffer >> 8 & 255u);
			int Result = this.Write(Buf, C);
			if (C < Count)
			{
				Result += this.Skip(Count - C);
			}
			return Result;
		}
		public int Write([In] sbyte Buffer)
		{
			return this.Write(new byte[]
			{
				(byte)Buffer
			}, 1);
		}
		public int Write([In] sbyte Buffer, int Count)
		{
			byte[] Buf = new byte[1];
			int C = Count;
			if (C > 1)
			{
				C = 1;
			}
			Buf[0] = (byte)Buffer;
			int Result = this.Write(Buf, C);
			if (C < Count)
			{
				Result += this.Skip(Count - C);
			}
			return Result;
		}
		public int Write([In] short Buffer)
		{
			return this.Write(new byte[]
			{
				(byte)(Buffer & 255), 
				(byte)((uint)((int)Buffer) >> 8 & 255)
			}, 2);
		}
		public int Write([In] short Buffer, int Count)
		{
			byte[] Buf = new byte[2];
			int C = Count;
			if (C > 2)
			{
				C = 2;
			}
			Buf[0] = (byte)(Buffer & 255);
			Buf[1] = (byte)((uint)((int)Buffer) >> 8 & 255);
			int Result = this.Write(Buf, C);
			if (C < Count)
			{
				Result += this.Skip(Count - C);
			}
			return Result;
		}
		public int Write([In] ushort Buffer)
		{
			return this.Write(new byte[]
			{
				(byte)(Buffer & 255), 
				(byte)((uint)Buffer >> 8 & 255u)
			}, 2);
		}
		public int Write([In] ushort Buffer, int Count)
		{
			byte[] Buf = new byte[2];
			int C = Count;
			if (C > 2)
			{
				C = 2;
			}
			Buf[0] = (byte)(Buffer & 255);
			Buf[1] = (byte)((uint)Buffer >> 8 & 255u);
			int Result = this.Write(Buf, C);
			if (C < Count)
			{
				Result += this.Skip(Count - C);
			}
			return Result;
		}
		public int Write([In] int Buffer)
		{
			return this.Write(new byte[]
			{
				(byte)(Buffer & 255), 
				(byte)((uint)Buffer >> 8 & 255), 
				(byte)((uint)Buffer >> 16 & 255), 
				(byte)((uint)Buffer >> 24 & 255)
			}, 4);
		}
		public int Write([In] int Buffer, int Count)
		{
			byte[] Buf = new byte[4];
			int C = Count;
			if (C > 4)
			{
				C = 4;
			}
			Buf[0] = (byte)(Buffer & 255);
			Buf[1] = (byte)((uint)Buffer >> 8 & 255);
			Buf[2] = (byte)((uint)Buffer >> 16 & 255);
			Buf[3] = (byte)((uint)Buffer >> 24 & 255);
			int Result = this.Write(Buf, C);
			if (C < Count)
			{
				Result += this.Skip(Count - C);
			}
			return Result;
		}
		public int Write([In] uint Buffer)
		{
			return this.Write(new byte[]
			{
				(byte)(Buffer & 255u), 
				(byte)(Buffer >> 8 & 255u), 
				(byte)(Buffer >> 16 & 255u), 
				(byte)(Buffer >> 24 & 255u)
			}, 4);
		}
		public int Write([In] uint Buffer, int Count)
		{
			byte[] Buf = new byte[4];
			int C = Count;
			if (C > 4)
			{
				C = 4;
			}
			Buf[0] = (byte)(Buffer & 255u);
			Buf[1] = (byte)(Buffer >> 8 & 255u);
			Buf[2] = (byte)(Buffer >> 16 & 255u);
			Buf[3] = (byte)(Buffer >> 24 & 255u);
			int Result = this.Write(Buf, C);
			if (C < Count)
			{
				Result += this.Skip(Count - C);
			}
			return Result;
		}
		public int Write([In] long Buffer)
		{
			return this.Write(BitConverter.GetBytes(Buffer), 8);
		}
		public int Write([In] long Buffer, int Count)
		{
			int C = Count;
			if (C > 8)
			{
				C = 8;
			}
			int Result = this.Write(BitConverter.GetBytes(Buffer), C);
			if (C < Count)
			{
				Result += this.Skip(Count - C);
			}
			return Result;
		}
		public int Write([In] ulong Buffer)
		{
			return this.Write(BitConverter.GetBytes(Buffer), 8);
		}
		public int Write([In] ulong Buffer, int Count)
		{
			int C = Count;
			if (C > 8)
			{
				C = 8;
			}
			int Result = this.Write(BitConverter.GetBytes(Buffer), C);
			if (C < Count)
			{
				Result += this.Skip(Count - C);
			}
			return Result;
		}
		public int Write([In] float Buffer)
		{
			return this.Write(BitConverter.GetBytes(Buffer), 4);
		}
		public int Write([In] float Buffer, int Count)
		{
			int C = Count;
			if (C > 4)
			{
				C = 4;
			}
			int Result = this.Write(BitConverter.GetBytes(Buffer), C);
			if (C < Count)
			{
				Result += this.Skip(Count - C);
			}
			return Result;
		}

		public int Write([In] double Buffer)
		{
			return this.Write(BitConverter.GetBytes(Buffer), 8);
		}

		public int Write([In] double Buffer, int Count)
		{
			int C = Count;
			if (C > 8)
			{
				C = 8;
			}
			int Result = this.Write(BitConverter.GetBytes(Buffer), C);
			if (C < Count)
			{
				Result += this.Skip(Count - C);
			}
			return Result;
		}
		public abstract long Seek([In] long Offset, TSeekOrigin Origin);
		public void ReadBuffer(byte[] Buffer, int Count)
		{
			if (Count != 0 && this.Read(ref Buffer, Count) != Count)
			{
				throw new EReadError("Stream read error");
			}
		}
		public void ReadBuffer(ref byte Buffer)
		{
			if (this.Read(ref Buffer) != 1)
			{
				throw new EReadError("Stream read error");
			}
		}
		public void ReadBuffer(ref byte Buffer, int Count)
		{
			if (Count != 0 && this.Read(ref Buffer, Count) != Count)
			{
				throw new EReadError("Stream read error");
			}
		}
		public void ReadBuffer(ref bool Buffer)
		{
			if (this.Read(ref Buffer) != 1)
			{
				throw new EReadError("Stream read error");
			}
		}
		public void ReadBuffer(ref bool Buffer, int Count)
		{
			if (Count != 0 && this.Read(ref Buffer, Count) != Count)
			{
				throw new EReadError("Stream read error");
			}
		}
		public void ReadBuffer(ref char Buffer)
		{
			if (this.Read(ref Buffer) != 2)
			{
				throw new EReadError("Stream read error");
			}
		}
		public void ReadBuffer(ref char Buffer, int Count)
		{
			if (Count != 0 && this.Read(ref Buffer, Count) != Count)
			{
				throw new EReadError("Stream read error");
			}
		}
		public void ReadBuffer(ref sbyte Buffer)
		{
			if (this.Read(ref Buffer) != 1)
			{
				throw new EReadError("Stream read error");
			}
		}
		public void ReadBuffer(ref sbyte Buffer, int Count)
		{
			if (Count != 0 && this.Read(ref Buffer, Count) != Count)
			{
				throw new EReadError("Stream read error");
			}
		}
		public void ReadBuffer(ref short Buffer)
		{
			if (this.Read(ref Buffer) != 2)
			{
				throw new EReadError("Stream read error");
			}
		}
		public void ReadBuffer(ref short Buffer, int Count)
		{
			if (Count != 0 && this.Read(ref Buffer, Count) != Count)
			{
				throw new EReadError("Stream read error");
			}
		}
		public void ReadBuffer(ref ushort Buffer)
		{
			if (this.Read(ref Buffer) != 2)
			{
				throw new EReadError("Stream read error");
			}
		}
		public void ReadBuffer(ref ushort Buffer, int Count)
		{
			if (Count != 0 && this.Read(ref Buffer, Count) != Count)
			{
				throw new EReadError("Stream read error");
			}
		}
		public void ReadBuffer(ref int Buffer)
		{
			if (this.Read(ref Buffer) != 4)
			{
				throw new EReadError("Stream read error");
			}
		}
		public void ReadBuffer(ref int Buffer, int Count)
		{
			if (Count != 0 && this.Read(ref Buffer, Count) != Count)
			{
				throw new EReadError("Stream read error");
			}
		}
		public void ReadBuffer(ref uint Buffer)
		{
			if (this.Read(ref Buffer) != 4)
			{
				throw new EReadError("Stream read error");
			}
		}
		public void ReadBuffer(ref uint Buffer, int Count)
		{
			if (Count != 0 && this.Read(ref Buffer, Count) != Count)
			{
				throw new EReadError("Stream read error");
			}
		}
		public void ReadBuffer(ref long Buffer)
		{
			if (this.Read(ref Buffer) != 8)
			{
				throw new EReadError("Stream read error");
			}
		}
		public void ReadBuffer(ref long Buffer, int Count)
		{
			if (Count != 0 && this.Read(ref Buffer, Count) != Count)
			{
				throw new EReadError("Stream read error");
			}
		}
		public void ReadBuffer(ref ulong Buffer)
		{
			if (this.Read(ref Buffer) != 8)
			{
				throw new EReadError("Stream read error");
			}
		}
		public void ReadBuffer(ref ulong Buffer, int Count)
		{
			if (Count != 0 && this.Read(ref Buffer, Count) != Count)
			{
				throw new EReadError("Stream read error");
			}
		}
		public void ReadBuffer(ref float Buffer)
		{
			if (this.Read(ref Buffer) != 4)
			{
				throw new EReadError("Stream read error");
			}
		}
		public void ReadBuffer(ref float Buffer, int Count)
		{
			if (Count != 0 && this.Read(ref Buffer, Count) != Count)
			{
				throw new EReadError("Stream read error");
			}
		}
		public void ReadBuffer(ref double Buffer)
		{
			if (this.Read(ref Buffer) != 8)
			{
				throw new EReadError("Stream read error");
			}
		}
		public void ReadBuffer(ref double Buffer, int Count)
		{
			if (Count != 0 && this.Read(ref Buffer, Count) != Count)
			{
				throw new EReadError("Stream read error");
			}
		}
		public void WriteBuffer([In] byte[] Buffer, int Count)
		{
			if (Count != 0 && this.Write(Buffer, Count) != Count)
			{
				throw new EWriteError("Stream write error");
			}
		}
		public void WriteBuffer([In] byte Buffer)
		{
			if (this.Write(Buffer) != 1)
			{
				throw new EWriteError("Stream write error");
			}
		}
		public void WriteBuffer([In] byte Buffer, int Count)
		{
			if (Count != 0 && this.Write(Buffer, Count) != Count)
			{
				throw new EWriteError("Stream write error");
			}
		}
		public void WriteBuffer([In] bool Buffer)
		{
			if (this.Write(Buffer) != 1)
			{
				throw new EWriteError("Stream write error");
			}
		}
		public void WriteBuffer([In] bool Buffer, int Count)
		{
			if (Count != 0 && this.Write(Buffer, Count) != Count)
			{
				throw new EWriteError("Stream write error");
			}
		}
		public void WriteBuffer([In] char Buffer)
		{
			if (this.Write(Buffer) != 2)
			{
				throw new EWriteError("Stream write error");
			}
		}
		public void WriteBuffer([In] char Buffer, int Count)
		{
			if (Count != 0 && this.Write(Buffer, Count) != Count)
			{
				throw new EWriteError("Stream write error");
			}
		}
		public void WriteBuffer([In] sbyte Buffer)
		{
			if (this.Write(Buffer) != 1)
			{
				throw new EWriteError("Stream write error");
			}
		}
		public void WriteBuffer([In] sbyte Buffer, int Count)
		{
			if (Count != 0 && this.Write(Buffer, Count) != Count)
			{
				throw new EWriteError("Stream write error");
			}
		}
		public void WriteBuffer([In] short Buffer)
		{
			if (this.Write(Buffer) != 2)
			{
				throw new EWriteError("Stream write error");
			}
		}
		public void WriteBuffer([In] short Buffer, int Count)
		{
			if (Count != 0 && this.Write(Buffer, Count) != Count)
			{
				throw new EWriteError("Stream write error");
			}
		}
		public void WriteBuffer([In] ushort Buffer)
		{
			if (this.Write(Buffer) != 2)
			{
				throw new EWriteError("Stream write error");
			}
		}
		public void WriteBuffer([In] ushort Buffer, int Count)
		{
			if (Count != 0 && this.Write(Buffer, Count) != Count)
			{
				throw new EWriteError("Stream write error");
			}
		}
		public void WriteBuffer([In] int Buffer)
		{
			if (this.Write(Buffer, 4) != 4)
			{
				throw new EWriteError("Stream write error");
			}
		}
		public void WriteBuffer([In] int Buffer, int Count)
		{
			if (Count != 0 && this.Write(Buffer, Count) != Count)
			{
				throw new EWriteError("Stream write error");
			}
		}
		public void WriteBuffer([In] uint Buffer)
		{
			if (this.Write(Buffer) != 4)
			{
				throw new EWriteError("Stream write error");
			}
		}
		public void WriteBuffer([In] uint Buffer, int Count)
		{
			if (Count != 0 && this.Write(Buffer, Count) != Count)
			{
				throw new EWriteError("Stream write error");
			}
		}
		public void WriteBuffer([In] long Buffer)
		{
			if (this.Write(Buffer) != 8)
			{
				throw new EWriteError("Stream write error");
			}
		}
		public void WriteBuffer([In] long Buffer, int Count)
		{
			if (Count != 0 && this.Write(Buffer, Count) != Count)
			{
				throw new EWriteError("Stream write error");
			}
		}
		public void WriteBuffer([In] ulong Buffer)
		{
			if (this.Write(Buffer) != 8)
			{
				throw new EWriteError("Stream write error");
			}
		}
		public void WriteBuffer([In] ulong Buffer, int Count)
		{
			if (Count != 0 && this.Write(Buffer, Count) != Count)
			{
				throw new EWriteError("Stream write error");
			}
		}
		public void WriteBuffer([In] float Buffer)
		{
			if (this.Write(Buffer) != 4)
			{
				throw new EWriteError("Stream write error");
			}
		}
		public void WriteBuffer([In] float Buffer, int Count)
		{
			if (Count != 0 && this.Write(Buffer, Count) != Count)
			{
				throw new EWriteError("Stream write error");
			}
		}
		public void WriteBuffer([In] double Buffer)
		{
			if (this.Write(Buffer) != 8)
			{
				throw new EWriteError("Stream write error");
			}
		}
		public void WriteBuffer([In] double Buffer, int Count)
		{
			if (Count != 0 && this.Write(Buffer, Count) != Count)
			{
				throw new EWriteError("Stream write error");
			}
		}
		public long CopyFrom(TStream Source, long Count)
		{
			if (Count == (long)((ulong)0))
			{
				Source.Position = (long)((ulong)0);
				Count = Source.Size;
			}
			long Result = Count;
			int BufSize;
			if (Count > (long)((ulong)61440))
			{
				BufSize = 61440;
			}
			else
			{
				BufSize = (int)Count;
			}
			byte[] Buffer = null;
			byte[] array = Buffer;
			int arg_35_0;
			if ((arg_35_0 = BufSize) < 0)
			{
				arg_35_0 = 0;
			}
			byte[] array2;
			byte[] expr_3A = array2 = new byte[arg_35_0];
			if (BufSize > 0 && array != null)
			{
				int num;
				if ((num = array.Length) > BufSize)
				{
					num = BufSize;
				}
				if (num > 0)
				{
					Array.Copy(array, array2, num);
				}
			}
			Buffer = expr_3A;
			while (Count != (long)((ulong)0))
			{
				int N;
				if (Count > (long)BufSize)
				{
					N = BufSize;
				}
				else
				{
					N = (int)Count;
				}
				Source.ReadBuffer(Buffer, N);
				this.WriteBuffer(Buffer, N);
				Count -= (long)N;
			}
			return Result;
		}
		public static implicit operator Stream([In] TStream Value)
		{
			Stream Result;
			if (Value is TCLRStreamWrapper)
			{
				Result = (Value as TCLRStreamWrapper).Handle;
			}
			else
			{
				Result = new TStreamToCLRStream(Value);
			}
			return Result;
		}
		public static implicit operator TStream([In] Stream Value)
		{
			return new TCLRStreamWrapper(Value);
		}

		public void Free()
		{
			TObjectHelper.Free(this);
		}

		public TStream()
		{
		}
	}
}
