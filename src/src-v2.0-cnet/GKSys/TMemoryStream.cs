using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GKSys
{

	public class TMemoryStream : TCustomMemoryStream
	{
		protected internal int Capacity
		{
			get
			{
				return this.GetCapacity();
			}
			set
			{
				this.SetCapacity(value);
			}
		}
		internal int GetCapacity()
		{
			int Result;
			if (this.FMemory != null)
			{
				Result = this.FMemory.Length;
			}
			else
			{
				Result = 0;
			}
			return Result;
		}
		internal void SetCapacity(int NewCapacity)
		{
			this.FMemory = this.Realloc(ref NewCapacity);
		}
		protected internal virtual byte[] Realloc(ref int NewCapacity)
		{
			if (NewCapacity > 0 && (long)NewCapacity != this.FSize)
			{
				NewCapacity = (NewCapacity + 8191 & -8192);
			}
			byte[] Result = this.FMemory;
			if (NewCapacity != ((Result != null) ? Result.Length : 0))
			{
				byte[] arg_38_0 = Result;
				int num = NewCapacity;
				byte[] array = arg_38_0;
				int arg_40_0;
				if ((arg_40_0 = num) < 0)
				{
					arg_40_0 = 0;
				}
				byte[] array2;
				byte[] expr_45 = array2 = new byte[arg_40_0];
				if (num > 0 && array != null)
				{
					int num2;
					if ((num2 = array.Length) > num)
					{
						num2 = num;
					}
					if (num2 > 0)
					{
						Array.Copy(array, array2, num2);
					}
				}
				Result = expr_45;
			}
			return Result;
		}
		protected internal override void SetSize(long NewSize)
		{
			int OldPosition = (int)this.FPosition;
			this.SetCapacity((int)NewSize);
			this.FSize = NewSize;
			if ((long)OldPosition > NewSize)
			{
				this.Seek((long)((ulong)0), TSeekOrigin.soEnd);
			}
		}
		public void Clear()
		{
			this.SetCapacity(0);
			this.FSize = (long)((ulong)0);
			this.FPosition = (long)((ulong)0);
		}
		public void LoadFromStream(TStream Stream)
		{
			Stream.Position = (long)((ulong)0);
			int Count = (int)Stream.Size;
			this.SetSize((long)Count);
			if (Count != 0)
			{
				Stream.ReadBuffer(this.FMemory, Count);
			}
		}
		public void LoadFromFile([In] string FileName)
		{
			TStream Stream = new TFileStream(FileName, 32);
			try
			{
				this.LoadFromStream(Stream);
			}
			finally
			{
				Stream.Free();
			}
		}
		public override int Write([In] byte[] Buffer, int Offset, int Count)
		{
			int Result;
			if (this.FPosition >= (long)((ulong)0) && Count >= 0)
			{
				int Pos = (int)(this.FPosition + (long)Count);
				if (Pos > 0)
				{
					if ((long)Pos > this.FSize)
					{
						int arg_38_0 = Pos;
						byte[] memory = base.Memory;
						if (arg_38_0 > ((memory != null) ? memory.Length : 0))
						{
							this.SetCapacity(Pos);
						}
						this.FSize = (long)Pos;
					}
					Array.Copy(Buffer, (long)Offset, base.Memory, this.FPosition, (long)Count);
					this.FPosition = (long)Pos;
					Result = Count;
					return Result;
				}
			}
			Result = 0;
			return Result;
		}
	}
}
