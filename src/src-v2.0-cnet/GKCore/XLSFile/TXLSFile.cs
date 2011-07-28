using GKSys;
using System;
using System.Runtime.InteropServices;

namespace XLSFile
{
	public class TXLSFile : IDisposable
	{
		internal TBOF BOF;
		internal TDimension FDimension;
		internal TList FList;
		internal TBIFFWriter FWriter;
		internal ushort OpcodeEOF;
		protected internal bool Disposed_;

		internal TCell AddCell(ushort vCol, ushort vRow, TCellAttributeSet vAttribute, Type CellRef)
		{
			TCell C = Activator.CreateInstance(CellRef) as TCell;
			C.Col = (ushort)((int)vCol - 1);
			C.Row = (ushort)((int)vRow - 1);
			C.Attribute = vAttribute;
			this.RegisterObj(C);
			return C;
		}

		internal void RegisterObj(TData MyPers)
		{
			this.FList.Add(MyPers);
		}

		public TXLSFile()
		{
			this.OpcodeEOF = 10;
			this.FList = new TList();
			this.FWriter = new TBIFFWriter();
			this.Clear();
		}

		public void Dispose()
		{
			if (!this.Disposed_)
			{
				this.Clear();
				this.FList.Free();
				this.FWriter.Free();
				this.Disposed_ = true;
			}
		}

		public void AddWordCell(ushort vCol, ushort vRow, TCellAttributeSet vAttribute, ushort aValue)
		{
			TWordCell tWordCell = this.AddCell(vCol, vRow, vAttribute, typeof(TWordCell)) as TWordCell;
			tWordCell.Value = aValue;
		}

		public void AddIntegerCell(ushort vCol, ushort vRow, TCellAttributeSet vAttribute, int aValue)
		{
			TIntegerCell tIntegerCell = this.AddCell(vCol, vRow, vAttribute, typeof(TIntegerCell)) as TIntegerCell;
			tIntegerCell.Value = aValue;
		}

		public void AddDoubleCell(ushort vCol, ushort vRow, TCellAttributeSet vAttribute, double aValue)
		{
			TDoubleCell tDoubleCell = this.AddCell(vCol, vRow, vAttribute, typeof(TDoubleCell)) as TDoubleCell;
			tDoubleCell.Value = aValue;
		}

		public void AddStrCell(ushort vCol, ushort vRow, TCellAttributeSet vAttribute, string aValue)
		{
			TStrCell tStrCell = this.AddCell(vCol, vRow, vAttribute, typeof(TStrCell)) as TStrCell;
			tStrCell.Value = aValue;
		}

		public void Clear()
		{
			int num = this.FList.Count - 1;
			int i = 0;
			if (num >= i)
			{
				num++;
				do
				{
					(this.FList[i] as TData).Free();
					i++;
				}
				while (i != num);
			}
			this.FList.Clear();
			this.BOF = new TBOF();
			this.FDimension = new TDimension();
			this.RegisterObj(this.BOF);
			this.RegisterObj(this.FDimension);
		}

		public void SaveToFile([In] string FileName)
		{
			TFileStream stream = new TFileStream(FileName, 65535);
			try
			{
				this.FWriter.Stream = stream;
				int num = this.FList.Count - 1;
				int i = 0;
				if (num >= i)
				{
					num++;
					do
					{
						TData data = this.FList[i] as TData;
						this.FWriter.WriteWord(data.opCode);
						this.FWriter.WriteWord(0);
						int pos = (int)stream.Position;
						data.Write(this.FWriter);
						int length = (int)(stream.Position - (long)pos);
						stream.Seek((long)(-(long)(length + 2)), TSeekOrigin.soCurrent);
						this.FWriter.WriteWord((ushort)length);
						stream.Seek((long)length, TSeekOrigin.soCurrent);
						i++;
					}
					while (i != num);
				}
				this.FWriter.WriteWord(this.OpcodeEOF);
				this.FWriter.WriteWord(0);
			}
			finally
			{
				stream.Free();
			}
		}

		public void Free()
		{
			TObjectHelper.Free(this);
		}

	}
}
