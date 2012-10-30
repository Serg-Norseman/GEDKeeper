using System;
using System.IO;
using System.Runtime.InteropServices;

using Ext.Utils;

namespace Ext.XLSFile
{
	public sealed class TXLSFile : IDisposable
	{
		private TBOF BOF;
		private TDimension FDimension;
		private TList FList;
		private TBIFFWriter FWriter;
		private ushort OpcodeEOF;
		private bool Disposed_;

		private TCell AddCell(ushort vCol, ushort vRow, TCellAttributeSet vAttribute, Type CellRef)
		{
			TCell C = Activator.CreateInstance(CellRef) as TCell;
			C.Col = (ushort)(vCol - 1);
			C.Row = (ushort)(vRow - 1);
			C.Attribute = vAttribute;
			this.RegisterObj(C);
			return C;
		}

		private void RegisterObj(TData MyPers)
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
				this.FList.Dispose();
				//this.FWriter.Dispose();
				this.Disposed_ = true;
			}
		}

		public void AddWordCell(ushort vCol, ushort vRow, TCellAttributeSet vAttribute, ushort aValue)
		{
			TWordCell wCell = this.AddCell(vCol, vRow, vAttribute, typeof(TWordCell)) as TWordCell;
			wCell.Value = aValue;
		}

		public void AddIntegerCell(ushort vCol, ushort vRow, TCellAttributeSet vAttribute, int aValue)
		{
			TIntegerCell iCell = this.AddCell(vCol, vRow, vAttribute, typeof(TIntegerCell)) as TIntegerCell;
			iCell.Value = aValue;
		}

		public void AddDoubleCell(ushort vCol, ushort vRow, TCellAttributeSet vAttribute, double aValue)
		{
			TDoubleCell dCell = this.AddCell(vCol, vRow, vAttribute, typeof(TDoubleCell)) as TDoubleCell;
			dCell.Value = aValue;
		}

		public void AddStrCell(ushort vCol, ushort vRow, TCellAttributeSet vAttribute, string aValue)
		{
			TStrCell sCell = this.AddCell(vCol, vRow, vAttribute, typeof(TStrCell)) as TStrCell;
			sCell.Value = aValue;
		}

		public void Clear()
		{
			int num = this.FList.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				(this.FList[i] as TData).Free();
			}
			this.FList.Clear();

			this.BOF = new TBOF();
			this.FDimension = new TDimension();
			this.RegisterObj(this.BOF);
			this.RegisterObj(this.FDimension);
		}

		public void SaveToFile([In] string FileName)
		{
			using (FileStream stream = new FileStream(FileName, FileMode.Create)) {
				this.FWriter.WStream = stream;

				int num = this.FList.Count - 1;
				for (int i = 0; i <= num; i++) {
					TData data = this.FList[i] as TData;
					this.FWriter.WriteWord(data.opCode);
					this.FWriter.WriteWord(0);

					int pos = (int)stream.Position;
					data.Write(this.FWriter);

					int length = (int)(stream.Position - pos);
					stream.Seek(-(length + 2), SeekOrigin.Current);

					this.FWriter.WriteWord((ushort)length);

					stream.Seek((long)length, SeekOrigin.Current);
				}

				this.FWriter.WriteWord(this.OpcodeEOF);
				this.FWriter.WriteWord(0);
			}
		}

		public void Free()
		{
			SysUtils.Free(this);
		}

	}
}
