using GKSys;
using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GKSys
{
	[TUInt32Subrange(0u, 255u, typeof(AnsiChar))]
	public enum AnsiChar : byte
	{}

	[StructLayout(LayoutKind.Auto)]
	public struct AnsiString
	{
		public byte[] Data;

		[System.Runtime.CompilerServices.IndexerName("Chars")]
		public AnsiChar this[int At]
		{
			get { return (AnsiChar)this.Data[At - 1]; }
			set { BDSSystem.LStrSetElem(ref this.Data, At, value); }
		}

		public AnsiString(byte[] AData)
		{
			this.Data = AData;
		}

		public override string ToString()
		{
			return BDSSystem.WStrFromLStr(this.Data);
		}

		public int Length()
		{
			return BDSSystem.LStrLen(this.Data);
		}

		public static implicit operator AnsiString(string Value)
		{
			AnsiString result;
			result.Data = BDSSystem.LStrFromWStr(Value);
			return result;
		}
	}
}
