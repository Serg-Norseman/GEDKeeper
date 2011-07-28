using System;
using System.ComponentModel;
using System.IO;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GKSys
{
	public class TFileStream : TCLRStreamWrapper
	{
		private string FFileName;
		[Browsable(false)]
		public string FileName
		{
			get
			{
				return this.FFileName;
			}
		}
		public TFileStream([In] string AFileName, ushort Mode) : this(AFileName, Mode, 0u)
		{
		}
		public TFileStream([In] string AFileName, ushort Mode, uint Rights) : base(null)
		{
			FileMode LMode;
			FileAccess LAccess;

			if (Mode == BDSSystem.fmCreate) {
				LMode = System.IO.FileMode.Create;
				LAccess = System.IO.FileAccess.ReadWrite;
			} else {
					LMode = System.IO.FileMode.Open;
					ushort m = (ushort)(Mode & 0xF);
					switch (m) {
						case BDSSystem.fmOpenReadWrite: 
							LAccess = System.IO.FileAccess.ReadWrite;
							break;
						case BDSSystem.fmOpenWrite: 
							LAccess = System.IO.FileAccess.Write;
							break;
						default:
							LAccess = System.IO.FileAccess.Read;
							break;
					}
			}

			ushort num2 = (ushort)(Mode & 240);
			FileShare LShare;
			switch (num2) {
				case BDSSystem.fmShareDenyWrite: 
					LShare = System.IO.FileShare.Read;
					break;
				case BDSSystem.fmShareDenyRead: 
					LShare = System.IO.FileShare.Write;
					break;
				case BDSSystem.fmShareDenyNone: 
					LShare = System.IO.FileShare.None;
					break;
				default:
					LShare = System.IO.FileShare.ReadWrite;
					break;
			}
			this.FHandle = new FileStream(AFileName, LMode, LAccess, LShare);
			this.FFileName = AFileName;
		}

		public TFileStream(Stream AHandle) : base(AHandle)
		{
		}
	}
}
