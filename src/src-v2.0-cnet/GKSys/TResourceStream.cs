using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Security.Permissions;

namespace GKSys
{

	public class EResNotFound : Exception
	{
		public EResNotFound()
		{
		}
		public EResNotFound(string message) : base(message)
		{
		}
		public EResNotFound(string message, Exception innerException) : base(message, innerException)
		{
		}
	}


	[SecurityPermission(SecurityAction.LinkDemand, UnmanagedCode=true)]
	public class TResourceStream : TCustomMemoryStream
	{
		public enum _72 : byte
		{
			_72_Value
		}

		public enum _82 : byte
		{
			_82_Value
		}

		internal int HResInfo;
		internal int HGlobal;

		internal void Initialize(int Instance, int ResInfo, [In] string Name)
		{
			this.HResInfo = ResInfo;
			if (this.HResInfo == 0)
			{
				TResourceStream._Initialize_Error(ref Name);
			}
			this.HGlobal = VCLUtils.LoadResource(Instance, this.HResInfo);
			if (this.HGlobal == 0)
			{
				TResourceStream._Initialize_Error(ref Name);
			}
			try
			{
				this.SetSize((long)((ulong)VCLUtils.SizeofResource(Instance, this.HResInfo)));
				Marshal.Copy(new IntPtr(this.HGlobal), this.FMemory, 0, (int)this.FSize);
			}
			finally
			{
				VCLUtils.FreeResource(this.HGlobal);
			}
		}

		protected internal override void SetSize(long NewSize)
		{
			int OldPosition = (int)this.FPosition;
			this.FSize = NewSize;
			byte[] arg_1E_0 = this.FMemory;
			int num = (int)this.FSize;
			byte[] array = arg_1E_0;
			int arg_26_0;
			if ((arg_26_0 = num) < 0)
			{
				arg_26_0 = 0;
			}
			byte[] array2;
			byte[] expr_2B = array2 = new byte[arg_26_0];
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
			this.FMemory = expr_2B;
			if ((long)OldPosition > this.FSize)
			{
				this.Seek((long)((ulong)0), TSeekOrigin.soEnd);
			}
		}
		public TResourceStream(int Instance, [In] string ResName, int ResType)
		{
			this.Initialize(Instance, VCLUtils.FindResource(Instance, ResName, ResType), ResName);
		}
		public TResourceStream(int Instance, [In] string ResName, [In] string ResType)
		{
			this.Initialize(Instance, VCLUtils.FindResource(Instance, ResName, ResType), ResName);
		}
		public TResourceStream(int Instance, int ResID, int ResType, [In] TResourceStream._72 _Dummy)
		{
			this.Initialize(Instance, VCLUtils.FindResource(Instance, ResID, ResType), ResID.ToString());
		}
		public TResourceStream(int Instance, int ResID, string ResType, [In] TResourceStream._82 _Dummy)
		{
			this.Initialize(Instance, VCLUtils.FindResource(Instance, ResID, ResType), ResID.ToString());
		}
		public override int Write([In] byte[] Buffer, int Offset, int Count)
		{
			throw new EStreamError("Can't write to a read-only resource stream");
		}

		public TResourceStream()
		{
		}

		private static void _Initialize_Error([In] ref string Name)
		{
			throw new EResNotFound(string.Format("Resource %s not found", new object[] { Name }));
		}
	}
}
