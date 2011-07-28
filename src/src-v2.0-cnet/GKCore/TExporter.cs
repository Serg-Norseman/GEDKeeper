using GedCom551;
using GKCore;
using GKSys;
using System;
using System.ComponentModel;
using System.IO;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GKCore
{
	[TSetElementType(typeof(AnsiChar))]
	public class C6 : TUniqueTypeModifier
	{
	}

	[TSetElementType(typeof(AnsiChar))]
	public class C8 : TUniqueTypeModifier
	{
	}

	public abstract class TExporter : IDisposable
	{
		internal TGenEngine FEngine;
		internal TGlobalOptions FOptions;
		internal string FPath;
		internal TGEDCOMTree FTree;
		protected internal bool Disposed_;
		[Browsable(false)]
		public TGlobalOptions Options
		{
			get
			{
				return this.FOptions;
			}
			set
			{
				this.FOptions = value;
			}
		}
		public TExporter(TGenEngine aEngine, string aPath)
		{
			this.FEngine = aEngine;
			this.FTree = this.FEngine.Tree;
			this.FPath = aPath;
			if (!Directory.Exists(this.FPath))
			{
				VCLUtils.CreateDir(this.FPath);
			}
		}

		public void Dispose()
		{
			if (!this.Disposed_)
			{
				this.Disposed_ = true;
			}
		}

		public abstract void Generate();
		public void Free()
		{
			TObjectHelper.Free(this);
		}
	}
}
