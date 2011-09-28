using System;
using System.ComponentModel;
using System.IO;

using GedCom551;
using GKCore.Sys;

namespace GKCore
{
	public abstract class TExporter : IDisposable
	{
		protected TGenEngine FEngine;
		protected TGlobalOptions FOptions;
		protected string FPath;
		protected TGEDCOMTree FTree;
		protected bool Disposed_;

		public TGlobalOptions Options
		{
			get { return this.FOptions; }
			set { this.FOptions = value; }
		}

		public TExporter(TGenEngine aEngine, string aPath)
		{
			this.FEngine = aEngine;
			this.FTree = this.FEngine.Tree;
			this.FPath = aPath;
			if (!Directory.Exists(this.FPath))
			{
				SysUtils.CreateDir(this.FPath);
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
	}
}
