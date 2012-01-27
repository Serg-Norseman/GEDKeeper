using System;
using System.IO;

using GedCom551;
using GKCore.Settings;
using GKSys;

/// <summary>
/// Localization: unknown
/// </summary>

namespace GKCore.IO
{
	public abstract class Exporter : IDisposable
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

		public Exporter(TGenEngine aEngine, string aPath)
		{
			this.FEngine = aEngine;
			this.FTree = this.FEngine.Tree;
			this.FPath = aPath;
			if (!Directory.Exists(this.FPath)) Directory.CreateDirectory(this.FPath);
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
