using System;
using System.IO;
using GedCom551;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKCore
{
	public abstract class Exporter : IDisposable
	{
		protected TGenEngine FEngine;
		protected GlobalOptions FOptions;
		protected string FPath;
		protected TGEDCOMTree FTree;
		protected bool Disposed_;

		public GlobalOptions Options
		{
			get { return this.FOptions; }
			set { this.FOptions = value; }
		}

		public Exporter(TGenEngine aEngine)
		{
			this.FEngine = aEngine;
			this.FTree = this.FEngine.Tree;
		}

		public Exporter(TGenEngine aEngine, string path)
		{
			this.FEngine = aEngine;
			this.FTree = this.FEngine.Tree;
			this.FPath = path;
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
