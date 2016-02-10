using System;
using System.Collections.Generic;

using BSLib;
using GKCommon.GEDCOM;

namespace GKCore.Tools
{
	/// <summary>
	/// 
	/// </summary>
	public sealed class PlaceObj : BaseObject
	{
		public string Name;
		public readonly List<GEDCOMCustomEvent> Facts;

		public PlaceObj()
		{
			this.Facts = new List<GEDCOMCustomEvent>();
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				//this.Facts.Dispose();
			}
			base.Dispose(disposing);
		}
	}
}
