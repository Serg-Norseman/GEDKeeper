using System;

namespace GKCommon.GEDCOM
{
	/// <summary>
	/// GEDCOMConsts.
	/// </summary>
	public static class GEDCOMConsts
	{
		public struct GEDCOMAppFormat
		{
			public string Sign;
			public string Name;
			
			public GEDCOMAppFormat(string aSign, string aName) {
				this.Sign = aSign;
				this.Name = aName;
			}
		}

		public static readonly GEDCOMAppFormat[] GEDCOMFormats;

		static GEDCOMConsts()
		{
			GEDCOMFormats = new GEDCOMAppFormat[6] {
				new GEDCOMAppFormat("", ""),
				new GEDCOMAppFormat("GEDKeeper", ""),
				new GEDCOMAppFormat("GENBOX", "Genbox Family History"),
				new GEDCOMAppFormat("ALTREE", "Agelong Tree"),
				new GEDCOMAppFormat("AGES", "Ages!"),
				new GEDCOMAppFormat("PAF", "Personal Ancestral File")
			};
		}
	}
}
