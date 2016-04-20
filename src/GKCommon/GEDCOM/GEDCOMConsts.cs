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
			
			public GEDCOMAppFormat(string sign, string name)
            {
				this.Sign = sign;
				this.Name = name;
			}
		}

		public static readonly GEDCOMAppFormat[] GEDCOMFormats;

		static GEDCOMConsts()
		{
			GEDCOMFormats = new GEDCOMAppFormat[] {
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
